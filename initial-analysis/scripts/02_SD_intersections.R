# -------------------------------------------------------------------------
# 02_SD_intersections
# Uses sangis data portal street data and creates dataset of intersections
# across the city
# -------------------------------------------------------------------------

# load libraries
library(dplyr)
library(readr)
library(sf)
library(tidyr)
library(here)

# data too big to upload to github but from sangis
roads <- read.csv(here("initial-analysis", "data-raw","Roads_All.csv"))



# The roads data has a to node (TNODE) and from node (FNODE) for each road segment
# Can use these to find where the intersections of roads are
# Create a from_pts containing FNODE and to_points containing the TNODEs
# LEVEL is the level of the roadway (for example a bridge might be 2, 
#while a road under the highway is listed as 1)

from_pts <- roads |>
  transmute(
    NODE = FNODE, LEVEL = F_LEVEL, PTX = FRXCOORD, PTY = FRYCOORD,
    ROADSEGID, ROADID, SPEED, RD30FULL, FUNCLASS, LJURISDIC, RJURISDIC, ONEWAY
  )

to_pts <- roads |>
  transmute(
    NODE = TNODE, LEVEL = T_LEVEL, PTX = TOXCOORD, PTY = TOYCOORD,
    ROADSEGID, ROADID, SPEED, RD30FULL, FUNCLASS, LJURISDIC, RJURISDIC, ONEWAY
  )

# combine from_pts and to_pts  
all_endpoints <- bind_rows(from_pts, to_pts) |>
  filter(!is.na(NODE), !is.na(LEVEL))

# group by NODE and LEVEL to match the nodes that are of roadsegments in a
# shared intersection 
# calculate the min max and mean speed of streets in the intersection 
node_summary <- all_endpoints |>
  group_by(NODE, LEVEL) |>
  summarise(
    FREQUENCY       = n(),
    COUNT_ROADSEGID = n_distinct(ROADSEGID),
    UNIQUE_ROADID   = n_distinct(ROADID),
    MEAN_PTX        = mean(PTX, na.rm = TRUE),
    MEAN_PTY        = mean(PTY, na.rm = TRUE),
    SPEED_MIN       = min(SPEED, na.rm = TRUE),
    SPEED_MAX       = max(SPEED, na.rm = TRUE),
    SPEED_MEAN      = mean(SPEED, na.rm = TRUE),
    .groups = "drop"
  )

# remove points that are not intersections (removes dead ends, cul de sac etc)
# Filter so that only intersections with 2+ roadsegments and 2+ unique roads
# or intersections with 3+ roadsegments are kept. 
true_intersections <- node_summary |>
  filter((COUNT_ROADSEGID >= 2 & UNIQUE_ROADID >= 2) | COUNT_ROADSEGID >= 3)

# remove all roads that are private from intersection calculation 
real_road_counts <- all_endpoints |>
  inner_join(true_intersections |> select(NODE, LEVEL), by = c("NODE", "LEVEL")) |>
  filter(!RD30FULL %in% c("PRIVATE RD", "PRIVATE ALY", "PRIVATE DR")) |> 
  group_by(NODE, LEVEL) |>
  summarise(REAL_ROAD_COUNT = n_distinct(ROADID), .groups = "drop")

true_intersections <- true_intersections |>
  left_join(real_road_counts, by = c("NODE", "LEVEL")) |>
  mutate(REAL_ROAD_COUNT = replace_na(REAL_ROAD_COUNT, 0)) |>
  filter(REAL_ROAD_COUNT >= 2)


# create intersection address labels by combining the names of the intersection
# streets into one label
street_labels <- all_endpoints |>
  inner_join(true_intersections |>
               select(NODE, LEVEL), by = c("NODE", "LEVEL")) |>
  distinct(NODE, LEVEL, RD30FULL) |>
  filter(!is.na(RD30FULL), RD30FULL != "") |>
  group_by(NODE, LEVEL) |>
  summarise(
    STREETS = paste(sort(unique(RD30FULL)), collapse = " / "),
    .groups = "drop"
  )

# create the final table of intersections by joining based on NODE and LEVEL
final_intersections <- true_intersections |>
  left_join(street_labels, by = c("NODE", "LEVEL")) |>
  select(
    NODE, FREQUENCY, COUNT_ROADSEGID, UNIQUE_ROADID, MEAN_PTX, MEAN_PTY,
    SPEED_MIN, SPEED_MAX, SPEED_MEAN, STREETS
  )


# project to EPSG:2230 NAD83 California State Plane Zone 6, US feet 
intersections_sf <- st_as_sf(
  final_intersections,
  coords = c("MEAN_PTX", "MEAN_PTY"),
  crs = 2230,
  remove = FALSE
)

st_write(intersections_sf, here("initial-analysis","data-clean","SD_intersections.geojson"), delete_dsn = TRUE)
