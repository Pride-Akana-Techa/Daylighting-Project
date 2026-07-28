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
library(tidygeocoder)
library(dplyr)
library(readxl)
library(tigris)
library(leaflet)
# -------------------------------------------------------------------------
# data too big to upload to github but from sangis
roads <- read_sf(here("initial-analysis", "data-raw","Roads_all_shapefile","Roads_All.shp"))

# Remove roads/segments in the data that are not relevent to project
# There is no data dictionary, but this is what i found by looking on ArcGIS
# Select based on SEGCLASS - which classifies road type by a number or letter
# REMOVE:
# Z = Apartment / weird housing alley roads 
# Y = 1 random road segment 
# W = Walkways
# P = Sidewalk or something def not a road
# M = Military
# K = 4 random road segments 
# A = alleyways 
# 6 = private road
# 7 = private road (appartment/housing)
# 8 = highway on/off ramp
# 9 = highway on/off circle ramp things
# 
# KEEP:
# H = normal streets 
# 5 = normal residential streets (majority of all segments)
# 4 = larger residential streets 
# 3 = arterial road 
# 2 = highway transition 
# 1 = highway

# 164675 obs -> 104131 obs 
roads_filtered <- roads |>
  filter(SEGCLASS %in% c("H", "5", "4", "3", "2", "1"))

san_diego <- places(state = "CA", cb = TRUE, year = 2024) |>
  filter(NAME == "San Diego") |>
  st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |>
  slice_max(area, n = 1) |>
  select(-area) |>
  st_transform(2230)

roads_sd_city <- roads_filtered |>
  st_transform(2230) |> 
  st_filter(san_diego) 

roads_sd_city <- roads_sd_city |>
  st_drop_geometry()


# The roads data has a to node (TNODE) and from node (FNODE) for each road segment
# Can use these to find where the intersections of roads are
# Create a from_pts containing FNODE and to_points containing the TNODEs
# LEVEL is the level of the roadway (for example a bridge might be 2, 
# while a road under the highway is listed as 1). Whenever two segments
# cross, a new one is started, however two segments crossing does not 
# necessarily indicate an intersection 
from_pts <- roads_sd_city |>
  transmute(
    NODE = FNODE, LEVEL = F_LEVEL, PTX = FRXCOORD, PTY = FRYCOORD,
    ROADSEGID, ROADID, SPEED, RD30FULL, FUNCLASS, LJURISDIC, RJURISDIC, ONEWAY, 
    SEGCLASS
  )

to_pts <- roads_sd_city |>
  transmute(
    NODE = TNODE, LEVEL = T_LEVEL, PTX = TOXCOORD, PTY = TOYCOORD,
    ROADSEGID, ROADID, SPEED, RD30FULL, FUNCLASS, LJURISDIC, RJURISDIC, ONEWAY,
    SEGCLASS
  )

# combine from_pts and to_pts  
endpoints <- bind_rows(from_pts, to_pts) |>
  filter(!is.na(NODE), !is.na(LEVEL))

# group by NODE and LEVEL to match the nodes that are of roadsegments in a
# shared intersection 
# calculate the min max and mean speed of streets in the intersection 
node_summary <- endpoints |>
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
# Filter so that only intersections with 3+ roadsegments are kept. 
true_intersections <- node_summary |>
  filter(COUNT_ROADSEGID >= 3)

real_road_counts <- endpoints |>
  st_drop_geometry() |>
  inner_join(
    true_intersections |>
      st_drop_geometry() |>
      select(NODE, LEVEL),
    by = c("NODE", "LEVEL")) |>
  group_by(NODE, LEVEL) |>
  summarise(
    REAL_ROAD_COUNT = n_distinct(ROADID),
    .groups = "drop"
  )

true_intersections <- true_intersections |>
  left_join(real_road_counts, by = c("NODE", "LEVEL")) |>
  mutate(REAL_ROAD_COUNT = replace_na(REAL_ROAD_COUNT, 0)) |>
  filter(REAL_ROAD_COUNT >= 2)


# create intersection address labels by combining the names of the intersection
# streets into one label
street_labels <- endpoints |>
  st_drop_geometry() |>
  inner_join(
    true_intersections |>
      st_drop_geometry() |>
      select(NODE, LEVEL),
    by = c("NODE", "LEVEL")
  ) |>
  distinct(NODE, LEVEL, RD30FULL) |>
  filter(!is.na(RD30FULL), RD30FULL != "") |>
  group_by(NODE, LEVEL) |>
  summarise(
    STREETS = paste(sort(unique(RD30FULL)), collapse = " / "),
    .groups = "drop"
  )

# create the final table of intersections by joining based on NODE and LEVEL
final_intersections <- true_intersections |>
  st_drop_geometry() |>
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

intersections_leaflet <- intersections_sf |>
  st_transform(4326)

leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(data = intersections_leaflet)

st_write(intersections_sf, here("initial-analysis","data-clean","SD_intersections3.geojson"), delete_dsn = TRUE)




# -------------------------------------------------------------------------
# Geocoding San Diego department of transportation intersection
# daylighting database 


# Because they are intersections, the addresses given are two cross streets
sd_daylit_intersections <- read_excel(
  here("initial-analysis", "data-clean", "san_diego_daylight_intersections.xlsx")) |>
  mutate(INTERSECTION = paste0(`CROSS STREET 1`,
                               " and ", `CROSS STREET 2`,
                               ", San Diego, CA ", `ZIP CODE`)
  )

sd_daylit_int_data <- sd_daylit_intersections |>
  geocode(
    address = INTERSECTION, 
    method = "arcgis",
    lat = Latitude,       
    long = Longitude      
  )
saveRDS(sd_daylit_int_data, here("initial-analysis","data-clean","sd_daylit_int_data.rds"))
