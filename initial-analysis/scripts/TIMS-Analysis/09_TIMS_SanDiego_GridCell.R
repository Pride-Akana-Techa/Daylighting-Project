# -------------------------------------------------------------------------
# 06_TIMS_SanDiego_GridCell
# The current plan (if I do not get data from DOT)
# aggregate all data into grid cells to create a yearly panel dataset 
# clip the panel so that it only contains cells with roads in it

# 1. Dependent Variable - Pedestrian Crashes (later crash dollars probably)
# Pedestrian crashes per cell per year 
# 2. Intersection density - Constant, number of intersection nodes per cell
# 3. Ab-413 citations per cell by year after policy enforcement
# -------------------------------------------------------------------------



# load libraries ----------------------------------------------------------
library(dplyr)
library(tidygeocoder)
library(sf)
library(here)
library(leaflet)
library(leafgl)
library(tigris)
library(lubridate)
library(ggplot2)
library(lmtest)
library(sandwich)
library(stargazer)
library(scales)
library(Matrix)
library(readxl)
library(tidyr)
# load data ---------------------------------------------------------------

# Daylighting Intersection Data
# Information on what has been evaluated, if its been daylight or not
sd_daylit_int_data <- readRDS(here("initial-analysis","data-clean","SD_daylit_int_data.rds")) |>
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  ) |>
  st_transform(2230)

# Citation Data
# geocoder code in 01_SD_citation_geocoder.R
sd_citation_data <- readRDS(here("initial-analysis","data-clean","SD_citation_geocode.rds")) |>
  st_transform(2230)

# Crash Data
sd_crash_data <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
  filter(COUNTY == "SAN DIEGO" & (PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")) |>
  st_transform(2230)


# Intersection Data
sd_intersection_data <- st_read(here("initial-analysis", "data-clean", "SD_intersections.geojson")) |>
  st_transform(2230)

# Road Data - speed limit + one way/two way
sd_road_data <- sf::read_sf(here("initial-analysis", "data-raw", "Roads_All_shapefile","Roads_All.shp")) |>
  st_transform(2230)

# # Transit Data
# sd_transit_data 
# 
# # Vehicle Miles Traveled (VMT) Data
# sd_vmt_data


# San Diego boundary polygon
san_diego_boundary <- counties(state = "CA", cb = FALSE) |>
  filter(COUNTYFP == "073") |>
  st_transform(2230)

# San Diego Places boundary polygon
sd_cities_geo <- places(state = "CA", cb = FALSE) |>
  st_transform(2230) |>
  st_filter(san_diego_boundary, .predicate = st_intersects)

# San Diego City boundary polygon
san_diego_city <- sd_cities_geo |>
  filter(NAME == "San Diego")


san_diego_city_leaflet <- st_transform(san_diego_city, 4326)

sd_daylit_int_data_leaflet <- st_transform(sd_daylit_int_data, 4326)

sd_citation_data_leaflet <- st_transform(sd_citation_data, 4326)

leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_city_leaflet,
    fillColor = "lightblue",
    fillOpacity = 0.2,
    color = "black",
    weight = 2,
  ) |>
  addGlPoints(
    data = sd_daylit_int_data_leaflet,
    opacity = 1,
    radius = 6,
    label = ~`INTERSECTION EVALUATION`,
    fillColor = sd_daylit_int_data$`INTERSECTION EVALUATION`)



# daylighting status map --------------------------------------------------
evaluation_colors <- c(
  "Not Evaluated" = "#9E9E9E",
  "Evaluated - No Action" = "#D73027",
  "Evaluated - Created Work Order" = "#91CF60",
  "Evaluated - Added to Slurry Seal Project - New Red Curb" = "#966fd6",
  "Evaluated - Added to Slurry Seal Project - Existing Red Curb" = "#B3EBF2",
  "Evaluated - Coordinate with SuMo" = "#4575B4")

daylighting_intersection_map<- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_city_leaflet,
    fillColor = "lightblue",
    fillOpacity = 0.15,
    color = "black",
    weight = 2) |>
  addGlPoints(
    data = filter(
      sd_daylit_int_data_leaflet,
      `INTERSECTION EVALUATION` == "Not Evaluated"),
    opacity = 0.3,
    radius = 5,
    fillColor = "#9E9E9E") |>
    addGlPoints(
    data = filter(
      sd_daylit_int_data_leaflet,
      `INTERSECTION EVALUATION` != "Not Evaluated"),
    opacity = 1,
    radius = 7,
    label = ~`INTERSECTION EVALUATION`,
    fillColor = ~evaluation_colors[`INTERSECTION EVALUATION`]) |>
  addLegend(
    position = "bottomright",
    colors = evaluation_colors,
    labels = names(evaluation_colors),
    opacity = 1)

daylighting_intersection_map


priority_level <- c(
  "NA" = "#9E9E9E",
  "1"  = "#D73027",
  "2"  = "#91CF60",
  "3"  = "#966FD6",
  "4"  = "#B3EBF2",
  "5"  = "#4575B4",
  "6"  = "yellow"
)

# priority rankings
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_city_leaflet,
    fillColor = "lightblue",
    fillOpacity = 0.15,
    color = "black",
    weight = 2
  ) |>
  addGlPoints(
    data = filter(sd_daylit_int_data_leaflet, is.na(`PRIORITY LEVEL`)),
    opacity = 0.3,
    radius = 5,
    fillColor = priority_level["NA"]
  ) |>
  addGlPoints(
    data = filter(sd_daylit_int_data_leaflet, !is.na(`PRIORITY LEVEL`)),
    opacity = 1,
    radius = 7,
    label = ~as.character(`PRIORITY LEVEL`),
    fillColor = ~priority_level[as.character(`PRIORITY LEVEL`)]
  ) |>
  addLegend(
    position = "bottomright",
    colors = priority_level,
    labels = names(priority_level),
    opacity = 1
  )




# daylighting enforcement map ---------------------------------------------

# Find the intersection_id closest to each citation location
citation_nearest_int <- st_nearest_feature(sd_citation_data, sd_daylit_int_data)

matched_intersection_ids <- sd_daylit_int_data$ID[citation_nearest_int]

citation_counts <- as.data.frame(table(intersection_id = matched_intersection_ids))|> 
  rename(total_citations = Freq) |>
  mutate(intersection_id = as.numeric(as.character(intersection_id)))

num_citation_int <- sd_daylit_int_data |> 
  left_join(citation_counts, by = c("ID" = "intersection_id")) |> 
  mutate(total_citations = coalesce(total_citations, 0)) |>
  st_transform(4326)
  

citation_pal <- colorNumeric(
  palette = "YlOrRd", 
  domain = num_citation_int$total_citations
)

leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_city_leaflet,
    fillColor = "lightblue",
    fillOpacity = 0.15,
    color = "black",
    weight = 2) |>
  addGlPoints(
    data = num_citation_int,
    opacity = 1,
    radius = 6,
    fillColor = citation_pal(num_citation_int$total_citations)
  ) |> 
  addLegend(
    pal = citation_pal,
    values = num_citation_int$total_citations,
    position = "bottomright"
  )
# crash map --------------------------------------------------------------
sd_crash_data_map <- sd_crash_data |>
  st_filter(san_diego_city) |>
  st_transform(4326) 
library(leaflet.extras)


leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_city_leaflet,
    fillColor = "lightblue",
    fillOpacity = 0.15,
    color = "black",
    weight = 2) |>
  addGlPoints(
    data = num_citation_int,
    opacity = 1,
    radius = 6,
    fillColor = citation_pal(num_citation_int$total_citations)
  ) |>
  addHeatmap(
    data = sd_crash_data_map
  )



# grid cells --------------------------------------------------------------
# Cell size of 300ft x 300ft 
san_diego_grid <- st_make_grid(san_diego_city,cellsize = 300, square = FALSE)
san_diego_grid_sf <- st_sf(geometry = san_diego_grid)

# keep only cells that intersect a road segment
# san_diego_grid_sf <- san_diego_grid_sf[
#   lengths(st_intersects(san_diego_grid_sf, sd_road_data)) > 0,
#   ] 

# create unique id for cells 
san_diego_grid_sf$id <- seq_len(nrow(san_diego_grid_sf))


# transform back to WGS84 for leaflet
san_diego_grid_leaflet <- st_transform(san_diego_grid_sf, 4326)

# aggregation -------------------------------------------------------------



# number of citations within each grid cell 
citation_count <- st_intersects(san_diego_grid_sf, sd_citation_data, sparse = TRUE)
san_diego_grid_sf$citation_count <- lengths(citation_count)

# number of crashes
crash_count <- st_intersects(san_diego_grid_sf, sd_crash_data, sparse = TRUE)
san_diego_grid_sf$crash_count <- lengths(crash_count)

# number of road segments 
roadseg_count <- st_intersects(san_diego_grid_sf, sd_road_data, sparse = TRUE)
san_diego_grid_sf$roadseg_count <- lengths(roadseg_count)

# number of intersections 
intersection_count <- st_intersects(san_diego_grid_sf, sd_intersection_data, sparse = TRUE)
san_diego_grid_sf$intersection_count <- lengths(intersection_count)

# number of intersections daylit pre policy
predaylit <- sd_daylit_int_data |>
  filter(sd_daylit_int_data$`INTERSECTION EVALUATION` %in% c("Evaluated - No Action", 
                                                             "Evaluated - Added to Slurry Seal Project - Existing Red Curb"))

predaylit_int_count <- st_intersects(san_diego_grid_sf, predaylit, sparse = TRUE)
san_diego_grid_sf$predaylit_int_count <- lengths(predaylit_int_count)

# number of intersections daylit post policy
postdaylit <- sd_daylit_int_data |>
  filter(sd_daylit_int_data$`INTERSECTION EVALUATION` %in% c("Evaluated - Added to Slurry Seal Project - New Red Curb",
  "Evaluated - Created Work Order",
  "Evaluated - Coordinate with SuMo")) 

postdaylit_int_count <- st_intersects(san_diego_grid_sf, postdaylit, sparse = TRUE)
san_diego_grid_sf$postdaylit_int_count <- lengths(postdaylit_int_count)

# number of intersections not yet evaluated for daylighting
noteval <- sd_daylit_int_data |>
  filter(sd_daylit_int_data$`INTERSECTION EVALUATION` == "Not Evaluated")

noteval_int_count <- st_intersects(san_diego_grid_sf, noteval, sparse = TRUE)
san_diego_grid_sf$noteval_int_count <- lengths(noteval_int_count)


# transform for leaflet
san_diego_grid_leaflet <- st_transform(san_diego_grid_sf, 4326)


# make panel data  -------------------------------------------------------------

# arbitrary small year selection for prototype 
years = 2022:2025

# creates an individual grid cell for every year 
grid_panel <- san_diego_grid_sf |>
  st_drop_geometry() |>
  dplyr::select(id, roadseg_count, intersection_count,
                predaylit_int_count, postdaylit_int_count, noteval_int_count) |>
  crossing(year = years)

crash_counts_yearly <- sd_crash_data |>
  st_filter(san_diego_city) |>
  st_join(san_diego_grid_sf, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(id, ACCIDENT_YEAR) |>
  summarize(crash_count = n(), .groups = 'drop')

sd_citation_data$year <- as.numeric(format(as.Date(sd_citation_data$date_issue), "%Y"))
citation_counts_yearly <- sd_citation_data |>
  st_filter(san_diego_city) |>
  st_join(san_diego_grid_sf, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(id, year) |>
  summarize(citation_count = n(), .groups = 'drop')

final_panel <- grid_panel |>
  left_join(crash_counts_yearly, by = c("id", year = "ACCIDENT_YEAR")) |>
  left_join(citation_counts_yearly, by = c("id", "year")) |>
  mutate(
    crash_count = replace_na(crash_count, 0),
    citation_count = replace_na(citation_count, 0)
  )


final_panel <- final_panel |>
  mutate(
    post_policy_2024 = ifelse(year == 2024, 1, 0),
    # in final version the policy does not actually start being enforced with 
    # ticketing until 3/1/2025, but this is easier for now
    post_policy_2025 = ifelse(year == 2025, 1, 0)
  )

final_panel <- final_panel |>
  mutate(
    postdaylit_int_count = as.numeric(unlist(postdaylit_int_count)),
    predaylit_int_count  = as.numeric(unlist(predaylit_int_count)),
    intersection_count   = as.numeric(unlist(intersection_count)),
    roadseg_count        = as.numeric(unlist(roadseg_count))
  )





# all years of crash data
years <- 2014:2026

# filter so that only grids with 1+ road or 1+ crash or 1+ citation are kept
san_diego_grid_filtered <- san_diego_grid_sf |>
  filter(
    
    lengths(st_intersects(san_diego_grid_sf, sd_road_data)) > 0 |
      lengths(st_intersects(san_diego_grid_sf, sd_crash_data)) > 0 |
      lengths(st_intersects(san_diego_grid_sf, sd_citation_data)) > 0
  )

panel_skeleton <- san_diego_grid_filtered |>
  dplyr::select(id) |>
  crossing(year = years)

# adding crashes by year
crash_counts_panel <- sd_crash_data |>
  st_filter(san_diego_city) |>
  st_join(san_diego_grid_sf, join = st_intersects) |>
  st_drop_geometry() |>
  mutate(year = as.numeric(ACCIDENT_YEAR)) |> 
  group_by(id, year) |>
  summarize(crash_count = n(), .groups = 'drop')

# adding citations by year
sd_citation_data$year <- as.numeric(format(as.Date(sd_citation_data$date_issue), "%Y"))
citation_counts_panel <- sd_citation_data |>
  st_filter(san_diego_city) |>
  st_join(san_diego_grid_sf, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(id, year) |>
  summarize(citation_count = n(), .groups = 'drop')


final_panel <- panel_skeleton |>
  left_join(crash_counts_panel, by = c("id", "year")) |>
  left_join(citation_counts_panel, by = c("id", "year")) |>
  mutate(
    crash_count = replace_na(crash_count, 0),
    citation_count = replace_na(citation_count, 0),
  )




map_data_2025 <- final_panel |>
  filter(year == 2025)

spatial_grid_clean <- san_diego_grid_filtered |>
  dplyr::select(id) 

spatial_map_sf <- spatial_grid_clean |>
  inner_join(map_data_2025, by = "id") |>
  st_as_sf() |>
  st_transform(4326)

pal <- colorNumeric(
  palette = "YlOrRd", 
  domain = spatial_map_sf$crash_count,
  na.color = "transparent"
)

leaflet(spatial_map_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal(crash_count),
    fillOpacity = 0.75,
    weight = 0.3,
    color = "white",
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = ~paste0(
      "<strong>Grid ID:</strong> ", id, "<br/>",
      "<strong>2025 Crashes:</strong> ", crash_count, "<br/>",
      "<strong>2025 Citations:</strong> ", citation_count
    )
  ) |>
  addLegend(
    pal = pal, 
    values = ~crash_count, 
    opacity = 0.7, 
    title = "2025 Crash Count",
    position = "bottomright"
  )




# display of aggregated points
library(mapview)

mapview(san_diego_grid_sf, zcol = "citation_count", layer.name = "Citations", col.regions = viridis::viridis(100)) +
  mapview(san_diego_grid_sf, zcol = "crash_count", layer.name = "Crashes", col.regions = viridis::magma(100)) +
  mapview(san_diego_grid_sf, zcol = "roadseg_count", layer.name = "Road Segments", col.regions = viridis::inferno(100)) +
  mapview(san_diego_grid_sf, zcol = "intersection_count", layer.name = "Intersections", col.regions = viridis::plasma(100))


