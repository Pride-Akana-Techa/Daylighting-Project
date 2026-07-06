# -------------------------------------------------------------------------
# 04_TIMS_OSM_intro
# 
# -------------------------------------------------------------------------
# load libraries
library(osmextract)   # download OpenStreetMap data as sf objects
library(sfnetworks)   # road network graph (nodes = intersections, edges = segments)
library(dodgr)        # fast weighted graph distances on street networks
library(tidytransit)  # GTFS transit data parsing
library(sf)           # spatial data manipulation (read, transform, buffer, filter)
library(dplyr)        # data wrangling
library(ggplot2)      # static maps
library(tmap)         # interactive maps
library(stringr)      # string extraction (used for OSM other_tags parsing)
library(tigris)
library(leaflet)
library(leafgl)
library(here)
library(readr)
library(leaflet.extras)
options(tigris_use_cache = TRUE)

# -------------------------------------------------------------------------
#OSM data cleaning and prep

# Outline of California State 
ca_boundary <- states(cb = FALSE) |>
  filter(NAME == "California")
saveRDS(ca_boundary, "initial-analysis/data-clean/ca_boundary.rds")

# Croswalk data from OSM for the state of california
cal_crossings_raw <- oe_get(
  "California",
  layer                 = "points",
  extra_tags            = c("crossing", "traffic_signals", "crossing:island", "lit"),
  query                 = "SELECT * FROM points WHERE highway = 'crossing'",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
)

# Road data from OSM for the state of california 
cal_streets_raw <- oe_get(
  "California",
  layer = "lines",
  extra_tags = c("maxspeed", "oneway", "lanes", "traffic_calming", "lit", "parking"),
  query = "
    SELECT *
    FROM lines
    WHERE highway IN (
      'motorway',
      'trunk',
      'primary',
      'secondary',
      'tertiary',
      'residential'
    )",
  quiet = TRUE
)

# filter for within California - removes misqueried data 
crossings_cal <- st_filter(cal_crossings_raw, 
                           st_transform(ca_boundary, st_crs(cal_crossings_raw)))
streets_cal <- st_filter(cal_streets_raw, 
                         st_transform(ca_boundary, st_crs(cal_streets_raw)))

# -------------------------------------------------------------------------
# Clean Open Street Map data 
# -------------------------------------------------------------------------
  # Crossings
# convert the crossing type into cleaned columns
# creates column with 5 different types of intersections
# signalized, marked (zebra), uncontrolled, unmarked, unspecified
crossings_cal <- crossings_cal |>
  mutate(crossing_type = case_when(
    !is.na(traffic_signals) & traffic_signals == "signal" 
    ~ "Signalized",
    crossing == "traffic_signals" |
      crossing == "pedestrian_signals" |
      crossing == "signals" |
      crossing == "traffic_signals;uncontrolled" | 
      crossing == "traffic_signals;marked"
    ~ "Signalized",
    crossing == "marked" | 
      crossing ==  "marked;uncontrolled" | 
      crossing == "zebra" |
      crossing == "zebra;marked" |
      crossing == "marked;traffic_signals"
    ~ "Marked (zebra)",
    crossing == "uncontrolled" |
      crossing == "uncontrolled;unmarked" |
      crossing == "uncontrolled;traffic_signals" |
      crossing == "uncontrolled;stop_sign" |
      crossing == "uncontrolled;marked" |
      crossing == "stop_sign"
      ~ "Uncontrolled",
    crossing == "unmarked"
    ~ "Unmarked",
    TRUE                                                   
    ~ "Unspecified"
  ))

# remove unecessary columns / columns without enough information
crossings_cal <- crossings_cal |>
  select(-name, -barrier, -ref, -address, -is_in, -place, -man_made,
        -traffic_signals, -lit, -other_tags, -crossing, -highway)

# saveRDS(crossings_cal, "OSM_california_crossings.rds")

  # Streets
# Convert the speedlimit column into clean rounded up to every 5 values
streets_cal <- streets_cal |>
  mutate(
    max_speed_round = case_when(
      TRUE ~ paste0(round(as.numeric(stringr::str_extract(maxspeed, "\\d+")) / 5) * 5, " mph")
    )
)

# Clean number of lanes data
streets_cal <- streets_cal |>
  mutate(
    num_lanes = case_when(
      lanes %in% c("1") ~ "1",
      lanes %in% c("2") ~ "2",
      lanes %in% c("3") ~ "3",
      lanes %in% c("4") ~ "4",
      lanes %in% c("5") ~ "5",
      lanes %in% c("6") ~ "6",
      lanes %in% c("7") ~ "7",
      lanes %in% c("8") ~ "8",
      lanes %in% c("9") ~ "9",
      lanes %in% c("10", "11", "12") ~ "10+"
  )
)

# Remove unnecessary columns 
streets_cal <- streets_cal |>
  select(-waterway, -aerialway, -barrier, -man_made, -railway, -traffic_calming,
         -lit, -parking)

# saveRDS(streets_cal, "OSM_california_streets.rds")



# -------------------------------------------------------------------------
# TIMS Data

cal_filt <- readRDS(here("initial-analysis", "data-clean", "01_TIMS_Cleaned.rds"))

bike_or_ped_acc <- cal_filt |>
  filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")

# saveRDS(bike_or_ped_acc, "OSM_bike_ped_accidents.rds")

cal_albers <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds"))

bike_or_ped_acc_geo <- cal_albers|>
  filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")


bike_acc_geo <- bike_or_ped_acc_geo |>
  filter(BICYCLE_ACCIDENT == "Y")
ped_acc_geo <- bike_or_ped_acc_geo |>
  filter(PEDESTRIAN_ACCIDENT == "Y")

coords <- st_coordinates(bike_or_ped_acc_geo) |>
  as.data.frame() |>
  rename(lng = X, lat = Y)

coords <- st_coordinates(bike_or_ped_acc_geo)

bike_or_ped_acc_geo <- bike_or_ped_acc_geo |>
  mutate(
    lng = coords[, 1],
    lat = coords[, 2]
  )

# Match a second layer to your California Albers layer
ca_boundary <- st_transform(ca_boundary, crs = st_crs(cal_albers))
crossings_cal <- st_transform(crossings_cal, crs = st_crs(cal_albers))
streets_cal <- st_transform(streets_cal, crs = st_crs(cal_albers))

#saveRDS(acc_coords, "TIMS_bike_ped_data.rds")



library(sf)
library(here)
library(dplyr)

# 1. Load your primary TIMS geocoded data (Ensure it is explicitly set to 3310)
cal_albers <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) %>%
  st_transform(3310)

# 2. Filter down to your specific incident types
bike_or_ped_acc_geo <- cal_albers %>%
  filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")

bike_acc_geo <- bike_or_ped_acc_geo %>% filter(BICYCLE_ACCIDENT == "Y")
ped_acc_geo  <- bike_or_ped_acc_geo %>% filter(PEDESTRIAN_ACCIDENT == "Y")

# 3. Extract and bind the Projected Coordinates (Units are Meters, not Lng/Lat)
coords <- st_coordinates(bike_or_ped_acc_geo)
bike_or_ped_acc_geo <- bike_or_ped_acc_geo %>%
  mutate(
    X_meters = coords[, 1],
    Y_meters = coords[, 2]
  )

# 4. Correctly transform all supporting layers to match California Albers (3310)
ca_boundary   <- st_transform(ca_boundary, crs = 3310)
crossings_cal <- st_transform(crossings_cal, crs = 3310) 
streets_cal   <- st_transform(streets_cal, crs = 3310)    


library(leaflet)
library(leafgl) 
library(sf)

leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  
  # 1. Transform California boundary to WGS84 for display
  addPolygons(
    data = st_transform(ca_boundary, 4326),
    fillColor = "lightgreen",
    fillOpacity = 0.2,
    color = "black",
    weight = 2,
    group = "California"
  ) |>
  addGlPoints(
    data = crossings_cal,
    group = "Crossings",
    opacity = .3,
    radius = 6,
    fillColor = "black"
  ) |>
  
  # 2. Transform your TIMS crash point layer to WGS84 for display
  addGlPoints(
    data = st_transform(bike_or_ped_acc_geo, 4326),
    group = "Accidents",
    opacity = 0.3,
    radius = 6,
    fillColor = "red"
  )


leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = ca_boundary,
    fillColor = "lightgreen",
    fillOpacity = 0.2,
    color = "black",
    weight = 2,
    group = "California"
  ) |>
  addGlPoints(
    data = crossings_cal,
    group = "Crossings",
    opacity = .3,
    radius = 6,
    fillColor = "black"
  ) |>
  addGlPoints(
    data = ped_acc_geo,
    group = "Ped Accidents",
    opacity = .5,
    radius = 6,
    fillColor = "red"
  ) |>
  addGlPoints(
    data = bike_acc_geo,
    group = "Bike Accidents",
    opacity = .5,
    radius = 6,
    fillColor = "blue"
  ) |>
  addLayersControl(
    overlayGroups = c("California", "Crossings", "Ped Accidents", "Bike Accidents"),
    options       = layersControlOptions(collapsed = FALSE)
  )



library(htmlwidgets)

# Assign your map to a variable
my_interactive_map <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data        = ca_boundary,
    fillColor   = "lightgreen",
    fillOpacity = 0.2,
    color       = "black",
    weight      = 2,
    group       = "California"
  ) |>
  addGlPoints(
    data    = crossings_cal,
    group   = "Crossings",
    opacity = 0.3,
    radius  = 6,
    fillColor = "black"
  ) |>
  addHeatmap(
    data      = acc_coords,
    lng       = ~lng,
    lat       = ~lat,
    blur      = 20,
    max       = 0.05,
    radius    = 15,
    gradient  = c("0.0" = "blue", "0.3" = "cyan", "0.6" = "yellow", "0.8" = "orange", "1.0" = "red"),
    group     = "Accident Heatmap"
  ) |>
  addLayersControl(
    overlayGroups = c("California", "Crossings", "Accident Heatmap"),
    options       = layersControlOptions(collapsed = FALSE)
  )




leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data        = ca_boundary,
    fillColor   = "lightgreen",
    fillOpacity = 0.2,
    color       = "black",
    weight      = 2,
    group       = "California"
  ) |>
  addGlPoints(
    data    = crossings_cal,
    group   = "Crossings",
    opacity = 0.3,
    radius  = 6,
    fillColor = "black"
  ) |>
  addHeatmap(
    data      = acc_coords,
    lng       = ~lng,
    lat       = ~lat,
    blur      = 20,
    max       = 0.05,
    radius    = 15,
    gradient  = c("0.0" = "blue", "0.3" = "cyan", "0.6" = "yellow", "0.8" = "orange", "1.0" = "red"),
    group     = "Accident Heatmap"
  ) |>
  addLayersControl(
    overlayGroups = c("California", "Crossings", "Accident Heatmap"),
    options       = layersControlOptions(collapsed = FALSE)
  )

saveWidget(my_interactive_map, file = "accident_heat_map.html")






# 20 feet buffer 
BUFFER_FT <- 20
BUFFER_M  <- BUFFER_FT * 0.3048   # 6.096 m
CRS_CAL    <- 3310 #NAD83 / California Albers

crossings_proj <- st_transform(crossings_cal, CRS_CAL)
buffers_proj   <- st_buffer(crossings_proj, dist = BUFFER_M)
buffers_20ft   <- st_transform(buffers_proj, 4326)   # back to WGS84 for mapping


print(table(crossings_cal$crossing_type))

crossings_cal <- crossings_cal |>
  select(-name, -barrier, -ref, -address, -is_in, -place, -man_made, -traffic_signals,
         -highway, -crossing, -other_tags)




intersection_incident_geo <- bike_or_ped_acc_geoloc |>
  filter(INTERSECTION == "Y" | (INTERSECTION == "N") & DISTANCE <= 20)

intersection_incident_geo_sf <- st_as_sf(bike_or_ped_acc_geoloc,
                                         coords = c("POINT_X", "POINT_Y"), crs = 4326)


nearest_idx <- st_nearest_feature(intersection_incident_geo_sf, crossings_cal)

closest_crash_int <- intersection_incident_geo_sf %>%
  mutate(
    closest_osm_int = crossings_cal[nearest_idx, ],
    distance_to_osm_int = st_distance(intersection_incident_geo_sf,
                                      crossings_cal[nearest_idx, ], 
                                      by_element = TRUE)
  )

#saveRDS(closest_crash_int, "TIMS_closest_crash_int.rds")


under_20_feet <- closest_crash_int |>
  filter(as.numeric(distance_to_osm_int) < 6.096)


leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data        = ca_boundary,
    fillColor   = "lightgreen",
    fillOpacity = 0.2,
    color       = "black",
    weight      = 2,
    group       = "California"
  ) |>
  addGlPoints(
    data    = crossings_cal,
    group   = "Crossings",
    opacity = 0.3,
    radius  = 6,
    fillColor = "black"
  ) |>
  addGlPoints(
    data = under_20_feet,
    group = "Bike Accidents",
    opacity = .5,
    radius = 6,
    fillColor = "blue"
  ) |>
  addLayersControl(
    overlayGroups = c("California", "Crossings", "Accident Heatmap"),
    options       = layersControlOptions(collapsed = FALSE)
  )






