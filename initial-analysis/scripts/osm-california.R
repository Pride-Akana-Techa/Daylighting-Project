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

##### Data Cleaning and Prep ####

# Outline of California State #
ca_boundary <- states(cb = FALSE) |>
  filter(NAME == "California")

# Croswalks from OSM 
cal_crossings_typed <- oe_get(
  "California",
  layer                 = "points",
  extra_tags            = c("crossing", "traffic_signals", "crossing:island", "lit"),
  query                 = "SELECT * FROM points WHERE highway = 'crossing'",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
)

# Roads from OSM 
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


# Filter for within California
crossings_cal <- st_filter(cal_crossings_typed, 
                           st_transform(ca_boundary, st_crs(cal_crossings_typed)))
streets_cal <- st_filter(cal_streets_raw, 
                         st_transform(ca_boundary, st_crs(cal_streets_raw)))

# saveRDS(streets_cal, "OSM_california_crossings.rds")


# Convert the crossing type into cleaned columns 
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

# remove unecessary columns 
crossings_cal <- crossings_cal |>
  select(-name, -barrier, -ref, -address, -is_in, -place, -man_made,
        -traffic_signals, -lit, -other_tags, -crossing, -highway)

# saveRDS(crossings_cal, "OSM_california_crossings.rds")

# Convert the speedlimit column into clean rounded up to every 5 values
streets_cal <- streets_cal |>
  mutate(
    max_speed_round = case_when(
      maxspeed %in% c("3 mph", "4 mph", "5 mph") ~ "5 mph", 
      maxspeed %in% c("7 mph", "8 mph", "9 mph", "10", "10 mph") ~ "10 mph",
      maxspeed %in% c("12 mph", "13 mph", "14 mph", "15", "15 mph") ~ "15 mph",
      maxspeed %in% c("16 mph", "17 mph", "18 mph", "19 mph", "20", "20 mph") ~ "20 mph",
      maxspeed %in% c("23 mph", "24", "24 mph", "25", "25 mph") ~ "25 mph",
      maxspeed %in% c("30", "30 mph") ~ "30 mph",
      maxspeed %in% c("32 mph", "33 mph", "34 mph", "35", "35 mph;30 mph",
                      "35 mph;25 mph", "35 mph") ~ "35 mph",
      maxspeed %in% c("40", "40 mph;35 mph", "40 mph") ~ "40 mph",
      maxspeed %in% c("45", "45 mph") ~ "45 mph",
      maxspeed %in% c("50", "50 mph") ~ "50 mph",
      maxspeed %in% c("50 mph;55 mph", "51 mph", "53 mph", "54 mph", "55",
                      "55-40", "55 mph;40 mph", "55 mph;50 mph", "55 mph") ~ "55 mph",
      maxspeed %in% c("60", "60 mph") ~ "60 mph",
      maxspeed %in% c("65 mph") ~ "65 mph",
      maxspeed %in% c("70 mph") ~ "70 mph",
      maxspeed %in% c("75 mph") ~ "75 mph"
      )
  )

print(table(streets_cal$max_speed_round))

streets_cal <- streets_cal |>
  select(-waterway, -aerialway, -barrier, -man_made, -railway, -traffic_calming,
         -lit, -parking)

print(table(streets_cal$num_lanes))

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

# saveRDS(streets_cal, "OSM_california_streets.rds")
# saveRDS(crossings_cal, "OSM_california_crossings.rds")





##### TIMS Data ######
#california_crashes <- read_csv(here("initial-analysis", "data-raw", "TIMS_Crashes_California.csv"))

cal_filt <- readRDS(here("initial-analysis", "data-clean", "TIMS_Filtered.rds"))

bike_or_ped_acc <- cal_filt %>%
  filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")

bike_or_ped_acc_geoloc <- bike_or_ped_acc %>%
  filter(!is.na(POINT_X) & !is.na(POINT_Y))

bike_or_ped_acc_sf <- bike_or_ped_acc_geoloc |>
  st_as_sf(
    coords = c("POINT_X", "POINT_Y"),
    crs = 4326
  )

bike_or_ped_acc_sf <- st_filter(bike_or_ped_acc_sf, 
                           st_transform(ca_boundary, st_crs(bike_or_ped_acc_sf)))

bike_acc_sf <- bike_or_ped_acc_sf %>%
  filter(BICYCLE_ACCIDENT == "Y")
ped_acc_sf <- bike_or_ped_acc_sf %>%
  filter(PEDESTRIAN_ACCIDENT == "Y")

acc_coords <- st_coordinates(bike_or_ped_acc_sf) |>
  as.data.frame() |>
  rename(lng = X, lat = Y)


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
    opacity = .4,
    radius = 6
  ) |>
  addGlPoints(
    data = bike_or_ped_acc_sf,
    group = "Accidents",
    opacity = .3,
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
    data = ped_acc_sf,
    group = "Ped Accidents",
    opacity = .5,
    radius = 6,
    fillColor = "red"
  ) |>
  addGlPoints(
    data = bike_acc_sf,
    group = "Bike Accidents",
    opacity = .5,
    radius = 6,
    fillColor = "blue"
  ) |>
  addLayersControl(
    overlayGroups = c("California", "Crossings", "Ped Accidents", "Bike Accidents"),
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






