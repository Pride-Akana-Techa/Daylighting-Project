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

options(tigris_use_cache = TRUE)

ca_boundary <- states(cb = FALSE) |>
  filter(NAME == "California")

cal_crossings_raw <- oe_get(
  "California",
  layer = "points",
  query = "SELECT * FROM points WHERE highway = 'crossing'",
  quiet = TRUE
)

crossings_cal <- st_filter(cal_crossings_raw, 
                           st_transform(ca_boundary, st_crs(cal_crossings_raw)))


cal_crossings_typed <- oe_get(
  "California",
  layer                 = "points",
  extra_tags            = c("crossing", "traffic_signals"),
  query                 = "SELECT * FROM points WHERE highway = 'crossing'",
  force_vectortranslate = TRUE,
  quiet                 = TRUE
)

crossings_cal <- st_filter(cal_crossings_typed, 
                           st_transform(ca_boundary, st_crs(cal_crossings_raw)))

crossings_cal <- crossings_cal |>
  mutate(crossing_type = case_when(
    !is.na(traffic_signals) & traffic_signals == "signal" ~ "Signalized",
    crossing == "marked"                                   ~ "Marked (zebra)",
    crossing == "uncontrolled"                             ~ "Uncontrolled",
    !is.na(crossing)                                       ~ crossing,
    TRUE                                                   ~ "Unspecified"
  ))

print(table(crossings_cal$crossing_type))


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
    opacity = 1,
    radius = 4
  )


# 20 feet buffer 
BUFFER_FT <- 20
BUFFER_M  <- BUFFER_FT * 0.3048   # 6.096 m
CRS_CAL    <- 3310 #NAD83 / California Albers

crossings_proj <- st_transform(crossings_cal, CRS_CAL)
buffers_proj   <- st_buffer(crossings_proj, dist = BUFFER_M)
buffers_20ft   <- st_transform(buffers_proj, 4326)   # back to WGS84 for mapping

# leaflet() |>
#   addProviderTiles(providers$CartoDB.Positron) |>
#   
#   addPolygons(
#     data = ca_boundary,
#     fillColor = "lightgreen",
#     fillOpacity = 0.2,
#     color = "black",
#     weight = 2,
#     group = "California"
#   ) |>
#   
#   addGlPolygons(
#     data = buffers_20ft,
#     color = "red",
#     opacity = 0.4,
#     group = "Buffers"
#   )