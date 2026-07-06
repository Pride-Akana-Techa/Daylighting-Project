# load libraries
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

daylight_intersections <- read.xlsx("sfmta_hearing_results_cleaned.xlsx", 3)

set.seed(42)


geo_intersections <- daylight_intersections |>
  sample_n(81) |>
  mutate(X8 = paste0(X8, ", San Francisco, CA")) |>
  geocode(address = X8, method = 'arcgis', lat = latitude, long = longitude)

# Convert to spatial object for spatial joining (Initial output is 4326)
geo_intersections_sf <- geo_intersections |>
  filter(!is.na(latitude) & !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#saveRDS(geo_sample_sf, "SD_citation_geocode.rds")

closest_crash_int <- readRDS(here("initial-analysis", "data-raw", "TIMS_closest_crash_int.rds"))

ca_boundary <- states(cb = FALSE) |>
  filter(NAME == "California")

san_francisco_boundary <- counties(state = "CA", cb = FALSE) |>
  filter(COUNTYFP == "075")


closest_crash_san_francisco <- st_filter(closest_crash_int, 
                                         st_transform(san_francisco_boundary, st_crs(closest_crash_int)))

library(leaflet)
library(leaflet.extras)

leaflet(geo_intersections_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  
  addCircleMarkers(
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~X8
  ) |>
  
  addHeatmap(
    data = closest_crash_san_francisco,
    radius = 8,
    blur = 15,
    max = 0.05
  ) 