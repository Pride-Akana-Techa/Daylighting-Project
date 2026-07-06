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


# load data
citation_data_1 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2025_part1_datasd.csv"))
citation_data_2 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2025_part2_datasd.csv"))
citation_data <- rbind(citation_data_1, citation_data_2) |>
  filter(vio_code == "CVC 22500(n)")  # AB-413 violations only
crash_data <- readRDS(here("initial-analysis", "data-clean", "TIMS_Filtered.rds")) |>
  filter(COUNTY == "SAN DIEGO")

set.seed(42)
geo_sample <- citation_data |>
  filter(location != "" & !is.na(location)) |>
  sample_n(18364) |>
  mutate(full_address = paste0(location, ", San Diego, CA")) |>
  geocode(address = full_address, method = 'arcgis', lat = latitude, long = longitude)

#convert to spatial object for spatial joining 4326
geo_sample_sf <- geo_sample |>
  filter(!is.na(latitude) & !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# saveRDS(geo_sample_sf, "SD_citation_geocode.rds")

# load geocoded citation data 
sd_citation_data <- readRDS(here("SD_citation_geocode.rds"))
