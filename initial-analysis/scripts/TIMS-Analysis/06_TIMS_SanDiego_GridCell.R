# -------------------------------------------------------------------------
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
library(scales)
library(Matrix)


# -------------------------------------------------------------------------
# load data
citation_data_1 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2025_part1_datasd.csv"))
citation_data_2 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2025_part2_datasd.csv"))
citation_data <- rbind(citation_data_1, citation_data_2) |>
  filter(vio_code == "CVC 22500(n)")  # AB-413 violations only
sd_crash_data <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
  filter(COUNTY == "SAN DIEGO" & (PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")) 


# load geocoded citation data 
# geocoder code in 01_SD_citation_geocoder.R
sd_citation_data <- readRDS(here("SD_citation_geocode.rds"))

# san diego boundary polygon
san_diego_boundary <- counties(state = "CA", cb = FALSE) |>
  filter(COUNTYFP == "073")

# -------------------------------------------------------------------------
# create crash costs to combine frequency and severity into one variable
# using national safety council 

# -------------------------------------------------------------------------
# Plots the citation locations and incorporated city jurisdictions in California
ca_places <- places(state = "CA", cb = FALSE)
sd_cities_geo <- ca_places |> 
  st_filter(san_diego_boundary, .predicate = st_intersects) |> 
  st_transform(4326)


leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_city,
    fillColor = "lightblue",
    fillOpacity = 0.2,
    color = "black",
    weight = 2,
  ) |>
  addGlPoints(
    data = sd_citation_data,
    opacity = 1,
    radius = 6,
    fillColor = "black"
  )

# -------------------------------------------------------------------------
# create grid over San Diego
# Difficult due to weird shape of boundary 
san_diego_city <- ca_places |>
  filter(NAME == "San Diego") |>
  st_transform(4326)

# transform to a projected CRS with feet as units
san_diego_city_ft <- st_transform(san_diego_city, 2230)
sd_citation_data <- st_transform(sd_citation_data, 2230)
sd_crash_data <- st_transform(sd_crash_data, 2230)

# create 300-ft grid
san_diego_grid <- st_make_grid(
  san_diego_city_ft,
  cellsize = 300,
  square = TRUE
)

san_diego_grid_sf <- st_sf(geometry = san_diego_grid)


# keep only cells that intersect the city
san_diego_grid_sf <- san_diego_grid_sf[
  lengths(st_intersects(san_diego_grid_sf, san_diego_city_ft)) > 0,
] 

# create unique id for cells 
san_diego_grid_sf$id <- seq_len(nrow(san_diego_grid_sf))


# transform back to WGS84 for leaflet
san_diego_grid_leaflet <- st_transform(san_diego_grid_sf, 4326)


# create a count for number of citations within each grid cell 
citation_count <- st_intersects(san_diego_grid_sf, sd_citation_data, sparse = TRUE)
san_diego_grid_sf$citation_count <- lengths(citation_count)

crash_count <- st_intersects(san_diego_grid_sf, sd_crash_data, sparse = TRUE)
san_diego_grid_sf$crash_count <- lengths(crash_count)

san_diego_grid_leaflet <- st_transform(san_diego_grid_sf, 4326)


color_breaks <- c(0, 1, 5, 10, 50, max(san_diego_grid_leaflet$citation_count, na.rm = TRUE))

pal <- colorBin(
  palette = "YlOrRd", 
  domain = san_diego_grid_leaflet$citation_count, 
  bins = color_breaks
)

leaflet(sample_n(san_diego_grid_leaflet, 117745)) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal(citation_count),
    fillOpacity = 0.9,
    weight = 0.1
  )


pal_citation <- colorBin("YlOrRd", domain = san_diego_grid_leaflet$citation_count, 
                         bins = unique(c(0, 1, 5, 10, 50, max(san_diego_grid_leaflet$citation_count, na.rm = TRUE))))
pal_crash <- colorBin("PuBu", domain = san_diego_grid_leaflet$crash_count, 
                      bins = unique(c(0, 1, 5, 10, 50, max(san_diego_grid_leaflet$crash_count, na.rm = TRUE))))

library(leafsync)

map_citation <- leaflet(san_diego_grid_leaflet) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(fillColor = ~pal_citation(citation_count), fillOpacity = 0.8, weight = 0.1, color = NA)

map_crash <- leaflet(san_diego_grid_leaflet) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(fillColor = ~pal_crash(crash_count), fillOpacity = 0.8, weight = 0.1, color = NA)

sync(map_citation, map_crash) 


ggplot(san_diego_grid_leaflet, aes(x = citation_count, y = crash_count)) +
  geom_jitter(alpha = 0.2, size = 1.5, color = "blue", width = 0.5, height = 0.5) + 
  labs(
    x = "Number of Daylighting Tickets",
    y = "Number of Pedestrian and Bicyclist Crashes"
  ) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +

  theme_minimal()
