# -------------------------------------------------------------------------
# Download and clean California daily weather data from Prism Group at Oregon
# State University

# Inputs:
# Outputs: initial-analysis/data/weather
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Set Up
# -------------------------------------------------------------------------

# Load packages
library(prism)
library(terra)
library(sf)
library(tigris)
library(tidyverse)

options(tigris_use_cache = TRUE)

# Set download directory

prism_set_dl_dir("initial-analysis/data/prism")

# -------------------------------------------------------------------------
# Download weather data
# -------------------------------------------------------------------------

# Download California boundary
ca <- states(cb = TRUE) |>
  filter(NAME == "California") |>
  st_transform(4326)

ca_vect <- vect(ca)

# Download Daily PRISM Data
start_date <- "2025-01-01"
end_date   <- "2025-12-31"

# Mean temperature
get_prism_dailys(type = "tmean",
                 minDate = start_date,
                 maxDate = end_date,
                 keepZip = TRUE)

# Precipitation
get_prism_dailys(type = "ppt",
                 minDate = start_date,
                 maxDate = end_date,
                 keepZip = TRUE)

# -------------------------------------------------------------------------
# Extract statewide average and Combine
# -------------------------------------------------------------------------
# -------------------------------
# Function to extract daily values
# -------------------------------

extract_statewide_mean <- function(files, variable){
  
  # Create raster stack from PRISM files
  r_stack <- pd_stack(files)
  
  # Crop California
  r_ca <- crop(r_stack,
               ca_vect)
  
  # Extract daily means
  values <- global(
    r_ca,
    mean,
    na.rm = TRUE
  ) |>
    as_tibble() |>
    rename(value = mean)
  
  
  # Get dates from raster layers
  dates <- pd_get_date(files)
  
  
  tibble(
    date = dates,
    variable = variable,
    value = values$value
  )
  
}


# -------------------------------
# Get PRISM files
# -------------------------------

all_files <- prism_archive_ls()


tmean_files <- all_files[
  str_detect(all_files, "tmean")
]


ppt_files <- all_files[
  str_detect(all_files, "ppt")
]


# Check
length(tmean_files)
length(ppt_files)


# -------------------------------
# Extract
# -------------------------------

tmean_daily <- extract_statewide_mean(
  tmean_files,
  "tmean"
)


ppt_daily <- extract_statewide_mean(
  ppt_files,
  "ppt"
)


# -------------------------------
# Combine
# -------------------------------

weather_daily <- bind_rows(
  tmean_daily,
  ppt_daily
) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
  arrange(date)


head(weather_daily)









# Combine
weather_daily <- bind_rows(tmean_daily,
                           ppt_daily) |>
  pivot_wider(names_from = variable,
              values_from = value) |>
  arrange(date)

# Rain indicator
weather_daily <- weather_daily |>
  mutate(rain_day = if_else(ppt > 0, 1, 0))

# Save
write_csv(weather_daily, 
          "initial-analysis/data/weather/california_daily_weather_2025.csv")

# Check output

glimpse(weather_daily)

head(weather_daily)