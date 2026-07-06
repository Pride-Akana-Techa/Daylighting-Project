
# -------------------------------------------------------------------------
# Merge and County Weather Data
# Inputs: initial-analysis/data/weather.csv

# -------------------------------------------------------------------------



# Set Up ------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Set folder path
weather_folder <- "initial-analysis/data/weather"  

# List all CSV files in folder
all_files <- list.files(weather_folder, pattern = "*.csv", full.names = TRUE)


# See all column names for every file
file_cols <- map(all_files, function(fp) {
  df <- read_csv(fp, show_col_types = FALSE, n_max = 1)
  cat("\n", basename(fp), ":", ncol(df), "cols\n")
  cat(paste(names(df), collapse = " | "), "\n")
})


# Count unique stations per file
station_count <- map_dfr(all_files, function(fp) {
  df <- read_csv(fp, show_col_types = FALSE)
  tibble(
    source_file  = basename(fp),
    n_stations   = n_distinct(df$STATION),
    has_NAME     = "NAME" %in% names(df),
    has_LAT_LON  = "LATITUDE" %in% names(df)
  )
})

print(station_count)


# Merge Datasets ----------------------------------------------------------

# Safe read function — adds missing columns as NA
read_noaa_safe <- function(filepath) {
  df <- read_csv(filepath, 
                 show_col_types = FALSE,
                 col_types = cols(
                   DATE      = col_character(),
                   STATION   = col_character(),
                   NAME      = col_character(),
                   LATITUDE  = col_double(),
                   LONGITUDE = col_double(),
                   PRCP      = col_double(),
                   TAVG      = col_double(),
                   TMAX      = col_double(),
                   TMIN      = col_double(),
                   SNOW      = col_double(),
                   SNWD      = col_double(),
                   ELEVATION = col_double(),
                   .default  = col_guess()  
                 ))

  
  if (!"STATION"   %in% names(df)) df$STATION   <- NA_character_
  if (!"NAME"      %in% names(df)) df$NAME      <- NA_character_
  if (!"DATE"      %in% names(df)) df$DATE      <- NA_character_
  if (!"LATITUDE"  %in% names(df)) df$LATITUDE  <- NA_real_
  if (!"LONGITUDE" %in% names(df)) df$LONGITUDE <- NA_real_
  if (!"PRCP"      %in% names(df)) df$PRCP      <- NA_real_
  if (!"TAVG"      %in% names(df)) df$TAVG      <- NA_real_
  if (!"TMAX"      %in% names(df)) df$TMAX      <- NA_real_
  if (!"TMIN"      %in% names(df)) df$TMIN      <- NA_real_
  if (!"SNOW"      %in% names(df)) df$SNOW      <- NA_real_  
  if (!"SNWD"      %in% names(df)) df$SNWD      <- NA_real_  
  if (!"ELEVATION" %in% names(df)) df$ELEVATION <- NA_real_ 
  df$source_file <- basename(filepath)
  return(df)
}

# Read and stack all files
weather_raw <- map_dfr(all_files, read_noaa_safe)

# Check how many unique stations
cat("Unique stations:", n_distinct(weather_raw$STATION), "\n")


# Clean Dataset -----------------------------------------------------------

# Check missing values for key variables
weather_raw %>%
  summarise(
    missing_DATE = sum(is.na(DATE)),
    missing_PRCP = sum(is.na(PRCP)),
    missing_TAVG = sum(is.na(TAVG)),
    missing_TMAX = sum(is.na(TMAX)),
    missing_TMIN = sum(is.na(TMIN)),
    missing_SNOW = sum(is.na(SNOW)),
    missing_SNWD = sum(is.na(SNWD))
  )

# Fix date format to YYYY-MM-DD 
weather_raw <- weather_raw %>%
  mutate(DATE = parse_date_time(DATE, orders = c("ymd", "mdy")) %>% as.Date())


# Removing variables with 70%+ null values
colSums(is.na(weather_raw))
weather_clean <- weather_raw |> 
  select(-c(DAPR, MDPR, WT01, WT02, WT03, WT04, WT05, WT06, WT07, WT08, WT09, WT10, WT11, DASF, MDSF))


# Saving data
weather_clean |> 
  saveRDS(file.path("initial-analysis/data-clean/NOAA_weather_data.rds"))


