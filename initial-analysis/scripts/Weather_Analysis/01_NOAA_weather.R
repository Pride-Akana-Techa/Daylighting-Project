
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

# Check how many files are found
cat("Files found:", length(all_files), "\n")

# Print the filenames
basename(all_files)

# Read just the first row of each file to see column names
file_audit <- map_dfr(all_files, function(fp) {
  df <- read_csv(fp, show_col_types = FALSE, n_max = 1)
  tibble(
    file   = basename(fp),
    n_cols = ncol(df),
    cols   = paste(names(df), collapse = ", ")
  )
})

# See all column names for every file (not truncated)
file_cols <- map(all_files, function(fp) {
  df <- read_csv(fp, show_col_types = FALSE, n_max = 1)
  cat("\n", basename(fp), ":", ncol(df), "cols\n")
  cat(paste(names(df), collapse = " | "), "\n")
})

# # Read just the NAME column from the two files that have it
name_check <- map_dfr(
  c("4340966.csv", "4341416.csv"),  # the two files with NAME
  function(fn) {
    fp <- file.path(weather_folder, fn)
    read_csv(fp, show_col_types = FALSE) %>%
      distinct(STATION, NAME) %>%         # unique station-name combos
      mutate(source_file = fn)
  }
)

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

# Check date range per file
date_range <- map_dfr(all_files, function(fp) {
  df <- read_csv(fp, show_col_types = FALSE)
  tibble(
    source_file = basename(fp),
    start_date  = min(ymd(df$DATE)),
    end_date    = max(ymd(df$DATE)),
    n_rows      = nrow(df)
  )
})

print(date_range)

# FIPS to county name lookup for all California counties in your files
fips_lookup <- tribble(
  ~fips,     ~county,
  "06001",   "Alameda",
  "06003",   "Alpine",
  "06005",   "Amador",
  "06007",   "Butte",
  "06011",   "Colusa",
  "06013",   "Contra Costa",
  "06017",   "El Dorado",
  "06023",   "Humboldt",
  "06027",   "Inyo",
  "06029",   "Kern",
  "06031",   "Kings",
  "06033",   "Lake",
  "06037",   "Los Angeles",
  "06041",   "Marin",
  "06047",   "Merced",
  "06051",   "Mono",
  "06053",   "Monterey",
  "06055",   "Napa",
  "06059",   "Orange",
  "06061",   "Placer",
  "06063",   "Plumas",
  "06065",   "Riverside",
  "06067",   "Sacramento",
  "06069",   "San Benito",
  "06071",   "San Bernardino",
  "06073",   "San Diego",
  "06075",   "San Francisco",
  "06079",   "San Luis Obispo",
  "06083",   "Santa Barbara",
  "06085",   "Santa Clara",
  "06095",   "Solano",
  "06107",   "Tulare",
  "06111",   "Ventura",
  "06087",   "Santa Cruz", 
  "06093",   "Siskiyou", 
  "06099",   "Stanislaus",
  "06025",   "Imperial",
  "06089",   "Shasta", 
  "06091",   "Sierra",
  "06101",   "Sutter",
  "06015",   "Del Norte",
  "06103",   "Tehama",
  "06081",   "San Mateo",
  "06077",   "San Joaquin",
  "06009",   "Calaveras",
  "06019",   "Fresno",
  "06021",   "Glenn",
  "06035",   "Lassen",
  "06039",   "Madera",
  "06043",   "Mariposa",
  "06045",   "Mendocino",
  "06049",   "Modoc",
  "06057",   "Nevada",
  "06097",   "Sonoma",
  "06105",   "Trinity",
  "06109",   "Tuolumne",
  "06113",   "Yolo",
  "06115",   "Yuba"
)

# Safe read function — adds missing columns as NA
read_noaa_safe <- function(filepath) {
  df <- read_csv(filepath, 
                 show_col_types = FALSE,
                 col_types = cols(DATE = col_character()))
  
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

# Check date range
cat("Date range:", as.character(min(weather_raw$DATE, na.rm = TRUE)),
    "to", as.character(max(weather_raw$DATE, na.rm = TRUE)), "\n")

# Check how many unique stations
cat("Unique stations:", n_distinct(weather_raw$STATION), "\n")

# Check missing values for our key variables
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

# Fix DATE format to YYYY-MM-DD 
weather_raw <- weather_raw %>%
  mutate(DATE = parse_date_time(DATE, orders = c("ymd", "mdy")) %>% as.Date())

# Check date range
cat("Date range:", as.character(min(weather_raw$DATE, na.rm = TRUE)),
    "to", as.character(max(weather_raw$DATE, na.rm = TRUE)), "\n")

# Extract FIPS codes from filename and join to county names
weather_with_county <- weather_raw %>%
  mutate(fips = str_extract_all(source_file, "06\\d{3}")) %>%
  
  # Expand so each FIPS code gets its own row
  unnest(fips) %>%
  
  # Join county names from lookup table
  left_join(fips_lookup, by = "fips")

# Removing variables with 70%+ null values
colSums(is.na(weather_with_county))
weather_clean <- weather_with_county |> 
  select(-c(DAPR, MDPR, WT01, WT02, WT03, WT04, WT05, WT06, WT07, WT08, WT09, WT10, WT11, DASF, MDSF))

# Check it worked
cat("Rows after county assignment:", nrow(weather_clean), "\n")
cat("Unique counties:", n_distinct(weather_clean$county), "\n")

# Check no counties are missing
weather_clean %>%
  filter(is.na(county)) %>%
  distinct(source_file, fips)

# See county distribution
weather_clean %>%
  count(county) %>%
  print(n = 40)

# Saving data
weather_clean |> 
  write.csv(file.path("initial-analysis/data/weather/NOAA_weather_data.csv"))

weather <- read_csv("initial-analysis/data/weather/NOAA_weather_data.csv")
