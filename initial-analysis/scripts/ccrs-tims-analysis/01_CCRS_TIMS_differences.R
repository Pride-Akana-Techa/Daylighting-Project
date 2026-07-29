
# -------------------------------------------------------------------------
# Analyze the differences in crash trends between TIMS and CCRS

# Inputs: initial-analysis/data
# Outputs: initial-analysis/figs
# -------------------------------------------------------------------------

# load packages
library(tidyverse)
library(sf)


# Read and Organize -------------------------------------------------------

# Read Datasets
tims_crashes <- readRDS("initial-analysis/scripts/01_TIMS_Cleaned.rds")
ccrs_crashes <- read_csv("initial-analysis/scripts/updated-crashes.csv")

# remove duplicate rows (YW: still have duplicates?)
ccrs_crashes <- ccrs_crashes |> distinct()

# Convert county codes to county names
ca_counties <- c(
  "1" = "Alameda", "2" = "Alpine", "3" = "Amador",
  "4" = "Butte", "5" = "Calaveras", "6" = "Colusa",
  "7" = "Contra Costa", "8" = "Del Norte", "9" = "El Dorado",
  "10" = "Fresno", "11" = "Glenn", "12" = "Humboldt",
  "13" = "Imperial", "14" = "Inyo", "15" = "Kern",
  "16" = "Kings", "17" = "Lake", "18" = "Lassen",
  "19" = "Los Angeles", "20" = "Madera", "21" = "Marin",
  "22" = "Mariposa", "23" = "Mendocino", "24" = "Merced",
  "25" = "Modoc", "26" = "Mono", "27" = "Monterey",
  "28" = "Napa", "29" = "Nevada", "30" = "Orange",
  "31" = "Placer", "32" = "Plumas", "33" = "Riverside",
  "34" = "Sacramento", "35" = "San Benito", "36" = "San Bernardino",
  "37" = "San Diego", "38" = "San Francisco", "39" = "San Joaquin",
  "40" = "San Luis Obispo", "41" = "San Mateo", "42" = "Santa Barbara",
  "43" = "Santa Clara", "44" = "Santa Cruz", "45" = "Shasta",
  "46" = "Sierra", "47" = "Siskiyou", "48" = "Solano",
  "49" = "Sonoma", "50" = "Stanislaus", "51" = "Sutter",
  "52" = "Tehama", "53" = "Trinity", "54" = "Tulare",
  "55" = "Tuolumne", "56" = "Ventura", "57" = "Yolo",
  "58" = "Yuba"
)

ccrs_crashes <- ccrs_crashes |>
  mutate(
    county_name = ca_counties[as.character(`County Code`)]
  )

colSums(is.na(ccrs_crashes))


# Individual Comparison ---------------------------------------------------

# number of crashes by counties
 # 1. TIMS
tims_crashes |> 
  filter(ACCIDENT_YEAR == 2025) |> 
  group_by(COUNTY) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |> 
  ggplot(aes(x = Total_crashes, y = COUNTY, fill = COUNTY)) +
  geom_col() +
  scale_color_viridis_b() +
  scale_x_continuous(breaks = seq(0, 100000, by = 10000),
                     labels = scales::comma,
                     expand = c(0, 0)) +
  theme_minimal() +
  labs(
    title = "Number of Crashes by Counties",
    x = "Number of Crashes",
    y = "County",
    color = "County",
    caption = "Source: TIMS 2016-2025"
  )

# 2. CCRS
ccrs_crashes |> 
  filter_out(is.na(county_name)) |> 
  filter(Year >= 2020) |> 
  group_by(county_name) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |> 
  ggplot(aes(x = Total_crashes, y = county_name, fill = county_name)) +
  geom_col() +
  scale_color_viridis_b() +
  scale_x_continuous(breaks = seq(0, 2000000, by = 100000),
                     labels = scales::comma,
                     expand = c(0, 0)) +
  theme_minimal() +
  labs(
    title = "Number of Crashes by Counties",
    x = "Number of Crashes",
    y = "County",
    color = "County",
    caption = "Source: CCRS 2016-2025"
  )



# Merged Comparison -------------------------------------------------------
# All Years from 2014 - 2025 

# TIMS summary
tims_summary <- tims_crashes |> 
  group_by(COUNTY) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |> 
  rename(County = COUNTY) |> 
  mutate(
    County = str_to_title(trimws(County)),
    Source = "TIMS"
  )

# CCRS summary
ccrs_summary <- ccrs_crashes |> 
  filter_out(is.na(county_name)) |> 
  group_by(county_name) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |> 
  rename(County = county_name) |> 
  mutate(
    County = str_to_title(trimws(County)),
    Source = "CCRS"
  )

# Combine
combined_crashes <- bind_rows(tims_summary, ccrs_summary)

# Plot
ggplot(combined_crashes,
       aes(x = Total_crashes,
           y = reorder(County, Total_crashes),
           fill = Source)) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 2000000, by = 100000),
                     labels = scales::comma,
                     expand = 0
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Crash Counts by County: TIMS vs CCRS",
    x = "Number of Crashes",
    y = "County",
    fill = "Dataset"
  )

# 2. Specific Years
combined_crashes |> 
  filter()
ggplot(aes(x = Total_crashes,
           y = reorder(County, Total_crashes),
           fill = Source)) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 2000000, by = 100000),
                     labels = scales::comma,
                     expand = 0
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Crash Counts by County: TIMS vs CCRS",
    x = "Number of Crashes",
    y = "County",
    fill = "Dataset"
  )

# Comparing Crash Trends --------------------------------------------------

# TIMS
# Selecting needed variables and making the dataset more uniform and descriptive
tims_updated <- tims_crashes |> 
  select(PED_ACTION, LIGHTING, WEATHER_1, ACCIDENT_YEAR, DAY_OF_WEEK, COLLISION_DATE, COUNTY) |> 
  mutate(month = month(ymd(COLLISION_DATE))) |> 
  rename(
    ped_action = PED_ACTION,
    lighting = LIGHTING,
    weather = WEATHER_1,
    year = ACCIDENT_YEAR,
    day = DAY_OF_WEEK,
    county_name = COUNTY
  ) 

tims_months = c(
  "1" = "January", "2" = "February", "3" = "March", "4" = "April",
  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
  "9" = "September", "10" = "October", "11" = "November", "12" = "December" 
)

tims_days = c(
  "1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday",
  "5" = "Friday", "6" = "Saturday", "7" = "Sunday"
)

tims_lighting = c(
  "A" = "Daylight", "B" = "Dusk/Dawn", "C" = "Dark",
  "D" = "Dark", "E" = "Dark"
)

tims_weather = c(
  "A" = "Clear", "B" = "Cloudy", "C" = "Raining",
  "D" = "Other", "E" = "Other", "F" = "Other", "G" = "Other"
)

tims_updated <- tims_updated |> 
  mutate(
    lighting  = tims_lighting[as.character(lighting)],
    weather = tims_weather[as.character(weather)],
    day = tims_days[as.character(day)],
    month = tims_months[as.character(month)],
    Source = "TIMS",
    county_name = str_to_title(trimws(county_name))
  )


# CCRS 
ccrs_updated <- ccrs_crashes |> 
  select(PedestrianActionCode, LightingDescription, `Weather 1`, Year, `Day Of Week`, Month, county_name) |> 
  rename(
    ped_action = PedestrianActionCode,
    lighting = LightingDescription,
    weather = `Weather 1`,
    year = Year,
    day = `Day Of Week`,
    month = Month
  )

ccrs_updated = ccrs_updated |> 
  mutate(weather = case_when(
    weather == "CLEAR" ~ "Clear",
    weather == "CLOUDY" ~ "Cloudy",
    weather == "RAINING" ~ "Raining",
    weather %in% c("WIND", "UNKNOWN", "SNOWING", "SMOKY", "SMOKEY", 
                      "OTHER", "FOG/VISIBILITY") ~ "Other"
  ),
  lighting = case_when(
    lighting == "DAYLIGHT" ~ "Daylight",                     
    lighting  %in% c("DARK-STREET LIGHTS NOT FUNCTIONING",
                               "DARK-STREET LIGHTS",
                               "DARK-NO STREET LIGHTS") ~ "Dark",
    lighting =="DUSK-DAWN" ~"Dusk-Dawn"
  ),
  Source = "CCRS",
  county_name = str_to_title(trimws(county_name))
  ) 

combined_crashes2 <- bind_rows(tims_updated, ccrs_updated)

# Comparing crashes from 2014 - 2025
combined_crashes2 |> 
  filter_out(is.na(county_name)) |> 
  filter(ped_action == "B") |> 
  group_by(county_name, Source, ped_action) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |>
ggplot(aes(x = Total_crashes,
           y = reorder(county_name, Total_crashes),
           fill = Source)) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 40000, by = 2500),
                     labels = scales::comma,
                     expand = 0
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Crashes Involving Pedestrians at Intersections by County: TIMS vs CCRS",
    x = "Number of Crashes",
    y = "County",
    fill = "Dataset"
  )

ggsave(file = "initial-analysis/figs/int-ped-crashes(2014-2025).png",
       height = 10,
       width = 15) 

# Comparing just 2025 
combined_crashes2 |> 
  filter_out(is.na(county_name)) |> 
  filter(year == 2025) |> 
  filter(ped_action == "B") |> 
  group_by(county_name, Source, ped_action) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |> 
  ggplot(aes(x = Total_crashes,
             y = reorder(county_name, Total_crashes),
             fill = Source)) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 2000, by = 100),
                     labels = scales::comma,
                     expand = 0
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Crashes Involving Pedestrians at Intersections by County: TIMS vs CCRS",
    x = "Number of Crashes",
    y = "County",
    fill = "Dataset"
  )

ggsave(file = "initial-analysis/figs/int-ped-crashes-2025.png",
       height = 10,
       width = 15) 


# Weather
combined_crashes2 |> 
  filter_out(is.na(weather)) |> 
  filter(year == 2025) |> 
  filter(ped_action == "B") |> 
  group_by(Source, ped_action, weather) |> 
  summarize(Total_crashes = n(),
            .groups = "drop") |>
  
  ggplot(aes(x = reorder(weather, Total_crashes),
             y = Total_crashes,
             fill = Source)) +
  scale_color_viridis_d() +
  scale_y_continuous(breaks = seq(0, 20000, by = 1000),
                     labels = scales::comma,
                     expand = 0
  ) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Crashes Involving Pedestrians at Intersections by Weather Condition",
    x = "Weather Condition",
    y = "Number of Crashes",
    fill = "Dataset"
  )
