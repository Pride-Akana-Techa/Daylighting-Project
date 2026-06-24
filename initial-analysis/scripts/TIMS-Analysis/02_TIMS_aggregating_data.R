# -------------------------------------------------------------------------
# 02_TIMS_aggregating_data
# Code contains different aggregations and graph outputs
# -------------------------------------------------------------------------
# load libraries

library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(here)
library(readr)

# -------------------------------------------------------------------------
# load cleaned data 
cal_filt <- readRDS("initial-analysis/data-clean/01_TIMS_Cleaned.rds")

# -------------------------------------------------------------------------
# add year start dates and policy start and enforcement dates 
year_start <- as.Date(c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                        "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01",
                        "2024-01-01", "2025-01-01", "2026-01-01"))
daylight_warn_date <- as.Date(c("2024-01-01"))
daylight_enforce_date <- as.Date(c("2025-01-01"))


# -------------------------------------------------------------------------
# statistics by day aggregation 
daily_data <- cal_filt |>
  mutate(day = floor_date(COLLISION_DATE, "day")) %>%
  group_by(day, day_of_week = DAY_OF_WEEK) %>%
  summarize(
    ped_killed = sum(COUNT_PED_KILLED, na.rm = TRUE),
    ped_injured = sum(COUNT_PED_INJURED, na.rm = TRUE),
    
    bike_killed = sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE),
    bike_injured = sum(COUNT_BICYCLIST_INJURED, na.rm = TRUE),
    
    total_killed = sum(COUNT_PED_KILLED, na.rm = TRUE) +
      sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE),
    total_injured = sum(COUNT_BICYCLIST_INJURED, na.rm = TRUE) +
      sum(COUNT_PED_INJURED, na.rm = TRUE),
    
    ped_killed_int = sum(COUNT_PED_KILLED[INTERSECTION == "Y"], na.rm = TRUE),
    ped_injured_int = sum(COUNT_PED_INJURED[INTERSECTION == "Y"], na.rm = TRUE),
    
    bike_killed_int = sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "Y"], na.rm = TRUE),
    bike_injured_int = sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "Y"], na.rm = TRUE),
    
    total_killed_int = sum(COUNT_PED_KILLED[INTERSECTION == "Y"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "Y"], na.rm = TRUE),
    
    total_injured_int = sum(COUNT_PED_INJURED[INTERSECTION == "Y"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "Y"], na.rm = TRUE),
    
    ped_killed_nonint = sum(COUNT_PED_KILLED[INTERSECTION == "N"], na.rm = TRUE),
    ped_injured_nonint = sum(COUNT_PED_INJURED[INTERSECTION == "N"], na.rm = TRUE),
    bike_killed_nonint = sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "N"], na.rm = TRUE),
    bike_injured_nonint = sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "N"], na.rm = TRUE),
    
    total_killed_nonint = sum(COUNT_PED_KILLED[INTERSECTION == "N"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "N"], na.rm = TRUE),
    
    total_injured_nonint = sum(COUNT_PED_INJURED[INTERSECTION == "N"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "N"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(day)


ggplot(daily_data, aes(x = day, y = ped_killed)) +
  stat_summary(
    aes(x = floor_date(day, "month")),
    fun = sum, geom = "line", group = 1) + 
  geom_vline(xintercept = year_start,
             linetype = "dashed", color = "grey", size = .5) +
  geom_vline(xintercept = daylight_warn_date,
             color = "blue", size = 2, alpha = .5) +
  geom_vline(xintercept = daylight_enforce_date,
             color = "green", size = 2, alpha = .5) +

# warning period   
  annotate("rect",
           xmin = daylight_warn_date,
           xmax = daylight_enforce_date,
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightblue") +
  
# enforcement period 
  annotate("rect",
           xmin = daylight_enforce_date,
           xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgreen") +
  scale_x_date(date_breaks = "2 year",
               date_labels = "%b %Y") +
  theme_minimal()

monthly_table <- daily_data %>%
  mutate(month = floor_date(day, "month")) %>%
  group_by(month) %>%
  summarize(
    killed = sum(killed, na.rm = TRUE),
    ped_killed = sum(ped_killed, na.rm = TRUE),
    ped_injured = sum(ped_injured, na.rm = TRUE),
    bike_killed = sum(bike_killed, na.rm = TRUE),
    bike_injured = sum(bike_injured, na.rm = TRUE),
    injured = sum(injured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month)

monthly_table


dow_data <- cal_filt %>%
  group_by(ACCIDENT_YEAR, day_of_week = DAY_OF_WEEK) %>%
  summarize(
    ped_killed = sum(COUNT_PED_KILLED, na.rm = TRUE),
    bike_killed = sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE)
  )


dow_data <- dow_data %>%
  mutate(day_of_week_name = factor(
    day_of_week, 
    levels = 1:7, 
    labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  ))


# ggplot(dow_data, aes(fill = ACCIDENT_YEAR, x = day_of_week_name, 
#                      y = ped_killed, 
#                      group = factor(ACCIDENT_YEAR))) +
#   geom_bar(position="dodge", stat="identity")

