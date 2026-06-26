### tims vs ccrs
library(ggplot2)
library(dplyr)
library(tidyverse)
library(here)
library(lubridate)
library(tidyr)

california_crashes <- read_csv(here("initial-analysis", "data-raw", "TIMS_Crashes_California.csv"))
#saveRDS(california_crashes, "TIMS_California_Crashes.rds")
california_2025 <- california_crashes |>
  filter(ACCIDENT_YEAR == "2025")

california_ccrs <- read_csv("C:/Users/kylek/Downloads/crashes_2025.csv")

california_2025$PEDESTRIAN_ACCIDENT[is.na(california_2025$PEDESTRIAN_ACCIDENT)] <- "N"
california_2025$BICYCLE_ACCIDENT[is.na(california_2025$BICYCLE_ACCIDENT)] <- "N"
california_2025$source <- "tims"
california_ccrs$source <- "ccrs"



california_2025 <- california_2025 |>
  select(COLLISION_DATE, PEDESTRIAN_ACCIDENT, BICYCLE_ACCIDENT, NUMBER_KILLED, NUMBER_INJURED, COUNT_PED_KILLED, 
         COUNT_PED_INJURED, COUNT_BICYCLIST_KILLED, COUNT_BICYCLIST_INJURED)


california_ccrs <- california_ccrs |>
  select(`Crash Date Time`, PedestrianActionCode, NumberKilled, NumberInjured)


california_2025_clean <- california_2025 %>%
  mutate(
    date = as.Date(COLLISION_DATE, format = "%Y-%m-%d"),
    is_pedestrian = ifelse(PEDESTRIAN_ACCIDENT == "Y", 1, 0)
  ) %>%
  group_by(date) %>%
  summarise(
    pedestrian_count = sum(is_pedestrian, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date_aggregated = date) %>%
  mutate(source = "tims")

california_ccrs_update <- california_ccrs %>%
  mutate(
    date = as.Date(`Crash Date Time`, format = "%m/%d/%Y %I:%M:%S %p")
)


california_ccrs_clean <- california_ccrs %>%
  mutate(
    date = as.Date(`Crash Date Time`, format = "%m/%d/%Y %I:%M:%S %p"),
    is_not_a = ifelse(PedestrianActionCode != "A", 1, 0)
  ) %>%
  group_by(date) %>%
  summarise(
    non_a_count = sum(is_not_a, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date_aggregated = date) %>%
  mutate(source = "ccrs")


california_tims_pivot <- california_2025_clean %>%
  rename(count = pedestrian_count) %>%
  select(date_aggregated, count, source)

california_ccrs_pivot <- california_ccrs_clean %>%
  rename(count = non_a_count) %>%
  select(date_aggregated, count, source)

combined_data <- bind_rows(california_tims_pivot, california_ccrs_pivot)

combined_data_cumulative <- combined_data %>%
  arrange(source, date_aggregated) %>%
  group_by(source) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()


california_tims_total <- california_2025 %>%
  mutate(
    date = as.Date(COLLISION_DATE, format = "%Y-%m-%d")
  ) %>%
  group_by(date) %>%
  summarise(
    entry_count = n(),
    .groups = "drop"
  ) %>%
  rename(date_aggregated = date) %>%
  mutate(source = "tims")


california_ccrs_entries <- california_ccrs %>%
  mutate(
    date = as.Date(`Crash Date Time`, format = "%m/%d/%Y %I:%M:%S %p")) 
  group_by(date) %>%
  summarise(
    entry_count = n(),
    .groups = "drop"
  ) %>%
  rename(date_aggregated = date) %>%
  mutate(source = "ccrs")



ggplot(combined_data_cumulative, aes(x = date_aggregated, y = cumulative_count, color = source, group = source)) +
  geom_line() +
#  geom_point(size = 2) +
  labs(
    title = "TIMS vs CCRS (2025)",
    x = "Date",
    y = "Count",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



