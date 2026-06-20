# Load packages
library(tidyverse)

# Read datasets
tims_crashes = readRDS("initial-analysis/scripts/TIMS_Filtered.rds")
ccrs_crashes = read_csv("initial-analysis/scripts/updated-crashes.csv")


# RDiT Model: Jan 1, 2024 -------------------------------------------------


# TIMS Plot of crashes
# pedestrian crashes at intersections
tims_months = c(
  "1" = "January", "2" = "February", "3" = "March", "4" = "April",
  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
  "9" = "September", "10" = "October", "11" = "November", "12" = "December" 
)

# Initial plot of crashes
tims_crashes |> 
  filter(ACCIDENT_YEAR == 2023 | ACCIDENT_YEAR == 2024) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
  ) |> 
  group_by(ACCIDENT_YEAR, Time, PED_ACTION, INTERSECTION, Post) |> 
  summarise(TOTAL_CRASHES = n(),
            .groups = "drop") |> 
  ggplot(aes(x = Time, y = TOTAL_CRASHES)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(-12, 12, by = 1)) +
  labs(title = "RDiT Model of Crashes Involving Pedestrians at Intersections",
       x = "Time",
       y = "Number of Crashes",
       caption = "Source: TIMS")



# Create the data for the model
rdit_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR == 2023 | ACCIDENT_YEAR == 2024) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
        ) |> 
  group_by(Time, Post) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Run the model
rdit_model <- lm(
  Total_crashes ~ Time + Post + Time:Post,
  data = rdit_data
)

summary(rdit_model)

# Plot the model
ggplot(rdit_data, aes(x = Time, y = Total_crashes)) +
  geom_point() +
  
  # Before policy trend
  geom_smooth(data = filter(rdit_data, Time < 0),
              method = "lm", 
              formula = y ~ x + I(x^2),
              se = TRUE) +
  # After policy trend
  geom_smooth(data = filter(rdit_data, Time >= 0),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = TRUE) +
  geom_vline(xintercept = 0,
            linetype = "dashed") +
  scale_x_continuous(breaks = seq(-12,12,1)) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(
    title = "RDiT Model of Pedestrian Crashes at Intersections",
    x = "Months Relative to Policy Implementation",
    y = "Number of Crashes",
    caption = "Source: TIMS"
  )



# RDiT Model: Jan 1, 2025 -------------------------------------------------


# Initial plot of crashes
tims_crashes |> 
  filter(ACCIDENT_YEAR == 2023 | ACCIDENT_YEAR == 2024) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
  ) |> 
  group_by(ACCIDENT_YEAR, Time, PED_ACTION, INTERSECTION, Post) |> 
  summarise(TOTAL_CRASHES = n(),
            .groups = "drop") |> 
  ggplot(aes(x = Time, y = TOTAL_CRASHES)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(-12, 12, by = 1)) +
  labs(title = "RDiT Model of Crashes Involving Pedestrians at Intersections",
       x = "Time",
       y = "Number of Crashes",
       caption = "Source: TIMS")



# Create the data for the model
rdit_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR == 2024 | ACCIDENT_YEAR == 2025) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
  ) |> 
  group_by(Time, Post) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Run the model
rdit_model2 <- lm(
  Total_crashes ~ Time + Post + Time:Post,
  data = rdit_data2
)

summary(rdit_model2)

# Plot the model
ggplot(rdit_data2, aes(x = Time, y = Total_crashes)) +
  geom_point() +
  
  # Before policy trend
  geom_smooth(data = filter(rdit_data2, Time < 0),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = TRUE) +
  # After policy trend
  geom_smooth(data = filter(rdit_data2, Time >= 0),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = TRUE) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(-12,12,1)) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(
    title = "RDiT Model of Pedestrian Crashes at Intersections",
    x = "Months Relative to Policy Implementation",
    y = "Number of Crashes",
    caption = "Source: TIMS"
  )





  
