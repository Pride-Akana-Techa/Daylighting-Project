library(tidyverse)

tims_crashes |> 
  
  filter(ACCIDENT_YEAR >= 2023) |> 
  
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  
  mutate(
    MONTH = month(ymd(COLLISION_DATE)),
    MONTH_NAME = month(ymd(COLLISION_DATE), label = TRUE)
  ) |> 
  
  group_by(ACCIDENT_YEAR, MONTH, MONTH_NAME) |> 
  
  summarise(
    TOTAL_CRASHES = n(),
    .groups = "drop"
  ) |> 
  
  ggplot(aes(
    x = MONTH_NAME,
    y = TOTAL_CRASHES,
    group = ACCIDENT_YEAR,
    color = factor(ACCIDENT_YEAR)
  )) +
  
  geom_line() +
  geom_point() +
  
  theme_minimal() +
  
  labs(
    title = "Monthly Pedestrian Crashes at Intersections by Year",
    x = "Month",
    y = "Number of Crashes",
    color = "Year",
    caption = "Source: TIMS"
  )


tims_crashes |> 
  
  filter(COLLISION_DATE >= as.Date("2023-01-01") &
           COLLISION_DATE <= as.Date("2025-12-31")) |> 
  
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  
  mutate(
    MONTH = floor_date(ymd(COLLISION_DATE), "month")
  ) |> 
  
  group_by(MONTH) |> 
  
  summarise(
    TOTAL_CRASHES = n(),
    .groups = "drop"
  ) |> 
  
  ggplot(aes(x = MONTH, y = TOTAL_CRASHES)) +
  
  geom_line() +
  geom_point() +
  
  geom_vline(
    xintercept = as.Date("2024-01-01"),
    linetype = "dashed"
  ) +
  
  theme_minimal() +
  
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  labs(
    title = "Monthly Pedestrian Crashes at Intersections (Jan 2023 - Dec 2025)",
    x = "Month",
    y = "Number of Crashes",
    caption = "Source: TIMS"
  )
