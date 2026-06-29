
# -------------------------------------------------------------------------

## RDiT on Weather Data

# -------------------------------------------------------------------------


# Load libraries and read data
library(tidyverse)
weather <- read_csv("initial-analysis/data/weather/NOAA_weather_data.csv")
weather_clean <- weather |> 
  select(-county) |> 
  distinct() |> 
  filter_out(is.na(TAVG)) 

# Getting Monthly weather data
monthly_weather <- weather_clean |> 
  mutate(date = as.Date(DATE),
         year  = year(date),
         month = month(date)) |> 
  filter(year >= "2022") |> 
  group_by(year, month)  |> 
  summarise(mean_temp = mean(TAVG, na.rm = TRUE),
            .groups = "drop")


monthly_weather |> 
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>  # rebuild date for plotting

# Plot
#ggplot(aes(x = date, y = mean_temp, 
#           color = factor(year), group = factor(year))) +
#  geom_line() +
#  geom_point(size = 2) +
#  scale_x_date(date_breaks = "3 months",
#               date_labels = "%b %Y") +
#  theme_minimal(base_size = 13) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  labs(title = "Monthly Mean Temperature (2022–2025)",
#       x = NULL,
#      y = "Mean Temperature (°F)")
  
  ggplot(aes(x = month, y = mean_temp, 
                              color = factor(year), 
                              group = factor(year))) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +  # converts 1-12 to Jan-Dec
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly Mean Temperature (2022–2025)",
       x = NULL,
       y = "Mean Temperature (°F)",
       color = "Year")  

## Temperature RDiT ##

# Prepare temperature data for RDiT
monthly_weather <- monthly_weather |>
  mutate(
    date      = as.Date(paste(year, month, "01", sep = "-")),
    cutoff    = as.Date("2024-01-01"),
    Time_temp = interval(cutoff, date) %/% months(1),  # months relative to cutoff
    Post_temp = ifelse(date >= cutoff, 1, 0)
  )

# RDiT plot
ggplot(monthly_weather, aes(x = Time_temp, y = mean_temp)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(monthly_weather, Post_temp == 0),
              method = "lm",
              se = FALSE) +
  
  geom_smooth(data = subset(monthly_weather, Post_temp == 1),
              method = "lm",
              se = FALSE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-24, 24, by = 3),
                     labels = function(x) {
                       as.Date("2024-01-01") %m+% months(x) |> format("%b %Y")
                     }) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(title = "Temperature Trends Around January 2024 Cutoff",
       x     = "Month",
       y     = "Mean Temperature (°F)")







# Step 1: Fit seasonal model for temperature
seasonal_temp_model <- lm(mean_temp ~ factor(month), data = monthly_weather)
summary(seasonal_temp_model)

# Step 2: Create seasonally adjusted temperature
monthly_weather2 <- monthly_weather |>
  mutate(temp_sa = residuals(seasonal_temp_model) + mean(mean_temp))

# Step 3: RDiT plot with seasonally adjusted temperature
ggplot(monthly_weather2, aes(x = Time_temp, y = temp_sa)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(monthly_weather2, Post_temp == 0),
              method = "lm",
              se = FALSE) +
  
  geom_smooth(data = subset(monthly_weather2, Post_temp == 1),
              method = "lm",
              se = FALSE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-24, 23, by = 1)) +
  
  labs(title = "Temperature Trends Around January 2024 Cutoff (Seasonally Adjusted)",
       x     = "Month",
       y     = "Mean Temperature (°F, Seasonally Adjusted)")
