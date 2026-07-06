
# -------------------------------------------------------------------------

## Implementing an RDiT model on weather data to test for continuity around     the cutoff date
## Inputs: initial-analysis/data-clean/NOAA_weather_data.rds

# -------------------------------------------------------------------------


# Load libraries and read data
library(tidyverse)

weather <- readRDS("initial-analysis/data-clean/NOAA_weather_data.rds")


# 1. Temperature ----------------------------------------------------------

# Getting Monthly temperature data
monthly_temp <- weather |> 
  mutate(date = as.Date(DATE),
         year  = year(date),
         month = month(date)) |> 
  filter(year >= "2022") |> 
  group_by(year, month)  |> 
  summarize(mean_temp = mean(TAVG, na.rm = TRUE),
            .groups = "drop")


# Adjust date for time series and plot
monthly_temp |> 
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>  

ggplot(aes(x = date, y = mean_temp)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly Mean Temperature (2022–2025)",
       x = NULL,
      y = "Mean Temperature (°F)")


## Temperature RDiT ##

# Prepare temperature data for RDiT
temp_rdit <- monthly_temp |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         cutoff    = as.Date("2024-01-01"),
         Time_temp = interval(cutoff, date) %/% months(1),
         Post_temp = ifelse(date >= cutoff, 1, 0))

# RDiT plot
ggplot(temp_rdit, aes(x = Time_temp, y = mean_temp)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(temp_rdit, Post_temp == 0),
              method = "lm",
              se = FALSE) +
  
  geom_smooth(data = subset(temp_rdit, Post_temp == 1),
              method = "lm",
              se = FALSE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-24, 24, by = 1)) +
  
  labs(title = "Temperature Trends Around January 2024 Cutoff",
       x     = "Month",
       y     = "Mean Temperature (°F)")


## Adjusting for Seasonality in Temperature ##

# Fit seasonal model for temperature
seasonal_temp_model <- lm(mean_temp ~ factor(month), data = monthly_weather)
summary(seasonal_temp_model)

# Create seasonally adjusted temperature
monthly_weather2 <- monthly_weather |>
  mutate(temp_sa = residuals(seasonal_temp_model) + mean(mean_temp))

# RDiT plot with seasonally adjusted temperature
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



# 2. Precipitation --------------------------------------------------------

monthly_ppt <- weather |> 
  filter_out(is.na(PRCP)) |> 
  mutate(year = year(DATE),
         month = month(DATE)) |> 
  filter(year >= "2022") |> 
  group_by(year, month) |> 
  summarise(total_ppt = sum(PRCP, na.rm = TRUE),
            n_rainy_days = n_distinct(DATE[PRCP > 0]),
            .groups = "drop")

# Plot Monthly precipitation amount
monthly_ppt |> 
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>  
  
  ggplot(aes(x = date, y = total_ppt)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly Mean Temperature (2022–2025)",
       x = NULL,
       y = "Total Precipitation (mm)")

  
