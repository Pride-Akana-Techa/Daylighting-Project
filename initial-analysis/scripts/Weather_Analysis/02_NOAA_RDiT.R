
# -------------------------------------------------------------------------

## Implementing an RDiT model on weather data to test for continuity around     the cutoff date
## Inputs: initial-analysis/data-clean/NOAA_weather_data.rds

# -------------------------------------------------------------------------


# Load libraries and read dataset
library(tidyverse)
library(rdrobust)

weather <- readRDS("initial-analysis/data-clean/NOAA_weather_data.rds")

# Checking stations consistencies
weather |> 
  mutate(year = year(as.Date(DATE)), month = month(as.Date(DATE))) |> 
  filter(year >= "2022") |> 
  group_by(year, month) |> 
  summarize(n_stations = n_distinct(STATION), .groups = "drop") |> 
  print(n = 48)


# 1. Temperature ----------------------------------------------------------

# Getting Monthly temperature data
monthly_temp <- weather |> 
  mutate(date = as.Date(DATE),
         year  = year(date),
         month = month(date)) |> 
  filter(year >= "2022") |> 
  group_by(year, month)  |> 
  summarize(mean_temp = mean(TAVG, na.rm = TRUE),
            n_stations = n_distinct(STATION),
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
         Post_temp = ifelse(date >= cutoff, 1, 0),
         Season_factor = factor(
           case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             month %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  select(Time_temp, Post_temp, Season_factor, mean_temp) 


# Model
temp_model <- rdrobust(y = temp_rdit$mean_temp,
                      x = temp_rdit$Time_temp,
                      covs = model.matrix(~ Season_factor, temp_rdit)[, -1],
                      c = 0,
                      p = 1,
                      h = 24,
                      kernel = "uniform")

summary(temp_model)

# Adjust for seasonality
temp_season_model <- lm(mean_temp ~ Season_factor,
                    data = temp_rdit)

temp_rdit$mean_temp_adj <- resid(temp_season_model) + 
  mean(temp_rdit$mean_temp)

# RDiT plot
temp_rd_out <- rdplot(y = temp_rdit$mean_temp_adj,
                 x = temp_rdit$Time_temp,
                 c = 0,
                 p = 1,
                 h = 24,
                 kernel = "uniform",
                 nbins = c(24, 24))

temp_rd_out$rdplot +
  labs(title = "Control RDiT Model",
       y = "Mean Temperature(°F)",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/temp_rdit.png",
       height = 10,
       width = 20)



# 2. Precipitation --------------------------------------------------------

# Getting Monthly precipitation data
monthly_ppt <- weather |> 
  filter(!is.na(PRCP)) |> 
  mutate(date = as.Date(DATE),
         year  = year(date),
         month = month(date)) |> 
  filter(year >= 2022) |> 
  group_by(STATION, year, month) |>              
  summarize(station_total_ppt = sum(PRCP, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(year, month) |>                        
  summarize(mean_ppt = mean(station_total_ppt, na.rm = TRUE),
            n_stations = n_distinct(STATION),
            .groups = "drop")



# Plot Monthly precipitation amount
monthly_ppt |> 
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>  
  
  ggplot(aes(x = date, y = mean_ppt)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly Precipitation (2022–2025)",
       x = NULL,
       y = "Total Precipitation (inches)")


## Precipitation RDiT ##

# Prepare precipitation data for RDiT
ppt_rdit <- monthly_ppt |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         cutoff    = as.Date("2024-01-01"),
         Time_ppt = interval(cutoff, date) %/% months(1),
         Post_ppt = ifelse(date >= cutoff, 1, 0),
         Season_factor = factor(
           case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             month %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  select(Time_ppt, Post_ppt, Season_factor, mean_ppt) 


# Model
ppt_model <- rdrobust(y = ppt_rdit$mean_ppt,
                       x = ppt_rdit$Time_ppt,
                       covs = model.matrix(~ Season_factor, ppt_rdit)[, -1],
                       c = 0,
                       p = 2,
                       h = 24,
                       kernel = "uniform")

summary(ppt_model)

# Adjust for seasonality
ppt_season_model <- lm(mean_ppt ~ Season_factor,
                        data = ppt_rdit)

ppt_rdit$mean_ppt_adj <- resid(ppt_season_model) + 
  mean(ppt_rdit$mean_ppt)

# RDiT plot
ppt_rd_out <- rdplot(y = ppt_rdit$mean_ppt_adj,
                      x = ppt_rdit$Time_ppt,
                      c = 0,
                      p = 1,
                      h = 24,
                      kernel = "uniform",
                      nbins = c(24, 24))

ppt_rd_out$rdplot +
  labs(title = "Control RDiT Model",
       y = "Precipitation (inches)",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/ppt_rdit.png",
       height = 10,
       width = 20)

  
