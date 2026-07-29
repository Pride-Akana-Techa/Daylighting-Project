
# -------------------------------------------------------------------------
## Investigating the relationship between weather conditions and crashes
## at the cutoff

# -------------------------------------------------------------------------

# load libraries and dataset
library(tidyverse)
library(rdrobust)

tims_data <-  readRDS("initial-analysis/data-clean/02_TIMS_Cleaned.rds")

# Check yearly weather condition
weather_distribution <- tims_data |> 
  filter(ACCIDENT_YEAR >= 2022 &
           PED_ACTION == "B" &
           INTERSECTION == "Y") |> 
  filter_out(is.na(WEATHER_1)) |> 
  group_by(ACCIDENT_YEAR, MONTH, WEATHER_1)|> 
  summarise(CRASHES = n(),
            .groups = "drop")


# Monthly Proportion
weather_monthly <- tims_data |> 
  filter(ACCIDENT_YEAR >= 2022,
         PED_ACTION == "B",
         INTERSECTION == "Y") |> 
  filter_out(is.na(WEATHER_1)) |>
  mutate(MONTH_DATE = floor_date(COLLISION_DATE, "month")) |>   
  group_by(MONTH_DATE, WEATHER_1) |> 
  summarise(CRASHES = n(), .groups = "drop") |> 
  group_by(MONTH_DATE) |> 
  mutate(PROPORTION = CRASHES / sum(CRASHES)) |> 
  ungroup()



# Clear Weather -----------------------------------------------------------

## RDiT Model for clear weather crashes ##
# Prepare data
clear_data <- weather_monthly |> 
  filter(WEATHER_1 == "Clear") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


clear_model <- rdrobust(y = clear_data$PROPORTION,
                           x = clear_data$Time,
                           covs = model.matrix(~ Season_factor, clear_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform")

summary(clear_model)

# Adjusting for seasonality before plotting
season_clear_model <- lm(PROPORTION ~ Season_factor,
                            data = clear_data)

clear_data$Crash_adj <- resid(season_clear_model) + 
  mean(clear_data$PROPORTION)

# plot
clear_rd_out <- rdplot(y = clear_data$Crash_adj,
                          x = clear_data$Time,
                          c = 0,
                          p = 1,
                          h = 24,
                          kernel = "uniform",
                          nbins = c(24, 24))

clear_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Clear Weather RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/clear_rdit.png",
       height = 10,
       width = 20)



# Raining Weather -----------------------------------------------------------

## RDiT Model for raining weather crashes ##
# Prepare data
raining_data <- weather_monthly |> 
  filter(WEATHER_1 == "Raining") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


raining_model <- rdrobust(y = raining_data$PROPORTION,
                        x = raining_data$Time,
                        covs = model.matrix(~ Season_factor, raining_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 24,
                        kernel = "uniform")

summary(raining_model)

# Adjusting for seasonality before plotting
season_raining_model <- lm(PROPORTION ~ Season_factor,
                         data = raining_data)

raining_data$Crash_adj <- resid(season_raining_model) + mean(raining_data$PROPORTION)

# plot
raining_rd_out <- rdplot(y = raining_data$Crash_adj,
                       x = raining_data$Time,
                       c = 0,
                       p = 1,
                       h = 24,
                       kernel = "uniform",
                       nbins = c(24, 24))

raining_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Raining Weather RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/raining_rdit.png",
       height = 10,
       width = 20)


# Cloudy Weather -----------------------------------------------------------

## RDiT Model for cloudy weather crashes ##
# Prepare data
cloudy_data <- weather_monthly |> 
  filter(WEATHER_1 == "Cloudy") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


cloudy_model <- rdrobust(y = cloudy_data$PROPORTION,
                        x = cloudy_data$Time,
                        covs = model.matrix(~ Season_factor, cloudy_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 24,
                        kernel = "uniform")

summary(cloudy_model)

# Adjusting for seasonality before plotting
season_cloudy_model <- lm(PROPORTION ~ Season_factor,
                         data = cloudy_data)

cloudy_data$Crash_adj <- resid(season_cloudy_model) + mean(cloudy_data$PROPORTION)

# plot
cloudy_rd_out <- rdplot(y = cloudy_data$Crash_adj,
                       x = cloudy_data$Time,
                       c = 0,
                       p = 1,
                       h = 24,
                       kernel = "uniform",
                       nbins = c(24, 24))

cloudy_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Cloudy Weather RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/cloudy_rdit.png",
       height = 10,
       width = 20)
