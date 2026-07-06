# -------------------------------------------------------------------------
# Apply the RDiT technique for causal inference (covariate: seasonal factor)
# - Use simple linear model for preliminary analysis 
# - Use the rdrobust package for more sophisticated analysis
#
# Inputs: initial-analysis/data
# -------------------------------------------------------------------------


# Set Up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(rdrobust) # for RDiT analysis and plots
library(rddensity) 
library(lmtest)
library(car)

# Read datasets
tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds")


# Controlling for Monthly Differences ---------------------------------------

## Create the data for the model controlling for Months ##
rdit_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Month_factor = factor((Time %% 12 + 12) %% 12)) |> 
  group_by(Time, Post, Month_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")



## Using rdrobust ##

rd_model <- rdrobust(y = rdit_data$Total_crashes,
                     x = rdit_data$Time,
                     covs = model.matrix(~ Month_factor, rdit_data)[, -1],
                     c = 0,
                     p = 1,
                     h = 24,
                     kernel = "uniform",
                     bwselect = "mserd")

summary(rd_model)

# Adjusting for seasonality before plotting
season_model <- lm(Total_crashes ~ Month_factor,
                   data = rdit_data)

rdit_data$Crash_adj <- resid(season_model) + mean(rdit_data$Total_crashes)

# plot
rdplot(y = rdit_data$Crash_adj,
       x = rdit_data$Time,
       c = 0,
       p = 1,
       h = 24,
       kernel = "uniform",
       nbins = c(24, 24),
       title = "RDiT with Controlled Months",
       x.label = "Months Relative to AB 413 Implementation",
       y.label = "Number of Crashes",
       
       x.lim = c(-24, 24))


# Controlling for Seasonal Differences ------------------------------------

## A.Create the data for the model controlling for Seasons ##
rdit_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

## Using rdrobust ##
rd_model2 <- rdrobust(y = rdit_data2$Total_crashes,
                      x = rdit_data2$Time,
                      covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                      c = 0,
                      p = 1,
                      h = 24,
                      kernel = "uniform",
                      bwselect = "mserd")

summary(rd_model2)

# Adjusting for seasonality before plotting
season_model2 <- lm(Total_crashes ~ Season_factor,
                    data = rdit_data2)

rdit_data2$Crash_adj2 <- resid(season_model2) + mean(rdit_data2$Total_crashes)

# plot
rd_out <- rdplot(y = rdit_data2$Crash_adj2,
                 x = rdit_data2$Time,
                 c = 0,
                 p = 1,
                 h = 24,
                 kernel = "uniform",
                 nbins = c(24, 24))

rd_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/season_rdit.png",
       height = 10,
       width = 20)



# Jan 2025 Cutoff ---------------------------------------------------------

## Using Jan 2025
rdit_data3 <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2024) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Run model
rd_model3 <- rdrobust(y = rdit_data3$Total_crashes,
                      x = rdit_data3$Time,
                      covs = model.matrix(~ Season_factor, rdit_data3)[, -1],
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "uniform",
                      bwselect = "mserd")

summary(rd_model3)

# Adjusting for seasonality before plotting
season_model3 <- lm(Total_crashes ~ Season_factor,
                    data = rdit_data3)

rdit_data3$Crash_adj3 <- resid(season_model3) + mean(rdit_data3$Total_crashes)

# plot
rd3_out <- rdplot(y = rdit_data3$Crash_adj3,
                  x = rdit_data3$Time,
                  c = 0,
                  p = 1,
                  h = 12,
                  kernel = "uniform",
                  nbins = c(12, 12))

rd3_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/Jan25_rdit.png",
       height = 10,
       width = 20)







