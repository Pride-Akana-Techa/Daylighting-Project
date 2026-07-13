
# -------------------------------------------------------------------------

## This script evaluates other crash trends, to determine how they change after ## the policy.
## Inputs: initial-analysis/

# -------------------------------------------------------------------------



# Set Up ------------------------------------------------------------------

# Load packages and dataset
library(tidyverse)
library(rdrobust)

tims_crashes <- readRDS("initial-analysis/data-clean/updated_tims.rds")


# Crashes with no pedestrians ---------------------------------------------

## Jan 2024 Cutoff
# Prepare model data
no_ped_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(PED_ACTION == "A") |>
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
no_ped_model <- rdrobust(y = no_ped_data$Total_crashes,
                      x = no_ped_data$Time,
                      covs = model.matrix(~ Season_factor, no_ped_data)[, -1],
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular")

summary(no_ped_model)

# Adjusting for seasonality before plotting
seasonal_model <- lm(Total_crashes ~ Season_factor,
                    data = no_ped_data)

no_ped_data$Crash_adj2 <- resid(seasonal_model) + mean(no_ped_data$Total_crashes)

# plot
no_ped_rd_out <- rdplot(y = no_ped_data$Crash_adj2,
                 x = no_ped_data$Time,
                 c = 0,
                 p = 1,
                 h = 12,
                 kernel = "triangular",
                 nbins = c(12, 12))

no_ped_rd_out$rdplot +
  labs(title = "RDiT Model(Crashes with No Pedestrians)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## 2. Jan 2025 Cutoff
# Prepare data for the model
no_ped_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(PED_ACTION == "A") |>
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

# Run the model
no_ped_model2 <- rdrobust(y = no_ped_data2$Total_crashes,
                         x = no_ped_data2$Time,
                         covs = model.matrix(~ Season_factor, no_ped_data2)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(no_ped_model2)

# Adjusting for seasonality before plotting
seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                     data = no_ped_data2)

no_ped_data2$Crash_adj2 <- resid(seasonal_model2) + mean(no_ped_data2$Total_crashes)

# plot
no_ped_rd_out2 <- rdplot(y = no_ped_data2$Crash_adj2,
                        x = no_ped_data2$Time,
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular",
                        nbins = c(12, 12))

no_ped_rd_out2$rdplot +
  labs(title = "RDiT Model (Crashes with No Pedestrians)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


# Pedestrians not at Intersections ----------------------------------------
## This includes:
## - Crossing in crosswalk not at intersection
## - Crossing not in crosswalk
## - In Road and Shoulder Area

## 1. Jan 2024 Cutoff
# Prepare model data
other_ped_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(PED_ACTION %in% c("C", "D", "E")) |>
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

# Run the model
other_ped_model <- rdrobust(y = other_ped_data$Total_crashes,
                         x = other_ped_data$Time,
                         covs = model.matrix(~ Season_factor, other_ped_data)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(other_ped_model)

# Adjusting for seasonality before plotting
seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                     data = other_ped_data)

other_ped_data$Crash_adj2 <- resid(seasonal_model2) + mean(other_ped_data$Total_crashes)

# plot
other_ped_rd_out <- rdplot(y = other_ped_data$Crash_adj2,
                        x = other_ped_data$Time,
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular",
                        nbins = c(12, 12))

other_ped_rd_out$rdplot +
  labs(title = "RDiT Model(Pedestrian Non-intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## 2. Jan 2025 Cutoff
# Prepare data for the model
other_ped_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(PED_ACTION %in% c("C", "D", "E")) |>
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

# Run the model
other_ped_model2 <- rdrobust(y = other_ped_data2$Total_crashes,
                          x = other_ped_data2$Time,
                          covs = model.matrix(~ Season_factor, other_ped_data2)[, -1],
                          c = 0,
                          p = 1,
                          h = 12,
                          kernel = "triangular")

summary(other_ped_model2)

# Adjusting for seasonality before plotting
seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                      data = other_ped_data2)

other_ped_data2$Crash_adj2 <- resid(seasonal_model2) + mean(other_ped_data2$Total_crashes)

# plot
other_ped_rd_out2 <- rdplot(y = other_ped_data2$Crash_adj2,
                         x = other_ped_data2$Time,
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular",
                         nbins = c(12, 12))

other_ped_rd_out2$rdplot +
  labs(title = "RDiT Model (Pedestrian Non-intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )







