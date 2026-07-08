
# -------------------------------------------------------------------------

## Regression Discontinuity Placebo Tests
## Inputs: initial/analysis/data-clean

# -------------------------------------------------------------------------

# Load libraries and datasets
library(tidyverse)
library(rdrobust)

tims_data <- readRDS("initial-analysis/data-clean/updated_tims.rds")


# Test 1 ------------------------------------------------------------------

# Create the data for the control model

placebo_data1 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2021, 2022, 2023)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2022-07-01"), MONTH) %/% months(1),
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


placebo_model1 <- rdrobust(y = placebo_data1$Total_crashes,
                           x = placebo_data1$Time,
                           covs = model.matrix(~ Season_factor, placebo_data1)[, -1],
                           c = 0,
                           p = 1,
                           h = 18,
                           kernel = "uniform")

summary(placebo_model1)

# Adjusting for seasonality before plotting
season_placebo_model1 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data1)

placebo_data1$Crash_adj1 <- resid(season_placebo_model1) + mean(placebo_data1$Total_crashes)

# plot
placebo_out1 <- rdplot(y = placebo_data1$Crash_adj1,
                      x = placebo_data1$Time,
                      c = 0,
                      p = 1,
                      h = 20,
                      kernel = "uniform",
                      nbins = c(18, 18))

placebo_out1$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to July 2022") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_1_rdit.png",
       height = 10,
       width = 20)



# Test 2 ------------------------------------------------------------------

# Create the data for the control model

placebo_data2 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2014, 2015, 2016, 2017)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2016-01-01"), MONTH) %/% months(1),
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
placebo_model2 <- rdrobust(y = placebo_data2$Total_crashes,
                           x = placebo_data2$Time,
                           covs = model.matrix(~ Season_factor, placebo_data2)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform")

summary(placebo_model2)

# Adjusting for seasonality before plotting
season_placebo_model2 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data2)

placebo_data2$Crash_adj2 <- resid(season_placebo_model2) + mean(placebo_data2$Total_crashes)

# plot
placebo_out2 <- rdplot(y = placebo_data2$Crash_adj2,
                       x = placebo_data2$Time,
                       c = 0,
                       p = 1,
                       h = 24,
                       kernel = "uniform",
                       nbins = c(24, 24))

placebo_out2$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2016") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_2_rdit.png",
       height = 10,
       width = 20)


# Test 3 ------------------------------------------------------------------

# Create the data for the control model

placebo_data3 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2015, 2016, 2017, 2018)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2017-01-01"), MONTH) %/% months(1),
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


placebo_model3 <- rdrobust(y = placebo_data3$Total_crashes,
                           x = placebo_data3$Time,
                           covs = model.matrix(~ Season_factor, placebo_data3)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform")

summary(placebo_model3)

# Adjusting for seasonality before plotting
season_placebo_model3 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data3)

placebo_data3$Crash_adj3 <- resid(season_placebo_model3) + mean(placebo_data3$Total_crashes)

# plot
placebo_out3 <- rdplot(y = placebo_data3$Crash_adj3,
                       x = placebo_data3$Time,
                       c = 0,
                       p = 1,
                       h = 24,
                       kernel = "uniform",
                       nbins = c(24, 24))

placebo_out3$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2017") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_3_rdit.png",
       height = 10,
       width = 20)



# Test 4 ------------------------------------------------------------------

# Create the data for the control model

placebo_data4 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2016, 2017, 2018, 2019)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2018-01-01"), MONTH) %/% months(1),
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


placebo_model4 <- rdrobust(y = placebo_data4$Total_crashes,
                           x = placebo_data4$Time,
                           covs = model.matrix(~ Season_factor, placebo_data4)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform")

summary(placebo_model4)

# Adjusting for seasonality before plotting
season_placebo_model4 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data4)

placebo_data4$Crash_adj4 <- resid(season_placebo_model4) + mean(placebo_data4$Total_crashes)

# plot
placebo_out4 <- rdplot(y = placebo_data4$Crash_adj4,
                       x = placebo_data4$Time,
                       c = 0,
                       p = 1,
                       h = 24,
                       kernel = "uniform",
                       nbins = c(24, 24))

placebo_out4$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2018") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_4_rdit.png",
       height = 10,
       width = 20)



