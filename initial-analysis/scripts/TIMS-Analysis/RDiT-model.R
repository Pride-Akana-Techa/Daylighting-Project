# -------------------------------------------------------------------------
# Use the RDiT technique for causal inference
# Inputs: initial-analysis/data
# -------------------------------------------------------------------------


# Set Up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(rdrobust) # for RDiT analysis and plots
library(rddensity) 

# Read datasets
tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds")
ccrs_crashes <-  read_csv("initial-analysis/dat/updated-crashes.csv")



# Initial Monthly Crash Plot ----------------------------------------------

# Create the plot data
plot_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
  ) |> 
  count(Time, Post, name = "Total_crashes")

# Plot the monthly trends
  ggplot(plot_data,
         aes(x = Time, y = Total_crashes)) +
  geom_point() +
  
  geom_smooth(data = subset(plot_data, Post == 0),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = FALSE) +
  
  geom_smooth(data = subset(plot_data, Post == 1),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = FALSE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed",
             linewidth = 1) +
    
    theme_minimal(base_size = 13) +
    scale_x_continuous(breaks = seq(-24, 23, by = 1)) +
    scale_color_viridis_d() +
  
  labs(title = "Model of Crashes Involving Pedestrians at Intersections",
       x = "Months Relative to Jan 01, 2024",
       y = "Monthly Pedestrian Crashes") 
  
# Save plot
ggsave(filename = "initial-analysis/figs/initial-lm.png",
       width = 20,
       height = 10)


# Prepare Data for Model --------------------------------------------------

# Cleaning and organizing the data with selected features
tims_months = c(
  "1" = "January", "2" = "February", "3" = "March", "4" = "April",
  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
  "9" = "September", "10" = "October", "11" = "November", "12" = "December" 
)

tims_days = c(
  "1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday",
  "5" = "Friday", "6" = "Saturday", "7" = "Sunday"
)

tims_lighting = c(
  "A" = "Daylight", "B" = "Dusk/Dawn", "C" = "Dark",
  "D" = "Dark", "E" = "Dark"
)

tims_weather = c(
  "A" = "Clear", "B" = "Cloudy", "C" = "Raining",
  "D" = "Other", "E" = "Other", "F" = "Other", "G" = "Other"
)

tims_road_surface = c(
  "A" = "Dry", "B" = "Wet", "C" = "Snowy/Icy", "D" = "Other"
)

tims_updated <- tims_crashes |> 
  mutate(
    LIGHTING  = tims_lighting[as.character(LIGHTING)],
    WEATHER_1 = tims_weather[as.character(WEATHER_1)],
    DAY_OF_WEEK = tims_days[as.character(DAY_OF_WEEK)],
  )


# Create the data for the model
rdit_data <- tims_updated |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  filter_out(is.na(WEATHER_1)) |> 
  filter_out(is.na(LIGHTING)) |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Month_factor = factor((Time %% 12 + 12) %% 12),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
        ) |> 
  group_by(Time, Post, Month_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")


# Run Model ---------------------------------------------------------------

## 1. Linear Model

# Run the model
c <- 0  # setting the cutoff
rdit_model <- lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) +                    Month_factor,
                 data = rdit_data)

summary(rdit_model)

# Sensitivity Check

tibble(Model = c("Narrow (±9mo)", "Full (±24mo)", "Wide (±18mo)"),
       Estimate = c(
         coef(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                 data = filter(rdit_data, Time >= -9 & Time <= 9)))["Post:I(Time - c)"],
         
         coef(rdit_model)["Post:I(Time - c)"],
         
         coef(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                 data = filter(rdit_data, Time >= -18 & Time <= 18)))["Post:I(Time - c)"]
       ),
       
       P_value = c(
         summary(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                    data = filter(rdit_data, Time >= -9 & Time <= 9)))$coefficients["Post:I(Time - c)", 4],
         
         summary(rdit_model)$coefficients["Post:I(Time - c)", 4],
         
         summary(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                    data = filter(rdit_data, Time >= -18 & Time <= 18)))$coefficients["Post:I(Time - c)", 4]
       ))



## 2. Using rdrobust

rd_model <- rdrobust(y = rdit_data$Total_crashes,
                     x = rdit_data$Time,
                     covs = model.matrix(~ Month_factor, rdit_data)[, -1],
                     c = 0,
                     p = 2,
                     h = 24,
                     kernel = "uniform",
                     bwselect = "mserd")

summary(rd_model)


# Plot Model --------------------------------------------------------------
ggplot(rdit_data,
       aes(Time, Total_crashes)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(rdit_data, Post == 0),
              method = "lm",
              se = TRUE) +
  
  geom_smooth(data = subset(rdit_data, Post == 1),
              method = "lm",
              se = TRUE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-24, 23, by = 1)) +
  
  labs(title = "Regression Discontinuity in Time: AB 413",
       x = "Months Relative to January 2024",
       y = "Monthly Pedestrian Crashes") 
  
# Save plot
ggsave(filename = "initial-analysis/figs/rdit.png",
       height = 10,
       width = 20)


# Placebo Test ------------------------------------------------------------
# Create the data for the control model
placebo_data <- tims_updated |> 
  filter(ACCIDENT_YEAR %in% c(2021, 2022, 2023)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  filter_out(is.na(WEATHER_1)) |> 
  filter_out(is.na(LIGHTING)) |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2022-07-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Month_factor = factor((Time %% 12 + 12) %% 12)) |> 
  group_by(Time, Post, Month_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")


# Run the model
placebo_model <- lm(Total_crashes ~ Time + Post + Time*Post + 
                   Month_factor,
                 data = placebo_data)

summary(placebo_model)

# Plot the test
ggplot(placebo_data,
       aes(Time, Total_crashes)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(placebo_data, Post == 0),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = TRUE) +
  
  geom_smooth(data = subset(placebo_data, Post == 1),
              method = "lm",
              formula = y ~ x + I(x^2),
              se = TRUE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-18, 17, by = 1)) +
  
  labs(title = "RDiT Placebo Test: AB 413",
       x = "Months Relative to July 2022",
       y = "Monthly Pedestrian Crashes") 

# Save plot
ggsave(filename = "initial-analysis/figs/placebo.png",
       height = 10,
       width = 20)











