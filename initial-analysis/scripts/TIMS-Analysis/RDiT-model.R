# -------------------------------------------------------------------------
# Use the RDiT technique for causal inference
# Inputs: initial-analysis/data
# -------------------------------------------------------------------------


# Set Up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(rdrobust) # for RDiT analysis and plots
library(lmtest) # to run a Durbin-Watson test
library(sandwich) # to compute HAC standard errors

# Read datasets
tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds")
ccrs_crashes <-  read_csv("initial-analysis/dat/updated-crashes.csv")


# RDiT Model: Jan 1, 2024 -------------------------------------------------


# TIMS Plot of crashes

tims_months = c(
  "1" = "January", "2" = "February", "3" = "March", "4" = "April",
  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
  "9" = "September", "10" = "October", "11" = "November", "12" = "December" 
)

# Initial plot of crashes
tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
  ) |> 
  group_by(ACCIDENT_YEAR, Time, PED_ACTION, INTERSECTION, Post) |> 
  summarise(TOTAL_CRASHES = n(),
            .groups = "drop") |> 
  ggplot(aes(x = Time, y = TOTAL_CRASHES)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = -0.5, 
             linetype = "dashed", 
             linewidth = 1) +
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-24, 23, by = 1)) +
  labs(title = "Model of Crashes Involving Pedestrians at Intersections",
       x = "Time",
       y = "Number of Crashes",
       caption = "Source: TIMS")

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

tims_updated <- tims_updated |> 
  mutate(
    lighting  = tims_lighting[as.character(lighting)],
    weather = tims_weather[as.character(weather)],
    day = tims_days[as.character(day)],
    month = tims_months[as.character(month)],
    Source = "TIMS",
    county_name = str_to_title(trimws(county_name))
  )

# Create the data for the model
rdit_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0)
        ) |> 
  group_by(Time, Post) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Running the model 
model_ols = lm(Total_crashes ~ Time + Post,
               data = rdit_data)

summary(model_ols)


# Run the model
rdit_model <- lm(
  Total_crashes ~ Time + Post + Time*Post,
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

model_data |>
  group_by(Post) |>
  summarise(avg_crashes = mean(Total_crashes)) |>
  arrange(avg_crashes)


# Method 2 ----------------------------------------------------------------

# Clean and data for the model
rdit_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(
    MONTH_DATE = floor_date(ymd(COLLISION_DATE), "month"),
    Calendar_Month = format(MONTH_DATE, "%m"),
    Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
    Post = ifelse(Time >= 0, 1, 0)
  ) 

# Dataset for Modeling: Group by covariates, Time and Calendar Month 
model_data <- rdit_data |> 
  group_by(Time, Post, Calendar_Month) |> 
  summarise(Total_crashes = n(), 
            .groups = "drop")


## Adjusting for Monthly Seasonal Patterns ##
  
# Set March as the baseline month and create calendar dummy variable
model_data$Calendar_Month <- relevel(factor(model_data$Calendar_Month),
                                     ref = "03")  

# Drop the intercept column with the baseline month
cov_matrix <- model.matrix(~ Calendar_Month, data = model_data)[, -1]

# Run the RDiT model using rdrobust
robust_rdit <- rdrobust(
  y = model_data$Total_crashes,
  x = model_data$Time,
  c = 0,                        # Cutoff is zero
                        
  covs = cov_matrix,            # Seasonal month controls
  h = 24,                       # Bandwidth covering full 48-month windows
  kernel = "uniform"            # Uniform weights for equal monthly evaluation
)

summary(robust_rdit)

# Run the model using lm
rdit_lm <- lm(Total_crashes ~ Time + Post + Time:Post + cov_matrix, 
              data = model_data)

summary(rdit_lm)

## Seasonally-adjusted Plot ##

# Linear model on the seasonal month dummies and time
season_lm <- lm(Total_crashes ~ cov_matrix + time, 
                data = model_data)
summary(season_lm)

# Time only model
time_lm <- lm(Total_crashes ~ Time, 
              data = model_data)
summary(time_lm)

# Extract model rediduals
model_data$Crashes_Adjusted <- residuals(season_lm) + mean(model_data$Total_crashes)

adjusted_plot_data <- model_data |> 
  mutate(Total_crashes_adj = Crashes_Adjusted)

ggplot(adjusted_plot_data, aes(x = Time, y = Total_crashes_adj)) +
  # Plot the clean, de-seasonalized monthly data points
  geom_point(size = 3.5, color = "blue") + 
  
  # Trend line before the law
  geom_smooth(data = filter(adjusted_plot_data, Post == 0), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, 
              color = "red") + 
  
  # Trend line after the law
  geom_smooth(data = filter(adjusted_plot_data, Post == 1), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, 
              color = "red") + 
  
  # Clear vertical boundary line between Dec 2023 (-1) and Jan 2024 (0)
  geom_vline(xintercept = -0.5, 
             linetype = "dashed", 
             linewidth = 1) + 
  
  scale_x_continuous(breaks = seq(-24, 23, 1)) + 
  theme_minimal(base_size = 13) + 
  labs(
    title = "RDiT Model: Pedestrian Crashes",
    x = "Months Relative to Policy Implementation (Jan 2024 = 0)",
    y = "Number of Crashes (Seasonally Adjusted)",
    caption = "Source: Transportation Injury Mapping System (TIMS)"
  )


  
