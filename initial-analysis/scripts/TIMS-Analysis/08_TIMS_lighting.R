
# -------------------------------------------------------------------------
## Investigating the relationship between lighting condition and crashes ##

# -------------------------------------------------------------------------

# load libraries and dataset
library(tidyverse)
library(rdrobust)

tims_data <-  readRDS("initial-analysis/data-clean/updated_tims.rds")

# Check yearly lighting condition
lighting_distribution <- tims_data |> 
  filter(ACCIDENT_YEAR >= "2022" &
           PED_ACTION == "B" &
           INTERSECTION == "Y") |> 
  filter_out(is.na(LIGHTING)) |> 
  group_by(ACCIDENT_YEAR, MONTH, WEATHER_1)|> 
  summarise(CRASHES = n(),
            .groups = "drop")


# Monthly Proportion
lighting_monthly <- tims_data |> 
  filter(ACCIDENT_YEAR >= "2022",
         PED_ACTION == "B",
         INTERSECTION == "Y") |> 
  filter(!is.na(LIGHTING)) |>
  mutate(MONTH_DATE = floor_date(COLLISION_DATE, "month")) |>   
  group_by(MONTH_DATE, LIGHTING) |> 
  summarise(CRASHES = n(), .groups = "drop") |> 
  group_by(MONTH_DATE) |> 
  mutate(PROPORTION = CRASHES / sum(CRASHES)) |> 
  ungroup()



# Daylight -----------------------------------------------------------

## RDiT Model for clear weather crashes ##
# Prepare data
daylight_data <- lighting_monthly |> 
  filter(LIGHTING == "Daylight") |> 
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


daylight_model <- rdrobust(y = daylight_data$PROPORTION,
                        x = daylight_data$Time,
                        covs = model.matrix(~ Season_factor, daylight_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 24,
                        kernel = "uniform")

summary(daylight_model)

# Adjusting for seasonality before plotting
season_daylight_model <- lm(PROPORTION ~ Season_factor,
                         data = daylight_data)

daylight_data$Crash_adj <- resid(season_daylight_model) + mean(daylight_data$PROPORTION)

# plot
daylight_rd_out <- rdplot(y = daylight_data$Crash_adj,
                 x = daylight_data$Time,
                 c = 0,
                 p = 1,
                 h = 24,
                 kernel = "uniform",
                 nbins = c(24, 24))

daylight_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Daylight RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/daylight_rdit.png",
       height = 10,
       width = 20)



# Dark ----------------------------------------------------------------------
## RDiT Model for clear weather crashes ##
# Prepare data
dark_data <- lighting_monthly |> 
  filter(LIGHTING == "Dark") |> 
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


dark_model <- rdrobust(y = dark_data$PROPORTION,
                           x = dark_data$Time,
                           covs = model.matrix(~ Season_factor, dark_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform")

summary(dark_model)

# Adjusting for seasonality before plotting
season_dark_model <- lm(PROPORTION ~ Season_factor,
                            data = dark_data)

dark_data$Crash_adj <- resid(season_dark_model) + mean(dark_data$PROPORTION)

# plot
dark_rd_out <- rdplot(y = dark_data$Crash_adj,
                          x = dark_data$Time,
                          c = 0,
                          p = 1,
                          h = 24,
                          kernel = "uniform",
                          nbins = c(24, 24))

dark_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Dark RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/dark_rdit.png",
       height = 10,
       width = 20)


# Dusk/Dawn ---------------------------------------------------------------

## RDiT Model for clear weather crashes ##
# Prepare data
dusk_data <- lighting_monthly |> 
  filter(LIGHTING == "Dusk/Dawn") |> 
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


dusk_model <- rdrobust(y = dusk_data$PROPORTION,
                           x = dusk_data$Time,
                           covs = model.matrix(~ Season_factor, dusk_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform",
                           bwselect = "mserd")

summary(dusk_model)

# Adjusting for seasonality before plotting
season_dusk_model <- lm(PROPORTION ~ Season_factor,
                            data = dusk_data)

dusk_data$Crash_adj <- resid(season_dusk_model) + mean(dusk_data$PROPORTION)

# plot
dusk_rd_out <- rdplot(y = dusk_data$Crash_adj,
                          x = dusk_data$Time,
                          c = 0,
                          p = 1,
                          h = 24,
                          kernel = "uniform",
                          nbins = c(24, 24))

dusk_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Dusk/Dawn RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/dusk_rdit.png",
       height = 10,
       width = 20)




# Trying ------------------------------------------------------------------

# -------------------------------------------------------------------------
## Investigating the relationship between lighting condition and crashes ##
# -------------------------------------------------------------------------

# load libraries and dataset
library(tidyverse)
library(rdrobust)

tims_data <- readRDS("initial-analysis/data-clean/updated_tims.rds")

# Check yearly lighting condition
lighting_distribution <- tims_data |> 
  filter(ACCIDENT_YEAR >= "2022" &
           PED_ACTION == "B" &
           INTERSECTION == "Y") |> 
  filter_out(is.na(LIGHTING)) |> 
  group_by(ACCIDENT_YEAR, MONTH, WEATHER_1) |> 
  summarise(CRASHES = n(),
            .groups = "drop")

# Monthly Proportion
lighting_monthly <- tims_data |> 
  filter(ACCIDENT_YEAR >= "2022",
         PED_ACTION == "B",
         INTERSECTION == "Y") |> 
  filter(!is.na(LIGHTING)) |>
  mutate(MONTH_DATE = floor_date(COLLISION_DATE, "month")) |>   
  group_by(MONTH_DATE, LIGHTING) |> 
  summarise(CRASHES = n(), .groups = "drop") |> 
  group_by(MONTH_DATE) |> 
  mutate(PROPORTION = CRASHES / sum(CRASHES)) |> 
  ungroup()


# Helper function: fit RDiT model + build seasonally-adjusted rdplot data ----
run_rdit <- function(lighting_level) {
  
  data <- lighting_monthly |> 
    filter(LIGHTING == lighting_level) |> 
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
  
  model <- rdrobust(y = data$PROPORTION,
                    x = data$Time,
                    covs = model.matrix(~ Season_factor, data)[, -1],
                    c = 0,
                    p = 1,
                    h = 24,
                    kernel = "uniform",
                    bwselect = "mserd")
  
  print(summary(model))
  
  # Adjust for seasonality before plotting
  season_model <- lm(PROPORTION ~ Season_factor, data = data)
  data$Crash_adj <- resid(season_model) + mean(data$PROPORTION)
  
  # Suppress the individual base plot rdplot() would otherwise draw
  rd_out <- rdplot(y = data$Crash_adj,
                   x = data$Time,
                   c = 0,
                   p = 1,
                   h = 24,
                   kernel = "uniform",
                   nbins = c(24, 24),
                   hide = TRUE)
  
  # Binned scatter points
  bins <- rd_out$vars_bins |> 
    transmute(Time = rdplot_mean_bin,
              Crash_adj = rdplot_mean_y,
              Type = "Binned mean")
  
  # Fitted polynomial line(s)
  poly <- rd_out$vars_poly |> 
    transmute(Time = rdplot_x,
              Crash_adj = rdplot_y,
              Type = "Fitted line")
  
  bind_rows(bins, poly) |> 
    mutate(LIGHTING = lighting_level,
           list(model = model))
}

# Run for all three lighting conditions and combine ---------------------
lighting_levels <- c("Daylight", "Dark", "Dusk/Dawn")

rdit_results <- map(lighting_levels, run_rdit)

combined_plot_data <- bind_rows(rdit_results) |> 
  mutate(LIGHTING = factor(LIGHTING, levels = lighting_levels))

# Faceted plot ------------------------------------------------------------
ggplot(combined_plot_data, aes(x = Time, y = Crash_adj)) +
  geom_point(data = filter(combined_plot_data, Type == "Binned mean"),
             color = "steelblue", size = 1.8) +
  geom_line(data = filter(combined_plot_data, Type == "Fitted line"),
            color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ LIGHTING, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "RDiT by Lighting Condition",
       x = "Months Relative to Jan 2024") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 13, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/lighting_rdit_facets.png",
       height = 8,
       width = 18)


