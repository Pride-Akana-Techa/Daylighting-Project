
# -------------------------------------------------------------------------

## Examining the implications of the law in specific cities at their
## enforcement dates


# -------------------------------------------------------------------------


# Load packages and data
library(tidyverse)
library(rdrobust)
library(tigris)
library(sf)

tims_crashes <- readRDS("initial-analysis/data/TIMS_Filtered.rds") 



# San Diego ---------------------------------------------------------------

san_diego <- tims_crashes |> 
  filter(CITY == "SAN DIEGO" & COLLISION_DATE >= "2024-05-01") |> 
  filter(PED_ACTION == "B") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2025-03-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")


# Run the model
san_diego_model <- rdrobust(y = san_diego$Total_crashes,
                       x = san_diego$Time,
                       covs = model.matrix(~ Season_factor, san_diego)[, -1],
                       c = 0, 
                       p = 1,
                       h = 10,
                       kernel = "triangular")

summary(san_diego_model)


# Plot the model
san_diego_season <- lm(Total_crashes ~ Season_factor,
                    data = san_diego)

san_diego$Crash_adj <- resid(san_diego_season) + mean(san_diego$Total_crashes)

san_diego_rd_out <- rdplot(y = san_diego$Crash_adj,
                 x = san_diego$Time,
                 c = 0,
                 p = 1,
                 h = 20,
                 kernel = "triangular",
                 nbins = c(20, 10))

san_diego_rd_out$rdplot +
  labs(title = "San Diego RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to March 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/san_diego_rdit.png",
       height = 10,
       width = 20)



# SD Specific -------------------------------------------------------------

sd <- read_csv("initial-analysis/data/San_Diego.csv")
sd_filtered <- sd |> 
  filter(person_role == "PEDESTRIAN") |> 
  mutate(Date = mdy_hm(date_time),
         MonthDate = floor_date(Date, "month"),
         Month = month(Date), 
         Year = year(Date)) |> 
  filter(Year %in% c(2024, 2025, 2026)) 

sd_data <- sd_filtered |> 
  mutate(Time = interval(as.Date("2025-03-01"), MonthDate) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(Month %in% c(12, 1, 2) ~ "Winter",
                     Month %in% c(3, 4, 5) ~ "Spring",
                     Month %in% c(6, 7, 8) ~ "Summer",
                     Month %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(Year, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
sd_model <- rdrobust(y = sd_data$Total_crashes,
                            x = sd_data$Time,
                            covs = model.matrix(~ Season_factor, sd_data)[, -1],
                            c = 0, 
                            p = 1,
                            h = 15,
                            kernel = "triangular")

summary(sd_model)
  


# Trying San Diego --------------------------------------------------------

san_diego2 <- places(state = "CA", cb = TRUE, year = 2024) |>
  filter(NAME == "San Diego") |>
  st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |>
  slice_max(area, n = 1) |>
  select(-area) |>
  st_transform(2230) # Cal Albers

crashes <- readRDS("initial-analysis/data-clean/02_TIMS_Geocoded.rds") |>
  dplyr::filter(ACCIDENT_YEAR >= 2022 & ACCIDENT_YEAR <= 2025) |>
  st_transform(2230) |> # Cal Albers
  st_filter(san_diego2)


san_diego2 <- crashes |> 
  filter(COLLISION_DATE >= "2024-05-01") |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2025-03-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
san_diego_model2 <- rdrobust(y = san_diego2$Total_crashes,
                            x = san_diego2$Time,
                            covs = model.matrix(~ Season_factor, san_diego2)[, -1],
                            c = 0, 
                            p = 1,
                            h = 10,
                            kernel = "triangular")

summary(san_diego_model2)


# Plot the model
san_diego_season2 <- lm(Total_crashes ~ Season_factor,
                       data = san_diego2)

san_diego2$Crash_adj <- resid(san_diego_season2) + mean(san_diego2$Total_crashes)

san_diego_rd_out2 <- rdplot(y = san_diego2$Crash_adj,
                           x = san_diego2$Time,
                           c = 0,
                           p = 1,
                           h = 10,
                           kernel = "triangular",
                           nbins = c(10, 10))

san_diego_rd_out2$rdplot +
  labs(title = "San Diego RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to March 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )



# San Francisco -----------------------------------------------------------

san_francisco <- tims_crashes |> 
  filter(CITY == "SAN FRANCISCO" & ACCIDENT_YEAR >= "2024") |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
san_francisco_model <- rdrobust(y = san_francisco$Total_crashes,
                            x = san_francisco$Time,
                            covs = model.matrix(~ Season_factor, 
                                                san_francisco)[, -1],
                            c = 0, 
                            p = 1,
                            h = 12,
                            kernel = "triangular")

summary(san_francisco_model)


# Plot the model
san_francisco_season <- lm(Total_crashes ~ Season_factor,
                       data = san_francisco)

san_francisco$Crash_adj <- resid(san_francisco_season) + mean(san_francisco$Total_crashes)

# plot
san_francisco_rd_out <- rdplot(y = san_francisco$Crash_adj,
                           x = san_francisco$Time,
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular",
                           nbins = c(12, 12))

san_francisco_rd_out$rdplot +
  labs(title = "San Francisco RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/san_francisco_rdit.png",
       height = 10,
       width = 20)




# Trying ------------------------------------------------------------------

# Per-city config: start date, cutoff date, bandwidth -----------------------
city_config <- tribble(
  ~city_name,       ~start_date,     ~cutoff_date,      ~h,
  "SAN DIEGO",      "2024-05-01",    "2025-03-01",      10,
  "SAN FRANCISCO",  "2024-01-01",    "2025-01-01",      12
)

# Helper function: fit RDiT model + build seasonally-adjusted rdplot data ---
run_city_rdit <- function(city_name, start_date, cutoff_date, h) {
  
  data <- tims_crashes |> 
    filter(CITY == city_name & COLLISION_DATE >= start_date) |> 
    filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
    mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |> 
    mutate(Time = interval(as.Date(cutoff_date), MONTH) %/% months(1),
           Post = ifelse(Time >= 0, 1, 0),
           Season_factor = factor(case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ), levels = c("Winter", "Spring", "Summer", "Fall"))
    ) |> 
    group_by(CITY, Time, Post, Season_factor) |> 
    summarise(Total_crashes = n(), .groups = "drop")
  
  model <- rdrobust(y = data$Total_crashes,
                    x = data$Time,
                    covs = model.matrix(~ Season_factor, data)[, -1],
                    c = 0,
                    p = 1,
                    h = h,
                    kernel = "uniform")
  
  cat("\n==", city_name, "==\n")
  print(summary(model))
  
  # Adjust for seasonality before plotting
  season_model <- lm(Total_crashes ~ Season_factor, data = data)
  data$Crash_adj <- resid(season_model) + mean(data$Total_crashes)
  
  rd_out <- rdplot(y = data$Crash_adj,
                   x = data$Time,
                   c = 0,
                   p = 1,
                   h = h,
                   kernel = "uniform",
                   nbins = c(24, 24),
                   hide = TRUE)
  
  bins <- rd_out$vars_bins |> 
    transmute(Time = rdplot_mean_bin, Crash_adj = rdplot_mean_y, Type = "Binned mean")
  poly <- rd_out$vars_poly |> 
    transmute(Time = rdplot_x, Crash_adj = rdplot_y, Type = "Fitted line")
  
  bind_rows(bins, poly) |> mutate(CITY = city_name)
}

# Run for both cities and combine --------------------------------------------
city_results <- pmap(city_config, run_city_rdit)

combined_plot_data <- bind_rows(city_results) |> 
  mutate(CITY = factor(CITY, levels = city_config$city_name))

# Faceted plot ----------------------------------------------------------------
ggplot(combined_plot_data, aes(x = Time, y = Crash_adj)) +
  geom_point(data = filter(combined_plot_data, Type == "Binned mean"),
             color = "steelblue", size = 1.8) +
  geom_line(data = filter(combined_plot_data, Type == "Fitted line"),
            color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ CITY, scales = "free") +
  labs(title = "RDiT by City at City-Specific Enforcement Dates",
       y = "Number of Crashes (seasonally adjusted)",
       x = "Months Relative to Enforcement Cutoff") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 13, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/city_rdit_facets.png",
       height = 8,
       width = 16)







