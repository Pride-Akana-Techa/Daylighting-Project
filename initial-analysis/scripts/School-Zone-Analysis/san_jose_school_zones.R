# San Jose schools zones

library(tigris)
library(sf)
library(here)

# load san jose 
san_jose_boundary <- places(state = "CA", cb = TRUE) |>
  filter(NAME == "San Jose") |>
  st_transform(crs = 3310)


# load and filter school data for just san jose 
gdb_path <- "initial-analysis/data-raw/CaliforniaSchools/CSCD_2025.gdb"


# only contains public schools (239 obs)
# i think that they said that there are approx 400 schools in San Jose
san_jose_schools <- st_read(dsn = gdb_path, layer = "Schools_Current_Stacked") |>
  st_transform(crs = 3310) |>
  st_filter(san_jose_boundary)

san_jose_school_zones <- st_buffer(san_jose_schools, dist = units::set_units(600, "ft"))|>
  st_union()


san_jose_crashes <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
  st_filter(san_jose_boundary) |>
  st_transform(crs = 3310)

schoolzone_crashes <- san_jose_crashes |>
  mutate(
    crash_date = as.Date(COLLISION_DATE), 
    in_school_zone = lengths(st_intersects(geometry, san_jose_school_zones)) > 0,
    policy_period = case_when(
      crash_date < as.Date("2024-01-01") ~ "Pre-Policy (Before 2024)",
      crash_date >= as.Date("2024-01-01") & crash_date < as.Date("2025-01-01") ~ "Warnings (1/1/2024)",
      crash_date >= as.Date("2025-01-01") ~ "Citations (1/1/2025)",
    ),
    policy_number = case_when(
      crash_date < as.Date("2024-01-01") ~ 0,
      crash_date >= as.Date("2024-01-01") & crash_date < as.Date("2025-01-01") ~ 1,
      crash_date >= as.Date("2025-01-01") ~ 2,
    )
  )

# aggregate by month
san_jose_monthly_crashes <- schoolzone_crashes |>
  st_drop_geometry() |>
  mutate(month_date = floor_date(as.Date(COLLISION_DATE), "month")) |>
  filter(year(month_date) >= 2021 & year(month_date) <= 2025) |> 
  group_by(month_date, in_school_zone) |>
  summarise(crashes = n(), .groups = "drop") |>
  mutate(
    zone_label = if_else(in_school_zone, "School Zone (Within 500ft)", "Control (Outside School Zone)"),
    zone_number = if_else(in_school_zone, 1, 0))
    
ped_crashes <- schoolzone_crashes|>
  filter(PEDESTRIAN_ACCIDENT =="Y")

# there are not that many pedestrian observations ngl 

san_jose_ped_monthly_crashes <- ped_crashes |>
  st_drop_geometry() |>
  mutate(month_date = floor_date(as.Date(COLLISION_DATE), "month")) |>
  filter(year(month_date) >= 2021 & year(month_date) <= 2025) |> 
  group_by(month_date, in_school_zone) |>
  summarise(crashes = n(), .groups = "drop") |>
  mutate(
    zone_label = if_else(in_school_zone, "School Zone (Within 500ft)", "Control (Outside School Zone)"),
    zone_number = if_else(in_school_zone, 1, 0))



# RDiT model for Crashes in School Zones ---------------------------------------

## Jan 2024 Cutoff
# Prepare data for model
in_school_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "TRUE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
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
  filter(Year %in% c(2023, 2024)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- in_school_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- in_school_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

in_school_rdit <- in_school_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
in_school_zone_model <- rdrobust(y = in_school_rdit$Crash_std,
                              x = in_school_rdit$Time,
                              covs = model.matrix(~ Season_factor, in_school_rdit)[, -1],
                              c = 0,
                              p = 1,
                              h = 12,
                              kernel = "triangular")

summary(in_school_zone_model)


## Jan 2025 Cutoff
# Prepare data for model
in_school1_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "TRUE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
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
  filter(Year %in% c(2024, 2025)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- in_school1_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- in_school1_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

in_school1_rdit <- in_school1_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
in_school_zone1_model <- rdrobust(y = in_school1_rdit$Crash_std,
                                 x = in_school1_rdit$Time,
                                 covs = model.matrix(~ Season_factor, in_school1_rdit)[, -1],
                                 c = 0,
                                 p = 1,
                                 h = 12,
                                 kernel = "triangular")

summary(in_school_zone1_model)


# RDiT model for Crashes in School Zones ---------------------------------------

## Jan 2024 Cutoff
# Prepare data for model
out_school_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "FALSE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
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
  filter(Year %in% c(2023, 2024)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- out_school_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- out_school_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

out_school_rdit <- out_school_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
out_school_zone_model <- rdrobust(y = out_school_rdit$Crash_std,
                                 x = out_school_rdit$Time,
                                 covs = model.matrix(~ Season_factor, out_school_rdit)[, -1],
                                 c = 0,
                                 p = 1,
                                 h = 12,
                                 kernel = "triangular")

summary(out_school_zone_model)


## Jan 2025 Cutoff
# Prepare data for model
out_school1_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "FALSE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
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
  filter(Year %in% c(2024, 2025)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- out_school1_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- out_school1_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

out_school1_rdit <- out_school1_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
out_school_zone1_model <- rdrobust(y = out_school1_rdit$Crash_std,
                                  x = out_school1_rdit$Time,
                                  covs = model.matrix(~ Season_factor, out_school1_rdit)[, -1],
                                  c = 0,
                                  p = 1,
                                  h = 12,
                                  kernel = "triangular")

summary(out_school_zone1_model)


# RD Effect by School Zone ----------------------------------------------------

# color palette (same as city analysis)
sunflower <- c(
  "#F2C94C",
  "#1E4E8C",
  "#D4A04A",
  "#8A9B5B",
  "#F7F4E7"
)


# Extract coefficients from rdrobust
extract_rd <- function(model, zone_type, cutoff){
  
  tibble(
    Zone = zone_type,
    Effect = model$coef[3,1],
    Cutoff = cutoff,
    SE = model$se[3,1],
    P_value = model$pv[3,1],
    CI_lower = model$ci[3,1],
    CI_upper = model$ci[3,2]
  )
}


# Build dataframe for school zone analysis
school_zone_analysis <- bind_rows(
  
  extract_rd(in_school_zone_model, 
             "School Zone", 
             "2024"),
  
  extract_rd(in_school_zone1_model, 
             "School Zone", 
             "2025"),
  
  extract_rd(out_school_zone_model, 
             "Non-School Zone", 
             "2024"),
  
  extract_rd(out_school_zone1_model, 
             "Non-School Zone", 
             "2025")
  
) |>
  
  mutate(
    Zone = factor(
      Zone,
      levels = c("School Zone", "Non-School Zone")
    ),
    
    Cutoff = factor(
      Cutoff,
      levels = c("2024", "2025")
    ),
    
    Sig = case_when(
      P_value < 0.01 ~ "***",
      P_value < 0.05 ~ "**",
      P_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    
    Label = round(Effect, 2)
  )


# Plot
ggplot(
  school_zone_analysis,
  aes(Zone, Effect, fill = Cutoff)
) +
  
  geom_col(
    position = position_dodge(width = 0.6),
    width = 0.55
  ) +
  
  geom_text(
    aes(
      label = Label,
      vjust = ifelse(Effect < 0, 1.15, -0.35),
      color = ifelse(Sig != "" & !is.na(Sig), "#800000", "black"),
      group = Cutoff
    ),
    position = position_dodge(width = 0.65),
    fontface = "bold",
    size = 4.5
  ) +
  
  geom_text(
    aes(
      label = Sig,
      y = ifelse(Effect < 0, Effect - 0.2, Effect + 0.2),
      vjust = ifelse(Effect < 0, -1.5, 1.5),
      hjust = ifelse(Effect < 0, -3.3, -1.0),
      color = "#800000",
      group = Cutoff
    ),
    position = position_dodge(width = 0.65),
    size = 4,
    fontface = "bold"
  ) +
  
  scale_color_identity() +
  
  scale_x_discrete(
    expand = expansion(mult = c(0.1,0.1))
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = .5
  ) +
  
  scale_fill_manual(
    values = sunflower
  ) +
  
  labs(
    title = "San José School Zone Analysis",
    subtitle = "Comparison of pedestrian crashes within and outside school zones",
    x = NULL,
    y = "RD Effect (Standardized by z-scores)",
    fill = NULL,
    caption = "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    legend.position = "bottom",
    
    plot.caption = element_text(
      hjust = 0.5,
      face = "italic",
      size = 10,
      color = "#800000"
    ),
    
    axis.text.x = element_text(
      face = "bold",
      size = 13
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save
ggsave(filename = "initial-analysis/figs/san_jose.png",
       width = 10,
       height = 10)

