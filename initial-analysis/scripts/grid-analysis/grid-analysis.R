
# Set Up ------------------------------------------------------------------

library(here)
library(sf)
library(dplyr)
library(tigris)
library(leaflet)
library(leafgl)
library(lubridate)
library(tidyverse)
library(future.apply)
plan(multisession, workers = 5)


### For San Diego geography

san_diego <- places(state = "CA", cb = TRUE, year = 2024) |>
  filter(NAME == "San Diego") |>
  st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |>
  slice_max(area, n = 1) |>
  select(-area) |>
  st_transform(2230) # Cal Albers


# Filter for crashes within SD from 2022 to 2025

crashes <- readRDS("initial-analysis/data-clean/02_TIMS_Geocoded.rds") |>
  filter(ACCIDENT_YEAR >= 2022 & ACCIDENT_YEAR <= 2025) |>
  st_transform(2230) |> # Cal Albers
  st_filter(san_diego)

intersections <- st_read(here("initial-analysis","data-clean", "SD_intersections3.geojson"))
intersections <- st_as_sf(
  intersections, 
  coords = c("Longitude", "Latitude"),
  crs = 4326) |> 
  st_transform(2230)

roads <- st_read(here("initial-analysis","data-raw", "Roads_All_shapefile", "Roads_All.shp"))

# citations geocoded using address and routing location type
# citations <- st_read(here("data-clean", "SD_daylighting_citations",  #"SD_daylighting_citations_all.shp")) |>

#  st_transform(2230) |> 
#  mutate(date_issue = ymd(date_issue))


sd_bbox <- st_bbox(san_diego)

hex_grid_raw <- st_make_grid(sd_bbox, cellsize = 600, square = FALSE) |> 
  st_sf() |>
  st_filter(san_diego) |>
  st_filter(roads)

hex_grid_raw$intersection_count <- lengths(st_intersects(hex_grid_raw, intersections))
hex_grid_raw$crash_count <- lengths(st_intersects(hex_grid_raw, crashes))
hex_grid_raw$road_count <- lengths(st_intersects(hex_grid_raw, roads))

hex_grid_cat0 <- hex_grid_raw |>
  filter(intersection_count == 0)

hex_grid_cat1 <- hex_grid_raw |>
  filter(intersection_count == 1)

hex_grid_cat2 <- hex_grid_raw |>
  filter(intersection_count == 2)

hex_grid_cat3 <- hex_grid_raw |>
  filter(intersection_count == 3)



hex_grid_raw <- hex_grid_raw |>
  mutate(hex_id = row_number(),
         intersection_cat = case_when(
           intersection_count == 0 ~ "0",
           intersection_count == 1 ~ "1",
           intersection_count == 2 ~ "2",
           intersection_count >= 3 ~ "3+"   
         ))

hex_grid_cat0 <- hex_grid_raw |> filter(intersection_cat == "0")
hex_grid_cat1 <- hex_grid_raw |> filter(intersection_cat == "1")
hex_grid_cat2 <- hex_grid_raw |> filter(intersection_cat == "2")
hex_grid_cat3 <- hex_grid_raw |> filter(intersection_cat == "3+")

crashes_cat <- crashes |>
  st_join(hex_grid_raw |> select(hex_id, intersection_cat), join = st_intersects) |>
  filter(!is.na(intersection_cat)) |>   # drop crashes outside any hex cell
  st_drop_geometry()


# Zero Intersections ------------------------------------------------------

# RDiT for hex cells with no intersections
cat0_rdit <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "0") |> 
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

# Standardizing crash outcomes
 pre_mean <- cat0_rdit |>
   filter(Post == 0) |>
   summarise(mean = mean(Total_crashes)) |>
   pull(mean)
 pre_sd <- cat0_rdit |>
   filter(Post == 0) |>
   summarise(sd = sd(Total_crashes)) |>
   pull(sd)
 cat0_rdit <- cat0_rdit |>
   mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat0_model <- rdrobust(y = cat0_rdit$Crash_std,
                            x = cat0_rdit$Time,
                            covs = model.matrix(~ Season_factor, cat0_rdit)[, -1],
                            c = 0,
                            p = 1,
                            h = 12,
                            kernel = "triangular")
summary(cat0_model)


# Jan 2025 Cutoff
cat0_rdit1 <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "0") |> 
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

 # Standardizing crash outcomes
 pre_mean <- cat0_rdit1 |>
   filter(Post == 0) |>
   summarise(mean = mean(Total_crashes)) |>
   pull(mean)
 pre_sd <- cat0_rdit1 |>
   filter(Post == 0) |>
   summarise(sd = sd(Total_crashes)) |>
   pull(sd)
 cat0_rdit1 <- cat0_rdit1 |>
   mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat0_model1 <- rdrobust(y = cat0_rdit1$Crash_std,
                       x = cat0_rdit1$Time,
                       covs = model.matrix(~ Season_factor, cat0_rdit1)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")
summary(cat0_model1)



# One Intersection --------------------------------------------------------


# Jan 2024 Cutoff

cat1_rdit <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "1") |> 
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

# Standardizing crash outcomes
pre_mean <- cat1_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)
pre_sd <- cat1_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)
cat1_rdit <- cat1_rdit |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat1_model <- rdrobust(y = cat1_rdit$Crash_std,
                       x = cat1_rdit$Time,
                       covs = model.matrix(~ Season_factor, cat1_rdit)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")
summary(cat1_model)


# Jan 2025 Cutoff
cat1_rdit1 <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |>
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "1") |> 
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

# Standardizing crash outcomes
pre_mean <- cat1_rdit1 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)
pre_sd <- cat1_rdit1 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)
cat1_rdit1 <- cat1_rdit1 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat1_model1 <- rdrobust(y = cat1_rdit1$Crash_std,
                        x = cat1_rdit1$Time,
                        covs = model.matrix(~ Season_factor, cat1_rdit1)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")
summary(cat1_model1)



# Two Intersections --------------------------------------------------------


# Jan 2024 Cutoff

cat2_rdit <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "2") |> 
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

# Standardizing crash outcomes
pre_mean <- cat2_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)
pre_sd <- cat2_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)
cat2_rdit <- cat2_rdit |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat2_model <- rdrobust(y = cat2_rdit$Crash_std,
                       x = cat2_rdit$Time,
                       covs = model.matrix(~ Season_factor, cat2_rdit)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")
summary(cat2_model)


# Jan 2025 Cutoff
cat2_rdit1 <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "2") |> 
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

# Standardizing crash outcomes
pre_mean <- cat2_rdit1 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)
pre_sd <- cat2_rdit1 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)
cat2_rdit1 <- cat2_rdit1 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat2_model1 <- rdrobust(y = cat2_rdit1$Crash_std,
                        x = cat2_rdit1$Time,
                        covs = model.matrix(~ Season_factor, cat2_rdit1)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")
summary(cat2_model1)


# Three or more Intersections -------------------------------------------------


# Jan 2024 Cutoff

cat3_rdit <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "3+") |> 
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

# Standardizing crash outcomes
pre_mean <- cat3_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)
pre_sd <- cat3_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)
cat3_rdit <- cat3_rdit |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat3_model <- rdrobust(y = cat3_rdit$Crash_std,
                       x = cat3_rdit$Time,
                       covs = model.matrix(~ Season_factor, cat3_rdit)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")
summary(cat3_model)


# Jan 2025 Cutoff
cat3_rdit1 <- crashes_cat |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
  filter(PEDESTRIAN_ACCIDENT == "Y" & intersection_cat == "3+") |> 
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

# Standardizing crash outcomes
pre_mean <- cat3_rdit1 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)
pre_sd <- cat3_rdit1 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)
cat3_rdit1 <- cat3_rdit1 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)

# Run model
cat3_model1 <- rdrobust(y = cat3_rdit1$Crash_std,
                        x = cat3_rdit1$Time,
                        covs = model.matrix(~ Season_factor, cat3_rdit1)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")
summary(cat3_model1)


# BAR PLOT ---------------------------------------------------------------

# color palette
sunflower <- c(
  "#8A9B5B",
  "#F7F4E7",
  "#F2C94C",
  "#1E4E8C",
  "#D4A04A",
)

# Extract coefficients from rdrobust
extract_rd <- function(model, intersections, cutoff){
  
  tibble(
    Intersections = intersections,
    Cutoff = cutoff,
    Effect = model$coef[3,1],
    SE = model$se[3,1],
    P_value = model$pv[3,1],
    CI_lower = model$ci[3,1],
    CI_upper = model$ci[3,2]
  )
  
}

# Build datframes for each category
num_intersections <- bind_rows(
  
  extract_rd(cat0_model,      "Zero", "Jan 2024"),
  extract_rd(cat0_model1,     "Zero", "Jan 2025"),
  
  extract_rd(cat1_model,     "One", "Jan 2024"),
  extract_rd(cat1_model1,    "One", "Jan 2025"),
  
  extract_rd(cat2_model,      "Two", "Jan 2024"),
  extract_rd(cat2_model1,     "Two", "Jan 2025"),
  
  extract_rd(cat3_model,   "Three +", "Jan 2024"),
  extract_rd(cat3_model1,  "Three +", "Jan 2025")
  
) |>
  
  mutate(Intersections = factor(Intersections,
                           levels = c("Zero", "One", "Two", "Three +")),
         Cutoff = factor(Cutoff, 
                         levels = c("Jan 2024","Jan 2025")),
         Sig = ifelse(P_value < 0.05, "**", ""), 
         Label = round(Effect, 2))

# Plot
ggplot(num_intersections,
       aes(Intersections, Effect, fill = Cutoff)) +
  
  geom_col(position = position_dodge(width = 0.6),
           width = .55) +
  
  geom_text(aes(label = Label,
                vjust = ifelse(Effect < 0,1.15,-0.35)),
            position = position_dodge(width = .65),
            fontface = "bold",
            size = 4.5) +
  
  geom_text(aes(label = Sig,
                y = ifelse(Effect < 0, Effect - 2, Effect + 2),
                vjust = ifelse(Effect < 0, -0.4, 8.5), 
                hjust = -1.5),
            position = position_dodge(width = 0.65), 
            size = 4, 
            fontface = "bold") +
  
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
  
  geom_hline(yintercept = 0,
             linewidth = .5) +
  
  scale_fill_manual(values = sunflower) +
  
  labs(title = "RD Effect by Number of Intersections",
       subtitle = "Comparison of January 2024 and January 2025 Cutoffs",
       x = NULL,
       y = "RD Effect (Standardized)",
       fill = NULL,
       caption = "** Significant at the 5% level") +
  
  theme_minimal(base_size = 13) +
  
  theme(legend.position="bottom",
        plot.caption = element_text(hjust = 0.5, 
                                    face = "italic", 
                                    size = 10),
        axis.text.x=element_text(face="bold",
                                 size=13),
        plot.title=element_text(face="bold", size=17),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank())
