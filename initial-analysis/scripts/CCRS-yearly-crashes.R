# load packages
library(tidyverse)
library(lubridate)

# Merge the datasets
folder_path <- "C:/Users/techap/Desktop/Cal-Walks-1/raw-data"
combined_crashes <- list.files(path = folder_path, pattern = "\\csv$", 
                               full.names = TRUE) |> 
  map_df(~read_csv(.x, col_types = cols(.default = "c"))) |> 
  
  write_csv(file.path("data/Crashes_California"))

# converts the crash time to standard date-time object
updated_crashes <- combined_crashes |> 
  mutate(Modified_time = parse_date_time(`Crash Date Time`, 
                                         orders = c("mdy HM", "mdy HMS"))) |> 

  # selects the month and year in the standard time and create a Month and year variables  
  mutate(Month = month(Modified_time, label = TRUE, abbr = FALSE),
         Year = year(Modified_time)) |> 
 
  
  # Remove columns with at least 70% null values 
  select(-c(IsAOIOneSameAsLocation, IsLocationReferToNarrative, HasDigitalMediaFiles, IsAdditonalObjectStruck, IsCountyRoad, ReportingDistrictCode, PedestrianActionDesc, LightingDescription, MotorVehicleInvolvedWithDesc, MotorVehicleInvolvedWithOtherDesc, MilepostDistance, MilepostMarker, MilepostUnitOfMeasure, `Weather 2`, `Road Condition 2`, MilepostDirection, ReportingDistrict, EvidenceNumber, NotificationTimeDescription, IsAttachmentsMailed, `Collision Type Other Desc`, HitRun, `Special Condition`, SketchDesc, IsFreeway))

# Check for null values in each variable
colSums(is.na(updated_crashes)) 

# Grouping for pedestrian and bicyclist crashes
updated_crashes |>  
  group_by(Year) |> 
  filter(MotorVehicleInvolvedWithCode %in% c("B", "G")) |>
  filter_out(Year == "2026") |> 
  summarize(Crashes_with_Pedestrians = sum(MotorVehicleInvolvedWithCode == "B"),             Crashes_with_Bicyclists = sum(MotorVehicleInvolvedWithCode == "G"),
            .groups = "drop") |> 

  pivot_longer(
    cols = c(Crashes_with_Pedestrians, Crashes_with_Bicyclists),
    names_to = "Crash Type",
    values_to = "Number of Crashes"
  ) |> 
  ggplot(aes(x = Year, y = `Number of Crashes`, color = `Crash Type`, group = `Crash Type`)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1))
  labs(title = "Pedestrian and Bicycle Crashes by Year",
      x = "Year", 
      y = "Number of Crashes", 
      color = "Crash Type")

  # Saving the plot
  ggsave("figs/yearly-ped-and-bike-crashes.png",
         width = 10, height = 6)

# Pedestrian Crashes at Intersections
  updated_crashes |>
    filter_out(Year == "2026") |> 
    group_by(Year) |> 
    filter(PedestrianActionCode == "B") |> 
    summarise(Pedestrian_Intersection_Crashes = n(),
              .groups = "drop") |> 
    
    ggplot(aes(x = Year, y = Pedestrian_Intersection_Crashes, group = 1)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2016, 2025, by = 1)) +
    labs(title = "Yearly Crashes at Intersections Involving Pedestrians",
         x = "Year",
         y = "Number of Crashes")
  
  # Saving the plot
  ggsave("figs/yearly-ped-intersection-crashes.png", width = 10, height = 6)

  # Analyzing Injuries of Pedestrians at Intersections
  updated_crashes |> 
  filter_out(Year == "2026") |> 
    filter(PedestrianActionCode == "B") |> 
    group_by(Year) |> 
    summarise(Number_Injured = sum(as.numeric(NumberInjured), na.rm = TRUE),
              Number_Killed = sum(as.numeric(NumberKilled), na.rm = TRUE),
              .groups = "drop") |> 
  
    ggplot(aes(x = Year, y = Number_Injured)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2016, 2025, by = 1)) +
    labs(title = "Pedestrians Injured at Intersections",
         x = "Year",
         y = "Number of Injured Pedestrians")
  
  ggsave("figs/yearly-ped-injuries.png",
         width = 10,
         height = 6)
  
  ggsave("figs/yearly-ped-int-injuries.png",
         width = 10,
         height = 6)
  