# Daylighting and Pedestrian Safety: Evidence from California AB 413

**Authors:** Kyle Klemba, Pride Akana Techa

## Project Overview

This project studies the effect of California Assembly Bill 413 (AB 413), which prohibits parking within 20 feet of crosswalks, on pedestrian and cyclist safety outcomes. The analysis uses a Regression Discontinuity in Time (RDiT) design centered on the law's statewide effective date of January 1, 2024 or enforcement date January 1, 2025.

The codebase is organized into two parts:

- **Part 1 — Data cleaning and analysis** (`initial-analysis/`)
- **Part 2 — R Shiny dashboard** (`ShinyApp/`)

------------------------------------------------------------------------

## Part 1: Data Cleaning and Analysis

### Data Sources

#### TIMS (Transportation Injury Mapping System)

- **Source:** UC Berkeley SafeTREC, via Street Story portal
- **Coverage:** California statewide, all counties, 2014–2025
- **Unit:** One row per police-reported crash
- **Key variables:** crash date/time, latitude/longitude, pedestrian action code, collision type, injury severity, city, county, intersection
- **Note:** TIMS uses spatially calibrated coordinates (more accurate than raw CHP records) and excludes property-damage-only (PDO) crashes.

#### NOAA Weather Data

- **Source:** NOAA Global Surface Summary of Day (GSOD)
- **Coverage:** California weather stations, 2020–2025
- **Key variables:** daily precipitation, max/min temperature, wind speed, visibility

### TIMS Data Processing

**Scripts:** `scripts/TIMS-Analysis/00_TIMS_cleaning.R` `scripts/TIMS-Analysis/03_TIMS_aggregating_data.R`

| Step | Description | Input | Output |
|------------------|------------------|------------------|------------------|
| 1 | Merge all CA county TIMS files | `data-raw/TIMS/*.csv` | `Crashes_California.csv` |
| 2 | Drop observations missing lon/lat or pedestrian-related variables | `Crashes_California.csv` | `data-clean/TIMS_Filtered.rds` |

Variables dropped if missing in Step 2:

- `Latitude`, `Longitude`
- `PedestrianActionCode`
- `NumberKilled`, `NumberInjured`

**Script:** `scripts/Weather-Analysis/01_NOAA_weather.R`

- Read and clean NOAA GSOD data for California stations.
- Outputs: `data-clean/NOAA_weather_data.rds`

### Descriptive Figures

**Scripts:** `scripts/TIMS-Analysis/04_TIMS_graphs.R`, `scripts/TIMS-Analysis/05_TIMS_OSM_intro.R`

- Monthly pedestrian and bicyclist injuries (statewide), 2014-2025.
- Monthly pedestrian and bicyclist fatalities (statewide), 2014-2025.
- Crash counts by weather condition (clear vs. raining).
- Crash counts by time of day (daytime vs. nighttime).
- Geographic distribution (county-level choropleth map).
- Seasonal patterns (month-of-year average)

### RDiT Analyses

All models use both January 2024 (warning start date) and January 2025 (enforcement start date) as the policy cutoffs. The running variable is `r_t` = months relative to the cutoff.

**Main specification:**

$$Y_t = \alpha + \tau D_t + \beta_1(t - t_0) + \beta_2 D_t (t - t_0) + \gamma \, \text{Season}_t + \varepsilon_t, \quad |t - t_0| \le h$$

#### State-Level Baseline

**Script:** `scripts/TIMS-Analysis/06_TIMS_RDiT_model.R`

- Unit: California statewide monthly crash count
- Bandwidth: 12 month (via `rdrobust`)
- Polynomial order: linear
- Output: `figs/XXX`

#### City-Level

**Script:** `scripts/TIMS-Analysis/08_TIMS_city_RDiT.R`

- Repeat RDiT for San Diego, San Francisco, and LA.
- Use city-specific enforcement dates.
- Output: `figs/XXX`

#### Placebo Test

**Script:** `scripts/TIMS-Analysis/12_TIMS_placebo.R`

- Applies the same RDiT to false cutoff dates in the pre-period (e.g., January 2017, January 2018, January 2023).
- A valid design should show no significant discontinuity at placebo dates.
- Output: `figs/XXX`

#### Weather RDiT

**Script:** `scripts/Weather-Analysis/02_NOAA_RDiT.R` - Tests the continuity of weather conditions (precipitation and temperature) at January 2024 and January 2025. - Rules out seasonal weather change as a confound for the main result. - Output: `figs/XXX`

#### Heterogeneity Analysis

**Scripts:** `scripts/TIMS-Analysis/13_TIMS_lighting.R`, `scripts/TIMS-Analysis/15_TIMS_other_crashes.R`, `scripts/TIMS-Analysis/16_TIMS_Collision_Severity.R`

Separate RDiT models for each subgroup:

| Dimension       | Subgroups                                             |
|-----------------|-------------------------------------------------------|
| Time of day     | Daytime crashes vs. nighttime crashes                 |
| Crash type      | Pedestrian-only vs. all other crashes (falsification) |
| Injury severity | Fatal, severe injury, minor injury                    |

Output: `figs/XXX`

### Hotspots and Coldspots Analyses

**Script:** `scripts/Hotspots-Analysis/01_getis_ord_hotspots.R`

------------------------------------------------------------------------

## Part 2: R Shiny Dashboard (`ShinyApp/`)

| Tab           | Content                                              |
|---------------|------------------------------------------------------|
| overview      | Policy and project introduction                      |
| literature    | Literature about pedestrian safety                   |
| maps & trends | Maps of crashes across the state                     |
| methodology   | RDiT specification and Hotspots analysis description |
| results       | Results in tables and figures                        |
| about us      | Introduce the whole team                             |

------------------------------------------------------------------------

## How to Reproduce {#how-to-reproduce}

Open `Daylighting-Project.Rproj` in RStudio first (ensures `here::here()` paths work), then run `Master.R`:

``` r
source("Master.R")
```

------------------------------------------------------------------------

## Dependencies {#dependencies}

``` r
install.packages(c(
  # Data handling
  "data.table", "tidyverse", "here", "lubridate",
  # Spatial
  "sf", "tigris",
  # RDiT estimation
  "rdrobust", "fixest",
  # Dashboard
  "shiny", "bslib", "DT", "leaflet", "plotly"
))
```

Tested on R 4.4.x.

------------------------------------------------------------------------

## Contact {#contact}

| Name | Institution | Email |
|------------------------|------------------------|------------------------|
| Kyle Klemba | College of William & Mary | [kyleklemba\@vt.edu](mailto:kyleklemba@vt.edu){.email} |
| Pride Akana Techa | Berea College | [tprideakana\@vt.edu](mailto:tprideakana@vt.edu){.email} |
