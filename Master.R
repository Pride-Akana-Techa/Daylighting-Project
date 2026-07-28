# ==========================================================
# Daylighting Project Master Script
# ==========================================================


# Set Up ------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(plotly)


# Data Cleaning and Aggregation (TIMS) ---------------------------------------

source("initial-analysis/scripts/TIMS-Analysis/00_TIMS_cleaning_data.R")
source("initial-analysis/scripts/TIMS-Analysis/03_TIMS_aggregating_data.R")


# Time series and maps ----------------------------------------------------

source("initial-analysis/scripts/TIMS-Analysis/04_TIMS_graphs.R")
source("initial-analysis/scripts/TIMS-Analysis/05_TIMS_OSM_intro.R")


# RDiT Model --------------------------------------------------------------

## Statewide Analysis ##
source("initial-analysis/scripts/TIMS-Analysis/06_TIMS_RDiT_model.R")

## City-level Analysis ##
source("initial-analysis/scripts/TIMS-Analysis/08_TIMS_city_RDiT.R")

## Heterogeneity Analysis ##
source("initial-analysis/scripts/TIMS-Analysis/13_TIMS_lighting.R")
source("initial-analysis/scripts/TIMS-Analysis/16_TIMS_Collision_Severity.R")
source("initial-analysis/scripts/TIMS-Analysis/13_TIMS_weather.R")

## Robustness Checks ##
source("initial-analysis/scripts/TIMS-Analysis/12_TIMS_placebo.R")
source("initial-analysis/scripts/TIMS-Analysis/15_TIMS_other_crashes.R")
  # weather data
source("initial-analysis/scripts/Weather-Analysis/01_NOAA_weather.R")
source("initial-analysis/scripts/Weather-Analysis/02_NOAA_RDiT.R")

## School Zone Analysis ##
source("initial-analysis/scripts/School-Zone-Analysis/01_school_analysis.R")
source("initial-analysis/scripts/School-Zone-Analysis/02_school_RDiT.R")
source("initial-analysis/scripts/School-Zone-Analysis/03_san_jose_school_zones.R")


# Hotspots Analysis -------------------------------------------------------
source("initial-analysis/scripts/Hotspots-Analysis/01_getis_ord_hotspots.R")

# Grid Analysis -----------------------------------------------------------


