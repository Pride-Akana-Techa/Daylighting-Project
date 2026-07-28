# ==========================================================================
# Packages, data and styling
# ==========================================================================

# Libraries
library(shiny)
library(shinyjs)
library(plotly)
library(tidyverse)
library(bslib)
library(tidycensus)
library(leaflet)
library(sf)
library(scales)
library(here)
library(htmlwidgets)
library(leafgl)
library(leaflet.extras)
library(rdrobust)
library(patchwork)
library(ggh4x)

# Palette
brand <- list(
  navy       = "#003B5C",   # deep blue — headings, borders, primary elements
  ink        = "#1C2B33",   # dark blue-charcoal — body text (complements navy)
  muted      = "#5A7381",   # steel blue-gray — metadata labels
  surface    = "#F7FAFB",   # cool off-white
  border     = "#DCE6EA",   # light blue-gray border
  highlight  = "#EAF1F4",   # soft blue highlight
  link       = "#F5ECD7",
  accent     = "#C16200"    # burnt orange
)
app_theme <- bs_theme(
  version      = 5,
  bg           = "white",
  fg           = brand$ink,
  primary      = brand$navy,
  secondary    = brand$muted,
  base_font    = font_google("Inter"),
  heading_font = font_google("Playfair Display")
) %>%
  bs_add_rules(paste0("
    :root {
      --brand-navy: ", brand$navy, ";
      --brand-ink: ", brand$ink, ";
      --brand-muted: ", brand$muted, ";
      --brand-surface: ", brand$surface, ";
      --brand-border: ", brand$border, ";
      --brand-highlight: ", brand$highlight, ";
      --brand-link: ", brand$link, ";
      --brand-accent: ", brand$accent, ";
    }

    .specimen-section { margin: 40px 0; }
    .specimen-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 24px;
      margin-top: 20px;
    }
    .specimen-card {
      background: #FFFFFF;
      border: 1px solid var(--brand-border);
      display: flex;
      flex-direction: column;
      transition: border-color .2s ease, box-shadow .2s ease;
    }
    .specimen-card:hover {
      border-color: var(--brand-navy);
      box-shadow: 0 4px 18px rgba(10, 17, 40, 0.08);
    }
    .specimen-frame {
      position: relative;
      width: 100%;
      aspect-ratio: 4 / 3;
      overflow: hidden;
      background: var(--brand-surface);
      border-bottom: 1px solid var(--brand-border);
    }
    .specimen-frame img {
      width: 100%;
      height: 100%;
      object-fit: cover;
      filter: grayscale(25%) contrast(1.02);
      transform: scale(1.0);
      transition: filter .35s ease, transform .5s ease;
    }
    .specimen-card:hover .specimen-frame img {
      filter: grayscale(0%) contrast(1.02);
      transform: scale(1.025);
    }
    .specimen-tag {
      position: absolute;
      top: 10px;
      left: 10px;
      background: var(--brand-navy);
      color: #F1F4F9;
      font-family: monospace;
      font-size: 0.68rem;
      text-transform: uppercase;
      letter-spacing: 0.06em;
      padding: 4px 8px;
      z-index: 2;
    }
    .specimen-body { padding: 16px 18px 18px 18px; flex: 1; display: flex; flex-direction: column; }
    .specimen-eyebrow {
      font-family: monospace;
      font-size: 0.72rem;
      text-transform: lowercase;
      letter-spacing: 0.05em;
      color: var(--brand-muted);
      margin-bottom: 4px;
    }
    .specimen-title {
      font-family: 'Playfair Display', serif;
      font-size: 1.15rem;
      color: var(--brand-navy);
      margin: 0 0 10px 0;
      line-height: 1.25;
    }
    .specimen-desc {
      font-size: 0.85rem;
      color: var(--brand-ink);
      line-height: 1.6;
      margin-bottom: 14px;
      flex: 1;
    }
    .specimen-meta {
      border-top: 1px dashed var(--brand-border);
      padding-top: 10px;
      display: flex;
      flex-direction: column;
      gap: 6px;
    }
    .specimen-meta-row {
      display: flex;
      justify-content: space-between;
      font-size: 0.74rem;
    }
    .specimen-meta-label {
      font-family: monospace;
      text-transform: uppercase;
      color: var(--brand-muted);
      letter-spacing: 0.04em;
    }
    .specimen-meta-value {
      color: var(--brand-ink);
      font-weight: 600;
      text-align: right;
    }
@media (max-width: 950px) {
  .specimen-grid {
    grid-template-columns: 1fr;
  }
}

.specimen-grid img {
  width: 100%;
  height: auto;
  object-fit: cover;
}
    /* Default Page */
    html, body { height: 100%; background-color: #FFFFFF; font-size: 0.95rem; -webkit-font-smoothing: antialiased; }

    /* Two column */
    .app-shell { display: flex; min-height: 100vh; align-items: stretch; }

    /* sidebar */
      .app-sidebar {
      width: 250px;
      flex: 0 0 250px;
      background: #003B5C;   
      border-right: 1px solid var(--brand-border);
      display: flex;
      flex-direction: column;
      padding: 48px 32px;
      position: sticky;
      top: 0;
      height: 100vh;
    }
    .sidebar-brand {
      padding-bottom: 28px;
      margin-bottom: 20px;
      border-bottom: 1px solid var(--brand-border);
    }
    .brand-title { font-weight: 700; font-size: 1.2rem; color: #F1F4F9; letter-spacing: -0.03em; line-height: 1.2; }
    .brand-subtitle { font-size: 0.78rem; color: var(--brand-surface); margin-top: 8px; line-height: 1.4; font-family: 'Inter', sans-serif; font-weight: 400; }

    .sidebar-nav { display: flex; flex-direction: column; gap: 6px; margin-top: 20px; }

    .sidebar-nav-link {
      display: block;
      padding: 8px 12px;
      margin-left: -12px;
      border-radius: 4px;
      color: var(--brand-surface) !important;
      text-decoration: none !important;
      font-size: 0.88rem;
      font-family: monospace;
      text-transform: lowercase;
      transition: all .15s ease;
    }
    .sidebar-nav-link:hover { color: var(--brand-navy) !important; background-color: var(--brand-surface); }
    .sidebar-nav-link.active {
      color: var(--brand-navy) !important;
      font-weight: 600;
      background-color: var(--brand-highlight);
    }

    .app-main { flex: 1 1 auto; min-width: 0; padding: 0px 40px; position: relative; }

    /* typographics */
    .section-eyebrow {
  text-transform: uppercase;
  font-family: 'Playfair Display', serif;
  font-weight: 700;
  font-size: 0.85rem;
  color: var(--brand-navy);
  margin-bottom: 8px;
  letter-spacing: 0.04em;
}
    .right-aligned-eyebrow {
  text-align: right;
  text-transform: uppercase;
  font-family: 'Playfair Display', serif;
  font-weight: 700;
  font-size: 0.85rem;
  color: var(--brand-navy);
}

    .page-title {
      font-weight: 400;
      font-size: 2.4rem;
      color: var(--brand-navy);
      margin: 8px 0 36px 0;
      letter-spacing: -0.02em;
      line-height: 1.2;
    }
    
    .tab-content {
  margin-top: 28px;
}

    .document-card {
  background: var(--brand-surface);
  border-radius: 12px;
  padding: 24px 28px;
  margin-bottom: 16px;
  border-left: 3px solid var(--card-accent, var(--brand-navy));
}

    .document-card-title {
  font-size: 0.95rem;
  font-family: 'Playfair Display', serif;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  color: var(--brand-navy);
  margin-top: 0;        
  margin-bottom: 16px;
}

        .double-hr {
      border-top: 3px double var(--brand-border);
      margin: 40px 0 16px 0;
    }

    .register-container {
      border: 1px solid var(--brand-border);
      background: #FFFFFF;
      margin-bottom: 24px;
    }
    .register-header {
  display: flex;
  justify-content: space-between;
  padding: 10px 14px;
  border-bottom: 1px solid var(--brand-border);
  background: var(--brand-surface);
  font-family: 'Playfair Display', serif;
  font-weight: 700;
  font-size: 0.85rem;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  color: var(--brand-navy);
}
    .register-row {
      display: flex;
      flex-direction: column;
      align-items: stretch;
      gap: 6px;
      padding: 10px 14px;
      border-bottom: 1px solid var(--brand-border);
      font-size: 0.88rem;
    }
    .register-row > span:first-child {
      font-weight: 600;
      color: var(--brand-ink);
    }
    .register-row .shiny-input-container,
    .register-row .form-group {
      width: 100% !important;
      max-width: 100% !important;
      margin-bottom: 0 !important;
    }
    .register-row select,
    .register-row .irs {
      width: 100% !important;
    }
    .register-row:last-child { border-bottom: none; }
    .register-row .shiny-input-container { margin-bottom: 0 !important; padding-top: 0 !important; }
    .form-check-inline { margin-right: 12px; font-size: 0.85rem; }
    .irs--headline .irs-bar { background: var(--brand-navy); }
    .map-workspace-container {
      border: 1px solid var(--brand-border);
      background: #FFFFFF;
      overflow: hidden;
      position: relative;
    }
    .map-legend-banner {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 12px 20px;
      background: var(--brand-surface);
      border-bottom: 1px solid var(--brand-border);
      font-family: monospace;
      font-size: 0.78rem;
    }
    .cluster-warning-banner,
    .range-warning-banner {
      min-height: 38px;
      margin-top: 14px;
      display: flex;
      align-items: center;
    }
    .inline-warning-content {
      width: 100%;
      padding: 8px 16px;
      background: var(--brand-highlight);
      border-left: 3px solid var(--brand-navy);
      font-family: monospace;
      font-size: 0.78rem;
      color: var(--brand-ink);
    }
    .text-link {
      background-color: var(--brand-link);
      padding: 0.05em 0.2em;
      border-radius: 2px;
    }

    /* Maps & Trends two-column layout — plain flexbox on purpose.
       bslib::layout_columns() wraps children in a CSS-grid container that
       (depending on the fillable context) can end up with overflow rules
       that silently break position:sticky on a child. A plain flex row
       with align-items:flex-start avoids that entirely. */
    .maps-flex-row {
      display: flex;
      align-items: flex-start;
      gap: 2rem;
      flex-wrap: nowrap;
    }
    .maps-flex-row > .filter-sticky-col {
      flex: 0 0 280px;
      max-width: 280px;
    }
    .maps-flex-row > .maps-right-col {
      flex: 1 1 auto;
      min-width: 320px;
    }
    .filter-sticky-col {
      position: -webkit-sticky;
      position: sticky;
      top: 20px;
      max-height: calc(100vh - 40px);
      overflow-y: auto;
      padding-right: 4px;
    }
    .filter-sticky-col::-webkit-scrollbar { width: 6px; }
    .filter-sticky-col::-webkit-scrollbar-thumb { background: var(--brand-border); border-radius: 3px; }

    /* Literature review content (pulled in from Word doc via pandoc) */
    .lit-content h1, .lit-content h2, .lit-content h3 {
      font-family: 'Playfair Display', serif;
      color: var(--brand-navy);
      margin-top: 28px;
    }
    .lit-content p {
      color: var(--brand-ink);
      line-height: 1.7;
      font-size: 0.92rem;
    }
    .lit-content ul, .lit-content ol {
      padding-left: 20px;
      color: var(--brand-ink);
      line-height: 1.7;
    }
    .lit-content blockquote {
      border-left: 3px solid var(--brand-border);
      padding-left: 16px;
      color: var(--brand-muted);
      font-style: italic;
    }

    /* Variable definition tables */
    .var-table-wrap {
      margin-top: 20px;
      border-top: 1px solid var(--brand-border);
      border-bottom: 1px solid var(--brand-border);
    }
    
   .var-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.88rem;
  background-color: #FFFFFF;
  border: 1px solid var(--brand-border);
  border-radius: 6px;
  overflow: hidden;
}
.var-table thead th {
  text-align: left;
  font-family: 'Inter', sans-serif;
  font-weight: 700;
  font-size: 0.8rem;
  color: var(--brand-ink);
  padding: 12px 16px;
  background-color: var(--brand-highlight);
  border-bottom: 1px solid var(--brand-border);
}
.var-table tbody td {
  padding: 12px 16px;
  vertical-align: top;
  color: var(--brand-ink);
  border-bottom: 1px solid var(--brand-highlight);
}
.var-table tbody tr:nth-child(even) {
  background-color: var(--brand-highlight);
}
.var-table .var-name {
  font-family: 'Playfair Display', serif;
  font-style: italic;
  white-space: nowrap;
  color: var(--brand-navy);
}
.var-table .var-role {
  color: var(--brand-muted);
  font-family: monospace;
  font-size: 0.78rem;
  text-transform: uppercase;
  white-space: nowrap;
}
    
    .results-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.88rem;
  background-color: #FFFFFF;   
  border: 1px solid var(--brand-border);   
  border-radius: 6px;   
  overflow: hidden;   
}
.results-table thead th {
  text-align: left;
  font-family: 'Inter', sans-serif;
  font-weight: 700;
  font-size: 0.8rem;
  color: var(--brand-ink);
  padding: 12px 16px;
  background-color: var(--brand-highlight);   
  border-bottom: 1px solid var(--brand-border);
}
.results-table tbody td {
  padding: 12px 16px;
  vertical-align: top;
  color: var(--brand-ink);
  border-bottom: 1px solid var(--brand-highlight);
}
.results-table tbody tr:nth-child(even) {
  background-color: var(--brand-highlight); 
}

.results-table .cutoff-name {
  font-family: 'Playfair Display', serif;
  font-style: italic;
  color: var(--brand-navy);
  white-space: nowrap;
}
.results-table .sig-stars {
  color: var(--brand-accent);
  font-weight: 700;
  margin-left: 2px;
}
.results-table-note {
  font-size: 0.8rem;
  font-style: italic;
  color: var(--brand-accent);   
  margin-top: 12px;
  line-height: 1.6;
}

.results-table .group-cell {
  font-family: 'Inter', sans-serif;
  font-weight: 700;
  color: var(--brand-navy);
  vertical-align: middle;
  border-right: 1px dashed var(--brand-border);
}
.results-table .subgroup-cell {
  font-family: 'Playfair Display', serif;
  font-style: italic;
  color: var(--brand-ink);
  vertical-align: middle;
  border-right: 1px dashed var(--brand-border);
}

.accordion-item {
  border: 1px solid var(--brand-border) !important;
  background: #FFFFFF;
}
.accordion-button {
  font-family: 'Playfair Display', serif;
  font-weight: 700;
  font-size: 0.95rem;
  color: var(--brand-navy) !important;
  background-color: var(--brand-surface) !important;
}
.accordion-button:not(.collapsed) {
  background-color: var(--brand-highlight) !important;
  box-shadow: none;
}
.accordion-button:focus {
  box-shadow: none;
  border-color: var(--brand-border);
}

    @media (max-width: 950px) {
      .app-shell { flex-direction: column; }
      .app-sidebar { width: 100%; height: auto; position: relative; border-right: none; border-bottom: 1px solid var(--brand-border); padding: 24px; }
      .sidebar-nav { flex-direction: row; flex-wrap: wrap; gap: 12px; margin-top: 12px; }
      .sidebar-brand { margin-bottom: 0; padding-bottom: 12px; }
      .app-main { padding: 32px 24px; }
    }
    /* Only collapse the filter column under the map/graph on genuinely
       narrow (phone-width) screens — the main sidebar breakpoint above is
       too wide for this and was causing the filter panel to stack full-width
       even in a normal desktop window. */
    @media (max-width: 700px) {
      .maps-flex-row { flex-wrap: wrap; }
      .filter-sticky-col { position: relative; top: 0; max-height: none; overflow-y: visible; flex-basis: 100%; max-width: 100%; }
      .maps-flex-row > .maps-right-col { flex-basis: 100%; min-width: 0; }
    }
  "))


# ==========================================================================
# Load Data
# ==========================================================================

ca_boundary   <- readRDS("shiny-data/ca_boundary.rds")
bike_or_ped_acc_all <- readRDS("shiny-data/TIMS_bike_ped_all.rds")
bike_or_ped_acc_sf  <- readRDS("shiny-data/TIMS_bike_ped_geo.rds")

bike_or_ped_acc_sf <- bike_or_ped_acc_sf %>%
  st_transform(4326)

bike_or_ped_acc_all <- bike_or_ped_acc_all %>%
  mutate(
    COLLISION_DATE = as.Date(COLLISION_DATE),
    year  = lubridate::year(COLLISION_DATE),
    month = lubridate::floor_date(COLLISION_DATE, "month")
  )

map_coords <- st_coordinates(bike_or_ped_acc_sf)
bike_or_ped_acc_sf <- bike_or_ped_acc_sf %>%
  st_drop_geometry() %>%
  mutate(
    COLLISION_DATE = as.Date(COLLISION_DATE),
    lng = map_coords[, 1],
    lat = map_coords[, 2],
    year  = lubridate::year(COLLISION_DATE),
    month = lubridate::floor_date(COLLISION_DATE, "month")
  )

# Severity code labels, shared across dropdown filters and the graph's
# secondary color-by aggregation
severity_labels <- c(
  "Fatal Injury" = "1",
  "Serious Injury" = "2",
  "Minor Injury" = "3",
  "Complaint of Pain" = "4"
)
severity_labels_rev <- setNames(names(severity_labels), severity_labels)

# Map-performance caps: keep the browser responsive and avoid crashes when a
# filter selection still matches a very large number of records
MAX_CLUSTER_POINTS <- 10000   # individual/clustered markers only shown below this count
TOP_N_COUNTIES     <- 10     # counties shown individually in the graph's "County" color-by legend; rest grouped as "Other"

# Convert word to HTML
docx_path <- normalizePath("shiny-data/literature_review.docx", mustWork = TRUE)

if (!dir.exists("www")) dir.create("www")
html_path <- file.path(normalizePath("www", mustWork = TRUE), "literature_review.html")

includeHTML("www/literature_review.html")


# ==========================================================================
# RDiT Model Fitting 
# ==========================================================================

tims_crashes <- readRDS("shiny-data/tims_filtered2.rds")

## Jan 2024 cut-off(4-year span)
rdit_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
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


# --- Jan 2024 cutoff (2-year span) ---------------------------------------
two_year_rdit <- tims_crashes |>
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |>
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
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
  summarise(Total_crashes = n(), .groups = "drop")

two_year_model <- rdrobust(
  y = two_year_rdit$Total_crashes,
  x = two_year_rdit$Time,
  covs = model.matrix(~ Season_factor, two_year_rdit)[, -1],
  c = 0, p = 1, h = 12, kernel = "triangular"
)

seasonal_model <- lm(Total_crashes ~ Season_factor, data = two_year_rdit)
two_year_rdit$Crash_adj <- resid(seasonal_model) + mean(two_year_rdit$Total_crashes)

# --- Jan 2025 cutoff -------------------------------------------------------
rdit_data3 <- tims_crashes |>
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |>
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
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
  summarise(Total_crashes = n(), .groups = "drop")

rd_model3 <- rdrobust(
  y = rdit_data3$Total_crashes,
  x = rdit_data3$Time,
  covs = model.matrix(~ Season_factor, rdit_data3)[, -1],
  c = 0, p = 1, h = 12, kernel = "triangular"
)

season_model3 <- lm(Total_crashes ~ Season_factor, data = rdit_data3)
rdit_data3$Crash_adj3 <- resid(season_model3) + mean(rdit_data3$Total_crashes)

# --- CI bands ---------------------------------------------------------------
h <- 12
make_ci_band <- function(data, y_var, side_filter, xseq) {
  d <- data |> filter(side_filter(Time))
  d$w <- (1 - abs(d$Time / h)) * (abs(d$Time / h) <= 1)
  fit <- lm(reformulate("Time", response = y_var), data = d, weights = w)
  pred <- predict(fit, newdata = data.frame(Time = xseq), se.fit = TRUE)
  data.frame(
    Time = xseq,
    fit = pred$fit,
    lwr = pred$fit - qt(0.975, fit$df.residual) * pred$se.fit,
    upr = pred$fit + qt(0.975, fit$df.residual) * pred$se.fit
  )
}

xseq_left  <- seq(-12, 0, length.out = 100)
xseq_right <- seq(0, 12, length.out = 100)

ci_2024 <- bind_rows(
  make_ci_band(two_year_rdit, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(two_year_rdit, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368, Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(rdit_data3, "Crash_adj3", function(t) t < 0, xseq_left),
  make_ci_band(rdit_data3, "Crash_adj3", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368, Model = "Jan 2025 Cutoff")

# --- Plot builder ------------------------------------------------------------
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(100, 500, by = 50)) {
  ggplot(ci_data) +
    geom_ribbon(aes(x = Date, ymin = lwr, ymax = upr), fill = line_color, alpha = 0.2) +
    geom_line(data = filter(ci_data, Time < 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_line(data = filter(ci_data, Time > 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black") +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_date(date_labels = "%b %Y", limits = x_limits, breaks = x_breaks, expand = c(0.02, 0)) +
    theme_minimal(base_size = 13) +
    labs(title = model_label, x = "Month", y = "Crash Count") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024, cutoff_date = as.Date("2024-01-01"),
  model_label = "Jan 2024 Cutoff (Warning Begins)", line_color = "#003B5C",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025, cutoff_date = as.Date("2025-01-01"),
  model_label = "Jan 2025 Cutoff (Enforcement Begins)", line_color = "#C16200",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

statewide_rdit_plot <- p_2024 + p_2025

# ==========================================================================
# Bandwidth Sensitivity Check
# ==========================================================================

bw_sensitivity <- function(data, y_var, x_var, covs_var, cutoff, bw_grid, kernel = "triangular") {
  y <- data[[y_var]]
  x <- data[[x_var]]
  
  covs <- model.matrix(~ data[[covs_var]])[, -1, drop = FALSE]
  
  results <- lapply(bw_grid, function(h) {
    fit <- tryCatch(
      rdrobust(y = y, x = x, c = cutoff, h = h, kernel = kernel, covs = covs),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(data.frame(bw = h, coef = NA, se = NA, ci_lower = NA, ci_upper = NA))
    }
    
    data.frame(
      bw       = h,
      coef     = fit$coef["Conventional", ],
      se       = fit$se["Conventional", ],
      ci_lower = fit$ci["Conventional", "CI Lower"],
      ci_upper = fit$ci["Conventional", "CI Upper"]
    )
  })
  
  bind_rows(results)
}

bw_grid_2024 <- seq(4, 24, by = 2)
bw_grid_2025 <- seq(4, 12, by = 1)

sens_2024 <- bw_sensitivity(
  data     = rdit_data2,   
  y_var    = "Total_crashes",
  x_var    = "Time",
  covs_var = "Season_factor",
  cutoff   = 0,
  bw_grid  = bw_grid_2024
)
sens_2024$cutoff_label <- "January 2024 (Warning)"

sens_2025 <- bw_sensitivity(
  data     = rdit_data3,      # Jan 2025 cutoff data (already built above)
  y_var    = "Total_crashes",
  x_var    = "Time",
  covs_var = "Season_factor",
  cutoff   = 0,
  bw_grid  = bw_grid_2025
)
sens_2025$cutoff_label <- "January 2025 (Enforcement)"

sens_all <- bind_rows(sens_2024, sens_2025)

bandwidth_sensitivity_plot <- ggplot(sens_all, aes(x = bw, y = coef)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ cutoff_label, scales = "free") +
  facetted_pos_scales(
    x = list(
      cutoff_label == "January 2024 (Warning)"     ~ scale_x_continuous(limits = c(4, 24), breaks = seq(4, 24, by = 2)),
      cutoff_label == "January 2025 (Enforcement)"  ~ scale_x_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 1))
    ),
    y = list(
      cutoff_label == "January 2024 (Warning)"      ~ scale_y_continuous(limits = c(-400, 250), breaks = seq(-300, 250, by = 100)),
      cutoff_label == "January 2025 (Enforcement)"  ~ scale_y_continuous(limits = c(-400, 250), breaks = seq(-300, 250, by = 100))
    )
  ) +
  labs(
    x = "Bandwidth (months)",
    y = "RD Estimate",
    title = "Bandwidth Sensitivity of RDiT Estimates",
    subtitle = "Shaded band = 95% confidence interval"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

# ==========================================================================
# Hot & Cold Spot Analysis (Getis-Ord Gi)
# ==========================================================================
hotspot_results <- tryCatch(
  readRDS("shiny-data/gi_results_by_city_scenarios.rds"),
  error = function(e) NULL
)

hot_cold_colors <- c(
  "Hot Spot (99% Conf.)"  = "#b2182b",
  "Hot Spot (95% Conf.)"  = "#ef8a62",
  "Hot Spot (90% Conf.)"  = "#fddbc7",
  "Cold Spot (90% Conf.)" = "#d1e5f0",
  "Cold Spot (95% Conf.)" = "#67a9cf",
  "Cold Spot (99% Conf.)" = "#2166ac"
)
make_gi_map <- function(res_data, geom_type = "bg", base_size = 13) {
  if (is.null(res_data)) return(NULL)
  
  panel_data <- switch(geom_type,
                       "hex"   = res_data$hex_panel,
                       "tract" = res_data$tract_panel,
                       res_data$bg_panel
  )
  optimal_k  <- switch(geom_type,
                       "hex"   = res_data$hex_k,
                       "tract" = res_data$tract_k,
                       res_data$bg_k
  )
  unit_name  <- switch(geom_type,
                       "hex"   = "Hexagon Grid",
                       "tract" = "Census Tracts",
                       "Block Groups"
  )
  if (is.null(panel_data)) return(NULL)
  
  city_boundary <- res_data$city_boundary
  city_lab    <- unique(panel_data$city)
  scen_lab    <- unique(panel_data$scenario)
  months_lab  <- unique(panel_data$window_months)
  
  title_text    <- sprintf("%s (%s)", city_lab, gsub("_", " ", scen_lab))
  subtitle_text <- sprintf("Gi* Change in Crashes Per Month (%s | %.1f-Month Window | Optimal k = %d)", 
                           unit_name, months_lab, optimal_k)
  
  ggplot() +
    geom_sf(data = city_boundary, fill = "#F5F5F5", color = NA) +
    geom_sf(
      data = panel_data, aes(fill = significance),
      color = "#ffffff", linewidth = 0.05, show.legend = TRUE
    ) +
    geom_sf(data = city_boundary, fill = NA, color = "#222222", linewidth = 0.6) +
    scale_fill_manual(
      values = hot_cold_colors, na.value = "white",
      na.translate = FALSE, drop = FALSE, name = "Confidence"
    ) +
    coord_sf(datum = NA) +
    theme_minimal(base_size = base_size, base_family = "Helvetica") +
    labs(title = title_text, subtitle = subtitle_text, x = NULL, y = NULL) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "italic", color = "grey30"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      panel.grid = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(fill = guide_legend(override.aes = list(fill = hot_cold_colors)))
}

hotspot_summary_df <- function(active_panel) {
  if (is.null(active_panel) || nrow(active_panel) == 0) return(NULL)
  
  df <- active_panel |> sf::st_drop_geometry()
  levels_order <- levels(df$significance)
  total_units <- nrow(df)
  has_nbr <- "neighbor_geoids" %in% names(df)
  
  df |>
    dplyr::filter(!is.na(significance)) |>
    dplyr::group_by(significance) |>
    dplyr::group_modify(~ {
      grp <- .x
      base <- data.frame(
        Count      = nrow(grp),
        Share      = nrow(grp) / total_units,
        Pre_Total  = sum(grp$pre, na.rm = TRUE),
        Post_Total = sum(grp$post, na.rm = TRUE),
        Avg_Change = mean(grp$change_rate_mo, na.rm = TRUE),
        Avg_Local_Mean = mean(grp$local_gi_mean, na.rm = TRUE)
      )
      if (has_nbr) {
        union_geoids <- unique(unlist(grp$neighbor_geoids))
        nbr_rows <- df[df$GEOID %in% union_geoids, ]
        base$Pre_Neighbor_Total  <- sum(nbr_rows$pre, na.rm = TRUE)
        base$Post_Neighbor_Total <- sum(nbr_rows$post, na.rm = TRUE)
      } else {
        base$Pre_Neighbor_Total  <- NA_real_
        base$Post_Neighbor_Total <- NA_real_
      }
      base
    }) |>
    dplyr::mutate(significance = factor(significance, levels = levels_order)) |>
    dplyr::arrange(significance)
}

# ==========================================================================
# City-Level RDiT Models (San Diego, San Francisco, Los Angeles)
# ==========================================================================

fit_city_rdit <- function(city, cutoff_date, year_filter = NULL, date_filter = NULL, h = 12) {
  d <- tims_crashes |> filter(CITY == city)
  
  if (!is.null(year_filter)) d <- d |> filter(ACCIDENT_YEAR %in% year_filter)
  if (!is.null(date_filter)) d <- d |> filter(COLLISION_DATE >= date_filter)
  
  d <- d |>
    filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
    mutate(
      MONTH = floor_date(ymd(COLLISION_DATE), "month"),
      Time  = interval(as.Date(cutoff_date), MONTH) %/% months(1),
      Post  = ifelse(Time >= 0, 1, 0),
      Season_factor = factor(
        case_when(
          month(MONTH) %in% c(12, 1, 2) ~ "Winter",
          month(MONTH) %in% c(3, 4, 5)  ~ "Spring",
          month(MONTH) %in% c(6, 7, 8)  ~ "Summer",
          month(MONTH) %in% c(9, 10, 11) ~ "Fall"
        ),
        levels = c("Winter", "Spring", "Summer", "Fall")
      )
    ) |>
    group_by(CITY, Time, Post, Season_factor) |>
    summarise(Total_crashes = n(), .groups = "drop")
  
  model <- rdrobust(
    y = d$Total_crashes, x = d$Time,
    covs = model.matrix(~ Season_factor, d)[, -1],
    c = 0, p = 1, h = h, kernel = "triangular"
  )
  
  # Seasonally-adjusted crash count, used for the CI-band plot
  season_fit <- lm(Total_crashes ~ Season_factor, data = d)
  d$Crash_adj <- resid(season_fit) + mean(d$Total_crashes)
  
  list(model = model, data = d)
}

san_diego_fit      <- fit_city_rdit("SAN DIEGO", "2024-01-01", year_filter = c(2023, 2024), h = 10)
san_diego1_fit     <- fit_city_rdit("SAN DIEGO", "2025-03-01", date_filter = as.Date("2024-05-01"), h = 10)
san_francisco_fit  <- fit_city_rdit("SAN FRANCISCO", "2024-01-01", year_filter = c(2023, 2024), h = 12)
san_francisco1_fit <- fit_city_rdit("SAN FRANCISCO", "2025-01-01", year_filter = c(2024, 2025), h = 12)
la_fit             <- fit_city_rdit("LOS ANGELES", "2024-01-01", year_filter = c(2023, 2024), h = 12)
la1_fit            <- fit_city_rdit("LOS ANGELES", "2025-01-01", year_filter = c(2024, 2025), h = 12)

san_diego_model      <- san_diego_fit$model
san_diego1_model     <- san_diego1_fit$model
san_francisco_model  <- san_francisco_fit$model
san_francisco1_model <- san_francisco1_fit$model
la_model             <- la_fit$model
la1_model            <- la1_fit$model


extract_rd <- function(model, city, cutoff) {
  tibble(
    City = city, Effect = model$coef[3, 1], Cutoff = cutoff,
    SE = model$se[3, 1], P_value = model$pv[3, 1],
    CI_lower = model$ci[3, 1], CI_upper = model$ci[3, 2]
  )
}

city_analysis <- bind_rows(
  extract_rd(san_diego_model,      "San Diego",     "2024"),
  extract_rd(san_diego1_model,     "San Diego",     "2025"),
  extract_rd(san_francisco_model,  "San Francisco", "2024"),
  extract_rd(san_francisco1_model, "San Francisco", "2025"),
  extract_rd(la_model,             "Los Angeles",   "2024"),
  extract_rd(la1_model,            "Los Angeles",   "2025")
) |>
  mutate(
    City = factor(City, levels = c("San Diego", "San Francisco", "Los Angeles")),
    Cutoff = factor(Cutoff, levels = c("2024", "2025")),
    Sig = case_when(
      P_value < 0.01 ~ "***",
      P_value < 0.05 ~ "**",
      P_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    Label = round(Effect, 2)
  )

build_city_rd_plot <- function(fit_pre, fit_post, cutoff_date_pre, cutoff_date_post,
                               city_label, x_limits_pre, x_breaks_pre,
                               x_limits_post, x_breaks_post) {
  
  ci_pre <- bind_rows(
    make_ci_band(fit_pre$data, "Crash_adj", function(t) t < 0, xseq_left),
    make_ci_band(fit_pre$data, "Crash_adj", function(t) t >= 0, xseq_right)
  ) |> mutate(Date = as.Date(cutoff_date_pre) + Time * 30.4368)
  
  ci_post <- bind_rows(
    make_ci_band(fit_post$data, "Crash_adj", function(t) t < 0, xseq_left),
    make_ci_band(fit_post$data, "Crash_adj", function(t) t >= 0, xseq_right)
  ) |> mutate(Date = as.Date(cutoff_date_post) + Time * 30.4368)
  
  p1 <- make_cutoff_plot(
    ci_pre, cutoff_date = as.Date(cutoff_date_pre),
    model_label = paste(city_label, "— Warning Phase Cutoff"),
    line_color = "#003B5C", event_label = "Warning Begins",
    x_limits = x_limits_pre, x_breaks = x_breaks_pre,
    y_breaks = seq(5, 35, by = 5)
  )
  
  p2 <- make_cutoff_plot(
    ci_post, cutoff_date = as.Date(cutoff_date_post),
    model_label = paste(city_label, "— Enforcement Phase Cutoff"),
    line_color = "#C16200", event_label = "Enforcement Begins",
    x_limits = x_limits_post, x_breaks = x_breaks_post,
    y_breaks = seq(5, 35, by = 5)
  )
  
  p1 + p2
}

city_rdit_plots <- list(
  "San Diego" = build_city_rd_plot(
    san_diego_fit, san_diego1_fit,
    cutoff_date_pre = "2024-01-01", cutoff_date_post = "2025-03-01",
    city_label = "San Diego",
    x_limits_pre  = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
    x_breaks_pre  = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months"),
    x_limits_post = c(as.Date("2024-05-01"), as.Date("2025-12-01")),
    x_breaks_post = seq(as.Date("2024-05-01"), as.Date("2025-12-01"), by = "2 months")
  ),
  "San Francisco" = build_city_rd_plot(
    san_francisco_fit, san_francisco1_fit,
    cutoff_date_pre = "2024-01-01", cutoff_date_post = "2025-01-01",
    city_label = "San Francisco",
    x_limits_pre  = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
    x_breaks_pre  = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months"),
    x_limits_post = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
    x_breaks_post = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
  ),
  "Los Angeles" = build_city_rd_plot(
    la_fit, la1_fit,
    cutoff_date_pre = "2024-01-01", cutoff_date_post = "2025-01-01",
    city_label = "Los Angeles",
    x_limits_pre  = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
    x_breaks_pre  = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months"),
    x_limits_post = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
    x_breaks_post = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
  )
)

# Bar plot
sunflower <- c("#F2C94C", "#1E4E8C", "#D4A04A", "#8A9B5B", "#F7F4E7")

city_rd_plot <- ggplot(city_analysis, aes(City, Effect, fill = Cutoff)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  geom_text(
    aes(label = Label,
        vjust = ifelse(Effect < 0, 1.15, -0.35),
        color = ifelse(Sig != "" & !is.na(Sig), "#C16200", "black"),
        group = Cutoff),
    position = position_dodge(width = 0.65), fontface = "bold", size = 4.5
  ) +
  geom_text(
    aes(label = Sig,
        y = ifelse(Effect < 0, Effect - 2, Effect + 2),
        vjust = ifelse(Effect < 0, -0.9, 1.5), hjust = -1.5,
        color = "#C16200"),
    position = position_dodge(width = 0.65), size = 4, fontface = "bold"
  ) +
  scale_color_identity() +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
  geom_hline(yintercept = 0, linewidth = .5) +
  scale_fill_manual(values = sunflower) +
  labs(
    title = "RD Effect by Major Cities",
    subtitle = "Comparison of San Diego, San Francisco, & Los Angeles",
    x = NULL, y = "RD Effect", fill = NULL,
    caption = "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10, color = "#C16200"),
    axis.text.x = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", size = 17),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# ==========================================================================
# Placebo Tests
# ==========================================================================

placebo_results <- tibble(
  cutoff = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018",
             "Jan 2019", "Jan 2022", "Jan 2023"),
  estimate = c(-76.197, -103.704, -68.854, -20.075,
               -58.669, -66.568, -49.719),
  ci_low = c(-124.301, -192.909, -203.998, -90.508,
             -146.671, -92.476, -79.862),
  ci_high = c(4.235, -25.260, 39.906, 76.878,
              29.847, -2.311, 8.596),
  p_value = c(0.067, 0.011, 0.187, 0.873,
              0.195, 0.039, 0.114)
) |>
  mutate(
    cutoff = factor(cutoff, levels = rev(cutoff)),
    significant = if_else(
      p_value < 0.05,
      "Significant at the 5% level",
      "Not significant"
    )
  )

placebo_plot <- ggplot(placebo_results,
                       aes(x = estimate, y = cutoff)) +
  geom_vline(xintercept = 0,
             color = "#EAF1F4",
             linewidth = 0.4) +
  geom_errorbar(
    aes(xmin = ci_low,
        xmax = ci_high,
        color = significant),
    height = 0.15,
    linewidth = 0.8
  ) +
  geom_point(aes(color = significant),
             size = 3) +
  scale_color_manual(
    values = c(
      "Significant at the 5% level" = "#C16200",
      "Not significant" = "#003B5C"
    )
  ) +
  labs(
    title = "Placebo Cutoff Tests",
    x = "Estimated RD Effect",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


# ==========================================================================
# Shared Input Component Layout
# ==========================================================================
filter_panel_register <- div(
  div(class = "section-eyebrow", "crash filters"),
  div(
    class = "register-container",
    div(class = "register-header", span("Map & Graph Controls")),
    div(
      class = "register-row",
      span("Road User Class"),
      selectInput(inputId = "mode", label = NULL, choices = c("All", "Pedestrian", "Bicyclist"), selected = "All", width = "160px")
    ),
    div(
      class = "register-row",
      span("Crash Location"),
      selectInput(inputId = "location_type", label = NULL, choices = c("All", "Intersection", "Non-Intersection"), selected = "All", width = "160px")
    ),
    div(
      class = "register-row",
      span("Time Frame"),
      sliderInput(inputId = "date_range", label = NULL, min = as.Date("2014-01-01"), max = as.Date("2025-12-31"), value = c(as.Date("2022-01-01"), as.Date("2025-12-31")), ticks = FALSE, timeFormat = "%Y-%m", dragRange = TRUE, width = "200px")
    )
  )
)

daylighting_specimen <- function(img_src, title, description) {
  tags$div(
    class = "specimen-card",
    tags$div(
      tags$img(src = img_src, alt = title)
    ),
    tags$div(
      class = "specimen-body",
      tags$h4(class = "specimen-title", title),
      tags$p(class = "specimen-desc", description)
    )
  )
}


# ==========================================================================
# Page Views Setup --------------------------------------------------------
# ==========================================================================
overview_page <- div(
  class = "page-content",
  layout_columns(
    col_widths = c(12),
    div(
      h1(class = "page-title", "Impacts of California AB 413 \"Daylighting Law\" on Pedestrian and Bicyclist Safety")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    gap = "3rem",
    
    # Left Narrative Column
    div(
      div(class = "document-card-title", "About the Law"),
      div(class = "document-card",
          p(
            p(
              "On October 10, 2023, ",
              HTML('<a href="https://leginfo.legislature.ca.gov/faces/billTextClient.xhtml?bill_id=202320240AB413" target="_blank" class="text-link">California Assembly Bill 413 (AB 413)</a>'),
              " was signed into law, adding Section 22500(n) to the California Vehicle Code. The legislation prohibits stopping, standing, or parking within 20 feet of the approach side of any marked or unmarked crosswalk or 15 feet where a curb extension is present. This restriction applies regardless of whether the curb is painted red, meaning the law took full legal effect independent of local signage or curb markings."
            ),
            p(
              "This traffic safety practice, known as ",
              HTML("<em>daylighting</em>,"),
              " is intended to improve intersection visibility by removing vehicles that can obstruct pedestrian and driver sightlines at intersections. Derived from the architectural term for allowing for natural light into a space, daylighting as a road safety tool dates back to the 1968 Vienna Convention on Road Traffic. California was one of only a handful of states without a statewide daylighting requirement; more than 40 states already mandated some form of it. The bill was authored by Assemblymember Alex Lee (D-AD 24), co-sponsored by the California Bicycle Coalition and Streets For All, and signed into law by Governor Gavin Newsom."
            )
          )
      ),
      
      div(class = "register-header", span("IMPLEMENTATION TIMELINE")),
      div(class = "register-container",
          div(
            class = "register-row",
            tags$span("Pre-enforcement"),
            tags$small("Before Jan 2024", style = "font-family: monospace; color: #991B1B; background: #FEE2E2; padding: 2px 8px; border-radius: 2px;")
          ),
          div(
            class = "register-row",
            tags$span("Warning Phase"),
            tags$small("Jan 2024 - Dec 2024", style = "font-family: monospace; color: #92400E; background: #FEF3C7; padding: 2px 8px; border-radius: 2px;")
          ),
          div(
            class = "register-row",
            tags$span("Citation Phase"),
            tags$small("After Jan 2025", style = "font-family: monospace; color: #065F46; background: #D1FAE5; padding: 2px 8px; border-radius: 2px;")
          )
      ),
      img(
        src = "SFA-Daylighting.jpg",
        width = "100%",
        style = "border: 1px solid var(--brand-border); opacity: 0.85; margin-top: 12px;"
      ) 
    ),
    
    # Right Context Column
    div(
      
      div(class = "document-card-title", "About this project"),
      div(class = "document-card",
          p(
            p(
              "This project evaluates the impact of AB 413 on pedestrian and bicyclist safety outcomes across California using statewide crash data and advanced statistical methods. The analysis examines changes in crash patterns before and after implementation of the law, while accounting for seasonal trends, weather conditions, lighting conditions, and differences across communities. In addition to measuring the overall effect of the policy, this project explores whether impacts vary by factors such as crash severity and local implementation strategies."
            ),
            p(
              "The findings from this research provide evidence on the effectiveness of daylighting policies as a transportation safety intervention and help identify where additional improvements may be needed. By combining data visualization, spatial analysis, and causal inference methods, this dashboard presents an accessible overview of the results and supports data-driven decision-making for transportation agencies, policymakers, and communities working to improve pedestrian and bicyclist safety."
            )
          )
      ),
      
      tags$head(
        tags$style(HTML("
    .specimen-grid {
      display: flex;
      flex-direction: column;
      gap: 20px;
      width: 100%;
      margin-top: 16px;
    }

    .specimen-grid > * {
      width: 100%;
      box-sizing: border-box;
    }

    .specimen-grid img {
      width: 100% !important;
      height: 220px;
      object-fit: cover;
      border-radius: 6px 6px 0 0;
      display: block;
    }
  "))
      ),
      tags$div(
        div(class = "document-card-title", "Daylighting Types"),
        # style = "margin-top: 16px;",
        # tags$h3(
        #   style = "font-family:'Playfair Display',serif; font-weight:400; font-size:1.7rem; color: var(--brand-navy); margin: 0 0 8px 0;",
        #   "Daylighting Types"
        # ),
        tags$p(
          style = "color: var(--brand-ink); line-height: 1.7; font-size: 0.92rem; max-width: 720px;",
          "There are several forms of daylighting. While AB 413 only implements red-curb daylighting, several cities, including Californian cities, have used other forms of daylighting."
        ),
        tags$div(
          class = "specimen-grid",
          daylighting_specimen(
            img_src = "vandykeave_vandykepl-redcurb.png",
            title = "Painted Red Curb",
            description = "Red curbs are the most affordable form of daylighting and are the style being implemented in respose to AB 413. A study by the New York City transportation department found no safety benefit from this form of daylighting."
          ),
          daylighting_specimen(
            img_src = "seventh&island-bulbout.png",
            title = "Curb Extension (Bulb-Out)",
            description = "Curb extention daylighting is a form of daylighting where the road is phyiscally narrowed at intersections. This improves pedestrian safety by reducing the crossing distance and creating natural turn calming due to a narrower roadway."
          ),
          daylighting_specimen(
            img_src = "haynes&laguna-hardened-bicycle.png",
            title = "Hardened Daylighting",
            description = "Hardened daylighting is a form of daylighting in which a physical barrier is placed within the roadway to prevent parking. Examples include bicycle racks or planters. The New York City transportation department found this to be more effective than sign-only daylighting."
          )
        )
      )
    )
  )
)

literature_page <- div(
  class = "page-content",
  h1(class = "page-title", "Literature"),
  div(
    class = "document-card lit-content",
    includeHTML("www/literature_review.html")
  )
)


maps_page <- div(
  class = "page-content",
  h1(class = "page-title", "Maps & Trends"),
  div(
    class = "maps-flex-row",
    # Left Configuration
    div(
      class = "filter-sticky-col",
      filter_panel_register <- div(
        div(class = "section-eyebrow", "crash filters"),
        div(
          class = "register-container",
          div(class = "register-header", span("Map & Graph Controls")),

          div(
            class = "register-row",
            span("Injury Severity"),
            selectInput(inputId = "collision_severity", label = NULL, choices = "All", selected = "All", width = "160px")
          ),
          div(
            class = "register-row",
            span("Road User Class"),
            selectInput(inputId = "mode", label = NULL, choices = c("All", "Pedestrian", "Bicyclist"), selected = "All", width = "160px")
          ),
          div(
            class = "register-row",
            span("Crash Location"),
            selectInput(inputId = "location_type", label = NULL, choices = c("All", "Intersection", "Non-Intersection"), selected = "All", width = "160px")
          ),
          div(
            class = "register-row",
            span("County"),
            selectInput(inputId = "county", label = NULL, choices = "All", selected = "All", width = "160px")
          ),
          div(
            class = "register-row",
            span("Time Frame"),
            sliderInput(inputId = "date_range", label = NULL, min = as.Date("2014-01-01"), max = as.Date("2025-12-31"), value = c(as.Date("2022-01-01"), as.Date("2025-12-31")), ticks = FALSE, timeFormat = "%Y-%m", dragRange = TRUE, width = "200px")
          )
        )
      ),
      div(class = "section-eyebrow", "aggregation filters"),
      div(
        class = "register-container",
        div(class = "register-header", span("Graph Controls")),
        div(
          class = "register-row",
          span("Interval"),
          radioButtons("granularity", label = NULL, choices = c("Yearly" = "year", "Monthly" = "month"), selected = "year", inline = TRUE)
        ),
        div(
          class = "register-row",
          span("Color By"),
          selectInput(inputId = "color_by", label = NULL,
                      choices = c("None" = "none", "Road User Type" = "mode", "Injury Severity" = "severity",
                                  "County" = "county", "Crash Location" = "location"),
                      selected = "none", width = "160px")
        )
      )
    ),
    # Right Layout
    div(
      class = "maps-right-col",
      div(
        class = "map-workspace-container",
        leafletOutput("map", height = "500px")
      ),
      div(
        class = "cluster-warning-banner",
        uiOutput("cluster_warning")
      ),
      div(
        style = "border-top: 1px solid var(--brand-border); padding-top: 16px; margin-top: 20px;",
        plotlyOutput(outputId = "main_plot", height = "320px")
      ),
      div(
        class = "range-warning-banner",
        uiOutput("range_warning")
      )
    )
  )
)

hotspot_city_section <- function(city_name, show_title = TRUE) {
  slug <- gsub(" ", "_", tolower(city_name))
  tagList(
    if (show_title) div(class = "document-card-title", city_name),
    div(
      class = "map-workspace-container",
      div(
        class = "map-legend-banner",
        style = "display: flex; justify-content: space-between; align-items: center;",

        radioButtons(
          inputId  = paste0("geom_type_", slug),
          label    = NULL,
          choices  = list("Block Groups" = "bg", "Census Tracts" = "tract", "Hexagons" = "hex"),
          selected = "bg",
          inline   = TRUE
        )
      ),
      div(
        style = "padding: 12px 20px;",
        plotOutput(paste0("hotspot_map_", slug), height = "440px")
      )
    ),
    div(
      class = "document-card",
      uiOutput(paste0("hotspot_table_", slug))
    )
  )
}
methodology_page <- div(
  class = "page-content methodology-page",
  h1(class = "page-title", "Methodology"),
  navset_tab(
    id = "methodology_tabs",
    nav_panel(
      title = "RDiT Model",
      value = "rdit",
      
      # MODEL SPECIFICATION
      div(class = "document-card-title", "Regression Discontinuity in Time"),
      div(class = "document-card document-card--model",
          p(
            "To estimate the causal effect of the implementation of AB 413 on crash frequency at a statewide level, we employ a sharp Regression Discontinuity in Time (RDiT) design using the ",
            tags$code("rdrobust"), " package. RDiT is a causal inference method that compares an outcome, monthly crash count in this case, just before and after a specific date, and measures any jump or change at the cutoff. Since there are two dates of interest, January 01, 2024 (start of warning phase) and January 01, 2025 (start of citation phase), we conducted this analysis twice with each of the cutoff dates. We estimate a local linear specification as follows:",
            style = "color: var(--brand-ink); line-height: 1.7;"
          ),
          div(
            style = "padding: 16px 0; text-align: center; font-size: 1.05rem;",
            "$$Y_t = \\alpha + \\tau D_t + \\beta_1(t - t_0) + \\beta_2 D_t (t - t_0) + \\gamma \\, \\text{Season}_t + \\varepsilon_t, \\quad |t - t_0| \\le h$$"
          )
      ),
      
      # VARIABLE DEFINITIONS
      div(class = "document-card-title", "Variable Definitions"),
      div(class = "document-card document-card--variable",
          div(
            class = "var-table-wrap",
            tags$table(
              class = "var-table",
              tags$thead(
                tags$tr(
                  tags$th("Variable"),
                  tags$th("Description"),
                  tags$th("Role in Analysis")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(class = "var-name", HTML("Y<sub>t</sub>")),
                  tags$td("Crash count (pedestrian/bicyclist collisions) at time t."),
                  tags$td(class = "var-role", "Dependent Variable")
                ),
                tags$tr(
                  tags$td(class = "var-name", HTML("&tau;")),
                  tags$td("Estimated treatment effect at the cutoff, the coefficient of interest."),
                  tags$td(class = "var-role", "Coefficient of Interest")
                ),
                tags$tr(
                  tags$td(class = "var-name", HTML("D<sub>t</sub>")),
                  tags$td("Indicator equal to 1 for periods after the enforcement cutoff."),
                  tags$td(class = "var-role", "Key Independent Variable")
                ),
                tags$tr(
                  tags$td(class = "var-name", HTML("t &minus; t<sub>0</sub>")),
                  tags$td("Running variable: time elapsed relative to the cutoff date."),
                  tags$td(class = "var-role", "Running Variable")
                ),
                tags$tr(
                  tags$td(class = "var-name", HTML("&beta;<sub>1</sub>, &beta;<sub>2</sub>")),
                  tags$td("Slope terms allowing pre- and post-cutoff trends to differ."),
                  tags$td(class = "var-role", "Control")
                ),
                tags$tr(
                  tags$td(class = "var-name", "Season_t"),
                  tags$td("Categorical control for season, capturing seasonal variation in crash frequency."),
                  tags$td(class = "var-role", "Control")
                ),
                tags$tr(
                  tags$td(class = "var-name", HTML("h")),
                  tags$td("Bandwidth defining the estimation window around the cutoff."),
                  tags$td(class = "var-role", "Tuning Parameter")
                ),

                tags$tr(
                  tags$td(class = "var-name", HTML("&epsilon;<sub>t</sub>")),
                  tags$td("Idiosyncratic error term."),
                  tags$td(class = "var-role", "Error Term")
                )
              )
            )
          )
      ),
      
      # DATA SOURCE
      div(class = "document-card-title", "Data Source"),
      div(class = "document-card document-card--data",
          p(
            "Crash data comes from the Transportation Injury Mapping System (TIMS), filtered to pedestrian-involved collisions at intersections. ",
            "Each observation is aggregated to a monthly count, with separate datasets built around each cutoff date to allow the pre- and post-period windows to be sized independently.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          )
      ),
      
      # BANDWIDTH & KERNEL SELECTION
      div(class = "document-card-title", "Bandwidth & Kernel Selection"),
      div(class = "document-card document-card--bandwidth",
          p(
            "We choose a bandwidth wide enough to produce estimates with a tight confidence interval, but narrow enough to avoid contamination from observations far from the cutoff. See the Results page for the comparison across bandwidths.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          )
      ),
      
      # ROBUSTNESS & VALIDITY CHECKS
      div(class = "document-card-title", "Robustness & Validity Checks"),
      div(class = "document-card document-card--robustness",
          p(
            "To make sure the estimated result reflects the true effect of the policy, we ran the following checks:",
            style = "color: var(--brand-ink); line-height: 1.7;"
          ),
          tags$ul(
            style = "color: var(--brand-ink); line-height: 1.7; padding-left: 20px;",
            tags$li(tags$strong("Placebo cutoffs. "), "We re-ran the same RDiT specification at dates with no policy meaning: January 2016, 2017, and 2018, to confirm the model does not estimate a jump at arbitrary dates."),
            tags$li(tags$strong("Covariate smoothness. "), "Using station-level NOAA weather data, we tested whether temperature and precipitation change abruptly at the cutoff dates. A real discontinuity should be specific to crashes. So, if weather also jumped at the same moment, that would point to a confound rather than a policy effect.")
          )
      ),
      
      # HETEROGENEITY TESTS
      div(class = "document-card-title", "Heterogeneity Analysis"),
      div(class = "document-card document-card--heterogeneity",
          p(
            "With the understanding that a policy's effect on safety may not be uniform across different categories such as lighting condition and collision severity, we re-estimate the RDiT model separately within subgroups rather than assuming one effect applies everywhere:",
            style = "color: var(--brand-ink); line-height: 1.7;"
          ),
          tags$ul(
            style = "color: var(--brand-ink); line-height: 1.7; padding-left: 20px;",
            tags$li(tags$strong("By lighting condition. "), "Daytime vs. nighttime crashes, to test whether the effect concentrates in low-visibility conditions where daylighting theoretically matters most."),
            tags$li(tags$strong("By crash severity. "), "Fatal, suspected serious injury, suspected minor injury, and possible injury/complaint of pain, to test whether the policy affects crash frequency broadly or is concentrated in specific outcomes.")
          )
      ),
      
      # CONCLUSION / CROSS-REFERENCE
      div(class = "document-card document-card--conclusion",
          p(
            "Each subgroup uses the same specification and cutoff dates as the primary model, applied to a filtered outcome variable. Full results for each subgroup are reported on the Results page.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          )
      )
    ),
    
    nav_panel(
      title = "Hot & Cold Spot Analysis",
      value = "hotspot",
      
      # OVERVIEW / MODEL SPECIFICATION
      div(class = "document-card-title", "Hot & Cold Spot Analysis (Getis-Ord Gi*)"),
      div(class = "document-card document-card--hotspot",
          p(
            "To complement the statewide RDiT results, we conduct a spatial hot spot analysis using the Getis-Ord Gi* statistic to identify statistically significant clusters of pedestrian intersection crashes at a finer geographic scale. This analysis focuses on three of California's largest cities — Los Angeles, San Diego, and San Francisco.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          ),
          p(
            "For each city, different levels of geography (block groups, census tracts, and hexagons) are compared on their change in monthly pedestrian intersection crash rate between a pre- and post-policy window. Spatial clustering uses a k-nearest-neighbor structure, where at each level of analysis, units are clustered with k surrounding units to identify hot and cold spots. A small k value will result in an a localized neighborhood, leading to high variance and less stable estimates. A large k value will smooth out and hide local variations. The optimal size k for each map is chosen by finding the k value that maximizes the z-score of a global Moran's I statistical test. The Getis-Ord Gi* statistic (via ",
            tags$code("spdep::localG()"),
            ") is then computed at that optimal k, and each geography is classified as a hot spot, cold spot, or not significant at the 90%, 95%, or 99% confidence level based on its two-tailed p-value.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          ),
          p(
            "Two comparison windows are used per city: a window fixed at the January 2024 statewide warning date (one year pre vs. one year post), and a window at each city's own citation-enforcement start date, extended through the end of 2025, the most recent data available.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          ),
          p(
            "This is an exploratory spatial data analysis to identify spatial autocorrelation and localized heterogeneity across change hotspots, while our RDiT model should be used to draw conclusions about the effectiveness of the policy.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          )
      ),
      
      # VARIABLE DEFINITIONS
      div(class = "document-card-title", "Variable Definitions"),
      div(class = "document-card document-card--variable",
          div(
            class = "var-table-wrap",
            tags$table(
              class = "var-table",
              tags$thead(
                tags$tr(
                  tags$th("Variable"),
                  tags$th("Description"),
                  tags$th("Role in Analysis")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(class = "var-name", "change_rate_mo"),
                  tags$td("Change in pedestrian intersection crashes per month at a block group, post-window rate minus pre-window rate."),
                  tags$td(class = "var-role", "Outcome Variable")
                ),
                tags$tr(
                  tags$td(class = "var-name", HTML("G<sub>i</sub><sup>*</sup> (z-score)")),
                  tags$td("Getis-Ord local statistic measuring the degree of spatial clustering of high or low change_rate_mo values around block group i; calculated as a z-score."),
                  tags$td(class = "var-role", "Test Statistic")
                ),
                tags$tr(
                  tags$td(class = "var-name", "p-value"),
                  tags$td("Two-tailed probability, computed from the Gi* z-score, that the observed clustering could occur under complete spatial randomness."),
                  tags$td(class = "var-role", "Significance Measure")
                ),
                tags$tr(
                  tags$td(class = "var-name", "Confidence Level"),
                  tags$td("Classification of each block group as a 90%, 95%, or 99% confidence hot spot, cold spot, or not significant."),
                  tags$td(class = "var-role", "Classification")
                ),
                tags$tr(
                  tags$td(class = "var-name", "k (nearest neighbors)"),
                  tags$td("Number of nearest centroids treated as neighbors; chosen per city and level of geography via a Moran's I test. Values capped between k = 6 and k = 16."),
                  tags$td(class = "var-role", "Tuning Parameter")
                )
              )
            )
          )
      ),
      
      hr(class = "double-hr"),
      
      # CITY SUMMARY
      div(class = "document-card-title", "Selected Cities"),
      div(class = "document-card document-card--cities",
          p(
            "We selected Los Angeles, San Diego, and San Francisco for city-level analysis. They are three of California's largest incorporated cities, with high-volume pedestrian activity, and each has a sufficiently large sample of intersection-level crashes to support reliable local cluster detection. Notably, all three cities had already implemented early daylighting measures, such as curb extensions or painted curbs at high-risk intersections before AB 413 was enacted. Because exact geocoded locations for these daylighting sites were not available, evaluating hot and cold spots allows us to identify areas where pedestrian risks remain elevated despite existing policy interventions.",
            style = "color: var(--brand-ink); line-height: 1.7;"
          )
      )
    )
)
)
results_page <- div(
  class = "page-content",
  h1(class = "page-title", "Results"),
  
  navset_tab(
    id = "results_tabs",
    
    # ======================================================
    # TAB 1: Main Results
    # ======================================================
    nav_panel(
      title = "Statewide Findings",
      value = "main",
      
      #--------------------------------------------------
      # Statewide Results
      #--------------------------------------------------
      
      div(
        div(class = "document-card-title", "Pedestrian Outcome"),
        div(
          class = "document-card",
          p(
            "Our analysis provides encouraging evidence that AB 413 is reducing pedestrian crashes at intersections statewide. Using a consistent 12-month window around each policy date, we found statistically significant reductions in pedestrian crashes at both the January 2024 cutoff (when the law took effect) and the January 2025 cutoff (when enforcement began): roughly 79 and 62 fewer crashes on average, respectively. These two estimates represent different comparisons: the first looks at changes after the law took effect, while the second examines changes after enforcement began. They should not be added together or viewed as the same effect. Instead, the consistent reductions across both periods suggest that AB 413 began improving pedestrian safety soon after it was implemented.",
            style = "line-height:1.8;"
          )
        )
      ),
      
      div(
        div(class = "document-card-title", "RDiT Plot: Pedestrian Crashes at Intersections"),
        div(
          class = "document-card",
          plotOutput("statewide_rd_plot", height = "360px")
        ),
        
        div(class = "document-card-title", "Model Estimates"),
        div(
          class = "document-card",
          uiOutput("statewide_results_table")   
        )
      ),
      
      div(
        div(class = "document-card-title", "Bicyclist Outcome"),
        div(
          class = "document-card",
          p(
            "We did not find clear evidence that AB 413 led to an immediate change in bicycle crash counts at intersections following either the January 2024 implementation of the law or the January 2025 start of statewide enforcement. Although the estimated effects suggested a decrease in bicycle crashes after the law took effect and a slight increase after enforcement began, neither change was statistically significant. This contrasts with the pedestrian crash analysis, where statistically significant reductions were observed at both policy milestones. Together, these findings suggest that while AB 413 appears to have had an early positive effect on pedestrian safety, a similar short-term impact on bicycle crashes was not evident in the available data.",
            style = "line-height:1.8;"
          ),
          
          uiOutput("bicyclist_results_table")
        )
      ),
      
      hr(class = "double-hr"),
      
      #--------------------------------------------------
      # Heterogeneity Analysis
      #--------------------------------------------------
      div(class = "document-card-title", "Heterogeneity Analysis"),
      div(
        class = "document-card",
        p(
          "Heterogeneity analysis is important because it helps identify whether a policy's effects differ across specific conditions or groups, providing a more detailed understanding than an overall average effect. Our analysis found that the reduction in crashes following AB 413 was concentrated among moderate-severity crashes, with significant declines in crashes involving possible injuries, complaints of pain, and suspected minor injuries, while fatal and serious injury crashes showed no statistically significant changes, likely due to their low frequency. We also found that the policy's effectiveness varied by lighting conditions. During the warning phase, crash reductions were driven primarily by declines in daytime crashes, whereas during the citation enforcement phase, significant reductions were observed mainly in nighttime crashes. These findings suggest that the impacts of AB 413 were not uniform, highlighting the value of examining how policy effects vary across different crash characteristics and environmental conditions.",
          style = "line-height:1.8;"
        ),
        
        uiOutput("heterogeneity_table")
        
      )
    ),
    
    # ======================================================
    # TAB 2: City Analysis
    # ======================================================
    nav_panel(
      title = "City-level Analysis",
      value = "city",
      
      div(class = "document-card-title", "A case study of San Diego, San Francisco, and Los Angeles"),
      div(
        class = "document-card",
        p(
          "While statewide analyses provide an overall assessment of AB 413's impact, city-level analyses offer insight into how the law performs under different local conditions. Differences in implementation, enforcement, traffic patterns, and pedestrian activity mean that the policy's effects may not be uniform across California. To explore this variation, we examined three major cities with high levels of pedestrian activity: San Diego, San Francisco, and Los Angeles. Results showed distinct patterns across the policy's implementation phases. In San Francisco, pedestrian crashes at intersections increased during the January 2024 warning period but declined significantly after statewide enforcement began in January 2025. San Diego experienced a significant reduction in crashes following its March 2025 enforcement date, while Los Angeles showed no statistically significant changes during either phase. Overall, these findings indicate that the effectiveness of AB 413 differs across cities, suggesting that local enforcement strategies, implementation approaches, and roadway characteristics may influence the policy's impact.",
          style = "line-height:1.8;"
        ),
        plotOutput("city_rd_plot", height = "480px")
      ),
      
      hr(class = "double-hr"),
      
      div(class = "document-card-title", "Individual City Detail"),
      div(
        class = "document-card",
        p(
          "Each city panel below shows its RDiT model alongside the corresponding Getis-Ord Gi* hot & cold spot results. See the Methodology page for details on both models and variable definitions.",
          style = "line-height:1.8;"
        )
      ),
      
      div(
        class = "register-container",
        div(class = "register-header", span("Hot & Cold Spot Comparison Window")),
        div(
          class = "register-row",
          span("Scenario"),
          radioButtons(
            inputId = "hotspot_scenario", label = NULL,
            choices = list("Warning Phase (Jan 2024 baseline)" = "Warning_Date",
                           "Enforcement Phase (city-specific)" = "Enforcement_Date"),
            selected = "Enforcement_Date", inline = TRUE
          )
        )
      ),
      
      accordion(
        id = "city_rdit_accordion",
        open = FALSE,
        accordion_panel(
          title = "SAN DIEGO",
          div(class = "document-card-title", "RDiT Model"),
          plotOutput("city_rdit_san_diego", height = "380px"),
          hr(class = "double-hr"),
          div(class = "document-card-title", "Hot & Cold Spot Analysis"),
          hotspot_city_section("San Diego", show_title = FALSE)
        ),
        accordion_panel(
          title = "SAN FRANCISCO",
          div(class = "document-card-title", "RDiT Model"),
          plotOutput("city_rdit_san_francisco", height = "380px"),
          hr(class = "double-hr"),
          div(class = "document-card-title", "Hot & Cold Spot Analysis"),
          hotspot_city_section("San Francisco", show_title = FALSE)
        ),
        accordion_panel(
          title = "LOS ANGELES",
          div(class = "document-card-title", "RDiT Model"),
          plotOutput("city_rdit_la", height = "380px"),
          hr(class = "double-hr"),
          div(class = "document-card-title", "Hot & Cold Spot Analysis"),
          hotspot_city_section("Los Angeles", show_title = FALSE)
        )
      ),
      
      hr(class = "double-hr")
    ),
    
    # ======================================================
    # TAB 3: Robustness Checks
    # ======================================================
    nav_panel(
      title = "Robustness Checks",
      value = "robustness",
      
      #--------------------------------------------------
      # Bandwidth Analysis
      #--------------------------------------------------
      div(class = "document-card-title", "Bandwidth Selection Analysis"),
      div(
        class = "document-card",
        p(
          "We use a 12-month bandwidth in the main specification. The binding constraint is data availability: at the January 2025 cutoff, only 12 months of post-period data are currently observed, so 12 months is the widest symmetric window available for that cutoff, and we apply it to both cutoffs for comparability. This choice falls within the range where estimates are both precise and stable. The table below reports the main results at 10- and 11-month bandwidths as a robustness check. The point estimates are similar in magnitude, though standard errors are larger at the narrower bandwidths, and the estimates are no longer significant at the 5% level. Because the point estimates move little, the loss of significance appears to reflect the reduced number of post-period observations rather than instability in the estimated effect.",
          style = "line-height:1.8;"
        ),
        plotOutput("bandwidth_sensitivity_plot", height = "380px"),
        uiOutput("bandwidth_results_table")
      ),
      
      hr(class = "double-hr"),
      
      #--------------------------------------------------
      # Weather Analysis
      #--------------------------------------------------
      div(class = "document-card-title", "Weather Covariate Analysis"),
      div(
        class = "document-card",
        p(
          "As a covariate smoothness check, we tested whether temperature(°F) and precipitation(inches) showed a discontinuity at the policy cutoffs, since a genuine confound would likely show up as a change in observable conditions, not just crashes. Neither covariate showed a significant break at Jan 2024 or Jan 2025, supporting that the cutoffs are not conflated with a shift in weather patterns.",
          style = "line-height:1.8;"
        ),
        uiOutput("weather_results_table")
      ),
      
      hr(class = "double-hr"),
      
      #--------------------------------------------------
      # Placebo Tests
      #--------------------------------------------------
      div(class = "document-card-title", "Placebo Tests"),
      div(
        class = "document-card",
        p(
          "To assess whether the estimated policy effects are driven by the implementation of AB 413 rather than arbitrary temporal variation, we repeated the Regression Discontinuity in Time analysis at several placebo cutoff dates for which no policy intervention occurred. Most placebo estimates are statistically insignificant and their confidence intervals include zero, indicating that the model does not systematically detect discontinuities when no treatment is present. Although significant effects are observed for January 2016 and January 2022, these isolated findings are not part of a consistent pattern across placebo years and likely reflect unrelated temporal variation. Overall, the placebo analysis provides additional support that the significant discontinuities observed at the January 2024 and January 2025 policy cutoffs could be attributable to the implementation of AB 413.",
          style = "line-height:1.8;"
        ),
        plotOutput("placebo_plot", height = "350px")
      )
    )
  )
)

# About Us Tab #
# Helper for a picture placeholder + caption, reusable across all bio sections
team_member_card <- function(name, description, img_src = NULL) {
  tags$div(
    style = "display: flex; flex-direction: column; align-items: center; text-align: center; max-width: 260px;",
    tags$div(
      style = "width: 200px; height: 200px; border-radius: 50%; background-color: var(--brand-highlight); border: 2px solid var(--brand-border); display: flex; align-items: center; justify-content: center; overflow: hidden; margin-bottom: 16px;",
      if (!is.null(img_src)) {
        tags$img(src = img_src, style = "width: 100%; height: 100%; object-fit: cover;")
      } else {
        tags$span(
          style = "font-family: monospace; font-size: 0.7rem; text-transform: uppercase; color: var(--brand-muted); letter-spacing: 0.05em;",
          "photo"
        )
      }
    ),
    tags$h4(
      style = "font-family: 'Playfair Display', serif; font-size: 1.1rem; color: var(--brand-navy); margin: 0 0 4px 0;",
      name
    ),
    tags$div(
      style = "font-family: monospace; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.04em; color: var(--brand-accent); margin-bottom: 8px;"
    ),
    tags$p(
      style = "font-size: 0.85rem; color: var(--brand-ink); line-height: 1.6;",
      description
    )
  )
}

about_page <- div(
  class = "page-content",
  h1(class = "page-title", "About Us"),
  
  # ======================================================
  # 1. Introductory paragraph about DSPG
  # ======================================================
  div(class = "document-card-title", "About the DSPG Program"),
  div(
    class = "document-card",
    p(
      "The Data Science for the Public Good (DSPG) program is a Virginia Tech initiative that pairs student interns with faculty and public-sector partners to apply data science methods to pressing policy and community challenges. This project, evaluating the impact of California's AB 413 Daylighting Law on pedestrian and bicyclist safety, was conducted as part of the summer DSPG cohort in partnership with transportation safety stakeholders.",
      style = "line-height:1.8;"
    )
  ),
  
  # ======================================================
  # 2. Undergraduate Interns
  # ======================================================
  div(class = "document-card-title", "Undergraduate Interns"),
  div(
    class = "document-card",
    style = "display: flex; justify-content: space-around; flex-wrap: wrap; gap: 32px; padding: 32px 28px;",
    team_member_card(
      img_src = "kyle.JPG",
      name = "Kyle Klemba",
      description = "Data Science & Economics, William & Mary"
    ),
    team_member_card(
      img_src = "pride.JPG",
      name = "Pride Akana Techa",
      description = "Computer Science & Mathematics, Berea College"
    )
  ),
  
  # ======================================================
  # 3. Graduate Mentor
  # ======================================================
  div(class = "document-card-title", "Graduate Mentor"),
  div(
    class = "document-card",
    style = "display: flex; justify-content: center; padding: 32px 28px;",
    team_member_card(
      img_src = "yuanyuan.JPG",
      name = "Yuanyuan Wen",
      description = "Department of Agricultural and Applied Economics, Virginia Tech"
    )
  ),
  
  # ======================================================
  # 4. Faculty Mentors
  # ======================================================
  div(class = "document-card-title", "Faculty Mentors"),
  div(
    class = "document-card",
    style = "display: flex; justify-content: space-around; flex-wrap: wrap; gap: 32px; padding: 32px 28px;",
    team_member_card(
      img_src = "drgao.png",
      name = "Yujuan Gao, PhD",
      description =  "Department of Agricultural and Applied Economics, Virginia Tech"
    ),
    team_member_card(
      img_src = "drcary.jpg",
      name = "Michael Cary, PhD",
      description = "Department of Agricultural and Applied Economics, Virginia Tech"
    )
  ),
  # ======================================================
  # 5. Calwalks
  # ======================================================
  div(class = "document-card-title", "California Walks"),
  div(
    class = "document-card",
    p(
      "\"California Walks makes communities across our state more walkable. We champion safe, inclusive, and enjoyable streets and public spaces for all Californians. California Walks drives transformative policy change, empowers local advocates, and supports community-led design to make every street safe and welcoming.\"",
      style = "line-height:1.8;"
    )
  )
)


# ==========================================================================
# UI Assembly Shell Configuration -----------------------------------------
# ==========================================================================

ui <- page_fluid(
  theme = app_theme,
  style = "padding: 0; max-width: none;",
  tags$head(
    tags$title("California Assembly Bill 413: Daylighting Law"),
    tags$link(rel = "icon", href = "data:,"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/3.2.2/es5/tex-mml-chtml.js")
  ),
  useShinyjs(),
  div(
    class = "app-shell",
    tags$aside(
      class = "app-sidebar",
      div(
        class = "sidebar-brand",
        div("California Assembly Bill 413", class = "brand-title"),
        div("Impacts of Daylighting Policy on Pedestrian and Bicyclist Safety", class = "brand-subtitle")
      ),
      tags$nav(
        class = "sidebar-nav",
        actionLink("nav_overview", "Overview", class = "sidebar-nav-link active"),
        actionLink("nav_literature", "Literature", class = "sidebar-nav-link"),
        actionLink("nav_maps", "Maps & Trends", class = "sidebar-nav-link"),
        actionLink("nav_methodology", "Methodology", class = "sidebar-nav-link"),
        actionLink("nav_results", "Results", class = "sidebar-nav-link"),
        actionLink("nav_about", "About Us", class = "sidebar-nav-link")
      )
    ),
    
    tags$main(
      class = "app-main",
      navset_hidden(
        id = "main_nav",
        nav_panel(title = "Overview",     value = "overview",     overview_page),
        nav_panel(title = "Literature",   value = "literature",   literature_page),
        nav_panel(title = "Maps",         value = "maps",         maps_page),
        nav_panel(title = "Methodology",  value = "methodology",  methodology_page),
        nav_panel(title = "Results",      value = "results",      results_page),
        nav_panel(title = "About Us",     value = "about",        about_page)
      )
    )
  )
)

# ==========================================================================
# Server ------------------------------------------------------------------
# ==========================================================================

server <- function(input, output, session) {
  # Sidebar
  nav_map <- list(
    "nav_overview" = "overview",
    "nav_literature" = "literature",
    "nav_maps" = "maps",
    "nav_methodology" = "methodology",
    "nav_results" = "results",
    "nav_about" = "about"
  )
  lapply(names(nav_map), function(link_id) {
    observeEvent(input[[link_id]], {
      for (id in names(nav_map)) {
        if (id == link_id) {
          shinyjs::addClass(id, "active")
        } else {
          shinyjs::removeClass(id, "active")
        }
      }
      nav_select("main_nav", selected = nav_map[[link_id]], session = session)
    })
  })
  
  apply_filters <- function(data) {
    mode_val <- if (is.null(input$mode)) "" else input$mode[1]
    
    if (mode_val == "Pedestrian") {
      data <- data %>% filter(PEDESTRIAN_ACCIDENT == "Y")
    } else if (mode_val == "Bicyclist") {
      data <- data %>% filter(BICYCLE_ACCIDENT == "Y")
    } else if (mode_val == "All") {
      data <- data %>% filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")
    } else {
      data <- data %>% filter(FALSE)
    }
    if (input$location_type == "Intersection") {
      data <- data %>% filter(INTERSECTION == "Y")
    } else if (input$location_type == "Non-Intersection") {
      data <- data %>% filter(INTERSECTION == "N")
    }
    if (!is.null(input$collision_severity) && input$collision_severity != "All") {
      data <- data %>% filter(COLLISION_SEVERITY == input$collision_severity)
    }
    if (!is.null(input$county) && input$county != "All") {
      data <- data %>% filter(COUNTY == input$county)
    }
    dr <- date_range_debounced()
    if (!is.null(dr) && !is.na(dr[1]) && !is.na(dr[2])) {
      data <- data %>% filter(!is.na(COLLISION_DATE), COLLISION_DATE >= dr[1], COLLISION_DATE <= dr[2])
    }
    return(data)
  }
  
  date_range_debounced <- reactive({ input$date_range }) %>% debounce(400)
  
  filtered_data_graph <- reactive({ apply_filters(bike_or_ped_acc_all) }) %>%
    bindCache(input$mode, input$location_type,
              input$collision_severity, input$county, date_range_debounced())
  
  filtered_data_map <- reactive({ apply_filters(bike_or_ped_acc_sf) }) %>%
    bindCache(input$mode, input$location_type,
              input$collision_severity, input$county, date_range_debounced())
  
  filtered_data_map_debounced <- filtered_data_map %>% debounce(200)
  
  output$cluster_warning <- renderUI({
    data_map <- filtered_data_map()
    if (nrow(data_map) > MAX_CLUSTER_POINTS) {
      div(
        class = "inline-warning-content",
        "Clustered points only display below ",
        scales::comma(MAX_CLUSTER_POINTS),
        " records. Current selection is ",
        scales::comma(nrow(data_map)),
        " points. Displaying heatmap only."
      )
    } else {
      NULL
    }
  })
  
  # output$range_warning <- renderUI({
  #   req(input$granularity == "year")
  #   start_partial <- format(as.Date(input$date_range[1]), "%m") != "01"
    # end_partial   <- format(as.Date(input$date_range[2]), "%m") != "12"
    # if (start_partial || end_partial) {
    #   div(
    #     class = "inline-warning-content",
    #     "Incomplete year chosen. Output will reflect truncated annual aggregates for incomplete year."
    #   )
    # } else {
    #   NULL
    # }
  # })
  
  output$main_plot <- renderPlotly({
    if (nrow(filtered_data_graph()) == 0) {
      plot_ly() |>
        layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          annotations = list(text = "No metrics matched selected arguments.", showarrow = FALSE, font = list(family = "monospace", size = 12, color = brand$muted))
        ) |>
        config(displaylogo = FALSE)
    } else {
      plot_data <- filtered_data_graph() %>% filter(!is.na(COLLISION_DATE))
      period_var <- if (input$granularity == "year") "year" else "month"
      
      if (input$granularity == "year") {
        dr <- date_range_debounced()
        start_year <- lubridate::year(dr[1])
        end_year   <- lubridate::year(dr[2])
        start_partial <- format(as.Date(dr[1]), "%m") != "01"
        end_partial   <- format(as.Date(dr[2]), "%m") != "12"
        
        exclude_years <- c(
          if (start_partial) start_year,
          if (end_partial) end_year
        )
        if (length(exclude_years) > 0) {
          plot_data <- plot_data %>% filter(!(lubridate::year(COLLISION_DATE) %in% exclude_years))
        }
      }
      
      color_choice <- if (is.null(input$color_by)) "none" else input$color_by
      
      format_period <- function(x) {
        if (input$granularity == "year") as.character(x) else format(x, "%b %Y")
      }
      
      if (color_choice == "none") {
        
        summary_data <- plot_data %>%
          group_by(ACCIDENT_PERIOD = .data[[period_var]]) %>%
          summarise(Total_Accidents = n(), .groups = "drop") %>%
          arrange(ACCIDENT_PERIOD) %>%
          mutate(ACCIDENT_PERIOD = format_period(ACCIDENT_PERIOD))
        
        plot_ly(data = summary_data, x = ~ACCIDENT_PERIOD) %>%
          add_trace(
            y = ~Total_Accidents,
            type = "bar",
            name = "Incidents",
            marker = list(color = brand$muted)
          ) %>%
          layout(
            dragmode = FALSE,
            margin = list(t = 10, b = 40, l = 40, r = 10),
            xaxis = list(title = "", showgrid = FALSE, fixedrange = TRUE, type = "category",
                         categoryorder = "array", categoryarray = summary_data$ACCIDENT_PERIOD,
                         font = list(family = "monospace"), tickfont = list(family = "monospace", size = 10, color = brand$muted)),
            yaxis = list(title = "Incident Count", showgrid = TRUE, gridcolor = brand$border, fixedrange = TRUE, font = list(family = "monospace"), tickfont = list(family = "monospace", size = 10, color = brand$muted)),
            plot_bgcolor  = "rgba(0,0,0,0)",
            paper_bgcolor = "rgba(0,0,0,0)"
          ) %>%
          config(displayModeBar = FALSE)
        
      } else {
        
        if (color_choice == "mode") {
          plot_data <- plot_data %>%
            mutate(Category = case_when(
              PEDESTRIAN_ACCIDENT == "Y" & BICYCLE_ACCIDENT == "Y" ~ "Pedestrian & Bicyclist",
              PEDESTRIAN_ACCIDENT == "Y" ~ "Pedestrian Only",
              BICYCLE_ACCIDENT == "Y" ~ "Bicyclist Only",
              TRUE ~ "Other"
            ))
          category_order <- c("Pedestrian Only", "Bicyclist Only", "Pedestrian & Bicyclist", "Other")
          category_colors <- c(
            "Pedestrian Only" = brand$navy,
            "Bicyclist Only" = brand$accent,
            "Pedestrian & Bicyclist" = brand$muted,
            "Other" = brand$border
          )
        } else if (color_choice == "severity") {
          plot_data <- plot_data %>%
            mutate(Category = dplyr::recode(as.character(COLLISION_SEVERITY), !!!severity_labels_rev, .default = "Unknown"))
          category_order <- c("Fatal Injury", "Serious Injury", "Minor Injury", "Complaint of Pain", "Unknown")
          category_colors <- c(
            "Fatal Injury" = "#8B1E1E",
            "Serious Injury" = brand$accent,
            "Minor Injury" = "#3b528b",
            "Complaint of Pain" = brand$muted,
            "Unknown" = brand$border
          )
        } else if (color_choice == "county") {
          top_counties <- plot_data %>%
            filter(!is.na(COUNTY)) %>%
            dplyr::count(COUNTY, sort = TRUE) %>%
            dplyr::slice_head(n = TOP_N_COUNTIES) %>%
            dplyr::pull(COUNTY) %>%
            as.character()
          
          plot_data <- plot_data %>%
            mutate(Category = ifelse(!is.na(COUNTY) & as.character(COUNTY) %in% top_counties,
                                     as.character(COUNTY), "Other Counties"))
          category_order <- c(top_counties, "Other Counties")
          county_palette <- if (length(top_counties) > 0) scales::hue_pal()(length(top_counties)) else character(0)
          category_colors <- setNames(c(county_palette, brand$border), category_order)
        } else if (color_choice == "location") {
          plot_data <- plot_data %>%
            mutate(Category = case_when(
              INTERSECTION == "Y" ~ "Intersection",
              INTERSECTION == "N" ~ "Non-Intersection",
              TRUE ~ "Unknown"
            ))
          category_order <- c("Intersection", "Non-Intersection", "Unknown")
          category_colors <- c(
            "Intersection" = brand$navy,
            "Non-Intersection" = brand$accent,
            "Unknown" = brand$border
          )
        } else {
          plot_data <- plot_data %>% mutate(Category = "Incidents")
          category_order <- c("Incidents")
          category_colors <- c("Incidents" = brand$muted)
        }
        
        summary_data <- plot_data %>%
          group_by(ACCIDENT_PERIOD = .data[[period_var]], Category) %>%
          summarise(Total_Accidents = n(), .groups = "drop") %>%
          mutate(Category = factor(Category, levels = category_order)) %>%
          arrange(ACCIDENT_PERIOD, Category) %>%
          mutate(ACCIDENT_PERIOD = format_period(ACCIDENT_PERIOD))
        
        period_order <- unique(summary_data$ACCIDENT_PERIOD)
        
        p <- plot_ly(data = summary_data, x = ~ACCIDENT_PERIOD)
        
        for (cat in intersect(category_order, unique(as.character(summary_data$Category)))) {
          cat_data <- summary_data %>% filter(Category == cat)
          p <- p %>% add_trace(
            data = cat_data,
            x = ~ACCIDENT_PERIOD, y = ~Total_Accidents,
            type = "bar", name = cat,
            marker = list(color = category_colors[[cat]])
          )
        }
        
        p %>% layout(
          dragmode = FALSE,
          barmode = "stack",
          margin = list(t = 10, b = 40, l = 40, r = 10),
          legend = list(font = list(family = "monospace", size = 10)),
          xaxis = list(title = "", showgrid = FALSE, fixedrange = TRUE, type = "category",
                       categoryorder = "array", categoryarray = period_order,
                       font = list(family = "monospace"), tickfont = list(family = "monospace", size = 10, color = brand$muted)),
          yaxis = list(title = "Incident Count", showgrid = TRUE, gridcolor = brand$border, fixedrange = TRUE, font = list(family = "monospace"), tickfont = list(family = "monospace", size = 10, color = brand$muted)),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
          config(displayModeBar = FALSE)
      }
    }
  })
  
  output$map <- renderLeaflet({
    initial_coords <- isolate(filtered_data_map())
    
    m <- leaflet(options = leafletOptions(attributionControl = FALSE, preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        data = ca_boundary,
        fillColor = "transparent", color = brand$ink,
        weight = 1.2, opacity = 0.4
      ) %>%
      addLayersControl(
        overlayGroups = c("Accident Heatmap", "Accident Clusters"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)
    
    if (nrow(initial_coords) > 0) {
      m <- m %>% addHeatmap(
        data = initial_coords, lng = ~lng, lat = ~lat,
        blur = 22, max = 0.02, radius = 14,
        gradient = c("0.2" = "#440154", "0.4" = "#3b528b",
                     "0.6" = "#21918c", "0.8" = "#5ec962", "1.0" = "#fde725"),
        group = "Accident Heatmap"
      )
      if (nrow(initial_coords) <= MAX_CLUSTER_POINTS) {
        m <- m %>% addCircleMarkers(
          data = initial_coords, lng = ~lng, lat = ~lat,
          radius = 3, stroke = TRUE, color = "white", weight = 0.5,
          fillColor = brand$navy, fillOpacity = 0.4,
          clusterOptions = markerClusterOptions(),
          group = "Accident Clusters"
        )
      }
    }
    
    m
  })
  
  observe({
    updateSelectInput(session, "collision_severity",
                      choices = c("All" = "All", severity_labels),
                      selected = "All"
    )
    updateSelectInput(session, "county",
                      choices = c("All", sort(unique(na.omit(bike_or_ped_acc_all$COUNTY)))),
                      selected = "All"
    )
  })
  
  observe({
    coords <- filtered_data_map_debounced()
    n_total <- nrow(coords)
    
    proxy <- leafletProxy("map") %>%
      clearHeatmap() %>%
      clearGroup("Accident Heatmap") %>%
      clearGroup("Accident Clusters")
    
    if (n_total > 0) {
      proxy <- proxy %>%
        addHeatmap(
          data = coords, lng = ~lng, lat = ~lat,
          blur = 22, max = 0.02, radius = 14,
          gradient = c("0.2" = "#440154", "0.4" = "#3b528b",
                       "0.6" = "#21918c", "0.8" = "#5ec962", "1.0" = "#fde725"),
          group = "Accident Heatmap"
        )
      
      if (n_total <= MAX_CLUSTER_POINTS) {
        proxy %>% addCircleMarkers(
          data = coords, lng = ~lng, lat = ~lat,
          radius = 3, stroke = TRUE, color = "white", weight = 0.5,
          fillColor = brand$navy, fillOpacity = 0.4,
          clusterOptions = markerClusterOptions(),
          group = "Accident Clusters"
        )
      }
    }
  })
  
  observe({
    proxy <- leafletProxy("map")
    
    if (input$county == "All") {
      bbox <- st_bbox(ca_boundary)
      proxy %>% flyToBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
    } else {
      county_data <- bike_or_ped_acc_sf %>% filter(COUNTY == input$county)
      
      if (nrow(county_data) > 0) {
        lng_range <- quantile(county_data$lng, probs = c(0.01, 0.99), na.rm = TRUE)
        lat_range <- quantile(county_data$lat, probs = c(0.01, 0.99), na.rm = TRUE)
        
        proxy %>% flyToBounds(
          lng1 = lng_range[[1]], lat1 = lat_range[[1]],
          lng2 = lng_range[[2]], lat2 = lat_range[[2]]
        )
      }
    }
  })
  
  # --- Results page: statewide RDiT plot and estimates table ---------------
  output$statewide_rd_plot <- renderPlot({
    statewide_rdit_plot
  })
  
  # city-level plot
  output$city_rd_plot <- renderPlot({
    city_rd_plot
  })
  
  output$city_rdit_san_diego <- renderPlot({
    city_rdit_plots[["San Diego"]]
  })
  
  output$city_rdit_san_francisco <- renderPlot({
    city_rdit_plots[["San Francisco"]]
  })
  
  output$city_rdit_la <- renderPlot({
    city_rdit_plots[["Los Angeles"]]
  })
  
  output$statewide_results_table <- renderUI({
    results_df <- tibble::tibble(
      Cutoff       = c("January 2024", "January 2025"),
      Estimate     = c(two_year_model$coef["Robust", 1], rd_model3$coef["Robust", 1]),
      `p-value`    = c(two_year_model$pv["Robust", 1],   rd_model3$pv["Robust", 1]),
      `Std. Error` = c(two_year_model$se["Robust", 1],   rd_model3$se["Robust", 1]),
      CI_lower     = c(two_year_model$ci["Robust", 1],   rd_model3$ci["Robust", 1]),
      CI_upper     = c(two_year_model$ci["Robust", 2],   rd_model3$ci["Robust", 2])
    )
    
    sig_stars <- function(p) {
      if (p < 0.01) "***"
      else if (p < 0.05) "**"
      else if (p < 0.10) "*"
      else ""
    }
    
    tagList(
      tags$table(
        class = "results-table",
        tags$thead(
          tags$tr(
            tags$th("Cutoff"),
            tags$th("Estimate"),
            tags$th("p-value"),
            tags$th("Std. Error"),
            tags$th("95% CI")
          )
        ),
        tags$tbody(
          purrr::pmap(results_df, function(Cutoff, Estimate, `Std. Error`, CI_lower, CI_upper, `p-value`) {
            stars <- sig_stars(`p-value`)
            tags$tr(
              tags$td(class = "cutoff-name", Cutoff),
              tags$td(
                sprintf("%.3f", Estimate),
                if (stars != "") tags$span(class = "sig-stars", stars)
              ),
              tags$td(sprintf("%.3f", `p-value`)),
              tags$td(sprintf("%.3f", `Std. Error`)),
              tags$td(sprintf("[%.3f, %.3f]", CI_lower, CI_upper))
            )
          })
        )
      ),
      tags$p(
        class = "results-table-note",
        "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level."
      )
    )
  })
  
  # Bicyclist Crash Outcome
  output$bicyclist_results_table <- renderUI({
    results_df <- tibble::tibble(
      Cutoff       = c("January 2024", "January 2025"),
      Estimate     = c(-38.669, 21.470),
      `p-value`    = c(0.778, 0.283),
      `Std. Error` = c(56.256, 64.348),
      CI_lower     = c(-126.120, -57.071),
      CI_upper     = c(-94.402, 195.168)
    )
    
    sig_stars <- function(p) {
      if (p < 0.01) "***"
      else if (p < 0.05) "**"
      else if (p < 0.10) "*"
      else ""
    }
    
    tagList(
      tags$table(
        class = "results-table",
        tags$thead(
          tags$tr(
            tags$th("Cutoff"),
            tags$th("Estimate"),
            tags$th("p-value"),
            tags$th("Std. Error"),
            tags$th("95% CI")
          )
        ),
        tags$tbody(
          purrr::pmap(results_df, function(Cutoff, Estimate, `Std. Error`, CI_lower, CI_upper, `p-value`) {
            stars <- sig_stars(`p-value`)
            tags$tr(
              tags$td(class = "cutoff-name", Cutoff),
              tags$td(
                sprintf("%.3f", Estimate),
                if (stars != "") tags$span(class = "sig-stars", stars)
              ),
              tags$td(sprintf("%.3f", `p-value`)),
              tags$td(sprintf("%.3f", `Std. Error`)),
              tags$td(sprintf("[%.3f, %.3f]", CI_lower, CI_upper))
            )
          })
        )
      ),
      tags$p(
        class = "results-table-note",
        "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level."
      )
    )
  })
  
  # Heterogeneity Analysis #
  output$heterogeneity_table <- renderUI({
    het_df <- tibble::tribble(
      ~Category,            ~Subcategory,       ~Cutoff,     ~Estimate,  ~pval, ~se,     ~ci_lo,    ~ci_hi,
      "Lighting Condition",  "Daylight",         "Jan 2024",  -39.312,    0.002, 11.038,  -55.159,   -11.893,
      "Lighting Condition",  "Daylight",         "Jan 2025",  -4.029,     0.925, 19.811,  -36.965,    40.693,
      "Lighting Condition",  "Dark Time",        "Jan 2024",  -40.067,    0.077, 24.293,  -90.645,     4.584,
      "Lighting Condition",  "Dark Time",        "Jan 2025",  -57.804,    0.008, 28.256, -130.360,   -19.597,
      "Collision Severity",  "Possible Injury",  "Jan 2024",  -42.439,    0.003, 14.019,  -69.147,   -14.194,
      "Collision Severity",  "Possible Injury",  "Jan 2025",  -47.156,    0.003, 19.396,  -94.741,   -18.711,
      "Collision Severity",  "Minor Injury",     "Jan 2024",  -34.980,    0.016, 13.158,  -57.393,    -5.813,
      "Collision Severity",  "Minor Injury",     "Jan 2025",  -8.036,     0.373,  9.138,  -26.061,     9.761,
      "Collision Severity",  "Fatal Injury",     "Jan 2024",  -0.377,     0.664,  1.366,   -3.270,     2.084,
      "Collision Severity",  "Fatal Injury",     "Jan 2025",  -1.364,     0.824,  2.602,   -5.681,     4.520,
      "Collision Severity",  "Serious Injury",   "Jan 2024",   0.164,     0.941,  6.996,  -14.225,    13.197,
      "Collision Severity",  "Serious Injury",   "Jan 2025",  -5.586,     0.201,  5.877,  -19.039,     4.000
    )
    
    sig_stars <- function(p) {
      if (p < 0.01) "***"
      else if (p < 0.05) "**"
      else if (p < 0.10) "*"
      else ""
    }
    
    # rowspan counts for Category and Subcategory groups
    cat_spans    <- het_df %>% dplyr::count(Category, name = "n")
    subcat_spans <- het_df %>% dplyr::count(Category, Subcategory, name = "n")
    
    rows <- list()
    cat_seen    <- character(0)
    subcat_seen <- character(0)
    
    for (i in seq_len(nrow(het_df))) {
      row <- het_df[i, ]
      stars <- sig_stars(row$pval)
      
      cat_cell <- if (!(row$Category %in% cat_seen)) {
        cat_seen <- c(cat_seen, row$Category)   # was <<-
        tags$td(
          class = "group-cell",
          rowspan = cat_spans$n[cat_spans$Category == row$Category],
          row$Category
        )
      } else NULL
      
      subcat_key <- paste(row$Category, row$Subcategory)
      subcat_cell <- if (!(subcat_key %in% subcat_seen)) {
        subcat_seen <- c(subcat_seen, subcat_key)   # was <<-
        tags$td(
          class = "subgroup-cell",
          rowspan = subcat_spans$n[subcat_spans$Category == row$Category & subcat_spans$Subcategory == row$Subcategory],
          row$Subcategory
        )
      } else NULL
      
      rows[[i]] <- tags$tr(
        cat_cell,
        subcat_cell,
        tags$td(row$Cutoff),
        tags$td(
          sprintf("%.2f", row$Estimate),
          if (stars != "") tags$span(class = "sig-stars", stars)
        ),
        tags$td(sprintf("%.2f", row$pval)),
        tags$td(sprintf("%.2f", row$se)),
        tags$td(sprintf("[%.2f, %.2f]", row$ci_lo, row$ci_hi))
      )
    }
    
    tagList(
      tags$table(
        class = "results-table",
        tags$thead(
          tags$tr(
            tags$th("Category"),
            tags$th("Sub-category"),
            tags$th("Cutoff"),
            tags$th("Estimate"),
            tags$th("p-value"),
            tags$th("Std. Error"),
            tags$th("95% CI")
          )
        ),
        tags$tbody(rows)
      ),
      tags$p(
        class = "results-table-note",
        "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level."
      )
    )
  })
  
  # Bandwidth sensitivity
  # Plot
  output$bandwidth_sensitivity_plot <- renderPlot({
    bandwidth_sensitivity_plot
  })
  
  # Table
  output$bandwidth_results_table <- renderUI({
    bw_df <- tibble::tribble(
      ~Cutoff,       ~Bandwidth,    ~Estimate, ~pval, ~se,    ~ci_lo,    ~ci_hi,
      "Jan 2024",    "10 months",   -76.811,   0.174, 51.667, -171.539,   30.990,
      "Jan 2024",    "11 months",   -74.540,   0.064, 40.298, -153.729,    4.237,
      "Jan 2024",    "12 months",   -77.632,   0.021, 32.295, -137.677,  -11.083,
      "Jan 2025",    "10 months",   -66.904,   0.163, 45.518, -152.770,   25.657,
      "Jan 2025",    "11 months",   -64.522,   0.049, 36.090, -141.641,   -0.171,
      "Jan 2025",    "12 months",   -62.143,   0.023, 32.122, -135.933,  -10.017
    )
    
    sig_stars <- function(p) {
      if (p < 0.01) "***"
      else if (p < 0.05) "**"
      else if (p < 0.10) "*"
      else ""
    }
    
    cutoff_spans <- bw_df %>% dplyr::count(Cutoff, name = "n")
    
    rows <- list()
    cutoff_seen <- character(0)
    
    for (i in seq_len(nrow(bw_df))) {
      row <- bw_df[i, ]
      stars <- sig_stars(row$pval)
      
      cutoff_cell <- if (!(row$Cutoff %in% cutoff_seen)) {
        cutoff_seen <- c(cutoff_seen, row$Cutoff)
        tags$td(
          class = "group-cell",
          rowspan = cutoff_spans$n[cutoff_spans$Cutoff == row$Cutoff],
          row$Cutoff
        )
      } else NULL
      
      rows[[i]] <- tags$tr(
        cutoff_cell,
        tags$td(class = "subgroup-cell", row$Bandwidth),
        tags$td(
          sprintf("%.3f", row$Estimate),
          if (stars != "") tags$span(class = "sig-stars", stars)
        ),
        tags$td(sprintf("%.3f", row$pval)),
        tags$td(sprintf("%.3f", row$se)),
        tags$td(sprintf("[%.3f, %.3f]", row$ci_lo, row$ci_hi))
      )
    }
    
    tagList(
      tags$table(
        class = "results-table",
        tags$thead(
          tags$tr(
            tags$th("Cutoff"),
            tags$th("Bandwidth"),
            tags$th("Estimate"),
            tags$th("p-value"),
            tags$th("Std. Error"),
            tags$th("95% CI")
          )
        ),
        tags$tbody(rows)
      ),
      tags$p(
        class = "results-table-note",
        "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level."
      )
    )
  })
  
  # Weather Analysis Table
  output$weather_results_table <- renderUI({
    
    results_df <- tibble::tibble(
      Covariate   = c("Temperature", "Temperature",
                      "Precipitation", "Precipitation"),
      Cutoff      = c("January 2024", "January 2025",
                      "January 2024", "January 2025"),
      Estimate    = c(-3.324, -0.913,
                      2.440, -1.520),
      `p-value`   = c(0.949, 0.402,
                      0.108, 0.258),
      `Std. Error`= c(4.986, 7.257,
                      1.416, 2.935),
      CI_lower    = c(-9.455, -8.143,
                      -0.497, -9.070),
      CI_upper    = c(10.089, 20.303,
                      5.053, 2.435)
    )
    
    sig_stars <- function(p) {
      if (p < 0.01) "***"
      else if (p < 0.05) "**"
      else if (p < 0.10) "*"
      else ""
    }
    
    tagList(
      
      tags$table(
        class = "results-table",
        
        tags$thead(
          tags$tr(
            tags$th("Covariate"),
            tags$th("Cutoff"),
            tags$th("Estimate"),
            tags$th("p-value"),
            tags$th("Std. Error"),
            tags$th("95% CI")
          )
        ),
        
        tags$tbody(
          
          # Temperature
          tags$tr(
            tags$td(class = "group-cell",
                    rowspan = 2,
                    "Temperature"),
            tags$td(class = "cutoff-name", "January 2024"),
            tags$td(
              sprintf("%.3f", -3.324),
              tags$span(class = "sig-stars", sig_stars(0.949))
            ),
            tags$td(sprintf("%.3f", 0.949)),
            tags$td(sprintf("%.3f", 4.986)),
            tags$td(sprintf("[%.3f, %.3f]", -9.455, 10.089))
          ),
          
          tags$tr(
            tags$td(class = "cutoff-name", "January 2025"),
            tags$td(
              sprintf("%.3f", -0.913),
              tags$span(class = "sig-stars", sig_stars(0.402))
            ),
            tags$td(sprintf("%.3f", 0.402)),
            tags$td(sprintf("%.3f", 7.257)),
            tags$td(sprintf("[%.3f, %.3f]", -8.143, 20.303))
          ),
          
          # Precipitation
          tags$tr(
            tags$td(class = "group-cell",
                    rowspan = 2,
                    "Precipitation"),
            tags$td(class = "cutoff-name", "January 2024"),
            tags$td(
              sprintf("%.3f", 2.440),
              tags$span(class = "sig-stars", sig_stars(0.108))
            ),
            tags$td(sprintf("%.3f", 0.108)),
            tags$td(sprintf("%.3f", 1.416)),
            tags$td(sprintf("[%.3f, %.3f]", -0.497, 5.053))
          ),
          
          tags$tr(
            tags$td(class = "cutoff-name", "January 2025"),
            tags$td(
              sprintf("%.3f", -1.520),
              tags$span(class = "sig-stars", sig_stars(0.258))
            ),
            tags$td(sprintf("%.3f", 0.258)),
            tags$td(sprintf("%.3f", 2.935)),
            tags$td(sprintf("[%.3f, %.3f]", -9.070, 2.435))
          )
          
        )
      ),
      
      tags$p(
        class = "results-table-note",
        "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level."
      )
      
    )
    
  })
  
  # Placebo Results
  output$placebo_plot <- renderPlot({
    placebo_plot
  })
  
  # --- Hot & Cold Spot Analysis: per-city map + summary table ---------------
  hotspot_cities <- c("Los Angeles", "San Diego", "San Francisco")
  
  lapply(hotspot_cities, function(city_name) {
    local({
      city_name <- city_name
      slug <- gsub(" ", "_", tolower(city_name))
      
      # Render Map
      output[[paste0("hotspot_map_", slug)]] <- renderPlot({
        req(hotspot_results)
        key <- paste(city_name, input$hotspot_scenario, sep = "_")
        res <- hotspot_results[[key]]
        req(res)
        
        selected_geom <- input[[paste0("geom_type_", slug)]] %||% "bg"
        
        validate(
          need(
            !is.null(if (selected_geom == "hex") res$hex_panel else res$bg_panel),
            sprintf("Data is not available for %s under this spatial layer.", city_name)
          )
        )
        
        make_gi_map(res, geom_type = selected_geom)
      })
      
      # Render Table
      output[[paste0("hotspot_table_", slug)]] <- renderUI({
        req(hotspot_results)
        key <- paste(city_name, input$hotspot_scenario, sep = "_")
        res <- hotspot_results[[key]]
        req(res)
        
        selected_geom <- input[[paste0("geom_type_", slug)]] %||% "bg"
        active_panel  <- switch(selected_geom,
                                "hex"   = res$hex_panel,
                                "tract" = res$tract_panel,
                                res$bg_panel
        )        
        validate(
          need(!is.null(active_panel), sprintf("Data is not available for %s under this spatial layer.", city_name))
        )
        
        summary_df <- hotspot_summary_df(active_panel)
        req(summary_df, nrow(summary_df) > 0)
        
        tagList(
          tags$table(
            class = "results-table",
            tags$thead(
              tags$tr(
                tags$th("Cluster Type"),
                tags$th("Count"),
                tags$th("Share"),
                tags$th("Pre Total"),
                tags$th("Post Total"),
                tags$th("Pre Total (Neighborhood)"),
                tags$th("Post Total (Neighborhood)"),
                tags$th("Avg. Mo. Change (Own)"),
                tags$th("Avg. Mo. Change (Neighborhood)")
              )
            ),
            tags$tbody(
              purrr::pmap(summary_df, function(significance, Count, Share, Pre_Total, Post_Total,
                                               Pre_Neighbor_Total, Post_Neighbor_Total,
                                               Avg_Change, Avg_Local_Mean) {
                sig_color <- hot_cold_colors[[as.character(significance)]]
                row_bg    <- scales::alpha(sig_color, 0.16)
                tags$tr(
                  style = sprintf("background-color: %s;", row_bg),
                  tags$td(
                    class = "cutoff-name",
                    style = sprintf("color: %s; font-weight: 700; border-left: 5px solid %s; padding-left: 12px;", sig_color, sig_color),
                    as.character(significance)
                  ),
                  tags$td(scales::comma(Count)),
                  tags$td(scales::percent(Share, accuracy = 0.1)),
                  tags$td(scales::comma(Pre_Total)),
                  tags$td(scales::comma(Post_Total)),
                  tags$td(scales::comma(Pre_Neighbor_Total)),
                  tags$td(scales::comma(Post_Neighbor_Total)),
                  tags$td(sprintf("%.2f", Avg_Change)),
                  tags$td(sprintf("%.2f", Avg_Local_Mean))
                )
              })
            )
          )
        )
      })
    })
  })
}
# Run the app
shinyApp(ui, server)