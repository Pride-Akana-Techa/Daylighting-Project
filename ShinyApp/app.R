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

# Palette
brand <- list(
  navy       = "#0A1128",   # text headings / rules
  ink        = "#1C2541",   # narrative text
  muted      = "#64748B",   # metadata labels
  surface    = "#F8FAFC",   
  border     = "#E2E8F0",   
  highlight  = "#EEF2F6",
  link       = "#F5ECD7",
  accent     = "#DC2626"    
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
      width: 320px;
      flex: 0 0 320px;
      background: #1E2A47;
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
    .brand-subtitle { font-size: 0.78rem; color: var(--brand-muted); margin-top: 8px; line-height: 1.4; font-family: 'Inter', sans-serif; font-weight: 400; }

    .sidebar-nav { display: flex; flex-direction: column; gap: 6px; margin-top: 20px; }

    .sidebar-nav-link {
      display: block;
      padding: 8px 12px;
      margin-left: -12px;
      border-radius: 4px;
      color: var(--brand-muted) !important;
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

    .app-main { flex: 1 1 auto; min-width: 0; padding: 56px 72px; position: relative; }

    /* typographics */
    .section-eyebrow {
      text-transform: lowercase;
      font-family: monospace;
      font-size: 0.8rem;
      color: var(--brand-muted);
      margin-bottom: 8px;
      letter-spacing: 0.05em;
    }
    .right-aligned-eyebrow {
      text-align: right;
      text-transform: lowercase;
      font-family: monospace;
      font-size: 0.8rem;
      color: var(--brand-muted);
    }

    .page-title {
      font-weight: 400;
      font-size: 2.4rem;
      color: var(--brand-navy);
      margin: 8px 0 36px 0;
      letter-spacing: -0.02em;
      line-height: 1.2;
    }

    /* Structured layout cards */
    .document-card {
      background: #FFFFFF;
      border-top: 2px solid var(--brand-navy);
      padding: 12px 0;
      margin-bottom: 16px;
    }
    .document-card-title {
      font-size: 0.82rem;
      font-family: monospace;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: var(--brand-muted);
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
      font-family: monospace;
      font-size: 0.75rem;
      text-transform: uppercase;
      color: var(--brand-muted);
    }
    .register-row {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 12px 14px;
      border-bottom: 1px solid var(--brand-border);
      font-size: 0.88rem;
    }
    .register-row:last-child { border-bottom: none; }
    .register-row .shiny-input-container { margin-bottom: 0 !important; padding-top: 0 !important; }
    .form-check-inline { margin-right: 12px; font-size: 0.85rem; }
    .irs--headline .irs-bar { background: var(--brand-navy); }
    .map-workspace-container {
      border: 1px solid var(--brand-border);
      background: #FFFFFF;
      overflow: hidden;
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
    .text-link {
      background-color: var(--brand-link);
      padding: 0.05em 0.2em;
      border-radius: 2px;
    }

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
    }
    .var-table thead th {
      text-align: left;
      font-family: 'Inter', sans-serif;
      font-weight: 700;
      font-size: 0.8rem;
      color: var(--brand-ink);
      padding: 12px 16px;
      border-bottom: 1px solid var(--brand-border);
    }
    .var-table tbody td {
      padding: 12px 16px;
      vertical-align: top;
      color: var(--brand-ink);
      border-bottom: 1px solid var(--brand-highlight);
    }
    .var-table tbody tr:nth-child(odd) {
      background-color: var(--brand-surface);
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

    @media (max-width: 950px) {
      .app-shell { flex-direction: column; }
      .app-sidebar { width: 100%; height: auto; position: relative; border-right: none; border-bottom: 1px solid var(--brand-border); padding: 24px; }
      .sidebar-nav { flex-direction: row; flex-wrap: wrap; gap: 12px; margin-top: 12px; }
      .sidebar-brand { margin-bottom: 0; padding-bottom: 12px; }
      .app-main { padding: 32px 24px; }
    }
  "))
    

# Load Data ---------------------------------------------------------------

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

# Convert word to HTML
docx_path <- normalizePath("shiny-data/literature_review.docx", mustWork = TRUE)

if (!dir.exists("www")) dir.create("www")
html_path <- file.path(normalizePath("www", mustWork = TRUE), "literature_review.html")

includeHTML("www/literature_review.html")


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
      span("Incident Severity"),
      selectInput(inputId = "victim_type", label = NULL, choices = c("All Incidents", "Injuries", "Fatalities"), selected = "All Incidents", width = "160px")
    ),
    div(
      class = "register-row",
      span("Road User Class"),
      checkboxGroupInput(inputId = "mode", label = NULL, choices = c("Pedestrian", "Bicyclist"), selected = c("Pedestrian", "Bicyclist"), inline = TRUE)
    ),
    div(
      class = "register-row",
      span("Crash Location"),
      selectInput(inputId = "location_type", label = NULL, choices = c("All", "Intersection", "Non-Intersection"), selected = "All", width = "160px")
    ),
    div(
      class = "register-row",
      span("Time Frame"),
      sliderInput(inputId = "date_range", label = NULL, min = as.Date("2014-01-01"), max = as.Date("2025-12-31"), value = c(as.Date("2014-01-01"), as.Date("2025-12-31")), ticks = FALSE, timeFormat = "%Y-%m", dragRange = TRUE, width = "200px")
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
      tags$p(class = "specimen-desc", description),
      
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
      h1(class = "page-title", "Impacts of California AB-413 \"Daylighting Law\" on Pedestrian and Bicyclist Safety")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    gap = "3rem",
    
    # Left Narrative Column
    div(
      div(
        class = "document-card",
        div(class = "document-card-title", "About the Law"),
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
          ),
          p(
            "AB-413 arrives amid a pedestrian safety crisis in California. The state recorded 1,106 pedestrian fatalities in 2023 alone, and pedestrians have consistently accounted for roughly a quarter or more of all California traffic deaths."
          )
        )
      )
    ),
    
    # Right Context Column
    div(
      div(
        class = "register-container",
        div(class = "register-header", span("IMPLEMENTATION TIMELINE")),
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
      height: 220px; /* Adjust height as preferred */
      object-fit: cover;
      border-radius: 6px 6px 0 0;
      display: block;
    }
  "))
      
      ),
      tags$div(
        class = "document-card specimen-section",
        style = "margin-top: 16px;",
        tags$h3(
          style = "font-family:'Playfair Display',serif; font-weight:400; font-size:1.7rem; color: var(--brand-navy); margin: 4px 0 8px 0;",
          "Daylighting Types"
        ),
        tags$p(
          style = "color: var(--brand-ink); line-height: 1.7; font-size: 0.92rem; max-width: 720px;",
          "............."
        ),
        tags$div(
          class = "specimen-grid",
          daylighting_specimen(
            img_src = "vandykeave_vandykepl-redcurb.png",
            title = "Painted Red Curb",
            description = "FILL"
          ),
          daylighting_specimen(
            img_src = "seventh&island-bulbout.png",
            title = "Curb Extension (Bulb-Out)",
            description = "DESCRIPTIOn"
          ),
          daylighting_specimen(
            img_src = "haynes&laguna-hardened-bicycle.png",
            title = "Hardened Daylighting",
            description = "FILL"
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
  layout_columns(
    col_widths = c(4, 8),
    gap = "2.5rem",
    # Left Configuration
    div(
      filter_panel_register <- div(
        div(class = "section-eyebrow", "crash filters"),
        div(
          class = "register-container",
          div(class = "register-header", span("Map & Graph Controls")),
          div(
            class = "register-row",
            span("Incident Severity"),
            selectInput(inputId = "victim_type", label = NULL, choices = c("All Incidents", "Injuries", "Fatalities"), selected = "All Incidents", width = "160px")
          ),
          div(
            class = "register-row",
            span("Worst Injury Severity"),
            selectInput(inputId = "collision_severity", label = NULL, choices = "All", selected = "All", width = "160px")
          ),
          div(
            class = "register-row",
            span("Road User Class"),
            checkboxGroupInput(inputId = "mode", label = NULL, choices = c("Pedestrian", "Bicyclist"), selected = c("Pedestrian", "Bicyclist"), inline = TRUE)
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
            sliderInput(inputId = "date_range", label = NULL, min = as.Date("2014-01-01"), max = as.Date("2025-12-31"), value = c(as.Date("2014-01-01"), as.Date("2025-12-31")), ticks = FALSE, timeFormat = "%Y-%m", dragRange = TRUE, width = "200px")
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
        )
      ),
      uiOutput("cluster_warning"),
      uiOutput("range_warning")
    ),
    # Right Layout
    div(
      div(
        class = "map-workspace-container",
        leafletOutput("map", height = "500px")
      ),
      div(style = "margin-top: 36px;"),
      div(
        style = "border-top: 1px solid var(--brand-border); padding-top: 16px;",
        plotlyOutput(outputId = "main_plot", height = "320px")
      )
    )
  )
)

methodology_page <- div(
  class = "page-content",
  h1(class = "page-title", "Methodology"),
  navset_tab(
    id = "methodology_tabs",
    nav_panel(
      title = "RDiT Model",
      value = "rdit",
      
      # MODEL SPECIFICATION  -----------------
      div(
        class = "document-card",
        div(class = "document-card-title", "Regression Discontinuity in Time"),
        p(
          "To estimate the causal effect of the implementation of AB 413 on crash frequency, we employ a sharp Regression Discontinuity in Time (RDiT) design using the ",
          tags$code("rdrobust"), " package. RDiT is a causal inference method that compares an outcome, monthly crash count in this case, just before and after a specific date, and measures any jump or change at the cutoff. Since there are two dates of interest, January 01, 2024 (start of warning phase) and January 01, 2025 (start of citation phase), we conducted this analysis twice with each of the cutoff dates. The local-linear specification estimated on either side of the cutoff is:",
          style = "color: var(--brand-ink); line-height: 1.7;"
        ),
        div(
          style = "padding: 16px 0; text-align: center; font-size: 1.05rem;",
          "$$Y_t = \\alpha + \\tau D_t + \\beta_1(t - t_0) + \\beta_2 D_t (t - t_0) + \\gamma \\, \\text{Season}_t + \\varepsilon_t, \\quad |t - t_0| \\le h$$"
        ),
        div(class = "document-card-title", "Variable Definitions"),
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
                tags$td("Bandwidth defining the estimation window around the cutoff, selected via MSE-optimal procedure."),
                tags$td(class = "var-role", "Tuning Parameter")
              ),
              tags$tr(
                tags$td(class = "var-name", HTML("&tau;")),
                tags$td("Estimated treatment effect at the cutoff, the coefficient of interest."),
                tags$td(class = "var-role", "Coefficient of Interest")
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
      # DATA SOURCE ------------------------------------------------------
      div(
        class = "document-card",
        div(class = "document-card-title", "Data Source"),
        p(
          "Crash data comes from the Transportation Injury Mapping System (TIMS), filtered to pedestrian-involved collisions at intersections. ",
          "Each observation is aggregated to a monthly count, with separate datasets built around each cutoff date to allow the pre- and post-period windows to be sized independently.",
          style = "color: var(--brand-ink); line-height: 1.7;"
        )
      ),
      # 4. BANDWIDTH & KERNEL SELECTION -------------------------------------
      div(
        class = "document-card",
        div(class = "document-card-title", "Bandwidth & Kernel Selection"),
        p(
          "Because the RD estimate can be sensitive to the number of data points on either side of the cutoff, we tested a range of bandwidths and kernel choices and selected the specification that produced the most stable, lowest-variance estimates. See the Results page for the comparison across bandwidths.",
          style = "color: var(--brand-ink); line-height: 1.7;"
        )
      ),
      # 5. ROBUSTNESS & VALIDITY CHECKS -------------------------------------
      div(
        class = "document-card",
        div(class = "document-card-title", "Robustness & Validity Checks"),
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
      # 6. HETEROGENEITY TESTS ----------------------------------------------
      div(
        class = "document-card",
        div(class = "document-card-title", "Heterogeneity Analysis"),
        p(
          "With the understanding that a policy's effect on safety may not be uniform across different categories such as lighting condition and collision severity, we re-estimate the RDiT model separately within subgroups rather than assuming one effect applies everywhere:",
          style = "color: var(--brand-ink); line-height: 1.7;"
        ),
        tags$ul(
          style = "color: var(--brand-ink); line-height: 1.7; padding-left: 20px;",
          tags$li(tags$strong("By lighting condition. "), "Daylight vs. dark-time crashes, to test whether the effect concentrates in low-visibility conditions where daylighting theoretically matters most."),
          tags$li(tags$strong("By crash severity. "), "Fatal, suspected serious injury, suspected minor injury, and possible injury/complaint of pain, to test whether the policy affects crash frequency broadly or is concentrated in specific outcomes.")
        ),
        p(
          "Each subgroup uses the same specification and cutoff dates as the primary model, applied to a filtered outcome variable. Full results for each subgroup are reported on the Results page.",
          style = "color: var(--brand-ink); line-height: 1.7;"
        )
      ),
    ),
    nav_panel(
      title = "Tobit Model (Grid Analysis)",
      value = "tobit",
      div(
        class = "document-card",
        div(class = "document-card-title", "Tobit Model for Spatial Grid Analysis"),
        p("Placeholder for Tobit model formula and description.", style = "color: var(--brand-muted);")
      )
    )
  )
)


results_page <- div(
  class = "page-content", 
  h1(class = "page-title", "Results"),
  # Placeholder structure — swap in your actual output
  div(
    class = "document-card",
    div(class = "document-card-title", "Statewide RDiT Results"),
    p(
      "The table and figure below report the primary statewide estimates, using our standardized specification (bandwidth h = 12 months, triangular kernel, linear polynomial) at each cutoff.",
      style = "color: var(--brand-ink); line-height: 1.7;"
    ),
    # Results table
    tags$table(
      class = "var-table",
      tags$thead(
        tags$tr(
          tags$th("Cutoff"), tags$th("Estimate (τ)"), tags$th("Std. Error"), tags$th("p-value")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("January 2024 (passage)"), tags$td("−77.6"), tags$td("[SE]"), tags$td("0.021")
        ),
        tags$tr(
          tags$td("January 2025 (enforcement)"), tags$td("−62.1"), tags$td("[SE]"), tags$td("0.023")
        )
      )
    ),
    # RD plot placeholder
    plotOutput("statewide_rd_plot"),
    p(
      "Both estimates use the robust bias-corrected inference approach from Cattaneo, Idrobo & Titiunik, which is the estimator we treat as authoritative for significance testing (see Methodology).",
      style = "color: var(--brand-muted); font-size: 0.9rem; line-height: 1.7;"
    )
  ))

about_page   <- div(
  class = "page-content", 
  h1(class = "page-title", "About Us"), p("Pide Akana Techa Kyle Klemba", 
                                          style = "color: var(--brand-muted);"))


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
        nav_panel(title = "Overview",       value = "overview",     overview_page),
        nav_panel(title = "Literature",       value = "literature", literature_page),
        nav_panel(title = "Maps",           value = "maps",         maps_page),
        nav_panel(title = "Methodology",   value = "methodology", methodology_page),
        nav_panel(title = "Results",        value = "results",      results_page),
        nav_panel(title = "About Us",       value = "about",        about_page)
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
    if (input$victim_type == "Fatalities") {
      data <- data %>% filter(COUNT_PED_KILLED > 0 | COUNT_BICYCLIST_KILLED > 0)
    } else if (input$victim_type == "Injuries") {
      data <- data %>% filter(COUNT_PED_INJURED > 0 | COUNT_BICYCLIST_INJURED > 0)
    }
    if (!is.null(input$mode) && length(input$mode) > 0) {
      ped_selected <- "Pedestrian" %in% input$mode
      bic_selected <- "Bicyclist" %in% input$mode
      if (ped_selected && bic_selected) {
        data <- data %>% filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")
      } else if (ped_selected) {
        data <- data %>% filter(PEDESTRIAN_ACCIDENT == "Y")
      } else if (bic_selected) {
        data <- data %>% filter(BICYCLE_ACCIDENT == "Y")
      }
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
  
  # add debounce to slider so dragging doesn't fire a filter instantly
  date_range_debounced <- reactive({ input$date_range }) %>% debounce(400)
  
  filtered_data_graph <- reactive({ apply_filters(bike_or_ped_acc_all) }) %>%
    bindCache(input$victim_type, input$mode, input$location_type,
              input$collision_severity, input$county, date_range_debounced())
  
  filtered_data_map <- reactive({ apply_filters(bike_or_ped_acc_sf) }) %>%
    bindCache(input$victim_type, input$mode, input$location_type,
              input$collision_severity, input$county, date_range_debounced())
  
  output$cluster_warning <- renderUI({
    data_map <- filtered_data_map()
    req(nrow(data_map) > 50000)
    div(
      style = "font-family: monospace; font-size: 0.75rem; color: #7A5205; background: #FCF1DA; padding: 10px; margin-top: 12px; border: 1px solid #F5E0B7; border-radius:3px;",
      "Clusters only display below 50,000 points. Current selection is ",
      scales::comma(nrow(data_map)),
      " points."
    )
  })
  
  output$range_warning <- renderUI({
    req(input$granularity == "year")
    start_partial <- format(as.Date(input$date_range[1]), "%m") != "01"
    end_partial   <- format(as.Date(input$date_range[2]), "%m") != "12"
    if (start_partial || end_partial) {
      div(
        style = "font-family: monospace; font-size: 0.75rem; color: #7A5205; background: #FCF1DA; padding: 10px; margin-top: 12px; border: 1px solid #F5E0B7; border-radius:3px;",
        "Incomplete year chosen. Output may reflect truncated annual aggregates."
      )
    } else { NULL }
  })
  
  output$main_plot <- renderPlotly({
    # FIX: Changed data() to your actual defined reactive source filtered_data_graph()
    if (nrow(filtered_data_graph()) == 0) { 
      plot_ly() |> 
        layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          annotations = list(text = "No metrics matched selected arguments.", showarrow = FALSE, font = list(family = "monospace", size = 12, color = brand$muted))
        ) |> 
        config(displaylogo = FALSE)
    } else {
      # FIX: Changed data() to filtered_data_graph() here as well
      plot_data <- filtered_data_graph() %>% filter(!is.na(COLLISION_DATE))  
      
      if (input$granularity == "year") {
        summary_data <- plot_data %>%
          group_by(ACCIDENT_PERIOD = year) %>%
          summarise(Total_Accidents = n(), .groups = "drop") %>%
          arrange(ACCIDENT_PERIOD)
        x_title <- "year"
      } else {
        summary_data <- plot_data %>%
          group_by(ACCIDENT_PERIOD = month) %>%
          summarise(Total_Accidents = n(), .groups = "drop") %>%
          arrange(ACCIDENT_PERIOD)
        x_title <- "month"
      }
      
      plot_ly(data = summary_data, x = ~ACCIDENT_PERIOD) %>%
        add_trace(
          y = ~Total_Accidents, 
          type = "bar", 
          name = "Incidents",
          marker = list(color = brand$muted)
        ) %>%
        layout(
          margin = list(t = 10, b = 40, l = 40, r = 10),
          xaxis = list(title = "", showgrid = FALSE, font = list(family = "monospace"), tickfont = list(family = "monospace", size = 10, color = brand$muted)),
          yaxis = list(title = "Incident Count", showgrid = TRUE, gridcolor = brand$border, font = list(family = "monospace"), tickfont = list(family = "monospace", size = 10, color = brand$muted)),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
      }
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        data = ca_boundary,
        fillColor = "transparent", color = brand$ink,
        weight = 1.2, opacity = 0.4
      ) %>%
      addLayersControl(
        overlayGroups = c("Accident Heatmap", "Accident Clusters"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  severity_labels <- c(
    "Fatal Injury" = "1",
    "Serious Injury" = "2",
    "Minor Injury" = "3",
    "Complaint of Pain" = "4"
  )
  
  # Updates dropdown menus safely on initialization
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
  
  # Updates spatial markers on mapping filters change
  observe({
    coords <- filtered_data_map()
    proxy <- leafletProxy("map") %>%
      clearHeatmap() %>%
      clearGroup("Accident Heatmap") %>%
      clearGroup("Accident Clusters") %>%
      clearGlLayers()
    
    if (nrow(coords) > 0) {
      coords_sf <- st_as_sf(coords, coords = c("lng", "lat"), crs = 4326, remove = FALSE)
      proxy <- proxy %>%
        addHeatmap(
          data = coords, lng = ~lng, lat = ~lat,
          blur = 22, max = 0.02, radius = 14,
          gradient = c("0.2" = "#440154", "0.4" = "#3b528b",
                       "0.6" = "#21918c", "0.8" = "#5ec962", "1.0" = "#fde725"),
          
          group = "Accident Heatmap"
        )
      if (nrow(coords) <= 50000) {
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
}
# Run the app
shinyApp(ui, server)
