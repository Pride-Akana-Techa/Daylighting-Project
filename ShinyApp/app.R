# Packages and Data Setup -------------------------------------------------

# add monthly totals to graph
# add warning if dates are giving incomplete year range for yearly totals graph


# load packages
library(shiny)
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

# Theme Setup using the bslib package
app_theme <- bs_theme(
  version      = 5,
  bg           = "white",
  fg           = "black",
  primary      = "navy",
  base_font    = font_google("Inter"),
  heading_font = font_google("DM Sans")
)

# Load data

# OSM Pedestrian Crossings
crossings_cal <- readRDS(here("initial-analysis", "data-raw", "OSM_california_crossings.rds"))

# California state boundary
ca_boundary <- readRDS(here("initial-analysis", "data-clean", "ca_boundary.rds"))

# Bike or ped crash data 
bike_or_ped_acc_all <- readRDS(here("initial-analysis", "data-raw", "TIMS_bike_ped_all.rds"))


# Bike or ped geolocated crash data only
bike_or_ped_acc_sf <- readRDS(here("initial-analysis", "data-raw", "TIMS_bike_ped_geo.rds"))

# Data preparation: ensure COLLISION_DATE is in Date format for both datasets
bike_or_ped_acc_all <- bike_or_ped_acc_all %>%
  mutate(
    COLLISION_DATE = as.Date(COLLISION_DATE)
  )

bike_or_ped_acc_sf <- bike_or_ped_acc_sf %>%
  mutate(
    COLLISION_DATE = as.Date(COLLISION_DATE)
  )


# Shiny App ---------------------------------------------------------------

# UI
ui <- page_navbar(
  title = div(
    style = "margin-right: 25px;",
    "California Assembly Bill 413: Daylighting Law",
    div(
      "Investigating the Impacts of Daylighting on Pedestrian and Bicyclist Safety",
      style = "font-size: 13px;"
    ),
    tags$img(
      src = "Virginia-Tech-Logo.png",
      height = "40px",
      style = "position: absolute; top: 8px; right: 20px;"
    )
  ),
  theme = app_theme,
  bg = "navy",
  
  nav_panel(
    "Overview",
    
    layout_columns(
      col_widths = c(5, 7),
      gap = "1.2rem",
      
      # Left column
      div(
        card(
          card_header("About the Law"),
          card_body(
            p(
              "On October 10, 2023,",
              tags$a(
                href   = "https://leginfo.legislature.ca.gov/faces/billTextClient.xhtml?bill_id=202320240AB413",
                target = "_blank",
                "California Assembly Bill 413 (AB 413)"
              ),
              "was signed into law. This law prohibits parking within 20 feet of
            the approach side of marked and unmarked crosswalks at 
            intersections, a practice known as", tags$em("daylighting"), ", with
            the aim of:"
            ),
            tags$ul(
              tags$li("improving sightlines at intersections"),
              tags$li("increasing driver awareness of crossing road users, and
                    ultimately"),
              tags$li("reducing pedestrian and bicyclist fatalities.")
            )
          )
        ),
        img(
          src = "SFA-Daylighting.jpg",
          width = "100%",
          height = "50%"
        )
      ),
      
      # Right Column
      div(
        p(tags$strong("Enforcement timeline:")),
        layout_columns(
          col_widths = c(4, 4, 4),
          gap = "0.4rem",
          div(
            style = "background:#FEE2E2; color:#991B1B; border-radius:8px;
                       padding:8px; font-size:0.78rem; text-align:center;",
            tags$strong("Pre-law"), br(),
            tags$small("Before Jan 2024")
          ),
          div(
            style = "background:#FEF3C7; color:#92400E; border-radius:8px;
                       padding:8px; font-size:0.78rem; text-align:center;",
            tags$strong("Warning phase"), br(),
            tags$small("Jan 2024")
          ),
          div(
            style = "background:#D1FAE5; color:#065F46; border-radius:8px;
                       padding:8px; font-size:0.78rem; text-align:center;",
            tags$strong("Citation phase"), br(),
            tags$small("After Jan 2025")
          )
        ), 
        
        ## Interactive Filters
        p(tags$strong("Filters")),
        
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          gap = "1rem",
          
          selectInput(
            inputId = "victim_type",
            label = "Victim Filters",
            choices = c("All Incidents", "Injuries", "Fatalities"),
            selected = "All Incidents"
          ),
          
          checkboxGroupInput(
            inputId = "mode",
            label = "Road User Type",
            choices = c("Pedestrian", "Bicyclist"),
            selected = c("Pedestrian", "Bicyclist")
          ),
          
          selectInput(
            inputId = "location_type",
            label = "Location Filters",
            choices = c("All", "Intersection", "Non-Intersection"),
            selected = "All"
          ),
          
          sliderInput(
            inputId = "date_range",
            label = "Date Filter",
            min   = as.Date("2014-01-01"),
            max   = as.Date("2025-12-31"),
            value = c(as.Date("2014-01-01"), as.Date("2025-12-31")),
            ticks = FALSE,
            timeFormat = "%Y-%m",
            dragRange = TRUE
          )
        ),
        
        # Visualization panels
        navset_card_tab(
          nav_panel(
            "Graph",
            plotlyOutput(outputId = "main_plot", height = "500px")
          ),
          nav_panel(
            "Map",
            leafletOutput("map", height = "500px")
          ),
          
        )
      )
    )
  ),
  
  nav_panel(
    "Before/After Analysis",
    layout_columns(
      col_widths = c(6, 6),
      gap = "1.2rem",
      
      div(
        card(
          card_header("A. Total Yearly Deaths of Pedestrians at Intersections in California"),
          img(src = "yearly_ped_int_death.png", width = "100%")
        ),
        p("This figure shows a drastic decrease in the number of deaths of pedestrians at intersections from January 2024, when the law was enforced. It is also observed that the number of deaths continued to decline after January 2025, when enforcement moved from warning based to citation based."
        )
      ),
      
      div(
        card(
          card_header("B. Total Monthly Deaths of Pedestrians at Intersections in California"),
          img(src = "monthly_ped_int_death.png", width = "100%")
        ),
        p(
          "In the monthly trend, the total number of deaths fluctuates throughout each year due to seasonal changes, but overall, there is a sharp decrease from the start of 2014 to the end of 2025."  
        )
      )
    )
  ),
  
  nav_panel(
    "Maps"
  ),
  nav_panel(
    "Results"
  ),
  nav_panel(
    "About Us"
  ),
  nav_spacer()
)


# Server
server <- function(input, output, session) {
  
  
  apply_filters <- function(data) {
    
    # Filter by victim type
    if (input$victim_type == "Fatalities") {
      data <- data %>%
        filter(COUNT_PED_KILLED > 0 | COUNT_BICYCLIST_KILLED > 0)
    } else if (input$victim_type == "Injuries") {
      data <- data %>%
        filter(COUNT_PED_INJURED > 0 | COUNT_BICYCLIST_INJURED > 0)
    }
    
    # Filter by road user
    if (!is.null(input$mode) && length(input$mode) > 0) {
      ped_selected <- "Pedestrian" %in% input$mode
      bic_selected <- "Bicyclist" %in% input$mode
      
      if (ped_selected && bic_selected) {
        data <- data %>%
          filter(PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")
      } 
      else if (ped_selected) {
        # Only pedestrian selected
        data <- data %>%
          filter(PEDESTRIAN_ACCIDENT == "Y")
      } 
      else if (bic_selected) {
        # Only bicyclist selected
        data <- data %>%
          filter(BICYCLE_ACCIDENT == "Y")
      }
    } else {
      data <- data %>%
        filter(FALSE)
    }
    
    # Filter by location type
    if (input$location_type == "Intersection") {
      data <- data %>%
        filter(INTERSECTION == "Y")
    } else if (input$location_type == "Non-Intersection") {
      data <- data %>%
        filter(INTERSECTION == "N")
    }
    
    # Filter by date
    if (!is.na(input$date_range[1]) && !is.na(input$date_range[2])) {
      data <- data %>%
        filter(
          !is.na(COLLISION_DATE),
          COLLISION_DATE >= input$date_range[1],
          COLLISION_DATE <= input$date_range[2]
        )
    }
    
    return(data)
  }
  
  # Reactive filtering for graph
  filtered_data_graph <- reactive({
    apply_filters(bike_or_ped_acc_all)
  })
  
  # Reactive filtering for map
  filtered_data_map <- reactive({
    apply_filters(bike_or_ped_acc_sf)
  })
  
  # Reactive filtering coords for map
  filtered_coords <- reactive({
    data <- filtered_data_map()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    coords_matrix <- st_coordinates(data)
    
    data %>%
      st_drop_geometry() %>%
      mutate(
        lng = coords_matrix[, 1],
        lat = coords_matrix[, 2]
      )
  })
  
  # Main plot output
  output$main_plot <- renderPlotly({
    data <- filtered_data_graph()  
    
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(
          title = "No Data Matches Selected Filters",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Count"),
          annotations = list(
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            font = list(size = 12)
          )
        )
    } 
    else {
      summary_data <- data %>%
        {if ("geometry" %in% names(.)) st_drop_geometry(.) else .} %>%  
        mutate(ACCIDENT_YEAR = lubridate::year(COLLISION_DATE)) %>%
        filter(!is.na(ACCIDENT_YEAR)) %>%
        group_by(ACCIDENT_YEAR) %>%
        summarise(
          Total_Accidents = n(),
          Fatalities = sum(COUNT_PED_KILLED, na.rm = TRUE) + sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE),
          Injuries = sum(COUNT_PED_INJURED, na.rm = TRUE) + sum(COUNT_BICYCLIST_INJURED, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(ACCIDENT_YEAR)
      
      if (nrow(summary_data) == 0) {
        plot_ly() %>%
          layout(
            title = "Unable to process data - check date format",
            annotations = list(
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5
            )
          )
      } 
      else {
        plot_ly(data = summary_data, x = ~ACCIDENT_YEAR) %>%
          add_trace(
            y = ~Total_Accidents, 
            type = "bar", 
            name = "Total Accidents",
            marker = list(color = "steelblue")
          ) %>%
          layout(
            title = paste("Accident Data Summary by Year"),
            xaxis = list(title = "Year"),
            yaxis = list(title = "Total"),
            hovermode = "x unified",
            plot_bgcolor = "rgba(240,240,240,0.5)"
          )
      }
    }
  })
  
  # Map output
  output$map <- renderLeaflet({
    data <- filtered_data_map()  # CHANGED: explicitly using map dataset
    coords <- filtered_coords()
    
    # Base map
    map <- leaflet(
      options = leafletOptions(
        attributionControl = FALSE
      )
    ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = ca_boundary,
        fillColor = "lightgreen",
        fillOpacity = 0.2,
        color = "black",
        weight = 2,
        group = "California"
      ) %>%
      addGlPoints(
        data = crossings_cal,
        group = "Crossings",
        opacity = 0.3,
        radius = 6,
        fillColor = "black"
      )
    
    if (!is.null(coords) && nrow(coords) > 0) {
      map <- map %>%
        addHeatmap(
          data = coords,
          lng = ~lng,
          lat = ~lat,
          blur = 20,
          max = 0.05,
          radius = 15,
          gradient = c(
            "0.0" = "blue",
            "0.3" = "cyan",
            "0.6" = "yellow",
            "0.8" = "orange",
            "1.0" = "red"
          ),
          group = "Accident Heatmap"
        ) %>%
        addCircleMarkers(
          data = coords,
          lng = ~lng,
          lat = ~lat,
          radius = 3,
          stroke = FALSE,
          fillOpacity = 0.6,
          group = "Accident Clusters",
          clusterOptions = markerClusterOptions()
        )
    }
    
    map %>%
      addLayersControl(
        overlayGroups = c(
          "California",
          "Crossings",
          "Accident Heatmap",
          "Accident Clusters"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      )
  })
}

# Run the app
shinyApp(ui, server)