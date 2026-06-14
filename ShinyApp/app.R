
# Packages and Data Setup -------------------------------------------------


# load packages
library(shiny)
library(plotly)
library(tidyverse)
library(bslib)

# Theme Setup using the bslib package
app_theme <- bs_theme(
  version      = 5,
  bg           = "white",
  fg           = "black",
  primary      = "navy",
  base_font    = font_google("Inter"),
  heading_font = font_google("DM Sans")
)


# Shiny App ---------------------------------------------------------------

# UI
ui <- page_navbar(
  title = div(
    style = "margin-right: 25px;",
    "California Assembly Bill 413: Daylighting Law",
    div(
      "Investigating the Impacts of Daylighting on Pedestrian and Bicyclist  Safety",
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
        
        ## Interactive Graph
        p(tags$strong("Interactive Graph")),
        
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          gap = "1rem",
          
          selectInput(
            inputId = "victim_type",
            label = "Victim Filters",
            choices = c("All Incidents", "Injuries", "Deaths")
          ),
          
          checkboxGroupInput(
            inputId = "mode",
            label = "Road User Type",
            choices = c("All Accidents", "Pedestrian", "Bicyclist")
          ),
          
          selectInput(
            inputId = "location_type",
            label = "Location Filters",
            choices = c("All", "Intersection", "Non-Intersection")
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
        
        # Plot below
        plotlyOutput(outputId = "main_plot")
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
  
}

# Run the app
shinyApp(ui, server)