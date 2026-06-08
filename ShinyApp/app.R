# Load packages
library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application
library(shiny)
library(ggplot2)
library(plotly)

ui <- navbarPage(
  title = div(
    "DSPG-26",
    tags$img(
      src = "Virginia-Tech-Logo.png",
      height = "40px",
      style = "position: absolute; top: 8px; right: 20px;"
    )
  ),
  
  tabPanel(
    "Overview",
    
    fluidRow(
      "On October 10th, 2023, ",
      tags$a(
        href = "https://leginfo.legislature.ca.gov/faces/billTextClient.xhtml?bill_id=202320240AB413",
        "California Assembly Bill 413 (AB-413)",
        target = "_blank"
      ),
      " was signed into law. The state-wide law prohibits parking within 20 feet on the approach side of an intersection. Commonly referred to as ",
      tags$i("daylighting"),
      ". This project aims to investigate the effectiveness of California's daylighting law on improving pedestrian and bicyclist safety at intersections."
    ),
    
    br(),
    
    fluidRow(
      column(
        6,
        p("A. Total Yearly Deaths of Pedestrians at Intersections in California"),
        img(src = "yearly_ped_int_death.png", width = "100%"),
        p("This figure shows a drastic decrease in the number of deaths of pedestrians at intersections from January 2024, when the law was enforced. It is also observed that the number of deaths continued to decline after January 2025, when enforcement moved from warning based to citation based.")
      ),
      
      column(
        6,
        p("B. Total Monthly Deaths of Pedestrians at Intersections in California"),
        img(src = "monthly_ped_int_death.png", width = "100%"),
        p("In the monthly trend, the total number of deaths fluctuates throughout each year due to seasonal changes, but overall, there is a sharp decrease from the start of 2014 to the end of 2025.")
      )
    ),
    
    titlePanel("Interactive Graph"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "victim_type",
          label = "Victim Filters",
          choices = c("Incidents", "Injuries", "Deaths")
        ),
        
        checkboxGroupInput(
          inputId = "mode",
          label = "Road User Type",
          choices = c("All Accidents", "Pedestrian", "Bicycle")
        ),
        
        selectInput(
          inputId = "location_type",
          label = "Location Filters",
          choices = c("All", "Intersections")
        ),
        
        sliderInput(
          inputId = "date_range",
          label = "Date Filter",
          min = as.Date("2014-01-01"),
          max = as.Date("2025-12-31"),
          value = c(as.Date("2014-01-01"), as.Date("2025-12-31")),
          ticks = FALSE,
          timeFormat = "%Y-%m",
          dragRange = TRUE
        )
      ),
      
      mainPanel(
        plotlyOutput(outputId = "main_plot")
      )
    )
  ),
  
  tabPanel(
    "Literature Review",
    fluidPage(
      # you can add content here later
    )
  ),
  
  tabPanel("Maps"),
  tabPanel("Analysis")
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
# Define server logic required 
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
