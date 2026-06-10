library(shiny)
library(ggplot2)
library(plotly)

ui <- navbarPage(
  title = div(
    "California's Daylighting Law (AB 413)",
    div("Investigating the Impacts of Daylighting on Pedestrian and Bicyclist  Safety", 
        style = "font-size: 13px;"),
    tags$img(
      src = "Virginia-Tech-Logo.png",
      height = "40px",
      style = "position: absolute; top: 8px; right: 20px;"
    )
  ),
  
  tabPanel(
    "Overview",
    
    fluidRow(
      column(
        6,
        
        wellPanel(
          
          h4("About the Law"),
          
          p(
            "On October 10, 2023, ",
            tags$a(
              href = "https://leginfo.legislature.ca.gov/faces/billTextClient.xhtml?bill_id=202320240AB413",
              "California Assembly Bill 413 (AB 413)",
              target = "_blank"
            ),
            " was signed into law."
          ),
          
          p(
            "The statewide law prohibits parking within 20 feet of the approach side of marked and unmarked crosswalks at intersections, a practice commonly referred to as ",
            tags$i("daylighting"),
            "."
          ),
          
          h5("Why Was the Law Enacted?"),
          
          tags$ul(
            tags$li("Improve sight distance at intersections"),
            tags$li("Reduce pedestrian and bicyclist crashes"),
            tags$li("Increase driver awareness of crossing road users")
          )
          
        )
      ),
      
      column(
        6,
        
        div(
          class = "well",
          
          h4("About this Study"),
          
          p(
            "This project investigates the effectiveness of California's daylighting law in improving pedestrian and bicyclist safety at intersections."
          ),
          
          h5("Research Questions"),
          
          tags$ul(
            tags$li("Have pedestrian and bicycle safety changed since AB 413 was implemented?"),
            tags$li("Do impacts vary across counties and regions?"),
            tags$li("Are impacts distributed equitably across demographic groups?"),
            tags$li("Does enforcement intensity influence outcomes?")
          )
        )
      )
    ),
    titlePanel("Interactive Graph"),
    
    sidebarLayout(
      sidebarPanel(
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
    "Before/After Analysis",
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
  ),
  
  tabPanel("Maps"),
  tabPanel("Results"),
  tabPanel("Data Sources"),
  tabPanel("About Us")
)


server <- function(input, output) {
  
  

  
}

shinyApp(ui = ui, server = server)
