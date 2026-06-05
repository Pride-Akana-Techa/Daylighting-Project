# Load packages
library(shiny)

# Define UI for application
ui <- navbarPage("DSPG-26",
                 tabPanel("Overview",
                          h3("Daylighting: Assessing the Impacts of California's Assembly Bill 413 on Pedestrian and Bicylist Safety"),
                          fluidRow(
                            column(6,p("This project aims to investigate the effectiveness of California's daylighting law on improving pedestrian and bicyclist safety at intersections.")),
                            column(6,p("The figure below shows the number of pedestrians killed at intersections from 2015 to 2025.")),
                            img(src="monthly_ped_int_death.png",width="100%")
                          )
                 ),
                 tabPanel("Literature Review"),
                 tabPanel("Maps"),
                 tabPanel("Analysis")
)

# Define server logic required 
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
