# Load packages
library(shiny)

# Define UI for application
ui <- navbarPage("DSPG-26",
                 tabPanel("Overview",
                          h3("Daylighting: Assessing the Impacts of California's Assembly Bill 413 on Pedestrian and Bicyclist Safety"),
                          fluidRow(
                            p("This project aims to investigate the effectiveness of California's daylighting law on improving pedestrian and bicyclist safety at intersections."),
                            column(6,p("A. Total Yearly Deaths of Pedestrians at Intersections in California"),
                                   img(src="yearly_ped_int_death.png",width="100%"),
                                   p("This figure shows a drastic decrease in the number of deaths of pedestrians at intersections from January 2024, when the law was enforced. It is also observed that the number of deaths continued to decline after January 2025, when enforcement moved from warning based to citation based.")),
                            column(6,p("B. Total Monthly Deaths of Pedestrians at Intersections in California"),
                                   img(src="monthly_ped_int_death.png",width="100%"),
                                   p("In the monthly trend, the total number of deaths fluctuates throughout each year due to seasonal changes, but overall, there is a sharp decrease from the start of 2014 to the end of 2025."))

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
