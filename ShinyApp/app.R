

library(shiny)


# Define UI for application that draws a histogram
ui <- navbarPage("DSPG",
                 tabPanel("Overview",
                          h3("title of your project"),
                          fluidRow(
                            column(4,p("ad ajkfadsf jkdsfkd sfdsaf dsa"),
                                   p("a second paragraph of the test blabalalaldhfjasdf")),
                            column(4,p("text for column2 kdkj afdsjf lkajfk dslfjad slfj ldsa")),
                            column(4,p("text for column3 kjs adljfa skjfkl dsjfd sklfjd sfd s")),
                            img(src="monthly_ped_int_death.png",width="100%")
                          )
                          ),
                 tabPanel("Lit Review"
                          )
                 )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
