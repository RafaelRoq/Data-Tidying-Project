#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

categories_worldMap <- c("Population","PopDens","NetMigration", "InfantMortality","GDP","Literacy",
                  "Birthrate","Deathrate")

# Define UI for application that draws a histogram
ui <- navbarPage("Countries of the World",
   tabPanel("Global map",
      fluidPage( 
        selectInput(
          inputId="worldMapFactor",
          label = h3("Select category to display in the map"),
          choices = categories_worldMap
        ),

      # Show a plot of the generated distribution
              mainPanel(
                tabsetPanel(id="worldMapPanel",
                            tabPanel("World Map", plotOutput("worldMap"), height="560px", width="950px")
                ) #tabsetPanel
              ) #mainPanel
      ) #fluidPage
   ), #tabPanel
   tabPanel("References",
            includeMarkdown("references.md")
   ) #tabPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$worldMap <- renderPlot({
    mapPolys(myWorldMap,nameColumnToPlot = input$worldMapFactor)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

