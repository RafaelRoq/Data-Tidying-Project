#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

source("Countries clean proc.R")

categories_worldMap <- c("Population","PopDens","NetMigration", "InfantMortality","GDP","Literacy",
                  "Birthrate","Deathrate")

# Define UI for application that draws a histogram
ui <- navbarPage("Countries of the World",
                 
                 
   ##First tab panel: Global map
   tabPanel("Global map",
      fluidPage( 
        useShinyjs(),  # Set up shinyjs
        selectInput(
          inputId="worldMapFactor",
          label = h3("Select category to display in the map"),
          choices = categories_worldMap
        ),

      # Show a plot of the generated distribution
              mainPanel(
                
                tabsetPanel(id="worldMapPanel",
                            tabPanel("World Map", plotOutput("worldMap"), height="560px", width="950px")
                ), #tabsetPanel
                
               h3(textOutput("factorExplanation"))
                
              ), #mainPanel
      
      
      actionButton("btn", "What is each factor?")

      
      ) #fluidPage
   ), #tabPanel
   ###########################################################################################
   ##Here starts the next tab
   
   
   ###########################################################################################
   
   ##References tab
   tabPanel("References",
            includeMarkdown("references.md")
   ) #tabPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ###########################################################################################
  
  
  ##Functions for the first tab: World Map
  output$worldMap <- renderPlot({
    mapPolys(myWorldMap,nameColumnToPlot = input$worldMapFactor)
  })
  
  observeEvent(input$btn, {
    # Change the following line for more examples
    output$factorExplanation <- cat(paste("Population: total of inhabitants in each country",
              "PopDens: Inhabitants per square mile",
              "NetMigration: Difference between inmigration and migration", sep="\n"))
    
    toggle("worldMapPanel")
    toggle("worldMap")
  })
  ##Functions for the first tab ends
  ###########################################################################################
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

