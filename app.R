#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##############################################################
###################### APP COUNTRIES #########################
##############################################################

data <- read.csv("countries of the world.csv")




library(shiny)
library(shinyjs)
library(ggplot2)
library(gridExtra)

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
   ##Here starts the next tab of regions
   
   tabPanel("Regions",
            fluidPage( 
              sidebarLayout(
                sidebarPanel(
              selectInput(inputId="region1",label = "Region 1",choices = unique(data$Region)),
              selectInput(inputId="region2",label="Region 2",choices = unique(data$Region)),
              selectInput(inputId="region3",label="Region 3",choices = unique(data$Region)),
              selectInput(inputId="region4",label="Region 4 ",choices = unique(data$Region))
                ),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotOutput(outputId = "regions")
              ) 
              )
            ) #fluidPage
   ),#tabPanel
   ###########################################################################################
   ###########################################################################################
   ##Here starts the next tab
   
   tabPanel("Classification by countries",
            fluidPage( 
              useShinyjs(),  # Set up shinyjs
              selectInput(inputId="countrySelector1",label = h3("Select country to display"),choices = unique(data$Country)),
              selectInput(inputId="countrySelector2",label=NULL,choices = unique(data$Country)),
              selectInput(inputId="countrySelector3",label=NULL,choices = unique(data$Country)),
              selectInput(inputId="countrySelector4",label=NULL,choices = unique(data$Country)),
              selectInput(inputId="countrySelector5",label=NULL,choices = unique(data$Country)),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotOutput(outputId = "country1")
                #tabsetPanel(id="countryPanel1",
                 #           tabPanel("Countries",plotOutput(outputId = "country1"))
                #)# tabsetPanel
              ) 
            ) #fluidPage
   ),#tabPanel
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
  ##Functions for the second tab: Classification by countries
  output$country1 <- renderPlot({
    
    
    countriesSelected<-(data$Country == input$countrySelector1)|(data$Country == input$countrySelector2)|
      (data$Country == input$countrySelector3)|(data$Country == input$countrySelector4)|(data$Country == input$countrySelector5)
    print(colnames(data))
    plot1<-ggplot(data[countriesSelected, ],
                  aes(x=Country, y=GDP)) +geom_bar(stat = "identity")
    
    plot2<-ggplot(data[countriesSelected, ],
                  aes(x=Country, y=Population)) +geom_bar(stat = "identity")
    
    plot3 <- ggplot(data[countriesSelected, ],
                    aes(x=Country, y=Literacy)) +geom_bar(stat = "identity")
    plot4 <- ggplot(data[countriesSelected, ],
                    aes(x=Country, y=NetMigration)) +geom_bar(stat = "identity")
    grid.arrange(plot1,plot2,plot3,plot4, nrow = 2, ncol=2)
    
  })
  
  
  
  
  ##Functions for the second tab ends
  ###########################################################################################
  
}

# Run the application 
shinyApp(ui = ui, server = server)

