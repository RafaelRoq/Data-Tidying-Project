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
#setwd("C:/Users/josue/Documents/Data-Tidying-Project/CountriesAppTidy")
#data <- read.csv("countries of the world.csv")
library(shiny)
library(shinyjs)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

library(markdown)
source("Countries clean proc.R")

categories_worldMap <- c("Population","PopDens","NetMigration", "InfantMortality","GDP","Literacy",
                  "Birthrate","Deathrate")

# Define UI for application that draws a histogram
ui <- navbarPage("Countries of the World",
                 
   includeCSS("design.css"),
   ##First tab panel: Global map
   tabPanel("Overview of the data",
     fluidPage( 
       useShinyjs(),  # Set up shinyjs
       
       # Show a plot of the generated distribution
       mainPanel(
        h3("This dataset describes some interesting topics (such as the demography, 
           climate, or economic) of 227 countries")
         )
       ) #mainPanel
     ), #tabPage
   
   
   
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
                actionButton("btn", "What is each variable?"),
                plotOutput("worldMap", height="700px", width="800px"),
                 #tabsetPanel
                hidden( ##Hidden text by default, make it visible with toggle()
                  verbatimTextOutput("factorExplanation")
                )
              ) #mainPanel
      ) #fluidPage
   ), #tabPanel
   
   ###########################################################################################
   ##Here starts the second tab: regions
   
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
   ##Here starts the third tab: Classification by countries
   
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
              ) 
            ) #fluidPage
   ),#tabPanel
   ###########################################################################################
   
   ##References tab
   tabPanel("References",
            includeMarkdown("references.md")
   ), #tabPanel
   
   tabPanel("Case study",
            fluidPage(
              mainPanel(
                textInput("test1",label=NULL)
              )
            )
   ) #tabPanel
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  ###########################################################################################
  
  ##Functions for the first tab: World Map
  output$worldMap <- renderPlot({
    mapPolys(myWorldMap,nameColumnToPlot = input$worldMapFactor)
  })
  
  output$factorExplanation <- renderText({
    paste(
      "Click again in the button in order to show again the map",
      "\n",
      "Population: Total of inhabitants in each country", 
      "PopDens: Population density per square mile",
      "NetMigration: Difference between the number of inmigrats and emigrats",
      "InfantMortality: Infant mortality per 1000 births", 
      "GDP: Gross Domestic Product",
      "Literacy: Percentage of alfabetism",
      "Birthrate: Births per 1000 population in a year",
      "Deathrate: Deaths per 1000 population in a year"
      , sep="\n")
  })
  
  observeEvent(input$btn, {
    # Change the following line for more examples
    toggle("worldMap")
    toggle("factorExplanation")
  })
  ##Functions for the first tab ends
  ###########################################################################################
  ##Functions for the second tab: Classification by countries
  output$country1 <- renderPlot({
    countriesSelected<-(data$Country == input$countrySelector1)|(data$Country == input$countrySelector2)|
      (data$Country == input$countrySelector3)|(data$Country == input$countrySelector4)|(data$Country == input$countrySelector5)
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
  ##Functions for the second tab: Classification by regions
  
  output$regions=renderPlot({
    
    #################Region 1
    
    r1= data %>% filter(Region==input$region1) %>% 
      select(Region,Agriculture,Industry,Service)
    r1=aggregate(cbind(Agriculture,Industry,Service)~Region,
                FUN=sum,data=r1)[,-1]
    r1=data.frame(t(r1))
    colnames(r1)=c("c1")
    
    plot1=ggplot(r1, aes(x="", y=c1, fill=rownames(r1)))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y",start = 0)+
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )+
      theme(axis.text.x=element_blank()) +
      guides(fill=guide_legend(title=paste("Sectors of",input$region1)))
    
    ######################Region 2
    
    r2= data %>% filter(Region==input$region2) %>% 
      select(Region,Agriculture,Industry,Service)
    r2=aggregate(cbind(Agriculture,Industry,Service)~Region,
                 FUN=sum,data=r2)[,-1]
    r2=data.frame(t(r2))
    colnames(r2)=c("c1")
    
    plot2=ggplot(r2, aes(x="", y=c1, fill=rownames(r2)))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y",start = 0)+
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )+
      theme(axis.text.x=element_blank()) +
      guides(fill=guide_legend(title=paste("Sectors of",input$region2)))
    
    ######################Region 3
    
    r3= data %>% filter(Region==input$region3) %>% 
      select(Region,Agriculture,Industry,Service)
    r3=aggregate(cbind(Agriculture,Industry,Service)~Region,
                 FUN=sum,data=r3)[,-1]
    r3=data.frame(t(r3))
    colnames(r3)=c("c1")
    
    plot3=ggplot(r3, aes(x="", y=c1, fill=rownames(r3)))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y",start = 0)+
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )+
      theme(axis.text.x=element_blank()) +
      guides(fill=guide_legend(title=paste("Sectors of",input$region3)))
    
    ######################Region 4
    
    r4= data %>% filter(Region==input$region4) %>% 
      select(Region,Agriculture,Industry,Service)
    r4=aggregate(cbind(Agriculture,Industry,Service)~Region,
                 FUN=sum,data=r4)[,-1]
    r4=data.frame(t(r4))
    colnames(r4)=c("c1")
    
    plot4=ggplot(r4, aes(x="", y=c1, fill=rownames(r4)))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y",start = 0)+
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )+
      theme(axis.text.x=element_blank()) +
      guides(fill=guide_legend(title=paste("Sectors of",input$region4)))
    
    grid.arrange(plot1,plot2,plot3,plot4, nrow = 2, ncol=2)
    
    
    
    
  })


  ##Functions for the second tab ends
  ###########################################################################################
  
  
  ###########################################################################################
  ##Functions for the third tab: Classification by countries
  output$country1 <- renderPlot({
    
    
    countriesSelected<-(data$Country == input$countrySelector1)|(data$Country == input$countrySelector2)|
      (data$Country == input$countrySelector3)|(data$Country == input$countrySelector4)|(data$Country == input$countrySelector5)
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
  ###########################################################################################
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

