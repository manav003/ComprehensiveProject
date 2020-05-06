library(shiny)

setwd("../shiny")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID Hashtag Wordclouds "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("hashtag", "Hashtags", choices = c("#COVID", "#COVID19", "#COVID-19", "#COVID_19"),
                        selected = "COVID")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    #source("../R/pre_shiny.R")
    
    
    output$distPlot <- renderPlot({
        
        library(wordcloud)
        
        tbl <- readRDS("for_shiny_0.rds")
        
        if (input$hashtag == "#COVID19") {
            tbl <- readRDS("for_shiny_0.rds")
        } else if (input$hashtag == "#COVID-19") {
            tbl <- readRDS("for_shiny_2.rds")
        } else if (input$hashtag == "#COVID_19") {
            tbl <- readRDS("for_shiny_3.rds")
        }

        wordcloud(tbl$wordNames, tbl$wordCounts, max.words=50)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# https://manav003.shinyapps.io/shiny/