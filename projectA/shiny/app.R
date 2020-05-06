library(shiny)

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

    library(wordcloud)
    
    output$distPlot <- renderPlot({
        
        
        tbl <- readRDS("for_shiny.rds")
        
        # if (input$hashtag == "#COVID") {
        #     tbl <- tbl
        # }

        wordcloud(tbl$wordNames, tbl$wordCounts, max.words=50)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
