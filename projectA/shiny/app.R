library(shiny)

#setwd("../shiny")

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

server <- function(input, output) {
    
    
    source("pre_shiny.R")

    library(twitteR)
    library(tidyverse)
    library(tm)
    library(qdap)
    library(textstem)
    library(RWeka)

    randText <- function(x) {
        lineNum <- sample(1:length(x), 1)
        print(lineNum)
        x[[lineNum]]$content
    }

    # Data Import and Cleaning

    api <- "rN5sTV6MAEHLVGwLfL2GeVd7J"
    secretKey <- "uM6NZknlicOfcePx5wu0bkYGZ1rne28QyOSnmttH8NW4af86P8"
    token <- "1244697675271815168-V2BZra642W8OxO8BQ3ohj05CCGRjmY"
    secretToken <- "NqueU63g7QSei8LAyij7JwTH4ei5OotYLzyZCLVr1pW9s"

    setup_twitter_oauth(api, secretKey, token, secretToken)

    # Now I am pulling the last 500 tweets for each of the 4 hashtags (to run the app online, please change the code below such that 400 tweets are being pulled for each of the 4 twitter searches)

    COVID0_tbl <- searchTwitter("#COVID", 400) %>%
        strip_retweets() %>%
        twListToDF()
    COVID0_tbl$text <- COVID0_tbl$text %>%
        iconv("UTF-8", "ASCII", sub="")

    COVID1_tbl <- searchTwitter("#COVID19", 400) %>%
        strip_retweets() %>%
        twListToDF()
    COVID1_tbl$text <- COVID1_tbl$text %>%
        iconv("UTF-8", "ASCII", sub="")

    COVID2_tbl <- searchTwitter("#COVID-19", 400) %>%
        strip_retweets() %>%
        twListToDF()
    COVID2_tbl$text <- COVID2_tbl$text %>%
        iconv("UTF-8", "ASCII", sub="")

    COVID3_tbl <- searchTwitter("#COVID_19", 400) %>%
        strip_retweets() %>%
        twListToDF()
    COVID3_tbl$text <- COVID3_tbl$text %>%
        iconv("UTF-8", "ASCII", sub="")


    clean(COVID0_tbl, "_0")
    clean(COVID1_tbl, "_1")
    clean(COVID2_tbl, "_2")
    clean(COVID3_tbl, "_3")
    
    
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

        wordcloud(tbl$wordNames, tbl$wordCounts, max.words=50, scale=c(2,0.25))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# https://manav003.shinyapps.io/shiny/