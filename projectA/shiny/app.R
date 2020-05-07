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
           plotOutput("distPlot"),
           tableOutput("dataset"),
           plotOutput("barchart")
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
    
    tbl0 <- readRDS("for_shiny_0.rds")
    tbl1 <- readRDS("for_shiny_1.rds")
    tbl2 <- readRDS("for_shiny_2.rds")
    tbl3 <- readRDS("for_shiny_3.rds")
    
    # list <- list(tbl0, tbl1, tbl2, tbl3)
    # 
    # matchesOver5 <- vector(mode="numeric")#, length=length(list))
    # 
    # for (i in 1:length(list)) {
    #     
    #     if ((i+1) <= length(list)){
    #         for (j in i+1:length(list)) {
    #             tbl <- list[[i]]
    #             tblNext <- list[[j]]
    #             
    #             matchesOver5[i] <- tbl[tbl$wordCounts > 5, ] %>%
    #                 inner_join(tblNext[tblNext$wordCounts > 5, ], by = "wordNames") %>% 
    #                 nrow() %>% 
    #                 as.numeric()
    #             print("j")
    #             print(j)
    #         }
    #         print("i")
    #         print(i)
    #     }
    # }
    
    
    tbl_01 <- tbl0[tbl0$wordCounts > 5, ] %>%
        inner_join(tbl1[tbl1$wordCounts > 5, ], by = "wordNames") %>% 
        nrow()
    
    tbl_02 <- tbl0[tbl0$wordCounts > 5, ] %>%
        inner_join(tbl2[tbl2$wordCounts > 5, ], by = "wordNames") %>% 
        nrow()
    
    tbl_03 <- tbl0[tbl0$wordCounts > 5, ] %>%
        inner_join(tbl3[tbl3$wordCounts > 5, ], by = "wordNames") %>% 
        nrow()
    
    tbl_12 <- tbl1[tbl1$wordCounts > 5, ] %>%
        inner_join(tbl2[tbl2$wordCounts > 5, ], by = "wordNames") %>% 
        nrow()
    
    tbl_13 <- tbl1[tbl1$wordCounts > 5, ] %>%
        inner_join(tbl3[tbl3$wordCounts > 5, ], by = "wordNames") %>% 
        nrow()
    
    tbl_23 <- tbl2[tbl2$wordCounts > 5, ] %>%
        inner_join(tbl3[tbl3$wordCounts > 5, ], by = "wordNames") %>% 
        nrow()
    
    comparisons <- c("COVID:COVID19",
                    "COVID:COVID-19", 
                    "COVID:COVID_19", 
                    "COVID19:COVID-19", 
                    "COVID19:COVID_19", 
                    "COVID-19:COVID_19")
    numMatchesOver5 <- c(tbl_01, tbl_02, tbl_03, tbl_12, tbl_13, tbl_23)
    
    summaryTbl <- tibble(comparisons, numMatchesOver5)
    
    
    forBar <- tbl0 %>%
        inner_join(tbl1, by = "wordNames") %>% 
        inner_join(tbl2, by = "wordNames") %>% 
        inner_join(tbl3, by = "wordNames") %>% 
        mutate(sum = rowSums(.[2:5])) %>% 
        select(wordNames, sum)
        
    barchart <- forBar %>%
        arrange(desc(sum)) %>%
        top_n(20) %>%
        mutate(wordNames = reorder(wordNames, sum)) %>%
        ggplot(aes(x=wordNames,y=sum)) + geom_col() + coord_flip()
    
    
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
    
    output$dataset <- renderTable({
        tbl <- summaryTbl
    })
    
    output$barchart <- renderPlot({
        barchart
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# https://manav003.shinyapps.io/shiny/