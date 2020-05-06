# R Studio API
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(twitteR)
library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)

randText <- function(x) {
  lineNum <- sample(1:length(x), 1)
  print(lineNum)
  x[[lineNum]]$content
}

# Data Import and Cleaning

# api <- "rN5sTV6MAEHLVGwLfL2GeVd7J"
# secretKey <- "uM6NZknlicOfcePx5wu0bkYGZ1rne28QyOSnmttH8NW4af86P8"
# token <- "1244697675271815168-V2BZra642W8OxO8BQ3ohj05CCGRjmY"
# secretToken <- "NqueU63g7QSei8LAyij7JwTH4ei5OotYLzyZCLVr1pW9s"
# 
# setup_twitter_oauth(api, secretKey, token, secretToken)
# 
# # Now I am pulling the last 500 tweets for each of the 4 hashtags
# 
# COVID0_tbl <- searchTwitter("#COVID", 500) %>%
#   strip_retweets() %>%
#   twListToDF()
# COVID0_tbl$text <- COVID0_tbl$text %>%
#   iconv("UTF-8", "ASCII", sub="")
# 
# COVID1_tbl <- searchTwitter("#COVID19", 500) %>%
#   strip_retweets() %>%
#   twListToDF()
# COVID1_tbl$text <- COVID1_tbl$text %>%
#   iconv("UTF-8", "ASCII", sub="")
# 
# COVID2_tbl <- searchTwitter("#COVID-19", 500) %>%
#   strip_retweets() %>%
#   twListToDF()
# COVID2_tbl$text <- COVID2_tbl$text %>%
#   iconv("UTF-8", "ASCII", sub="")
# 
# COVID3_tbl <- searchTwitter("#COVID_19", 500) %>%
#   strip_retweets() %>%
#   twListToDF()
# COVID3_tbl$text <- COVID3_tbl$text %>%
#   iconv("UTF-8", "ASCII", sub="")


# write.csv(COVID0_tbl, "temp_output/COVID0_tbl.csv")
# write.csv(COVID1_tbl, "temp_output/COVID1_tbl.csv")
# write.csv(COVID2_tbl, "temp_output/COVID2_tbl.csv")
# write.csv(COVID3_tbl, "temp_output/COVID3_tbl.csv")

COVID0_tbl <- read.csv("temp_output/COVID0_tbl.csv")
COVID1_tbl <- read.csv("temp_output/COVID1_tbl.csv")
COVID2_tbl <- read.csv("temp_output/COVID2_tbl.csv")
COVID3_tbl <- read.csv("temp_output/COVID3_tbl.csv")


covid0CP <- VCorpus(VectorSource(COVID0_tbl$text)) 

covid0CP <- tm_map(covid0CP, PlainTextDocument)

# below line removes all hashtags
covid0CP<-tm_map(covid0CP, content_transformer( (function(x) { 
  str_remove_all( x, pattern = "#+[a-zA-Z0-9(_)]{0,}")
})))


covid0CP<-tm_map(covid0CP, content_transformer(replace_abbreviation))
covid0CP<-tm_map(covid0CP, content_transformer(replace_contraction))
covid0CP<-tm_map(covid0CP, content_transformer(str_to_lower))
covid0CP<-tm_map(covid0CP, removeNumbers)
covid0CP<-tm_map(covid0CP, removePunctuation)

#below line removes all hashtags that begin with or are web URLS
covid0CP<-tm_map(covid0CP, content_transformer( (function(x) {
  str_remove_all( x, pattern = "http[a-zA-Z0-9(_)]{0,}")
})))


# below line removes all stopwords as well as the word 'psychology', as that
#is the hashtag term that was searched for, so that doesn't add meaning to
#our analysis
covid0CP<-tm_map(covid0CP, removeWords, c(stopwords("en"), "covid"))
covid0CP<-tm_map(covid0CP, stripWhitespace)
covid0CP<-tm_map(covid0CP, content_transformer(lemmatize_strings))


#WHAT DOES HE MEAN BY N GRAM???
# myTokenizer <- function(x) {
#   NGramTokenizer(x, Weka_control(min = 1, max = 2))
# }
# 
# twitter_dtm <- DocumentTermMatrix(twitter_cp,
#                                   control = list(tokenize = myTokenizer))


covid0_dtm <- DocumentTermMatrix(covid0CP)
covid0_slimmed_dtm <- removeSparseTerms(covid0_dtm, .99) #sparsity term of 0.99, otherwise I am left with too few terms

tokenCounts <- apply(covid0_dtm, 1, sum)
covid0_dtm <- covid0_dtm[tokenCounts > 0, ]

DTM.matrix <- as.matrix(covid0_dtm)

dropped_tbl <- COVID0_tbl[tokenCounts > 0, ]




COVID0_tbl <- as_tibble(DTM.matrix)


wordCounts <- colSums(COVID0_tbl)
wordNames <- names(COVID0_tbl)
wordcloud(wordNames, wordCounts, max.words=50)


wordcloud_tbl <- tibble(wordNames = names(COVID0_tbl), wordCounts = colSums(COVID0_tbl))
wordcloud(wordcloud_tbl$wordNames, wordcloud_tbl$wordCounts, max.words=50)



saveRDS(wordcloud_tbl, "../shiny/for_shiny.rds")


# Analysis


# Visualization


