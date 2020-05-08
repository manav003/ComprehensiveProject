# Analysis (function for cleaning, used in app.R)

#I have decided to create a function to do the cleaning process, as I need to do the same steps for each of the 4 twitter searches
#I have decided to keep this function in a separate R file so that app.R doesn't get too cluttered 

clean <- function(tbl, nameBit) {
  covid0CP <- VCorpus(VectorSource(tbl$text)) 
  
  covid0CP <- tm_map(covid0CP, PlainTextDocument)
  
  # below line removes all hashtags
  covid0CP <- tm_map(covid0CP, content_transformer( (function(x) { 
    str_remove_all( x, pattern = "#+[a-zA-Z0-9(_)]{0,}")
  })))
  
  
  covid0CP<-tm_map(covid0CP, content_transformer(replace_abbreviation))
  covid0CP<-tm_map(covid0CP, content_transformer(replace_contraction))
  covid0CP<-tm_map(covid0CP, content_transformer(str_to_lower))
  covid0CP<-tm_map(covid0CP, removeNumbers)
  covid0CP<-tm_map(covid0CP, removePunctuation)
  
  #below line removes all hashtags that begin with or are web URLS
  covid0CP <- tm_map(covid0CP, content_transformer( (function(x) {
    str_remove_all( x, pattern = "http[a-zA-Z0-9(_)]{0,}")
  })))
  
  
  # below line removes all stopwords as well as the word 'psychology', as that
  #is the hashtag term that was searched for, so that doesn't add meaning to
  #our analysis
  covid0CP <- tm_map(covid0CP, removeWords, c(stopwords("en"), "covid"))
  covid0CP <- tm_map(covid0CP, stripWhitespace)
  covid0CP <- tm_map(covid0CP, content_transformer(lemmatize_strings))
  
  # for N-gram tokenization, I decided to look for just unigrams and bigrams as those are the ones that mostly commonly appear (I went ahead and tried a max of 3, just to see if there may be some common 3 token phrases but, for the analyses here, tri-grams do not show up, so I kept a max of 2)
  myTokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min = 1, max = 2))
  }

  covid0_dtm <- DocumentTermMatrix(covid0CP,
                                    control = list(tokenize = myTokenizer))
  
  
  #covid0_dtm <- DocumentTermMatrix(covid0CP)
  covid0_slimmed_dtm <- removeSparseTerms(covid0_dtm, .99) #sparsity term of 0.99, otherwise I am left with too few terms
  
  
  
  tokenCounts <- apply(covid0_dtm, 1, sum)
  covid0_dtm <- covid0_dtm[tokenCounts > 0, ]
  
  DTM.matrix <- as.matrix(covid0_dtm)
  
  dropped_tbl <- tbl[tokenCounts > 0, ]
  
  tbl <- as_tibble(DTM.matrix)
  
  wordcloud_tbl <- tibble(wordNames = names(tbl), wordCounts = colSums(tbl))
  #wordcloud(wordcloud_tbl$wordNames, wordcloud_tbl$wordCounts, max.words=50)
  
  name <- paste0("../shiny/for_shiny", nameBit, ".rds")
  
  saveRDS(wordcloud_tbl, name)
  
}

