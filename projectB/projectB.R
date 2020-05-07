# R Studio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(rvest)
library(httr)

# Data Import and Cleaning
#papers3 <- read_html("https://scholar.google.com/scholar?start=20&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,48&as_vis=1")
  
  #"https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=%22covid-19%22+source%3Apsychology&btnG=")



## READ ALL PAPERS IN
allPapers <- list()

for (i in 1:24) { 
  
  j <- (i - 1)*10
  link <- paste0("https://scholar.google.com/scholar?start=", j, "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,48&as_vis=1")
  
  allPapers[[i]] <- read_html(link)
  
  Sys.sleep(5)
  
  print(link)
}


## READ ALL TITLES IN, ALL LINKS IN 

allTitleText <- c()
#allTitleText <- c(allTitleText, c(9))

allLinksText <- c()

allInfoText <- c()

for (i in 1:length(allPapers)){
  allTitleNodes <- html_nodes(allPapers[[i]], ".gs_rt a")
  allTitleText <- c(allTitleText, html_text(allTitleNodes))

  #allLinkNodes <- html_nodes(allPapers[[i]], ".gs_rt a")
  allLinksText <- c(allLinksText, html_attr(allTitleNodes, "href"))
  
  infoNodes <- html_nodes(allPapers[[i]], ".gs_a")
  allInfoText <- c(allInfoText, html_text(infoNodes))
  
  print(i)
}


#The instructions ask for 4 columns, but it's asking for 5 different pieces of data (article titles, author lists, journal title, year and link to each article), so I'm assuming you actually want 5 columns


split <- str_split(allInfoText, "-", 3)

allAuthorsText <- c()
journalYear <- c()


for (i in 1:length(split)) {
  allAuthorsText[i] <- split[[i]][1]
  journalYear[i] <- split[[i]][2]
}

journalName <- c()
year <- c()

for (i in 1:length(journalYear)) {
  if (str_detect(journalYear[i], pattern = ", [0-9]{4}", negate = FALSE)) { #if both year and journal there
    tempSplit <- str_split(journalYear[i], ", [0-9]{4}")
    journalName[i] <- tempSplit[[1]][1]
    
    year[i] <- str_extract(journalYear[i], "([0-9]{4})")
    
     
    #year[i] <- tempSplit[[1]][2]
  } else {
    if(str_detect(journalYear[i], pattern = "[0-9]{4}", negate = FALSE)) { #if only year
      year[i] <- journalYear[i]
    } else { #if only journal
      journalName[i] <- journalYear[i]
    }
  }

}


df <- tibble("ArticleTitle" = allTitleText, "AuthorList" = allAuthorsText, "JournalTitle" = journalName, "Year" = year, "Link" = allLinksText)

# linkNodes <- html_nodes(allPapers[[24]], ".gs_rt a")
# linkText <- html_attr(linkNodes, "href")


#allPapers <- list(papers, papers2)
#allPapers[[3]] <- papers3
# str(allPapers)
# str(comb)
# 
# str(papers2)


#there are 240 results, 10 per page

# Visualization

topJournals <- df %>%
  group_by(JournalTitle) %>% 
  count() %>% 
  drop_na() %>% 
  arrange(desc(n))
  # top_n(10)

topJournals <- topJournals[1:10,]

plot <- df %>% 
  right_join(topJournals, by = "JournalTitle") %>% 
  mutate(Year = as.numeric(Year)) %>%
  select(Year) %>% 
  #select(-n) %>% 
  count(Year) %>% 
  ggplot(aes(x = Year, y = n)) + geom_point()

plot