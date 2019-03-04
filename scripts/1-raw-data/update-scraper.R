library(tidyverse)
library(rvest)
library(data.table)

# Get all fights you already scraped
fights <- readRDS(file = "./scripts/2-clean-data/data/fights_clean.rds")


# What's the most recent Date you have already scraped, minus 10 days
lastDate <- fights %>% 
  arrange(Date) %>% 
  pull(Date) %>% 
  tail(1) %>%
  -10


events_page <- read_html("http://www.sherdog.com/events/")

# fucntion that returns all links from revent events page
recEvents <- function(events) {
  event_Dates <- events %>% 
    html_nodes("#recentfights_tab td:nth-child(1)") %>% 
    html_text() %>% 
    gsub("\n|\t", "",.) %>% 
    trimws() %>%
    .[-1] %>%
    as.Date(format = "%B %d %Y")
  
  event_Names <- events %>% 
    html_nodes("#recentfights_tab td:nth-child(2)") %>%
    html_text() %>%
    paste(events %>% 
            html_nodes("#recentfights_tab td:nth-child(3)") %>%
            html_text(),
          sep = " - ") %>%
    .[-1]
  
  event_Links <- events %>% 
    html_nodes("#recentfights_tab td:nth-child(2) a") %>% 
    html_attr("href")
  
  return(tibble(Dates = event_Dates, Names = event_Names, Links = event_Links))
}

event_tbl <- recEvents(events_page)

# get all events more recent than lastDate
i = 2
while (tail(event_tbl %>% pull(Dates), 1) > lastDate) {
  events_pagei <- read_html(paste0("http://www.sherdog.com/events/recent/",i,"-page"))
  event_tbl <- rbind(event_tbl, recEvents(events_pagei))
  i = i+1
}

# filter events greater than last Date that are not already in our fights table
event_tbl <- event_tbl %>%
  filter(Dates > lastDate)



rm(events_page, events_pagei, i)
####################################################################

fightInfo <- function(link) {
  
  event <- read_html(paste0("http://www.sherdog.com", link))
  noRecords <- event %>% html_nodes("td .final_result") %>% html_text()
  if (!(TRUE %in% (c("win", "draw", "loss") %in% noRecords))) {
    print(paste(i, "Yet to Come"))
    return(tibble())
  } 
  
  fights_tbl <- event %>% html_nodes("td span") %>% html_text(T) %>% matrix(ncol = 5, byrow = T) %>% .[,-4] %>% matrix(ncol=4)
  MethodReferee <- event %>% html_nodes("td:nth-child(5)") %>% html_text() %>% .[-(1:2)]
  R <- event %>% html_nodes("td:nth-child(6)") %>% html_text()
  Time <- event %>% html_nodes("td:nth-child(7)") %>% html_text()
  Links <- event %>% html_nodes("td [itemprop=url]") %>% html_attr("href") %>% 
    matrix(ncol=2, byrow = T) %>% as.tibble()
  
  fights_tbl <- as.tibble(fights_tbl) %>%
    mutate(MethodReferee, R, Time) %>%
    cbind(Links)
  
  # Featured Match
  f3 <- event %>% html_nodes(".event [itemprop=name], .event .final_result") %>% html_text(T) %>% head(-1)
  l4 <- event %>% html_nodes(".event tr td") %>% html_text(T) %>% 
    sub("\\w* ", "", .) %>% .[c(3, 2, 4, 5)]
  l2 <- event %>% html_nodes(".event [itemprop=url]") %>% html_attr("href")
  
  
  fights_tbl <- rbind(fights_tbl, c(f3,l4,l2))
  colnames(fights_tbl)[c(1:4, 8:9)] <- c("Fighter1", "Result", "Fighter2", "Referee", "Link1", "Link2")
  
  fights_tbl <- fights_tbl %>% mutate(Event = event_tbl$Names[match(link, event_tbl$Links)],
                                      Date = event_tbl$Dates[match(link, event_tbl$Links)])
  return(fights_tbl)
}

fights_tbl2 <- tibble()
i=1
for (i in i:length(event_tbl$Links)) {
  fights_tbl2 <- fightInfo(event_tbl$Links[i]) %>% rbind(fights_tbl2)
}


for (i in 1:nrow(fights_tbl2)) {
  fights_tbl2$MethodReferee[i] <- gsub(fights_tbl2$Referee[i], "", 
                                       fights_tbl2$MethodReferee[i])
}  

fights_tbl2 <- fights_tbl2 %>% 
  filter(Result != "yet to come") %>%
  mutate(
    Method = MethodReferee %>%
      sapply(function(x) x %>%
               gsub('\\(.*','', .) %>%
               gsub("N/A", "", .)
      ),
    Method_d = MethodReferee %>% 
      sapply(function(x) x %>%
               gsub('^(.*)\\(','', .) %>%
               gsub('\\)(.*)','', .) %>%
               gsub("N/A", "", .)
      )
  ) %>%
  select(-MethodReferee)


# Featured bout on events page only has Referee's first name,
# so go to the fighter pages to get ref's full name.

source('./scripts/1-raw-data/parallel-scraper.R', echo=TRUE)
fights_tbl2 %>% setDT()
fNameRef <- fights_tbl2[grepl("^\\w*$", Referee),]

for (link in pull(fNameRef, Link1)) {
  fighter_page <- tryCatch({
    link %>%
      paste0("http://www.sherdog.com/fighter/", .) %>%
      read_html()
  }, 
    error=function(cond) {
      message(cond)
      no_errors = FALSE
      return(fighter_page)
    },
    warning=function(cond) {
      message(cond)
      no_errors = FALSE
      return(fighter_page)
    })
  # Delete featured fighters who are only amateurs
  if(fighter_page %>% html_text(trim = TRUE) %>% grepl("Fight History - Pro", .)) {
    fighterFights <- scrape(link)[[1]] %>% setDT() %>% head(1)
    fNameRef[grep(link, Link1), Referee := fighterFights$Referee]
    print(link)
  } else {
    fights_tbl2 <- fights_tbl2[Link1 != link,]
    fNameRef <- fNameRef[Link1 != link,]
  }
}

fights_tbl2[grepl("^\\w*$", Referee), Referee := fNameRef$Referee]

source('./scripts/2-clean-data/cleaner.R', echo=TRUE)
birthdayTable <- readRDS(file = "./scripts/2-clean-data/data/birthdayTable.rds")
fights_tbl2 <- clean(fights_tbl2)
fights <- full_join(fights, fights_tbl2)
fights <- addBD(fights, birthdayTable)
saveRDS(fights, file = "./scripts/2-clean-data/data/fights_clean.rds")

rm(list=ls())
