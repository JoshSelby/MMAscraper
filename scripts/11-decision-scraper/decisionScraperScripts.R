library(tidyverse)
library(lubridate)
library(rvest)

# Scrapes all fights from a given Event
eventsScraper <- function(year) {
  yearPage <- read_html(paste0("http://www.mmadecisions.com/decisions-by-event/", year,"/"))
  
  events <- yearPage %>% 
    html_nodes(".list a") %>% 
    html_attr("href") %>% 
    gsub(";.*", "", .)
  
  return(events)
}

# Scrapes all events for a single fighter
fightsScraper <- function(eventLink) {
  eventPage <- read_html(paste0("http://www.mmadecisions.com/", eventLink))
  
  fights <- eventPage %>% 
    html_nodes(".list2 a") %>% 
    html_attr("href") %>%
    gsub(";.*", "", .)
  
  return(fights)
}

winAttr <- function(score1, score2, rounds) {
  fighter1Attr <- (rounds+score1-score2)/(rounds*2)
  return(fighter1Attr)
}

decisionScraper <- function(fightLink) {
  if (!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
  if (!require("httr")) install.packages("httr")
  library(httr)
  if (!require("xml2")) install.packages("xml2")
  library(xml2)
  if (!require("rvest")) install.packages("rvest")
  library(jsonlite)
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  
  fightPage <- read_html(paste0("http://www.mmadecisions.com/", fightLink))
  
  date <- fightPage %>% 
    html_nodes(":nth-child(5) .decision-top2") %>% 
    html_text() %>% 
    gsub("(.*)(\\d{4}-\\d{2}-\\d{2})(.*)", "\\2", .) %>%
    as.Date()
  
  eventLink <- fightPage %>% 
    html_nodes(".decision-top2 a") %>% 
    html_attr("href")
  
  result <- fightPage %>%
    html_nodes(".decision-middle i") %>%
    html_text() %>%
    trimws()
  
  fighter1 <- fightPage %>%
    html_nodes(".decision-top a") %>%
    html_text() %>%
    trimws() %>%
    gsub("\\s", " ", .)
  
  fighter2 <- fightPage %>%
    html_nodes(".decision-bottom a") %>%
    html_text() %>%
    trimws() %>%
    gsub("\\s", " ", .)
  
  fighter1_lname <- fighter1 %>% 
    gsub("([[:print:]]* )(.*)", "\\2", .)
  
  fighter2_lname <- fighter2 %>% 
    gsub("([[:print:]]* )(.*)", "\\2", .)
  
  fighter1Link <- fightPage %>%
    html_nodes(".decision-top a") %>%
    html_attr("href")
  
  fighter2Link <- fightPage %>%
    html_nodes(".decision-bottom a") %>%
    html_attr("href")
  
  mediaDecisions <- fightPage %>%
    html_nodes(".decision .external") %>%
    html_text()
  
  judgeDecisions <- fightPage %>%
    html_nodes("td td td .bottom-cell") %>%
    html_text() %>% 
    matrix(3,2,byrow=T) %>%
    apply(1, paste, collapse = "-")
  
  fanvotes <- fightPage %>%
    html_nodes("#scorecards_submitted b") %>%
    html_text() %>%
    as.numeric()
  
  fanDecisions_tbl <- fightPage %>%
    html_nodes("#scorecard_totals .list, .selectedTop") %>% 
    html_text() %>% 
    matrix(ncol = 3, byrow = T) %>% 
    as.tibble() %>%
    mutate(num = V3 %>% 
             gsub("%", "", .) %>%
             as.numeric() %>%
             '/'(100) %>%
             '*'(fanvotes) %>%
             round(digits = 0),
           V4 = if_else(grepl(paste0(fighter2_lname,".*",fighter1_lname), V1),
                        gsub("(.*)(-)(.*)", "\\3\\2\\1", V2), V2)
           )
  
  fanDecisions <- rep(fanDecisions_tbl$V4, fanDecisions_tbl$num)
  
  decisions <- c(judgeDecisions, mediaDecisions, fanDecisions)
  
  rounds <- fightPage %>% 
    html_nodes("td td td:nth-child(2) .list:nth-child(1)") %>% 
    html_text() %>%
    as.numeric() %>%
    tail(1)
  
  decisionsToScore <- function(decisions, fighter = 1) {
    if(fighter == 1) {
      score <- decisions %>% 
        gsub("-.*", "", .) %>%
        as.numeric()
    }
    else if(fighter == 2){
      score <- decisions %>% 
        gsub(".*-", "", .)%>%
        as.numeric()
    }
  
    return(score)
  }
  
  
  fighter1Scores <- decisionsToScore(decisions, 1)
  fighter2Scores <- decisionsToScore(decisions, 2)
  
  fighter1winAttr <- winAttr(mean(fighter1Scores), mean(fighter2Scores), rounds)
  f1winAttr_media <- winAttr(mean(decisionsToScore(mediaDecisions, 1)),
                             mean(decisionsToScore(mediaDecisions, 2)), rounds)
  f1winAttr_judge <- winAttr(mean(decisionsToScore(judgeDecisions, 1)),
                             mean(decisionsToScore(judgeDecisions, 2)), rounds)
  f1winAttr_fan <- winAttr(mean(decisionsToScore(fanDecisions, 1)),
                           mean(decisionsToScore(fanDecisions, 2)), rounds)
  
  
  return(tibble(date, 
                fighter1, 
                result,
                fighter2, 
                f1Score = mean(fighter1Scores), 
                f2Score = mean(fighter2Scores), 
                rounds, 
                fighter1winAttr, 
                media = length(mediaDecisions), 
                judge = length(judgeDecisions), 
                fan = fanvotes,
                f1winAttr_media,
                f1winAttr_judge,
                f1winAttr_fan,
                fighter1Link, 
                fighter2Link, 
                eventLink,
                fightLink))
    
}


parScrapDecisions <- function(fightsMMAdec, num = 100) {
  NumberOfCluster <- detectCores()
  cl <- NumberOfCluster %>% makeCluster(outfile="log.txt")
  registerDoSNOW(cl)
  
  
  print(system.time({
    pb <- fightsMMAdec %>%
      head(num) %>% 
      length %>%
      txtProgressBar(max = ., style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    decisions2 <<- foreach(fightLink = head(fightsMMAdec, num), 
                             .combine = 'rbind', .multicombine = TRUE, 
                             .maxcombine = 2, .export = c("decisionScraper", "winAttr"), 
                             .options.snow = opts) %dopar% {
                               decisionScraper(fightLink)
                             } 
  }))
  
  stopCluster(cl)
  rm(NumberOfCluster, opts, pb)
  
}


