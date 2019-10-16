if (!require("foreach")) install.packages("foreach")
library(foreach)
if (!require("doSNOW")) install.packages("doSNOW")
library(doSNOW)
if (!require("parallel")) install.packages("parallel")
library(parallel)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("jsonlite")) install.packages("jsonlite")
library(jsonlite)

fighterAttributeColNames <- readRDS("./scripts/Tapology/1-raw-data/fighterAttributeColNames.RDS")

# Helper function, add columns if they don't exist
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}


# Start our search with Lord Lobov. Can replace this with ANY fighter's Tapology link.
link = "13910-Artem-Lobov"
fighter_id = 13910

# Scrapes a single page and creates list
scrape <- function(link, fighter_id) {
  if (!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
  if (!require("httr")) install.packages("httr")
  library(httr)
  if (!require("xml2")) install.packages("xml2")
  library(xml2)
  if (!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  if (!require("RSQLite")) install.packages("RSQLite")
  library(RSQLite)
  options(scipen=999)
  
  if(link == "216978-claire-france") {
    link <- "214038-claire-thevenon"
    fighter_id <- 214038
  }
    
  
  # Initialize scraper
  searched_links <- tibble(link = character(), fighter_id = numeric())
  toSearch <- tibble(link = character(), fighter_id = numeric())
  
  url_referrer <- paste0("https://www.tapology.com/fightcenter/fighters/", link)
  api_url <- paste0("https://api.tapology.com/v1/internal_fighters/", as.character(fighter_id))
  
  getdata <- GET(url = api_url, 
                 add_headers("Authorization" = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiaW50ZXJuYWxfYXBpIiwiZXhwIjoyNTM3NjU0NDAwfQ.C1E9hhkQOH7XrfZ5c7aTYS4CKN3ACkJ1nvgvx2v10YY",
                             "content-type" = "application/vnd.api+json",
                             "Origin" = "https://www.tapology.com",
                             "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36",
                             "Referer" = url_referrer)
                 )
  
  jsonData <- fromJSON(content(getdata, type="text"), flatten = TRUE)
  
  fights <- as_tibble(jsonData$included, validate=F)
  if(any(grepl("ranking", names(fights)))) {
    fights <- fights %>% 
      unite("ranking", grep("ranking", names(fights)), sep="; ") %>%
      mutate(ranking = gsub("; $", "", gsub("NA; |\\s*NA$|; $", "", ranking)),
             ranking = ifelse(ranking == "", NA, ranking))
  }
  fights$fighter_url <- link
  fights <- fncols(fights, c("ranking", "fighter_url")) %>%
    filter(attributes.event_date < Sys.Date()) %>%
    select(fighter_url, everything()) %>%
    mutate(lastUpdate = as.character(Sys.Date()))
  names(fights) <- gsub("([^\\.]*\\.)(.*)", "\\2", names(fights))

 
  
  jsonData$data$attributes$stats <- NULL
  fighterAttributes <- do.call(cbind, jsonData$data$attributes) %>% as.tibble()
  fighterAttributes$fighter_id <- fighter_id
  fighterAttributes$tapology_url <- link
  if("has_valid_chart_bouts?" %in% names(fighterAttributes)) {
    fighterAttributes <- fighterAttributes %>% 
      rename("has_valid_chart_bouts" = "has_valid_chart_bouts?")
  }
  
  fighterAttributes <- fncols(fighterAttributes, fighterAttributeColNames) %>%
    select(fighterAttributeColNames) %>%
    mutate(short_name = gsub("\\s+", " ", short_name),
           lastUpdate = as.character(Sys.Date()))
  
  
  searched_links <- searched_links %>% 
    add_row(link = link, fighter_id = fighter_id) %>%
    unique()
  
  toSearch <- fights %>% 
    select(opponent_fighter_url, opponent_fighter.data.id) %>% 
    rename(link = opponent_fighter_url, fighter_id = opponent_fighter.data.id) %>%
    mutate(link = gsub("https://www.tapology.com/fightcenter/fighters/", "", link),
           fighter_id = as.numeric(fighter_id)) %>%
    filter(!is.na(link))
    
  
  return(list("fights" = fights,
              "fighterAttributes" = fighterAttributes,
              "searched" = searched_links,
              "toSearch" = toSearch))
}

# Combines the scraped results
bind <- function(a, b) {
  fighter_tbl = rbind(a[[1]], b[[1]]) %>% unique
  fighterAttributes = rbind(a[[2]], b[[2]]) %>% unique
  searched = rbind(a[[3]], b[[3]]) %>% unique
  toSearch = rbind(a[[4]], b[[4]]) %>% 
    unique %>%
    anti_join(searched, by = c("link", "fighter_id"))
  
  return(list("fights" = fighter_tbl, 
              "fighterAttributes" = fighterAttributes, 
              "searched" = searched, 
              "toSearch" = toSearch))
}
  


parScrap <- function(fightsList, num=100) {
  NumberOfCluster <- detectCores()
  cl <- NumberOfCluster %>% makeCluster(outfile="log.txt")
  registerDoSNOW(cl)
  
  
  # Will run until all fighters to scrape are exausted.
  # Scrapes 100 fighters per iteration, taking about 12 seconds each. ~30k/hour
  # Script may stop from timing out. You can simply resume from where you left off.
  fights_list <- fightsList
  
  print(system.time({
    pb <- fights_list[[4]] %>%
      head(num) %>% length %>%
      txtProgressBar(max = ., style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    fights_list2 <<- foreach(link2 = head(pull(fights_list[[4]],link), num), 
                             fighter_id2 = head(pull(fights_list[[4]],fighter_id), num),
                             .combine = 'bind', .multicombine = TRUE, 
                             .maxcombine = 2, .export = c("link", "scrape", "fncols", "fighterAttributeColNames"), 
                             .options.snow = opts) %dopar% {
                               scrape(link2, fighter_id2)
                             } 
  }))
  
  stopCluster(cl)
  rm(NumberOfCluster, opts, pb)
  
}

