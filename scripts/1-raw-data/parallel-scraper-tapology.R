if (!require("foreach")) install.packages("foreach")
library(foreach)
if (!require("doSNOW")) install.packages("doSNOW")
library(doSNOW)
if (!require("parallel")) install.packages("parallel")
library(parallel)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)


# Start our search with Chael P. Sonnen. Can replace this with ANY fighter's Tapology link.
link = "13910-Artem-Lobov"

# Scrapes a single page and creates list
scrape <- function(link) {
  if (!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
  if (!require("rvest")) install.packages("rvest")
  library(rvest)
  if (!require("httr")) install.packages("httr")
  library(httr)
  if (!require("xml2")) install.packages("xml2")
  library(xml2)
  if (!require("RSelenium")) install.packages("RSelenium")
  library(RSelenium)
  
  
  # Initialize scraper
  searched_links <- tibble(link = character(), birthday = date())
  links_to_search <- c()
  
  no_errors = TRUE
  # read the webpage page of the fighter that we are interested in
  fighter_page <- tryCatch({
    link %>%
      paste0("https://www.tapology.com/fightcenter/fighters/", .) %>%
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
  
  # Track Fighter name
  fighter_name <- fighter_page %>%
    # use CSS selector to extract relevant entries from html
    html_nodes("#stats li:nth-child(1) span") %>%
    # turn the html output into simple text fields
    html_text()
  
  # Fighter Nickname
  fighter_nickname <- fighter_page %>% 
    html_nodes("#stats li:nth-child(3) span") %>%
    html_text()
  
  # Fighter Birthday
  fighter_birthday <- fighter_page %>%
    html_nodes("#stats li:nth-child(5) span:nth-child(4)") %>%
    html_text() %>%
    gsub("\\.","-",.) %>%
    as.Date()
  
  # Fighter Height
  fighter_height_cm <- fighter_page %>% 
    html_nodes("#stats li:nth-child(9) span:nth-child(2)") %>%
    html_text() %>%
    gsub("(.*\\()(\\d*)(.*)","\\2",.) %>%
    as.numeric()
  
  fighter_reach_cm <- fighter_page %>% 
    html_nodes("#stats li:nth-child(9) span:nth-child(4)") %>%
    html_text() %>%
    gsub("(.*\\()(\\d*)(.*)","\\2",.) %>%
    as.numeric()
  
  fighter_birthplace <- fighter_page %>%
    html_nodes("#stats li:nth-child(11) span") %>%
    html_text()
  
  fighter_tbl <- tibble(
    link, fighter_name, fighter_birthday, 
    fighter_height_cm, fighter_reach_cm, fighter_birthplace, fighter_nickname
  )
    
  
  # Opposing fighters' links
  fighter_links <- tbl_num %>% 
    paste0("section:nth-child(", ., ") td:nth-child(2) a") %>%
    html_nodes(fighter_page, .) %>%
    html_attr("href")
  
  # Extract fight history from page, wrap text to form a table
  fighter_page_table <- tbl_num %>%
    paste0("section:nth-child(", ., ") td") %>%
    html_nodes(fighter_page, .) %>%
    html_text() %>%
    matrix(ncol = 6, byrow = T)
  
  # Convert table to tibble
  colnames(fighter_page_table) <- fighter_page_table[1,] %>% 
    gsub("\\/", "", .)
  
  
  fighter_tbl <- fighter_page_table %>%
    as.tibble %>%
    .[-1,] %>%
    add_column(Fighter1 = fighter_name, .before = 1) %>%
    # Split Method/Referee columns
    mutate(Referee = tbl_num %>%
             paste0("section:nth-child(", ., ") td:nth-child(4) .sub_line") %>%
             html_nodes(fighter_page, .) %>%
             html_text() %>%
             na.omit(),
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
             ),
           Date = tbl_num %>%
             paste0("section:nth-child(", ., ") td:nth-child(3) .sub_line") %>%
             html_nodes(fighter_page, .) %>%
             html_text() %>%
             as.Date(format="%B / %d / %Y") %>%
             na.omit(),
           Event = tbl_num %>%
             paste0("section:nth-child(", ., ") td:nth-child(3) a") %>%
             html_nodes(fighter_page, .) %>%
             html_text() %>%
             na.omit(),
           # Add both fighters' links
           Link1 = link,
           Link2 = fighter_links
    ) %>%
    select(Fighter1, Result, Fighter2=Fighter, Method, Method_d, R, Time, 
           Referee, Event, Date, Link1, Link2) %>%
    # Remove rows which are already in final table
    subset(!(Link2 %in% searched_links$link))
  
  
  if (no_errors == TRUE) {
    searched_links <- searched_links %>% 
      add_row(link = link, birthday = fighter_birthday) %>% 
      unique
    links_to_search <- fighter_links[fighter_links != "javascript:void();"] %>% unique()
  }
  
  return(list("fights" = fighter_tbl,
              "searched" = searched_links,
              "toSearch" = links_to_search))
  
}

# Combines the scraped results
bind <- function(a, b) {
  fighter_tbl = rbind(a[[1]], b[[1]]) %>% unique
  searched = rbind(a[[2]], b[[2]]) %>% unique
  toSearch = append(a[[3]], b[[3]]) %>% 
    setdiff(searched$link) %>% unique
  
  return(list("fights" = fighter_tbl, "searched" = searched, "toSearch" = toSearch))
}


# Initialize fights table
fights_list <- list("fights" = NULL,
                    "searched" = tibble("searched" = character(), "birthday" = date()),
                    "toSearch" = link)


parScrap <- function(fightsList) {
  NumberOfCluster <- detectCores()
  cl <- NumberOfCluster %>% makeCluster(outfile="log.txt")
  registerDoSNOW(cl)
  
  fights_list <- fightsList
  
  # Will run until all fighters to scrape are exausted.
  # Scrapes 100 fighters per iteration, taking about 12 seconds each. ~30k/hour
  # Script may stop from timing out. You can simply resume from where you left off.
  
  while(fights_list[[3]] %>% length >= 1) {
    print(system.time({
      pb <- fights_list[[3]] %>%
        head(100) %>% length %>%
        txtProgressBar(max = ., style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      
      fights_list2 <<- foreach(link2=head(fights_list[[3]], 100), .combine='bind', .multicombine=TRUE, 
                               .maxcombine=2, .export=c("link", "scrape"), 
                               .options.snow = opts) %dopar% {
                                 scrape(link2)
                               } %>% bind(fights_list, .)
      fights_list <- fights_list2
    }))
    rm(fights_list2)
  }
  
  stopCluster(cl)
  rm(NumberOfCluster, opts, pb)
  
}


