library(tidyverse)
library(rvest)
library(lubridate)
# 12/5/18: Run the oddsScraper loop again on line 172, will take ~1 hour. Need to remake the odds_list.

source('./scripts/7-scrape-odds/oddsScraperScripts.R', echo=TRUE)

odds_list <- list("odds" = tibble(),
                  "searched_links" = c(),
                  "searched_events" = c(),
                  "toSearch" = "Chael-Sonnen-106")

# Scrape all fighters on bestfightodds.com. Takes about an hour.
while(length(odds_list$toSearch) > 0) {
  odds_list <- bind_odds(odds_list, oddsScraper(odds_list$toSearch[1]))
}

saveRDS(odds_list, "./scripts/7-scrape-odds/data/odds_list.RDS")

odds_list <- readRDS("./scripts/7-scrape-odds/data/odds_list.RDS")

pastOdds <- tibble()
for (i in 1:length(odds_list$searched_events)) {
  pastOdds <- rbind(pastOdds, eventOddsScraper(odds_list$searched_events[i]))
  print(i)
}

pastOdds <- pastOdds %>% 
  filter(Date < today()) %>% 
  rename(WilliamH = colnames(pastOdds)[13])


saveRDS(pastOdds, "./scripts/7-scrape-odds/data/pastOdds.RDS")
  




