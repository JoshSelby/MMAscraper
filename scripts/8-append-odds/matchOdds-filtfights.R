library(data.table)
library(tidyverse)
library(stringr)
library(stringdist)

pastOdds <- readRDS("./scripts/7-scrape-odds/data/pastOdds.RDS")
futureOdds <- readRDS("./scripts/7-scrape-odds/data/futureOdds.RDS")
filtfights <- readRDS(file = "./scripts/5-metrics/data/filtfights.rds")

# Events with duplicate links
dupedEvents <- pastOdds %>% 
  group_by(eventName, eventLink, Date) %>% 
  summarise(num = n()) %>% 
  group_by(eventName, Date) %>% 
  summarise(num = n()) %>% 
  filter(num > 1) %>% 
  pull(eventName)

# Find the link that is incorrect
wrongEvents <- c()
for (i in 1:length(dupedEvents)) {
  wrongEvents[i] <- sapply(pastOdds %>% 
                             filter(eventName == dupedEvents[i]) %>% 
                             pull(eventLink) %>% 
                             unique(), function(x) stringdist(dupedEvents[i], x)) %>%
    which.max() %>%
    names()
}

# delete
pastOdds <- pastOdds %>%
  filter(!(eventLink %in% wrongEvents))

rm(wrongEvents, dupedEvents, i)

# Fix a few fighter's names, create stripped version of name
pastOdds <- pastOdds %>% 
  mutate(fighter = ifelse(fighter == "Jacob-Volkman-1246", "Jacob-Volkmann-3471", fighter),
         opponent = ifelse(opponent == "Jacob-Volkman-1246", "Jacob-Volkmann-3471", opponent),
         fighter = ifelse(fighter == "Luigi-Vandramini-8636", "Luigi-Vendramini-8636", fighter),
         opponent = ifelse(opponent == "Luigi-Vandramini-8636", "Luigi-Vendramini-8636", opponent),
         fighter = ifelse(fighter == "Elizeu-Zaleski-5594", "Elizeu-Zaleski-Dos-Santos-6667", fighter),
         opponent = ifelse(opponent == "Elizeu-Zaleski-5594", "Elizeu-Zaleski-Dos-Santos-6667", opponent),
         fighterLink1s = gsub("(.*)(-\\d*$)", "\\1", fighter) %>% gsub("-", "", .) %>% tolower(),
         fighterLink2s = gsub("(.*)(-\\d*$)", "\\1", opponent) %>% gsub("-", "", .) %>% tolower(),
         rownum = row_number(),
         eventName = gsub(": ", " - ", eventName)) %>%
  filter(!(fighter == "Al-Iaquinta-3221" & opponent == "Paul-Felder-5116") & 
         !(fighter == "Paul-Felder-5116" & opponent == "Al-Iaquinta-3221") &
         !(fighter == "John-Howard-692" & opponent == "Shamil-Gamzatov-6071") & 
         !(fighter == "Shamil-Gamzatov-6071" & opponent == "John-Howard-692") &
           Date >= "2010-01-01") # Since filtfights is also filtered from 2010

filtfights <- filtfights %>%
  mutate(fighter1Name = gsub("(.*)(-\\d*$)", "\\1", Link1) %>% gsub("-", "", .) %>% tolower(),
         fighter2Name = gsub("(.*)(-\\d*$)", "\\1", Link2) %>% gsub("-", "", .) %>% tolower())

# Begin Matching
filtfightsOdds <- tibble()
for (i in 0:2) {
  pastOdds <- pastOdds %>%
    mutate(Date2 = Date - i + 1)
  
  filtfightsOdds <- inner_join(filtfights, pastOdds, by = c("fighter1Name" = "fighterLink1s", "Date" = "Date2")) %>%
    select(colnames(filtfights), '5Dimes', 'rownum') %>%
    rbind(filtfightsOdds)
  filtfightsOdds <- inner_join(filtfights, pastOdds, by = c("fighter2Name" = "fighterLink2s", "Date" = "Date2")) %>%
    select(colnames(filtfights), '5Dimes', 'rownum') %>%
    rbind(filtfightsOdds) %>%
    unique
}

# Fix issues with two Dong Hyun Kim's fighting on UFC Fight Night 79
filtfightsOdds <- filtfightsOdds %>%
  filter(!(Link1 == "Dong-Hyun-Kim-16374" & Date == "2015-11-28" & `5Dimes` != -800 |
         Link1 == "Dominic-Waters-68971" & Date == "2015-11-28" & `5Dimes` != 550 |
         Link1 == "Dong-Hyun-Kim-21673" & Date == "2015-11-28" & `5Dimes` != 110 |
         Link1 == "Dominique-Steele-47845" & Date == "2015-11-28" & `5Dimes` != -130))

# Fights which are still not matched
filtfightsNotMatched <- anti_join(filtfights, filtfightsOdds, by = "match_id")
pastOddsNotMatched <- anti_join(pastOdds, filtfightsOdds, by = "rownum")

filtfightsOdds <- filtfightsOdds %>% 
  select(-rownum)

filtfightsOdds <- left_join(filtfightsOdds, filtfightsOdds %>% select(match_id, Link2, `5Dimes`), 
                            by = c("Link1" = "Link2", "match_id" = "match_id")) %>%
  as.tibble() %>%
  rename(odds5Dimes1 = `5Dimes.x`,
         odds5Dimes2 = `5Dimes.y`)
  

saveRDS(filtfightsOdds, file = "./scripts/8-append-odds/data/filtfightsOdds.rds")
saveRDS(filtfightsOdds, file = "./Shiny App/MatchPredictor/data/filtfightsOdds.rds")

