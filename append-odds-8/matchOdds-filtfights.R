library(data.table)
library(tidyverse)
library(stringr)

pastOdds <- readRDS("./scrape-odds-7/data/pastOdds.RDS")
futureOdds <- readRDS("./scrape-odds-7/data/futureOdds.RDS")
filtfights <- readRDS(file = "./metrics-5/filtfights.rds")

pastOdds <- pastOdds %>% 
  mutate(fighter = ifelse(fighter == "Jacob-Volkman-1246", "Jacob-Volkmann-3471", fighter),
         opponent = ifelse(opponent == "Jacob-Volkman-1246", "Jacob-Volkmann-3471", opponent),
         fighterLink1s = gsub("(.*)(-\\d*$)", "\\1", fighter) %>% tolower(),
         fighterLink2s = gsub("(.*)(-\\d*$)", "\\1", opponent) %>% tolower()) %>%
  filter(eventLink != "/events/ufc-230-shevchenko-vs-eubanks-1562")

filtfights <- filtfights %>%
  mutate(fighter1Name = gsub("(.*)(-\\d*$)", "\\1", Link1) %>% tolower(),
         fighter2Name = gsub("(.*)(-\\d*$)", "\\1", Link2) %>% tolower())


eventDates <- filtfights$Date %>% unique

test <- left_join(filtfights, pastOdds, by = c("fighter1Name" = "fighterLink1s", "fighter2Name" = "fighterLink2s",
                                       "Date" = "Date"))
