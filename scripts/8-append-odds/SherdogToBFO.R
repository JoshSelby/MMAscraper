library(data.table)
library(tidyverse)
library(stringr)
library(stringdist)

pastOdds <- readRDS("./scripts/7-scrape-odds/data/pastOdds.RDS")
filtfights <- readRDS(file = "./scripts/5-metrics/data/fightMetricsEvent.rds") %>%
  filter(Date >= "2007-06-16")

pastOdds <- pastOdds %>% 
  mutate(odds = coalesce(`5Dimes`, Bovada, `William H.`, SportBet, Pinnacle)) %>%
  select(1:5, odds)

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
         rownum = ceiling(row_number()/2),
         eventName = gsub(": ", " - ", eventName)) %>%
  filter(!(fighter %in% c("Al-Iaquinta-3221", "Paul-Felder-5116") & opponent %in% c("Al-Iaquinta-3221", "Paul-Felder-5116")) & 
         !(fighter %in% c("John-Howard-692", "Shamil-Gamzatov-6071") & opponent %in% c("John-Howard-692", "Shamil-Gamzatov-6071")) & 
         !(fighter %in% c("Bekbulat-Magomedov-6022", "Carl-Deaton-5200") & opponent %in% c("Bekbulat-Magomedov-6022", "Carl-Deaton-5200")) & 
         !(fighter %in% c("Darrick-Minner-4482", "Timur-Valiev-4975") & opponent %in% c("Darrick-Minner-4482", "Timur-Valiev-4975")))

filtfights <- filtfights %>%
  mutate(fighter1Name = gsub("(.*)(-\\d*$)", "\\1", Link1) %>% gsub("-", "", .) %>% tolower(),
         fighter2Name = gsub("(.*)(-\\d*$)", "\\1", Link2) %>% gsub("-", "", .) %>% tolower())


# 1st round of matching by fighter name (lower case, no spaces)
filtfightsOdds <- tibble()
for (i in 0:2) {
  pastOdds <- pastOdds %>%
    mutate(Date2 = Date - i + 1)
  
  filtfightsOdds <- inner_join(filtfights, pastOdds, 
                               by = c("fighter1Name" = "fighterLink1s", "fighter2Name" = "fighterLink2s", "Date" = "Date2")) %>%
    select(colnames(filtfights), 'odds', 'rownum', 'fighter', 'opponent') %>%
    rename(BFO1 = fighter, BFO2 = opponent) %>%
    rbind(filtfightsOdds)
}

# Create Sher to BFO link table
Sherdog_to_BFO <- filtfightsOdds %>% 
  select(Link1, BFO1) %>%
  rename(Sherdog = Link1, BFO = BFO1) %>%
  unique()

filtfightsNotMatched <- anti_join(filtfights, filtfightsOdds, by = "match_id")
pastOddsNotMatched <- anti_join(pastOdds, filtfightsOdds, by = "rownum")


# 2nd Round of Matching using the partial Sherdog_to_BFO table
filtfights2 <- as.tibble()
filtfightsOdds2 <- as.tibble()
for (i in 0:2) {
  pastOddsNotMatched <- pastOddsNotMatched %>%
    mutate(Date2 = Date - i + 1)
  
  filtfights2 <- left_join(filtfightsNotMatched, Sherdog_to_BFO, by= c("Link1" = "Sherdog")) %>%
    rename("BFO1" = "BFO") %>%
    merge((left_join(filtfightsNotMatched, Sherdog_to_BFO, by= c("Link2" = "Sherdog")) %>%
            rename("BFO2" = "BFO"))) %>%
    rbind(filtfights2) %>%
    arrange(match_id) %>%
    distinct()
  
  
  filtfightsOdds2a <- inner_join(filtfights2, pastOddsNotMatched, 
                               by = c("BFO1" = "fighter","Date" = "Date2")) %>%
    select(colnames(filtfights2), 'odds', 'rownum', 'BFO1', 'BFO2')
  
  filtfightsOdds2b <- inner_join(filtfights2, pastOddsNotMatched, 
                                 by = c("BFO2" = "opponent","Date" = "Date2")) %>%
    select(colnames(filtfights2), 'odds', 'rownum', 'BFO1', 'BFO2')
  
  filtfightsOdds2 <- rbind(filtfightsOdds2a, filtfightsOdds2b, filtfightsOdds2)
  
  rm(filtfightsOdds2a, filtfightsOdds2b)
  
}

filtfightsOdds <- filtfightsOdds2 %>%
  filter(!is.na(BFO1) & !is.na(BFO2)) %>%
  rbind(filtfightsOdds)

filtfightsNotMatched <- anti_join(filtfights, filtfightsOdds, by = "match_id")
pastOddsNotMatched <- anti_join(pastOdds, filtfightsOdds, by = "rownum")

rm(filtfightsOdds2, filtfights2)


# 3rd round of matching using stringdist logic
filtfightsOdds2 <- as.tibble()
for (i in 0:2) {
  pastOddsNotMatched <- pastOddsNotMatched %>%
    mutate(Date2 = Date - i + 1)
  
  filtfightsOdds2 <- inner_join(filtfightsNotMatched, pastOddsNotMatched, 
                                by = c("fighter1Name" = "fighterLink1s", "Date" = "Date2")) %>%
    select(colnames(filtfights), 'fighterLink2s', 'odds', 'rownum', 'fighter', 'opponent') %>%
    full_join(
      inner_join(filtfightsNotMatched, pastOddsNotMatched, 
                 by = c("fighter2Name" = "fighterLink2s", "Date" = "Date2")) %>%
      select(colnames(filtfights), 'fighterLink1s', 'odds', 'rownum', 'fighter', 'opponent')) %>%
    rbind(filtfightsOdds2) 
  
}
filtfightsOdds <- filtfightsOdds2 %>% 
  filter(stringdist(fighter2Name, fighterLink2s, method = 'qgram') < 8 |
         stringdist(fighter1Name, fighterLink1s, method = 'qgram') < 8) %>%
  rename(BFO1 = fighter, BFO2 = opponent) %>%
  select(-fighterLink1s, -fighterLink2s) %>%
  rbind(filtfightsOdds)

Sherdog_to_BFO <- filtfightsOdds %>% 
  select(Link1, BFO1) %>%
  rename(Sherdog = Link1, BFO = BFO1) %>%
  rbind(Sherdog_to_BFO) %>%
  unique()

filtfightsNotMatched <- anti_join(filtfights, filtfightsOdds, by = "match_id")
pastOddsNotMatched <- anti_join(pastOdds, filtfightsOdds, by = "rownum")

rm(filtfightsOdds2)



# 4th Round of Matching using the partial Sherdog_to_BFO table
filtfights2 <- as.tibble()
filtfightsOdds2 <- as.tibble()
for (i in 0:2) {
  pastOddsNotMatched <- pastOddsNotMatched %>%
    mutate(Date2 = Date - i + 1)
  
  filtfights2 <- left_join(filtfightsNotMatched, Sherdog_to_BFO, by= c("Link1" = "Sherdog")) %>%
    rename("BFO1" = "BFO") %>%
    merge((left_join(filtfightsNotMatched, Sherdog_to_BFO, by= c("Link2" = "Sherdog")) %>%
             rename("BFO2" = "BFO"))) %>%
    rbind(filtfights2) %>%
    arrange(match_id) %>%
    distinct()
  
  
  filtfightsOdds2a <- inner_join(filtfights2, pastOddsNotMatched, 
                                 by = c("BFO1" = "fighter","Date" = "Date2")) %>%
    select(colnames(filtfights2), 'odds', 'rownum', 'BFO1', 'BFO2')
  
  filtfightsOdds2b <- inner_join(filtfights2, pastOddsNotMatched, 
                                 by = c("BFO2" = "opponent","Date" = "Date2")) %>%
    select(colnames(filtfights2), 'odds', 'rownum', 'BFO1', 'BFO2')
  
  filtfightsOdds2 <- rbind(filtfightsOdds2a, filtfightsOdds2b, filtfightsOdds2)
  
  rm(filtfightsOdds2a, filtfightsOdds2b)
  
}

filtfightsOdds <- filtfightsOdds2 %>%
  filter(!is.na(BFO1) & !is.na(BFO2)) %>%
  group_by(Link1) %>% mutate(BFO1 =  first(BFO1)) %>% 
  group_by(Link2) %>% mutate(BFO2 = first(BFO2)) %>%
  ungroup() %>%
  rbind(filtfightsOdds) %>%
  unique()

filtfightsNotMatched <- anti_join(filtfights, filtfightsOdds, by = "match_id")
pastOddsNotMatched <- anti_join(pastOdds, filtfightsOdds, by = "rownum")

rm(filtfightsOdds2, filtfights2)











##############
Sherdog_to_BFO <- Sherdog_to_BFO %>%
  rbind(c("Yuta-Sasaki-63070", "Ulka-Sasaki-8251"), 
        c("Alexander-Volkanovski-101527", "Alex-Volkanovski-6723"),
        c("Kayla-Harrison-237039","Kayla-Harrison-8276"),
        c("Aaron-Pico-191127", "Aaron-Pico-7170"),
        c("Kyle-Nelson-95383", "Kyle-Nelson-3633"),
        c("Maria-de-Oliveira-Neta-207835", "Maria-Oliveira-7507"),
        c("Sergei-Pavlovich-184051", "Sergey-Pavlovich-7219")
        )








