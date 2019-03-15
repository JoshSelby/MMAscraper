library(tidyverse)
library(rlang)

source('~/GitHub/MMAscraper/Shiny App/MatchPredictor/scriptsForApp.R', echo=TRUE)


filtData <- filtfightsOdds %>%
  select(Link1, Link2, Result2, Method, Date, Event, odds, r1b, r2b, Age1, Age2, ratIncrease1, ratIncrease2) %>% 
  filter(Result2 %in% c("win", "loss")) %>%
  mutate(bet = 10,
         winnings = ifelse(Result2 %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result2=="win", odds*bet/100, -bet),
                                                                  ifelse(Result2=="win", -100/odds * bet, -bet))) %>%
           round(2)
  ) %>%
  arrange(desc(Date))




test <- filtfightsOdds %>% 
  rowwise() %>%
  mutate(winper = line_to_per(odds),
    oddsCheck = paste0("between(odds, ",odds %>% round(0),", ",
                            per_to_line(winper-0.05) %>% round(0),")"),
    ageCheck = paste0("between(Age1-Age2, ",round(Age1-Age2-1.5, 1),", ", round(Age1-Age2+1.5, 1),")"),
    ratCheck = paste0("between(r1b-r2b, ",round(r1b-r2b-50, 0),", ", round(r1b-r2b+50, 0),")"),
    datCheck = paste0("Date < ", "'",Date,"'"),
    ROI = NA,
    count = NA
  ) %>%
  arrange(match_id)


for (i in 1:nrow(test)) {
  data <- filtData %>% 
    filter(eval_tidy(parse_expr(paste(test[i, 61:63] %>% as.character, collapse=" & ")))) %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result2=="win"),
              count = n(),
              winPerSimFight = ifelse(count==0, NA, wins/count),
              ROI = round(sum(winnings)/(mean(bet)*n())*100, 2))
  test$winPerSimFight[i] <- data %>% pull(winPerSimFight)
  test$ROI[i] <- data %>% pull(ROI)
  test$count[i] <- data %>% pull(count)
  print(i)
}



test <- test %>%
  mutate(bet = 10,  winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                                                                             ifelse(Result=="win", -100/odds * bet, -bet))) %>%
           round(2)) %>%
  ungroup()

test %>%
  filter(Date >= '2015-01-01' & count > 25 & Result %in% c("win", "loss")) %>%
  group_by(year(Date), first = ROI<0 & winPerSimFight - line_to_per(odds)  > 0.25 | ROI >0 & winPerSimFight - line_to_per(odds)  > 0.08) %>%
  summarise(
    fights = n(),
    winnings = sum(winnings),
    ROI = round(sum(winnings)/sum(bet)*100,2),
    winper = sum(Result=="win")/n()
  )



