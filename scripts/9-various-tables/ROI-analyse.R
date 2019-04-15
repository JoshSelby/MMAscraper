library(tidyverse)
library(lubridate)
library(rlang)

source('~/GitHub/MMAscraper/Shiny App/MatchPredictor/scriptsForApp.R', echo=TRUE)


filtData <- filtfightsOdds %>%
  select(Link1, Link2, Result2, Method, Date, Event, odds, r1b, r2b, Age1, Age2, ratIncrease1, ratIncrease2, ratIncrease1_3, ratIncrease2_3) %>% 
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
    oddsCheck = paste0("between(odds, ",per_to_line(winper+0.05) %>% round(0),", ",
                            per_to_line(winper-0.05) %>% round(0),")"),
    ageCheck = paste0("between(Age1-Age2, ",round(Age1-Age2-1.5, 1),", ", round(Age1-Age2+1.5, 1),")"),
    ratCheck = paste0("between(r1b-r2b, ",round(r1b-r2b-50, 0),", ", round(r1b-r2b+50, 0),")"),
    dateCheck = paste0("Date < ", "'",Date,"'"),
    trendCheck = paste0("between(ratIncrease1_3-ratIncrease2_3, ",round(ratIncrease1_3-ratIncrease2_3-50, 0),", ", 
                        round(ratIncrease1_3-ratIncrease2_3+50, 0),")")
  ) %>%
  arrange(match_id)

# rating, age diff
for (i in 1:nrow(test)) {
  data <- filtData %>% 
    filter(eval_tidy(parse_expr(paste(test[i, 61:63] %>% as.character, collapse=" & ")))) %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result2=="win"),
              count = n(),
              winPerSimFight = ifelse(count==0, NA, wins/count),
              ROI = round(sum(winnings)/(mean(bet)*n())*100, 2))
  test$winPerSimFight3[i] <- data %>% pull(winPerSimFight)
  test$ROI3[i] <- data %>% pull(ROI)
  test$count3[i] <- data %>% pull(count)
  message(i)
}

# rating, age, odds diff
for (i in 1:nrow(test)) {
  data <- filtData %>% 
    filter(eval_tidy(parse_expr(paste(test[i, 60:63] %>% as.character, collapse=" & ")))) %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result2=="win"),
              count = n(),
              winPerSimFight = ifelse(count==0, NA, wins/count),
              ROI = round(sum(winnings)/(mean(bet)*n())*100, 2))
  test$winPerSimFight4[i] <- data %>% pull(winPerSimFight)
  test$ROI4[i] <- data %>% pull(ROI)
  test$count4[i] <- data %>% pull(count)
  message(i)
}


test <- test %>%
  mutate(bet = 10,  winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                                                                             ifelse(Result=="win", -100/odds * bet, -bet))) %>%
           round(2)) %>%
  ungroup()

test %>%
  filter(Date >= '2015-01-01' & count3 > 50 & Result %in% c("win", "loss")) %>%
  group_by(
    (((ROI3>=3 | ROI4>=3) & odds>0) | (ROI3>=10 & odds<0)) &
    ROI3 > -11) %>%
  summarise(
    fights = n(),
    winnings = sum(winnings),
    ROI = round(sum(winnings)/sum(bet)*100,2),
    winper = sum(Result=="win")/n()
  )

test %>% filter((((ROI3>=3 | ROI4>=3) & odds>0) | (ROI3>=10 & odds<0)) &
                  ROI3 > -11, year(Date) == 2019, count3 > 50) %>% View



############### Find the best future fights to bet on
filtData2 <- futureFights %>%
  select(Link1, Link2, Date, Event, odds, r1b, r2b, Age1, Age2, ratIncrease1, ratIncrease2, ratIncrease1_3, ratIncrease2_3) %>% 
  mutate(bet = 10,
         potWinnings = ifelse(odds>0, odds*bet/100, -100/odds * bet) %>%
           round(2)
  )




test2 <- filtData2 %>% 
  rowwise() %>%
  mutate(winper = line_to_per(odds),
         oddsCheck = paste0("between(odds, ",per_to_line(winper+0.05) %>% round(0),", ",
                            per_to_line(winper-0.05) %>% round(0),")"),
         ageCheck = paste0("between(Age1-Age2, ",round(Age1-Age2-1.5, 1),", ", round(Age1-Age2+1.5, 1),")"),
         ratCheck = paste0("between(r1b-r2b, ",round(r1b-r2b-50, 0),", ", round(r1b-r2b+50, 0),")"),
         trendCheck = paste0("between(ratIncrease1_3-ratIncrease2_3, ",round(ratIncrease1_3-ratIncrease2_3-50, 0),", ", 
                             round(ratIncrease1_3-ratIncrease2_3+50, 0),")")
  ) 

for (i in 1:nrow(test2)) {
  data <- filtData %>% 
    filter(eval_tidy(parse_expr(paste(test2[i, 18:19] %>% as.character, collapse=" & ")))) %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result2=="win"),
              count = n(),
              winPerSimFight = ifelse(count==0, NA, wins/count),
              ROI = round(sum(winnings)/(mean(bet)*n())*100, 2))
  test2$winPerSimFight3[i] <- data %>% pull(winPerSimFight)
  test2$ROI3[i] <- data %>% pull(ROI)
  test2$count3[i] <- data %>% pull(count)
  print(i)
}

for (i in 1:nrow(test2)) {
  data <- filtData %>% 
    filter(eval_tidy(parse_expr(paste(test2[i, 17:19] %>% as.character, collapse=" & ")))) %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result2=="win"),
              count = n(),
              winPerSimFight = ifelse(count==0, NA, wins/count),
              ROI = round(sum(winnings)/(mean(bet)*n())*100, 2))
  test2$winPerSimFight4[i] <- data %>% pull(winPerSimFight)
  test2$ROI4[i] <- data %>% pull(ROI)
  test2$count4[i] <- data %>% pull(count)
  print(i)
}


test2 %>% 
  filter((((ROI3>3 | ROI4>3) & odds>0) |(ROI3>10 & odds<0)) & ROI3 > -11,
         year(Date) == 2019, count3 > 50) %>% 
  select(Link1, Link2, Date, Event, odds, r1b, r2b, Age1, Age2, bet, potWinnings, winper, ROI3, ROI4) %>% 
  View
