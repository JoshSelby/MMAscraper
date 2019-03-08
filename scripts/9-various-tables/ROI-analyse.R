library(tidyverse)
library(rlang)

filtfightsOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/filtfightsOdds.rds")
line_to_per <- function(x) {
  return(if_else(x < 0,
                 -x/((-x + 100)),
                 100/(x+100)))
}

per_to_line <- function(x) {
  return(if_else(x <= 0.5,
                 (1-x)/x * 100,
                 x/(1-x) * -100))
}

odds_to_return <- function(x, bet=10) {
  return(if_else(x < 0, 
                 bet/-x*100,
                 x*bet/100))
}

filtData <- filtfightsOdds %>%
  select(Link1, Link2, Result, Method, score, Date, Event, odds, r1b, r2b, Age1, Age2, ratIncrease1, ratIncrease2) %>%
  mutate(bet = 100/odds_to_return(odds,10),
         winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                                                                  ifelse(Result=="win", -100/odds * bet, -bet))) %>%
           round(2)
  ) %>%
  arrange(desc(Date))




test <- filtfightsOdds %>% 
  rowwise() %>%
  mutate(winper = line_to_per(odds),
    oddsCheck = paste0("between(odds, ",per_to_line(winper+0.05) %>% round(0),", ",
                            per_to_line(winper-0.05) %>% round(0),")"),
    ageCheck = paste0("between(Age1-Age2, ",round(Age1-Age2-2, 1),", ", round(Age1-Age2+2, 1),")"),
    ratCheck = paste0("between(r1b-r2b, ",round(r1b-r2b-50, 0),", ", round(r1b-r2b+50, 0),")"),
    datCheck = paste0("Date < ", "'",Date,"'"),
    ROI = NA,
    count = NA
  )


for (i in 1:nrow(test)) {
  data <- filtData %>% 
    filter(eval_tidy(parse_expr(paste(test[i, 61:63] %>% as.character, collapse=" & ")))) %>%
    summarise(wins = sum(Result == "win"),
              count = sum(!is.na(score)),
              winPer = wins/count,
              totalBet = sum(bet * !is.na(score)),
              totalWinnings = sum(winnings),
              ROI = round(totalWinnings/totalBet*100,2))
  test$ROI[i] <- data %>% pull(ROI)
  test$count[i] <- data %>% pull(count)
  test$winper[i] <- data %>% pull(winPer)
  print(i)
}



test <- test %>%
  mutate(bet = 100/odds_to_return(odds,10),
         winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                                                                             ifelse(Result=="win", -100/odds * bet, -bet))) %>%
           round(2)) %>%
  ungroup() %>%
  group_by(match_id) %>%
  mutate(higherROI = max(ROI) == ROI)

test %>%
  filter(Date>='2015-01-01' & count > 25 & Result %in% c("win", "loss")) %>%
  group_by(line_to_per(odds)+0.1 < winper, ROI2 = ROI %>% cut(breaks=c(-100,3,5,10,15,20,25,30,500))) %>%
  summarise(
    fights = n(),
    wins = sum(Result=="win"),
    onlyWins = sum(winnings * (Result == "win")),
    onlyLosses = sum(winnings * (Result == "loss")),
    winnings = sum(winnings),
    totBet = sum(bet),
    ROI = round(sum(winnings)/sum(bet)*100,2),
    winper = sum(Result=="win")/n()
  )
