library(tidyverse)
library(rlang)

filtData <- filtfightsOdds %>%
  select(Link1, Link2, Result, Method, Date, Event, odds, r1b, r2b, Age1, Age2, ratIncrease1, ratIncrease2) %>% 
  mutate(bet = 10,
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
    filter(eval_tidy(parse_expr(paste(test[i, 57:60] %>% as.character, collapse=" & ")))) %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result=="win"),
              count = n(),
              winPer = paste0(round(wins/count * 100,2),"%"),
              bet = mean(bet),
              ROI = round(sum(winnings)/(bet*n())*100, 2))
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
  filter(Date>='2017-01-01' & count > 25 & Result %in% c("win", "loss")) %>%
  group_by(ROI2 = ROI %>% cut(breaks=c(-500,-100,0,5,10,15,20,25,30,35,40,45,50,100,500)), odds>0) %>%
  summarise(
    fights = n(),
    winnings = sum(winnings),
    ROI = paste0(round(sum(winnings)/sum(bet)*100,2), "%"),
    winper = sum(Result=="win")/n()
    
  )



