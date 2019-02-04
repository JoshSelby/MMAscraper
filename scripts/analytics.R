library(tidyverse)

filtfightsOdds <- readRDS("./scripts/8-append-odds/data/filtfightsOdds.RDS")
fightMetricsEvents <- readRDS("./scripts/5-metrics/data/fightMetricsEvent.rds")
futureFights <- readRDS("./scripts/10-future-fights/data/futureFights.RDS")

test <- filtfightsOdds %>% 
  filter(Date > "2012-01-01" & Age1 <27 & Age2 > 33 & r1b < r2b & odds < 0 & Age2-Age1 < 8) %>%
  select(Link1, Link2, Result, Method, Date, Event, odds, r1b, r2b, Age1, Age2, highestWin1_5, highestWin2_5) %>% 
  mutate(bet = 10,
         winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                           ifelse(Result=="win", -100/odds * bet, -bet)))
         )

test %>%
  summarise(avgWin = sum(winnings)/n(),
            wins = sum(Result=="win"),
            count = n(),
            winPer = paste0(round(wins/count * 100,2),"%"),
            bet = mean(bet),
            ROI = paste0(round(sum(winnings)/(bet*n())*100, 2), "%"))

test %>% View()
