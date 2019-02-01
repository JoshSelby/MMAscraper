filtfightsOdds <- readRDS("./scripts/8-append-odds/data/filtfightsOdds.RDS")
futureFights <- readRDS("./scripts/10-future-fights/data/futureFights.RDS")

filtfightsOdds %>% 
  select(Link1, Link2, Result, Date, Event, odds, r1b, r2b) %>% 
  mutate(bet = 10,
         winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                           ifelse(Result=="win", -100/odds * bet, -bet)))
         ) %>%
  summarise(avgWin = sum(winnings)/n(),
            wins = sum(Result=="win"),
            count = n(),
            winPer = paste0(round(wins/count * 100,2),"%"),
            bet = mean(bet),
            ROI = paste0(round(sum(winnings)/(bet*n())*100, 2), "%"))



filtfightsOdds %>% 
  filter(r1b-r2b >= 100 & odds > 200)
