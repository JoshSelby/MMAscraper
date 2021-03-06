library(tidyverse)
library(stringr)
library(lubridate)
library(stringdist)
library(data.table)
library(zoo)
library(RcppRoll)

fightersTable <- readRDS("./scripts/9-various-tables/data/fightersTable.rds")
fightMetricsEvent <- readRDS("./scripts/5-metrics/data/fightMetricsEvent.rds")
filtfightsOdds <- readRDS("./scripts/8-append-odds/data/filtfightsOdds.RDS")
Sherdog_to_BFO <- readRDS("./scripts/8-append-odds/data/Sherdog_to_BFO.RDS")
futureOdds <- readRDS("./scripts/7-scrape-odds/data/futureOdds.RDS")

futureOdds <- futureOdds %>% 
  filter((opponent == "John-Makdessi-2163" & fighter == "Jesus-Pinedo-8714") |
           (fighter == "John-Makdessi-2163" & opponent == "Jesus-Pinedo-8714") |
           !grepl("John-Makdessi|Jesus-Pinedo",paste(fighter,opponent)))


futureFights <- futureOdds %>%
  left_join(Sherdog_to_BFO, by=c("fighter" = "BFO")) %>%
  left_join(Sherdog_to_BFO, by=c("opponent" = "BFO"))

availFighters <- fightersTable %>%
  pull(Link)

noSherdog <- futureFights %>%
  select(fighter, Sherdog.x) %>%
  filter(is.na(Sherdog.x)) %>%
  pull(fighter)

i=1
minitbl <- tibble()
if (length(noSherdog)!=0) {
  for (i in 1:length(noSherdog)) {
    minitbl <- tibble(Sherdog = availFighters, BFO = noSherdog[i]) %>%
      mutate(availStrip = Sherdog %>% gsub("(.*)(-\\d*$)", "\\1", .) %>% gsub("-", "", .) %>% tolower(),
             noSherstrip = BFO %>% gsub("(.*)(-\\d*$)", "\\1", .) %>% gsub("-", "", .) %>% tolower(),
             dist = stringdist(availStrip, noSherstrip, method = 'qgram')) %>%
      arrange(dist) %>%
      filter(dist == 0) %>%
      select(Sherdog, BFO) %>%
      rbind(minitbl)
  }
  # Not a perfect match
  noSherdog %>% setdiff(minitbl$BFO)

  #Manually enter
  Sherdog_to_BFO <- minitbl %>%
    full_join(Sherdog_to_BFO) %>%
    unique()
}


# Manually change
futureOdds <- futureOdds %>%
  filter((fighter != "Alex-Gorgees-8767" & opponent != "Alex-Gorgees-8767") &
         (fighter != "Ryan-Spann-3502" & opponent != "Ryan-Spann-3502"))

rm(minitbl, availFighters, i, noSherdog)


futureFights <- futureOdds %>%
  left_join(Sherdog_to_BFO, by=c("fighter" = "BFO"), suffix = c("1","2")) %>%
  left_join(Sherdog_to_BFO, by=c("opponent" = "BFO"), suffix = c("1","2")) %>%
  rename(Link1 = Sherdog1, Link2 = Sherdog2) %>%
  left_join(fightersTable, by = c("Link1" = "Link"), suffix = c("1","2")) %>%
  left_join(fightersTable, by = c("Link2" = "Link"), suffix = c("1","2")) %>%
  mutate(rownum = ceiling(row_number()/2)) %>%
  select(rownum, Name1, Name2, eventName, Date, rating1, rating2, Birthday1, Birthday2, wins1, loss1, draw1, nc1, wins2, loss2, draw2, nc2, `5Dimes`, Link1, Link2) %>%
  rename(r1b = rating1, r2b = rating2, odds = `5Dimes`, Event = eventName, BD1 = Birthday1, BD2 = Birthday2)



lastFiveFights <- fightMetricsEvent %>%
  filter(Link1 %in% futureFights$Link1)

lastFiveFights <- fightMetricsEvent %>%
  filter(match_id %in% lastFiveFights$match_id)


futureFights <- lastFiveFights %>%
  full_join(futureFights) %>%
  arrange(Date) %>%
  as.data.table()


futureFights[,
             ':='(fightLag1 = as.numeric(c(0, diff(Date))), # Days since last fight
                  fightLag1_5 = (Date - coalesce(shift(Date,5), first(Date))) %>% as.numeric(),
                  ratIncrease1 = c(0, diff(r1b)), # Rating increase since last fight
                  ratIncrease1_3 = r1b - coalesce(shift(r1b,3), first(r1b)), # Rating increase from 3 fights ago
                  oppRat1_5 = coalesce(cummean(r2b), roll_meanr(r2b, 5)) %>%
                    shift(), # Average rating of last 5 opponents
                  highestWin1_5 = coalesce(roll_maxr((Result2=="win")*r2b, 5), cummax(((Result2=="win")*r2b))) %>%
                    shift(), # Highest rated fighter defeated in last 5 fights
                  lowestLoss1_5 = coalesce(roll_minr(if_else((Result2=="loss")*r2b == 0, 10000,(Result2=="loss")*r2b), 5),
                                           cummin(if_else((Result2=="loss")*r2b == 0, 10000,(Result2=="loss")*r2b))) %>%
                    shift(), # Lowest rated fighter lost to in last 5 fights
                  koLosses1 = cumsum(Result == "loss" & (Method=="TKO"|Method=="KO")) %>%
                    shift(), # number of KO losses
                  diff1_5 = coalesce(cummean((r1b-r2b)), roll_meanr((r1b-r2b), 5)) %>%
                    shift(),
                  Age1 = interval(BD1, Date) %>%
                    time_length("years")
             ),
             by=Link1]

futureFights[,
             ':='(fightLag2 = as.numeric(c(0, diff(Date))), 
                  fightLag2_5 = (Date - coalesce(shift(Date,5), first(Date))) %>% as.numeric(),
                  ratIncrease2 = c(0, diff(r2b)),
                  ratIncrease2_3 = r2b - coalesce(shift(r2b,2), first(r2b)),
                  oppRat2_5 = coalesce(cummean(r1b), roll_meanr(r1b, 5)) %>%
                    shift(),
                  highestWin2_5 = coalesce(roll_maxr((Result2=="loss")*r1b, 5), cummax(((Result2=="loss")*r1b))) %>%
                    shift(),
                  lowestLoss2_5 = coalesce(roll_minr(if_else((Result2=="win")*r1b == 0, 10000,(Result2=="win")*r1b), 5),
                                           cummin(if_else((Result2=="win")*r1b == 0, 10000,(Result2=="win")*r1b))) %>%
                    shift(),
                  koLosses2 = cumsum(Result == "win" & (Method=="TKO"|Method=="KO")) %>%
                    shift(),
                  diff2_5 = coalesce(cummean((r2b-r1b)), roll_meanr((r2b-r1b), 5)) %>%
                    shift(),
                  Age2 = interval(BD2, Date) %>%
                    time_length("years")
             ),
             by=Link2]


futureFights <- futureFights %>%
  as.tibble() %>%
  filter(!is.na(odds)) %>%
  mutate(highestWin1_5 = if_else(highestWin1_5==0, NA_real_, highestWin1_5),
         highestWin2_5 = if_else(highestWin2_5==0, NA_real_, highestWin2_5),
         lowestLoss1_5 = if_else(lowestLoss1_5==10000, NA_real_, lowestLoss1_5),
         lowestLoss2_5 = if_else(lowestLoss2_5==10000, NA_real_, lowestLoss2_5)) %>%
  mutate_at(c("r1b", "r2b", "wins1", "loss1", "draw1", "nc1", "wins2", "loss2", "draw2", "nc2", "fightLag1", "fightLag2", "rownum", "odds"),
            as.integer) %>%
  select(-match_id, -Result, -Method, -Method_d, -r1a, -r2a, -Fighter1, -Fighter2, -R, -Time, -Referee, -Result2, -Org)


saveRDS(Sherdog_to_BFO, "./scripts/8-append-odds/data/Sherdog_to_BFO.RDS")
saveRDS(futureFights, "./scripts/10-future-fights/data/futureFights.RDS")
saveRDS(futureFights, "./Shiny App/MatchPredictor/data/futureFights.RDS")

rm(list=ls())
