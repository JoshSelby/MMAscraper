library(tidyverse)
library(data.table)
library(zoo)
library(RcppRoll)

# Read in data
fightsElo <- readRDS(file = "./scripts/4-ratings/data/fightsElo.rds") %>%
  rename(score=score1)


# Double the data
fights1 <- fightsElo
fights2 <- fightsElo %>%
  mutate(Result = if_else(Result=="win", "loss", Result),
         Result2 = if_else(Result2 == "win", "loss", Result2),
         score = 1-score)

colnames(fights2) <- fights2 %>% 
  colnames %>% 
  gsub(1,3,.) %>% 
  gsub(2,1,.) %>%
  gsub(3,2,.) %>%
  gsub("Result1", "Result2", .)

fightMetrics <- full_join(fights1, fights2) %>%
  arrange(match_id) %>% 
  as.tibble()

rm(fights1, fights2, fightsElo)

fightMetrics <- fightMetrics %>%
  group_by(Link1) %>%
  mutate(r1b = if_else(wins1+loss1+draw1+nc1==0, 1000, r1b),
         r1a = na.locf(r1a, na.rm = FALSE),
         r1b = if_else(is.na(r1b), r1a, r1b),
         r1a = if_else(is.na(r1a), r1b, r1a)
         ) %>%
  ungroup()


fightMetrics <- fightMetrics %>%
  group_by(Link2) %>%
  mutate(r2b = if_else(wins2+loss2+draw2+nc2==0, 1000, r2b),
         r2a = na.locf(r2a, na.rm = FALSE),
         r2b = if_else(is.na(r2b), r2a, r2b),
         r2a = if_else(is.na(r2a), r2b, r2a)) %>%
  ungroup() %>%
  as.data.table()


# Create new metrics
fightMetrics[, 
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
               shift()
        ),
        by=Link1]



fightMetrics[, 
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
               shift()
        ),
        by=Link2]

fightMetrics[wins1+loss1+draw1+nc1==0,
             ':='(fightLag1 = NA,
                  fightLag1_5 = NA,
                  ratIncrease1 = NA,
                  ratIncrease1_3 = NA,
                  koLosses1 = 0)]

fightMetrics[wins2+loss2+draw2+nc2==0,
             ':='(fightLag2 = NA,
                  fightLag2_5 = NA,
                  ratIncrease2 = NA,
                  ratIncrease2_3 = NA,
                  koLosses2 = 0)]

fightMetrics <- fightMetrics %>%
  as.tibble() %>%
  mutate(highestWin1_5 = if_else(highestWin1_5==0, NA, highestWin1_5),
         highestWin2_5 = if_else(highestWin2_5==0, NA, highestWin2_5),
         lowestLoss1_5 = if_else(lowestLoss1_5==10000, NA, lowestLoss1_5),
         lowestLoss2_5 = if_else(lowestLoss2_5==10000, NA, lowestLoss2_5))



saveRDS(fightMetrics, file = "./scripts/5-metrics/data/fightMetrics.rds")

rm(list=ls())

