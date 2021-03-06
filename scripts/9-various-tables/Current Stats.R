library(tidyverse)
library(data.table)

fightMetricsEvent <- readRDS(file = "./scripts/5-metrics/data/fightMetricsEvent.rds")

lastFight <- fightMetricsEvent %>%
  group_by(Link1) %>%
  arrange(match_id) %>%
  slice(n()) %>%
  ungroup


fightersTable <- lastFight %>%
  mutate(wins1 = ifelse(Result == "win", wins1 + 1, wins1),
         loss1 = ifelse(Result == "loss", loss1 + 1, loss1),
         draw1 = ifelse(Result == "draw", draw1 + 1, draw1),
         nc1 = ifelse(Result == "NC", nc1 + 1, nc1),
         ratInc = r1a - r1b,
         BDchar = as.character(BD1)) %>%
  select(Link1, Result, Link2, Date, r1a, Fighter1, ratInc, BD1, BDchar, wins1, loss1, draw1, nc1, Org) %>%
  rename(Link = Link1, Last.Result = Result, Last.Opponent = Link2, Last.Date = Date, rating = r1a, Name = Fighter1, 
         Birthday = BD1, wins = wins1, loss = loss1, draw = draw1, nc = nc1) %>%
  arrange(desc(rating)) 


saveRDS(fightersTable, "./scripts/9-various-tables/data/fightersTable.rds")

