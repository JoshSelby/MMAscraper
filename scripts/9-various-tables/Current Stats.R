fightMetricsEvent <- readRDS(file = "./scripts/5-metrics/data/fightMetricsEvent.rds")
fights <- readRDS(file = "./scripts/3-records/data/fights_records.rds")


lastFight <- fightMetricsEvent %>%
  group_by(Link1) %>%
  arrange(match_id) %>%
  slice(n()) %>%
  ungroup

lastFight2 <- lastFight %>%
  mutate(wins1 = ifelse(Result == "win", wins1 + 1, wins1),
         loss1 = ifelse(Result == "loss", loss1 + 1, loss1),
         draw1 = ifelse(Result == "draw", draw1 + 1, draw1),
         nc1 = ifelse(Result == "NC", nc1 + 1, nc1)) %>%
  select(Link1, Result, Link2, Date, r1a, Fighter1, BD1, wins1, loss1, draw1, nc1, Org) %>%
  rename(Link = Link1, Last.Result = Result, Last.Opponent = Link2, Last.Date = Date, rating = r1a, Name = Fighter1, 
         Birthday = BD1, wins = wins1, loss = loss1, draw = draw1, NC = nc1) %>%
  arrange(desc(rating))


FiveFightsAgo <- fightMetricsEvent %>%
  group_by(Link1) %>%
  arrange(desc(match_id)) %>%
  slice(4) %>%
  ungroup
