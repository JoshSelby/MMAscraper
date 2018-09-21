library(EloRating)
library(dplyr)
library(data.table)

fights <- readRDS("~/GitHub/MMAscraper/raw-data/fights_clean.rds")

fights <- fights %>% 
  arrange(Date) %>%
  mutate(match_id = row_number()) %>%
  data.table()

# Add fighter records at time of fight
fights[, wins1 := shift(cumsum(Result == "win"), 1, fill=0L), by=Link1]
fights[, loss1 := fights[fights, on = .(Link2 = Link1, match_id < match_id), sum(Result == 'win', na.rm = T), by = .EACHI]$V1]
fights[, draw1a := shift(cumsum(Result == "draw"), 1, fill=0), by=Link1]
fights[, draw1b := fights[fights, on = .(Link2 = Link1, match_id < match_id), sum(Result == 'draw', na.rm = T), by = .EACHI]$V1]
fights[, draw1 := draw1a + draw1b]
fights[, nc1a := shift(cumsum(Result == "NC"), 1, fill=0), by=Link1]
fights[, nc1b := fights[fights, on = .(Link2 = Link1, match_id < match_id), sum(Result == 'NC', na.rm = T), by = .EACHI]$V1]
fights[, nc1 := nc1a + nc1b]

fights[, wins2 := fights[fights, on = .(Link1 = Link2, match_id < match_id), sum(Result == 'win', na.rm = T), by = .EACHI]$V1]
fights[, loss2 := shift(cumsum(Result == "win"), 1, fill=0), by=Link2]
fights[, draw2a := shift(cumsum(Result == "draw"), 1, fill=0), by=Link2]
fights[, draw2b := fights[fights, on = .(Link1 = Link2, match_id < match_id), sum(Result == 'draw', na.rm = T), by = .EACHI]$V1]
fights[, draw2 := draw2a + draw2b]
fights[, nc2a := shift(cumsum(Result == "NC"), 1, fill=0), by=Link2]
fights[, nc2b := fights[fights, on = .(Link1 = Link2, match_id < match_id), sum(Result == 'NC', na.rm = T), by = .EACHI]$V1]
fights[, nc2 := nc2a + nc2b]

# Remove intermediate variables
fights[, c("draw1a", "draw1b", "draw2a", "draw2b", "nc1a", "nc1b", "nc2a", "nc2b") := NULL]



winCar <- fights %>%
  filter(Result == "win") %>%
  group_by(Fighter = Link1) %>%
  summarise(wins = n())

lossCar <- fights %>%
  filter(Result == "win") %>%
  group_by(Fighter = Link2) %>%
  summarise(loss = n())

drawCar <- fights %>% 
  filter(Result == "draw") %>% 
  select(Link1, Link2) %>% 
  unlist %>% 
  table() %>% 
  as.tibble

colnames(drawCar) <- c("Fighter", "draw")

fighterRecords <- winCar %>% 
  full_join(lossCar) %>% 
  full_join(drawCar) %>%
  mutate(matches = rowSums(.[2:4], na.rm=T))


saveRDS(fights, file = "~/GitHub/MMAscraper/records/fights_records.rds")
