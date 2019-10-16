if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("RSQLite")) install.packages("RSQLite")
library(RSQLite)

mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

if(!exists("fightsMMA_clean")) {
  fightsMMA_clean <- dbReadTable(mydb, "fightsMMA_clean")
}

fightsMMA_clean <- fightsMMA_clean %>%
  as.data.table()

# Add fighter records at time of fight
fightsMMA_clean[, wins1 := shift(cumsum(result == "Win"), 1, fill=0L), by=fighter1id]
fightsMMA_clean[, loss1 := fightsMMA_clean[fightsMMA_clean, on = .(fighter2id = fighter1id, fight_id < fight_id), sum(result == "Win", na.rm = T), by = .EACHI]$V1]
fightsMMA_clean[, draw1a := shift(cumsum(result == "Draw"), 1, fill=0), by=fighter1id]
fightsMMA_clean[, draw1b := fightsMMA_clean[fightsMMA_clean, on = .(fighter2id = fighter1id, fight_id < fight_id), sum(result == "Draw", na.rm = T), by = .EACHI]$V1]
fightsMMA_clean[, draw1 := draw1a + draw1b]
fightsMMA_clean[, nc1a := shift(cumsum(result == "No Contest"), 1, fill=0), by=fighter1id]
fightsMMA_clean[, nc1b := fightsMMA_clean[fightsMMA_clean, on = .(fighter2id = fighter1id, fight_id < fight_id), sum(result == "No Contest", na.rm = T), by = .EACHI]$V1]
fightsMMA_clean[, nc1 := nc1a + nc1b]

fightsMMA_clean[, wins2 := fightsMMA_clean[fightsMMA_clean, on = .(fighter1id = fighter2id, fight_id < fight_id), sum(result == "Win", na.rm = T), by = .EACHI]$V1]
fightsMMA_clean[, loss2 := shift(cumsum(result == "Win"), 1, fill=0), by=fighter2id]
fightsMMA_clean[, draw2a := shift(cumsum(result == "Draw"), 1, fill=0), by=fighter2id]
fightsMMA_clean[, draw2b := fightsMMA_clean[fightsMMA_clean, on = .(fighter1id = fighter2id, fight_id < fight_id), sum(result == "Draw", na.rm = T), by = .EACHI]$V1]
fightsMMA_clean[, draw2 := draw2a + draw2b]
fightsMMA_clean[, nc2a := shift(cumsum(result == "No Contest"), 1, fill=0), by=fighter2id]
fightsMMA_clean[, nc2b := fightsMMA_clean[fightsMMA_clean, on = .(fighter1id = fighter2id, fight_id < fight_id), sum(result == "No Contest", na.rm = T), by = .EACHI]$V1]
fightsMMA_clean[, nc2 := nc2a + nc2b]

# Remove intermediate variables
fightsMMA_clean[, c("draw1a", "draw1b", "draw2a", "draw2b", "nc1a", "nc1b", "nc2a", "nc2b") := NULL]


