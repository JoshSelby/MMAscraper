if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("RSQLite")) install.packages("RSQLite")
library(RSQLite)
if (!require("rvest")) install.packages("rvest")
library(rvest)
if (!require("foreach")) install.packages("foreach")
library(foreach)
if (!require("doSNOW")) install.packages("doSNOW")
library(doSNOW)
if (!require("parallel")) install.packages("parallel")
library(parallel)

mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

if(!exists("fightsMMA_clean")) {
  fightsMMA_clean <- dbReadTable(mydb, "fightsMMA_clean") %>%
    as_tibble()
}


fightsMMA_NC <- fightsMMA_clean %>%
  filter(result == "No Contest" & grepl("http", bout_url)) %>% 
  select(fight_id, bout_url) %>% 
  mutate(explanation = NA_character_) %>%
  unique


if(dbExistsTable(mydb, "fightsMMA_NC")) {
  fightsMMA_NCdb <- dbReadTable(mydb, "fightsMMA_NC") %>%
    as_tibble()
  
  fightsMMA_NC <- fightsMMA_NC %>% 
    anti_join(fightsMMA_NCdb, by = "bout_url")
}



scrapeNCFight <- function(bout_url, fight_id = NA) {
  if (!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
  if (!require("rvest")) install.packages("rvest")
  library(rvest)
  
  explanation <- read_html(bout_url) %>%
    html_nodes(".writeupContents") %>% 
    html_text %>%
    trimws %>%
    ifelse(length(.)==0, NA, .)
  
  if(is.na(explanation)) {
    explanation <- read_html(bout_url) %>%
      html_nodes(".blurb") %>% 
      html_text %>%
      trimws %>%
      ifelse(length(.)==0, NA, .)
  }
  
  return(tibble(fight_id, bout_url, explanation))
}


parScrapNCFight <- function(fightsMMA_NC, num=100) {
  NumberOfCluster <- detectCores()
  cl <- NumberOfCluster %>% 
    makeCluster(outfile="log.txt")
  registerDoSNOW(cl)
  
  
  print(system.time({
    pb <- fightsMMA_NC %>%
      head(num) %>% 
      length %>%
      txtProgressBar(max = ., style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    fightsMMA_NC2 <<- foreach(link = head(pull(fightsMMA_NC, bout_url), num),
                              fight_id = head(pull(fightsMMA_NC, fight_id), num),
                             .combine = 'rbind', 
                             .multicombine = TRUE, 
                             .maxcombine = 2, 
                             .export = c("scrapeNCFight"), 
                             .options.snow = opts) %dopar% {
                               scrapeNCFight(link, fight_id)
                             } 
  }))
  
  stopCluster(cl)
  rm(NumberOfCluster, opts, pb)
  
}

fightsMMA_NC3 <- tibble("fight_id" = as.integer(), "bout_url" = as.character(), "explanation" = as.character())
fightsMMA_NC2 <- fightsMMA_NC
while(nrow(fightsMMA_NC)>0) {
  parScrapNCFight(fightsMMA_NC, 300)
  fightsMMA_NC <- anti_join(fightsMMA_NC, fightsMMA_NC2, by=c("bout_url"))
  fightsMMA_NC3 <- rbind(fightsMMA_NC2, fightsMMA_NC3)
}



fightsMMA_NC <- left_join(fightsMMA_NC3, 
                          fightsMMA_clean %>% 
                            filter(fight_id %in% fightsMMA_NC3$fight_id), 
                          by = c("fight_id" = "fight_id", "bout_url" = "bout_url")) %>% 
  group_by(fight_id, bout_url, explanation) %>% 
  summarise(fighterA = min(short_name1), fighterB = max(short_name2), date = max(date)) %>%
  arrange(explanation) %>%
  add_column(resultA = NA_real_, resultB = NA_real_) %>%
  left_join(fightsMMA_clean %>% 
              filter(fight_id %in% fightsMMA_NC3$fight_id) %>%
              select(fight_id, short_name1, short_name2, fighter1id, fighter2id),
            by = c("fight_id" = "fight_id", "fighterA" = "short_name1", "fighterB" = "short_name2"))


dbWriteTable(mydb, "fightsMMA_NC", fightsMMA_NC, append = T)


# fightsMMA_NCbackup <- dbReadTable(mydb, "fightsMMA_NC_backup") %>% as_tibble()
# fightsMMA_NCdb <- fightsMMA_NCdb %>% 
#   left_join(fightsMMA_NCbackup %>% 
#               select(bout_url, fighter1id, fighter2id, resultA, resultB),
#             by = c("bout_url", "fighter1id", "fighter2id")) %>%
#   mutate(resultA = coalesce(resultA.x, resultA.y),
#          resultB = coalesce(resultB.x, resultB.y)) %>%
#   select(colnames(fightsMMA_NCdb))
# 
# dbWriteTable(mydb, "fightsMMA_NC", fightsMMA_NCdb, overwrite = T)

rm(list=ls())
