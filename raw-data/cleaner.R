library(tidyverse)

fights_list <- readRDS("~/GitHub/MMAscraper/raw-data/fights_table.rds")

fights <- fights_list[[1]] %>%
  mutate()
fights <- 
  
  
testmeth <- fights %>% filter(Result == "draw") %>% head(20)
mapply(gsub, testmeth$Referee, "", testmeth$Method)