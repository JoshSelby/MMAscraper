library(data.table)
library(tidyverse)

fightMetricsEventOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/fightMetricsEventOdds.rds")

fightMetricsEventOdds %>%
  filter(grepl("Jon-Jones", Link1)) %>% pull(Link1) %>% unique

searched <- c()
toSearch <- "Jon-Jones-27944"
fighterOpp <- tibble(Link1 = as.character(NA), Link0 = toSearch)
fighterOpp1 <- tibble()
i= 1
while (length(toSearch)>0) {
  
  fighterOpp1 <- fightMetricsEventOdds %>%
    filter(Link1 %in% toSearch & Result == "win") %>%
    select(Link1, Link2) %>%
    unique %>%
    filter(!(Link2 %in% c(searched, toSearch, "Javio-Flores-10477"))) %>%
    left_join(fighterOpp, ., by=c("Link0" = "Link1")) 
  
  colnames(fighterOpp1) <- paste0("Link", (i+1):0)
  colnames(fighterOpp) <- paste0("Link", (i+1):1)
  
  fighterOpp <- bind_rows(fighterOpp, fighterOpp1) %>% unique
  
  searched <- c(toSearch, searched)
  toSearch <- fighterOpp %>% pull(fighterOpp %>% colnames %>% tail(1)) %>% 
    unique %>%
    na.omit %>% 
    as.character() %>% 
    setdiff(searched)
  i=i+1
  print(paste0("iter: ", i, ", to search: ", length(toSearch), ", searched: ", length(searched)))
}

rm(fighterOpp1)

fighterOpp <- fighterOpp %>% 
  select(colnames(fighterOpp) %>% head(-1) %>% tail(-1))

fighterOpp <- fighterOpp %>% select(colnames(fighterOpp) %>% rev) %>% as.data.table()

fighterOpp <- setnames(fighterOpp[, {un <- unlist(.SD); as.list(un[order(un=='')])},
                          .(grp = 1:nrow(fighterOpp))][, grp := NULL], names(fighterOpp))[]

fighterOpp <- fighterOpp %>% group_by(Link1) %>% slice(1)

fighterOpp$distance <- rowSums(!is.na(fighterOpp))-1


#rm(list = ls())
