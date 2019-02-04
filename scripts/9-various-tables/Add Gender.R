library(data.table)
library(tidyverse)

fightersTable <- readRDS("~/GitHub/MMAscraper/scripts/9-various-tables/data/fightersTable.rds")
fightMetricsEvent <- readRDS("~/GitHub/MMAscraper/scripts/5-metrics/data/fightMetricsEvent.rds")

fightersTable$Sex <- "M"
searched <- c()
toSearch <- "Ronda-Rousey-73073"
femOpp <- tibble(Link1 = as.character(NA), Link0 = toSearch)
femOpp1 <- tibble()
i= 1
while (length(toSearch)>0) {
  
  fightersTable <- fightersTable %>% 
    mutate(Sex = ifelse(Link %in% toSearch, "F", Sex))
  
  femOpp1 <- fightMetricsEvent %>%
    filter(Link1 %in% toSearch) %>%
    select(Link1, Link2) %>%
    unique %>%
    filter(!(Link2 %in% c(searched, toSearch, "Javio-Flores-10477"))) %>%
    left_join(femOpp, ., by=c("Link0" = "Link1")) 
  
  colnames(femOpp1) <- paste0("Link", (i+1):0)
  colnames(femOpp) <- paste0("Link", (i+1):1)

  femOpp <- bind_rows(femOpp, femOpp1) %>% unique
  
  searched <- femOpp %>% pull(femOpp %>% colnames %>% tail(2) %>% head(1)) %>% unique %>% na.omit %>% as.character()
  toSearch <- femOpp %>% pull(femOpp %>% colnames %>% tail(1)) %>% unique %>% na.omit %>% as.character()
  i=i+1
}

rm(femOpp1)

femOpp <- femOpp %>% 
  select(colnames(femOpp) %>% head(-1) %>% tail(-1))

femOpp <- femOpp %>% select(colnames(femOpp) %>% rev) %>% as.data.table()

femOpp <- setnames(femOpp[, {un <- unlist(.SD); as.list(un[order(un=='')])},
              .(grp = 1:nrow(femOpp))][, grp := NULL], names(femOpp))[]

femOpp <- femOpp %>% group_by(Link1) %>% slice(1)

femOpp$distance <- rowSums(!is.na(femOpp))-1


saveRDS(fightersTable, "./scripts/9-various-tables/data/fightersTable.rds")

rm(list = ls())