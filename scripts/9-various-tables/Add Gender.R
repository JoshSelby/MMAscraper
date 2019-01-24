library(data.table)
library(tidyverse)

fightersTable <- readRDS("~/GitHub/MMAscraper/scripts/9-various-tables/data/fightersTable.rds")
fightMetricsEvent <- readRDS("~/GitHub/MMAscraper/scripts/5-metrics/data/fightMetricsEvent.rds")

fightersTable$Sex <- "M"
searched <- c()
toSearch <- "Ronda-Rousey-73073"
femOpp <- tibble(Link1 = as.character(NA), Link0 = toSearch)
i= 1
for (i in 1:15) {
  
  fightersTable <- fightersTable %>% 
    mutate(Sex = ifelse(Link %in% toSearch, "F", Sex))
  
  femOpp <- fightMetricsEvent %>%
    filter(Link1 %in% toSearch) %>%
    select(Link1, Link2) %>%
    unique %>%
    filter(!(Link2 %in% c(searched, toSearch, "Javio-Flores-10477"))) %>%
    left_join(femOpp, ., by=c("Link0" = "Link1"))
  
  colnames(femOpp) <- paste0("Link", (i+1):0)
  
  searched <- femOpp %>% pull(femOpp %>% colnames %>% tail(2) %>% head(1)) %>% unique
  toSearch <- femOpp %>% pull(femOpp %>% colnames %>% tail(1))
  i=i+1
}

