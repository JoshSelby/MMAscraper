library(tidyverse)
library(data.table)

fightMetrics <- readRDS(file = "./scripts/5-metrics/data/fightMetrics.rds")


events <- tibble(Event = unique(fightMetrics$Event),
                 Org = gsub(" -.*", "", Event)) %>% 
  mutate(Org = ifelse(Org == "UFC", gsub(".* -", "", Event), Org),
         Org = ifelse(grepl("^UFC - The Ultimate Fighter|^UFC \\d+|^UFC Fight Night|^UFC Live \\d|UFC on Fox \\d+|^UFC on Fuel TV \\d+|^UFC on FX|^UFC on ESPN", Event),
                      "UFC", Org),
         Org = ifelse(grepl("Underground Fight Club", Event), "Underground Fight Club", Org),
         Org = ifelse(grepl("Bellator Fighting Championships|Bellator MMA|^Bellator \\d{3}|Bellator Newcastle", Event), "Bellator", Org),
         Org = gsub(" \\d+$", "", Org),
         Org = ifelse(grepl("^M-1", Event), "M-1", Org),
         Org = ifelse(grepl("^Strikeforce", Event), "Strikeforce", Org),
         Org = ifelse(Org == "Pride", "Pride FC", Org),
         Org = ifelse(grepl("^Absolute Championship Berkut", Event), "ACB", Org),
         Org = ifelse(grepl("^Fight Nights|^EFN - Fight Nights|^FNG - Fight Nights", Event), "FNG", Org),
         Org = ifelse(Org == "One FC", "One Championship", Org),
         Org = ifelse(grepl("Rizin", Event), "Rizin", Org),
         Org = ifelse(Event == "EFN 50 - Emelianenko vs. Maldonado", "FNG", Org),
         Org = ifelse(grepl("Dana White", Org), "Dana White's TNCS", Org))


fightMetrics <- fightMetrics %>% full_join(events)


# Only select fights since 2009 in select organizations
filtfights <- fightMetrics %>%
  filter(Org %in% c("ACB", "Bellator", "CES MMA", "CWFC", "Dana White's TNCS", "FNG", "Golden Boy MMA",
                    "Invicta FC", "KSW", "LFA", "M-1", "One Championship", "Professional Fighters League", "Rizin", 
                    "Strikeforce", "Titan FC", "UFC"),
         Date >= "2009-01-01")

# Highest skilled leagues
filtfights %>% 
  group_by(Org) %>% 
  summarise(average_lvl = mean(r1b+r2b), fights = n()/2) %>% 
  arrange(-average_lvl)


# Highest skilled leagues of 2018
fightMetrics %>% 
  filter(wins1 + loss1 + draw1 >= 5,
         wins2 + loss2 + draw2 >= 5,
         Date > "2018-01-01") %>%
  group_by(Org) %>% 
  summarise(average_lvl = mean(r1b+r2b), 
            fights = n()/2,
            combinedFights = sum(wins1+wins2+loss1+loss2+draw1+draw2+nc1+nc2)/(2*fights)) %>% 
  filter(fights > 25) %>%
  arrange(-average_lvl)


saveRDS(filtfights, "./scripts/5-metrics/data/filtfights.rds")
saveRDS(fightMetrics, "./scripts/5-metrics/data/fightMetricsEvent.rds")
rm(list=ls())
