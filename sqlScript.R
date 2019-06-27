library(RSQLite)
library(tidyverse)

fightersTable <- readRDS("./scripts/9-various-tables/data/fightersTable.rds") %>%
  select(Link, Name, Birthday, Sex) 


mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

dbWriteTable(mydb, "fighters", fightersTable %>% mutate(Birthday = as.character(Birthday)), 
             field.types = c("Birthday"="date"), overwrite = TRUE)



fightersTable <- dbReadTable(mydb, "fighters") %>%
  as.tibble %>%
  mutate(Birthday = as.Date(Birthday))



saveRDS(fightersTable, "./scripts/9-various-tables/data/fightersTable.rds")





# Get all fights you already scraped
fights <- readRDS(file = "./scripts/2-clean-data/data/fights_clean.rds")


dbWriteTable(mydb, "fightsRaw", fights %>% mutate_at(funs(as.character), .vars = c("Date", "BD1", "BD2")),
             field.types = c("Date"="date", "BD1"="date", "BD2"="date"), overwrite = TRUE)
