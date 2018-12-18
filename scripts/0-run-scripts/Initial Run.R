### Scrape all the data ###


source('./scripts/1-raw-data/parallel-scraper.R', echo=TRUE)

while (TRUE) {
  parScrap(fights_list)
}

if(exists("fights_list2")) {
  saveRDS(fights_list2, file = "./scripts/1-raw-data/data/fights_table.rds")
}
rm(list=ls())

### Clean the Data ###

fights_list <- readRDS("~/GitHub/MMAscraper/scripts/1-raw-data/data/fights_table.rds")
fightsRaw <- fights_list[[1]]
birthdayTable <- fights_list[[2]]
rm(fights_list)
source('./scripts/2-clean-data/cleaner.R', echo=TRUE)

birthdayTable <- cleanBD(birthdayTable)
fights <- clean(fightsRaw)
rm(fightsRaw)
fights <- addBD(fights, birthdayTable)

saveRDS(fights, file = "./scripts/2-clean-data/data/fights_clean.rds")
saveRDS(birthdayTable, file = "./scripts/2-clean-data/data/birthdayTable.rds")

rm(list=ls())


