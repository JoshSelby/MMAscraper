library(lubridate)
### Scrape all the data ###
source('./scripts/Tapology/1-raw-data/parallel-scraper-tapology.R', echo=TRUE)


# Initialize fights list
fights_list2 <- scrape(link, fighter_id)
mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")
dbWriteTable(mydb, "fights", fights_list2$fights, overwrite = T)
dbWriteTable(mydb, "fighterAttributes", fights_list2$fighterAttributes, overwrite = T)

i=0
while (TRUE) {
  searched_init <- fights_list2$searched
  toSearch_init <- fights_list2$toSearch
  parScrap(fights_list2, 300)
  dbWriteTable(mydb, "fights", fights_list2$fights, overwrite = F, append = T)
  print(paste(nrow(fights_list2$fights), "fights written"))
  dbWriteTable(mydb, "fighterAttributes", 
               fights_list2$fighterAttributes %>% 
                 mutate(lastUpdate = today() %>% as.character()), overwrite = F, append = T)
  print(paste(nrow(fights_list2$fighterAttributes), "fighterAttributes written"))
  
  fights_list2$searched <- rbind(searched_init, fights_list2$searched) %>% distinct
  fights_list2$toSearch <- anti_join(rbind(fights_list2$toSearch, toSearch_init),
                                     fights_list2$searched, by = c("link", "fighter_id")) %>% distinct
  
  print(paste("Total:",nrow(searched_init), "fighters searched"))
  print(paste("Total:",nrow(toSearch_init), "fighters in queue"))
  i=i+1
  print(i)
  if(nrow(fights_list2$toSearch)<1)
    break
}

rm(list=ls())


