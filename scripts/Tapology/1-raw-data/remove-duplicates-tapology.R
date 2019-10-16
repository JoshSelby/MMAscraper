testfighters <- dbReadTable(mydb, "fighterAttributes") %>%
  as.tibble

testfighters <- testfighters %>% distinct

dbWriteTable(mydb, "fighterAttributes", testfighters, overwrite = T)



testfights <- dbReadTable(mydb, "fights") %>%
  as.tibble

testfights <- testfights %>% distinct

dbWriteTable(mydb, "fights", testfights, overwrite = T)




searched_init <- testfights %>% 
  select(fighter_url, relationships.fighter.data.id) %>% distinct()
searched_init <- searched_init %>% 
  rename(link = fighter_url, fighter_id = relationships.fighter.data.id) %>% 
  mutate(fighter_id = as.numeric(fighter_id))


toSearch_init <- testfights %>%
  select(links.opponent_fighter_url, relationships.opponent_fighter.data.id) %>% 
  rename(link = links.opponent_fighter_url, fighter_id = relationships.opponent_fighter.data.id) %>%
  mutate(link = gsub("https://www.tapology.com/fightcenter/fighters/", "", link),
         fighter_id = as.numeric(fighter_id)) %>%
  filter(!is.na(link)) %>%
  distinct() %>%
  anti_join(searched_init)

fights_list2$searched <- searched_init
fights_list2$toSearch <- toSearch_init
