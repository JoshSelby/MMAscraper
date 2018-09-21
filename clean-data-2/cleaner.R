library(tidyverse)
library(stringi)

fights_list <- readRDS("~/GitHub/MMAscraper/raw-data-1/fights_table.rds")
fightsRaw <- fights_list[[1]]
rm(fights_list)

fightsRaw2 <- fightsRaw %>% 
  select(Fighter1 = Fighter2, Result, Fighter2 = Fighter1, Method, Method_d, R, Time, Referee,
         Event, Date, Link1 = Link2, Link2 = Link1) %>% 
  mutate(Result = ifelse(Result == "win", "loss1", Result),
         Result = ifelse(Result == "loss", "win", Result),
         Result = ifelse(Result == "loss1", "loss", Result))
fightsRaw <- rbind(fightsRaw, fightsRaw2)
rm(fightsRaw2)
fightsRaw <- unique(fightsRaw)

fightsRaw <- fightsRaw %>%
  filter(Result != "loss",
         Result == "win" | (Result != "win" & Link1 < Link2)) %>%
  arrange(Date)

# Remove all fightsRaw where Link2 is an unknown fighter
# Remove fight where Link1=Link2
# Remove Marcus Vinicius Cruz
fightsRaw <- fightsRaw %>%
  filter(Fighter1!= "Unknown Fighter" & Fighter2 != "Unknown Fighter") %>%
  filter(Link1 != Link2) %>%
  filter(Link1 != "/fighter/Marcus-Vinicius-Cruz-58601" & 
           Link2 != "/fighter/Marcus-Vinicius-Cruz-58601")
  

# Remove Referee names from Method and Method_d columns
# Convert all foreign/accented letters to latin letters
# Remove /fighter/ from links
fightsRaw <- fightsRaw %>%
  mutate(Method = Method %>% 
           str_remove(Referee) %>%
           stri_trans_totitle() %>%
           stri_trans_general("latin"),
         Method_d = Method_d %>% 
           str_remove(Referee) %>%
           stri_trans_totitle() %>%
           stri_trans_general("latin"),
         Fighter1 = stri_trans_general(Fighter1, "latin"),
         Fighter2 = stri_trans_general(Fighter2, "latin"),
         Referee = stri_trans_general(Referee, "latin"),
         Link1 = Link1 %>% gsub("/fighter/", "", .),
         Link2 = Link2 %>% gsub("/fighter/", "", .)
         ) %>%
  mutate_all(str_trim) %>%
  mutate_all(str_squish)

# Make individual corrections here
fights <- fightsRaw %>% 
  mutate(Referee = ifelse(Referee == 18062, "Steve Mazzagatti", Referee),
         Referee = ifelse(Referee == 194661, "", Referee),
         Referee = ifelse(Referee == "N/A", "", Referee),
         Referee = gsub("\\\\", "", Referee),
         Date = ifelse(grepl("GCVT - Gaisei Challenge", Event), "1994-07-25", Date),
         Date = ifelse(grepl("Combat Super Fight - Carangolas", Event), "2018-07-16", Date),
         Date = ifelse(grepl("NG - New Generation", Event) & Date=="0000-11-30", "2017-11-30", Date),
         Method = ifelse(Method=="Isaac Yap", "Submission", Method),
         Method_d = ifelse(Method_d=="Isaac Yap", "Triangle Choke", Method_d),
         Method = ifelse(grepl("^Nc$|N/C|No Conest", Method), "No Contest", Method),
         Method = ifelse(Method == 83185, "", Method),
         Method = gsub("imssion$|mision$|misison$|missio$|misson$|mssion$|mbission$", "mission", Method),
         Method = gsub("Ubmission", "Submission", Method),
         Method = gsub("Techincal|Technial", "Technical", Method),
         Method = gsub("Sumission", "Submission", Method), 
         Method_d = gsub("No .* - ", "", Method_d),
         Method = gsub("No Decision.*|^Nd$", "No Contest", Method),
         Method = gsub(" - .*", "", Method),
         Method = gsub("Tko|tko|Tko\\/Medical Stoppage", "TKO", Method),
         Method = gsub(".*Submission.*|Armbar|.*Choke|Tapout|Mata Leon", "Submission", Method),
         Method = gsub("Ko.*|Knockout|K\\)", "KO", Method),
         Method = gsub("Dq|Disqualification|Disqualifcation", "DQ", Method),
         Method = gsub("Drew|.*Draw.*", "Draw", Method),
         Method = gsub(".*decision.*", "Decision", Method, ignore.case = T),
         Method = ifelse(Fighter1 == "Evgeniy Polevov" & Date == "2018-05-12", "Decision", Method),
         Method_d = ifelse(Fighter1 == "Evgeniy Polevov" & Date == "2018-05-12", "Unanimous", Method_d),
         Method = gsub("Overturned By Promoter", "No Contest", Method),
         Method = gsub("Referee Stoppage", "TKO", Method),
         Method_d = ifelse(Method == "Towel", paste0("Towel - ", Method_d), Method_d),
         Method = gsub("Towel|Retirement", "TKO", Method),
         Method_d = ifelse(Method == "Points", paste0("Points - ", Method_d), Method_d),
         Method = gsub("Points", "Decision", Method),
         Method_d = gsub("Sttopage", "Stoppage", Method_d),
         Method_d = ifelse(Method == "Decision", gsub("Decision|Decision | Decision", "", Method_d), Method_d),
         Method_d = gsub("Unaminous|Unaninous|Unanimmous", "Unanimous", Method_d)
         )


saveRDS(fights, file = "~/GitHub/MMAscraper/clean-data-2/fights_clean.rds")
