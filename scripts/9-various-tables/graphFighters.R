library(tidyverse)

fightMetricsEventOdds2 <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/fightMetricsEventOdds.rds") %>%
  mutate(r1b = as.integer(r1b),
         r2b = as.integer(r2b),
         odds = as.integer(odds)) %>%
  arrange(match_id)


graphFighters <- function(fighterNames, title=NULL) {
  Fighters <- fightMetricsEventOdds2 %>% 
    filter(Fighter1 %in% fighterNames) %>% 
    group_by(Link1, Fighter1) %>% 
    summarise(maxRat = max(r1a)) %>% 
    arrange(desc(maxRat)) %>% 
    group_by(Fighter1) %>%
    slice(1) %>% 
    pull(Link1)
  
  g <- ggplot(fightMetricsEventOdds2 %>% 
                filter(Link1 %in% Fighters),
              aes(x = Date, y = r1a, col = Fighter1)) + 
    geom_line() + 
    geom_point() + 
    scale_x_date(breaks = function(x) seq.Date(from=min(x), to=max(x), by="2 years"), date_labels = "%Y") +
    theme(legend.position="bottom") +
    ylab("Elo Rating") +
    ggtitle(title)
  
  # test <<- fightMetricsEventOdds %>% 
  #   filter(Fighter1 %in% fighterNames)
  
  return(g)
}


list(
graphFighters(c("Henry Cejudo", "Joseph Benavidez", "Demetrious Johnson", "Kyoji Horiguchi", "Kevin Belingon",
                "Jussier da Silva"),
              title = "Flyweight Ratings"),

graphFighters(c("T.J. Dillashaw", "Dominick Cruz", "Marlon Moraes", "Petr Yan", "Cody Garbrandt",
                "Darrion Caldwell"),
              title = "Bantamweight Ratings"),

graphFighters(c("Jose Aldo", "Patricio Freire", "Frankie Edgar", "Max Holloway", "Zabit Magomedsharipov", 
                "Artem Lobov"),
              title = "Featherweight Ratings"),

graphFighters(c("Khabib Nurmagomedov", "Conor McGregor", "Dustin Poirier", "Tony Ferguson", "Max Holloway",
                "Michael Chandler"),
              title = "Lightweight Ratings"),

graphFighters(c("Colby Covington", "Tyron Woodley", "Kamaru Usman", "Georges St. Pierre", "Ben Askren",
                "Rory MacDonald", "Sijara Eubanks"),
              title = "Welterweight Ratings"),

graphFighters(c("Robert Whittaker", "Yoel Romero", "Kelvin Gastelum", "Anderson Silva", "Israel Adesanya",
                "Gegard Mousasi"),
              title = "Middleweight Rankings"),

graphFighters(c("Jon Jones", "Daniel Cormier", "Alexander Gustafsson", "Ryan Bader", "Thiago Santos",
                "Johnny Walker"),
              title = "Light Heavyweight Ratings"),

graphFighters(c("Daniel Cormier", "Stipe Miocic", "Fabricio Werdum", "Francis Ngannou", "Junior dos Santos",
                "Ryan Bader"),
              title = "Heavyweight Ratings"),

graphFighters(c("Rena Kubota", "Kanna Asakura", "Ayaka Hamasaki", "Seo Hee Ham", "Jinh Yu Frey", 
                "Angela Lee"),
              title = "Women's Atomweight Ratings"),

graphFighters(c("Rose Namajunas", "Joanna Jedrzejczyk", "Jessica Andrade", "Tatiana Suarez", "Weili Zhang",
                "Nina Ansaroff"),
              title = "Women's Strawweight Ratings"),

graphFighters(c("Joanna Jedrzejczyk", "Jessica Eye", "Valentina Shevchenko", "Ilima-Lei Macfarlane", "Nicco Montano",
                "Paige VanZant"),
              title = "Women's Flyweight Ratings"),

graphFighters(c("Amanda Nunes", "Holly Holm", "Germaine de Randamie", "Ronda Rousey", "Aspen Ladd",
                "Yana Kunitskaya"),
              title = "Women's Bantamweight Ratings"),

graphFighters(c("Amanda Nunes", "Cristiane Justino", "Julia Budd", "Megan Anderson", "Kayla Harrison",
                "Felicia Spencer"),
              title = "Women's Featherweight Ratings"),

graphFighters(c("Gabi Garcia", "Bob Sapp", "Eric Esch", "Emmanuel Yarborough", "Phil Brooks",
                "Travis Fulton"),
              title = "Memeweight Ratings"),

graphFighters(c("Anderson Silva", "Georges St. Pierre", "Jon Jones", "Daniel Cormier", "Max Holloway",
                "Fedor Emelianenko"),
              title = "GOATweight Ratings"),

graphFighters(c("Rickson Gracie", "Royce Gracie", "Kron Gracie", "Renzo Gracie", "Roger Gracie",
                "Neiman Gracie"),
              title = "Gracieweight Ratings"),

graphFighters(c("Royce Gracie", "Bas Rutten", "Ken Shamrock", "Igor Vovchanchyn", "Kazushi Sakuraba",
                "Masakatsu Funaki"),
              title = "Legendweight Ratings"),

graphFighters(c("Khabib Nurmagomedov", "Zabit Magomedsharipov", "Magomed Magomedov", "Magomed Magomedkerimov", "Rashid Magomedov",
                "Magomedrasul Khasbulaev"),
              title = "Magomedweight Ratings")
)

names(listGraphs) <- sapply(listGraphs, function(x) x$labels$title)

lapply(names(listGraphs), function(x) ggsave(filename = paste0("../../../Desktop/Rating Pictures/",x,".png"),
                                             plot = listGraphs[[x]], device = "png"))


