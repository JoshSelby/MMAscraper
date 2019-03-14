detach("package:tidyverse")
library(shiny)
library(shinyjs)
library(shinysky)
library(plotly)
library(tidyverse)
library(DT)


# read datasets
futureFights <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/futureFights.RDS")

fightMetricsEventOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/fightMetricsEventOdds.rds") %>%
  mutate(Date = as.character(Date),
         r1b = as.integer(r1b),
         r2b = as.integer(r2b),
         odds = as.integer(odds)) %>%
  arrange(desc(match_id))

filtfightsOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/filtfightsOdds.rds")

topFighters <- fightMetricsEventOdds %>% 
  group_by(Link1, Fighter1) %>%
  arrange(desc(Date)) %>%
  slice(1) %>%
  select(Link1, Fighter1, r1a) %>%
  arrange(desc(r1a)) %>% 
  head(5000)

# extra functions
with_plus <- function(x, ...) {
  if (x > 0) {
    return(sprintf(fmt = "+%s", format(x, ...)))
  }
  else {
    return(x)
  }
}

line_to_per <- function(x) {
  return(if_else(x < 0, -x/((-x + 100)), 100/(x+100)))
}

per_to_line <- function(x) {
  return(if_else(x <= 0.5, (1-x)/x * 100, x/(1-x) * -100))
}

odds_to_return <- function(x, bet=10) {
  return(if_else(x < 0, bet/-x*100, x*bet/100))
}



######
graphFighters <- function(fighterNames, title=NULL) {
  Fighters <- fightMetricsEventOdds %>% 
    filter(Fighter1 %in% fighterNames) %>% 
    group_by(Link1, Fighter1) %>% 
    summarise(maxRat = max(r1a)) %>% 
    arrange(desc(maxRat)) %>% 
    group_by(Fighter1) %>%
    slice(1) %>% 
    pull(Link1)
  
  g <- ggplot(fightMetricsEventOdds %>% 
                mutate(Date = as.Date(Date)) %>%
                rename(Rating = r1a, Fighter = Fighter1) %>%
                filter(Link1 %in% Fighters),
              aes(x = Date, y = Rating, col = Fighter)) + 
    geom_line() + 
    geom_point() + 
    scale_x_date(breaks = function(x) seq.Date(from=min(x), to=max(x), by="2 years"), date_labels = "%Y") +
    xlab("Date") +
    ylab("Elo Rating") +
    ggtitle(title)
  
  # test <<- fightMetricsEventOdds %>% 
  #   filter(Fighter1 %in% fighterNames)
  
}
g <- graphFighters(c("Darren Till", "Jorge Masvidal"))
ggplotly(g)
