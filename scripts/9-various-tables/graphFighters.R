
graphFighters <- function(fighterNames) {
  g <- ggplot(fightMetricsEventOdds %>% 
                filter(Fighter1 %in% fighterNames) %>% 
                mutate(Date = as.Date(Date)) %>%
                arrange(match_id),
              aes(x = Date, y = r1a, col = Link1)) + 
    geom_line() + 
    geom_point() + theme(legend.position="bottom")
  
  test <<- fightMetricsEventOdds %>% 
    filter(Fighter1 %in% fighterNames)
  
  return(g)
}


graphFighters(c("Tony Ferguson", "Khabib Nurmagomedov", "Conor McGregor"))

