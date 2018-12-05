# Run following files to completely update everything

source('./raw-data-1/update-scraper.R', echo=TRUE)
source('./records-3/recordAdder.R', echo=TRUE)
source('./ratings-4/add_ratings.R', echo=TRUE)
source('./metrics-5/metrics.R', echo=TRUE)
source('./metrics-5/events.R', echo=TRUE)
source('./model-6/model.R', echo=TRUE)

source('./scrape-odds-7/futureOdds.R', echo=TRUE)
source('./scrape-odds-7/updateOdds.R', echo=TRUE)



