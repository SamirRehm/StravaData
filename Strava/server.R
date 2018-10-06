library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(stringr)
library(plyr)
library(sp)
library(googleway)
library(dplyr)
server <- function(input, output) { 
  map_key <- 'AIzaSyC2_ZQpyvUMYMm2FZh3HouwqavsnF8QRG4'
  RActivites <- fromJSON(rawToChar(GET(url = "https://www.strava.com/api/v3/athlete/activities?after=1519601753&per_page=200&access_token=738ba7d3a2a53c870f699ae5a297383eef11f537&page=1")$content))
  RActivites2 <- fromJSON(rawToChar(GET(url = "https://www.strava.com/api/v3/athlete/activities?after=1519601753&per_page=200&access_token=738ba7d3a2a53c870f699ae5a297383eef11f537&page=2")$content))
  RActivites <- rbind(RActivites, RActivites2)
  RActivites$start_date_local <- strtrim(RActivites$start_date_local, 10)
  RActivites$distance <- RActivites$distance/1000.0
  
  RunningData <- RActivites[,c(4,5,6,7,8,14)]
  i <- rep(RunningData$start_date_local[[1]], nrow(RunningData))
  RunningData$Week <- as.numeric(floor(difftime(RunningData$start_date_local, i, units = "weeks") + 0.01) + 1)
  RunningData$WeekStart = as.Date("2018-02-26") + (RunningData$Week-1) * 7
  
  distancePerWeek = RunningData %>% 
    group_by(WeekStart) %>% 
    summarise(Distance = sum(distance))
  
  output$runs = plotly::renderPlotly({
    p<-plot_ly(data = distancePerWeek, x=~WeekStart, y=~Distance, type='bar') %>%
      layout(title = "Distance per week", xaxis = list(title = "Week"), yaxis = list(title = "Distance (km)"))
  })
  
  
}