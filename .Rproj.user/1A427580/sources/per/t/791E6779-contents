setwd("/Users/Samir Rehmtulla/Documents/Training-Data-Website")
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(stringr)
library(plyr)
library(sp)
library(googleway)
rm(list=ls())
options(stringsAsFactors = FALSE)
RActivites <- fromJSON(rawToChar(GET(url = "https://www.strava.com/api/v3/athlete/activities?after=1519601753&per_page=80&access_token=738ba7d3a2a53c870f699ae5a297383eef11f537")$content))
RActivites$start_date_local <- strtrim(RActivites$start_date_local, 10)

steadyRuns <- grep("steady", RActivites$name, ignore.case = TRUE)
restRuns <- grep("recovery", RActivites$name, ignore.case = TRUE)
workRuns <- grep("work", RActivites$name, ignore.case = TRUE)
tempoRuns <- grep("tempo", RActivites$name, ignore.case = TRUE)
trackRuns <- grep("track", RActivites$name, ignore.case = TRUE)
longRuns <- grep("Long", RActivites$name, ignore.case = TRUE)
intenseRuns <- grep("intense", RActivites$name, ignore.case = TRUE)
WarmupRuns <- grep("warm", RActivites$name, ignore.case = TRUE)
CooldownRuns <- grep("cool", RActivites$name, ignore.case = TRUE)

ids <- RActivites$id
urlP1 <- rep("https://www.strava.com/api/v3/activities/", length(ids))
urlP2 <- rep("access_token=738ba7d3a2a53c870f699ae5a297383eef11f537", length(ids))
urlP3 <- rep("?", length(ids))
urlP4 <- rep("/streams?keys=time,distance,velocity_smooth&key_by_type=true&", length(ids))
PolylineURLs <- paste(urlP1, ids, urlP3, urlP2, sep="")
StreamURLs <- paste(urlP1, ids, urlP4, urlP2, sep= "")
Polylines <- c()
DistanceStreams <- c()
TimeStreams <- c()
VelocityStreams <- c()
for(i in 1:length(PolylineURLs)) {
  Activity <- fromJSON(rawToChar(GET(url = PolylineURLs[[i]])$content))$map
  if(is.null(Activity$polyline))
  { Polylines[[i]] <- "" } else { Polylines[[i]] <- Activity$polyline}
}
for(i in 1:length(StreamURLs)) {
  Activity <- fromJSON(rawToChar(GET(url = StreamURLs[[i]])$content))
  DistanceStream <- Activity$distance$data
  TimeStream <- Activity$time$data
  VelocityStream <- Activity$velocity_smooth$data
  if(is.null(DistanceStream))
  { DistanceStreams[[i]] <- 0 } else { DistanceStreams[[i]] <- DistanceStream}
  if(is.null(TimeStream))
  { TimeStreams[[i]] <- 0 } else { TimeStreams[[i]] <- TimeStream}
  if(is.null(VelocityStream))
  { VelocityStreams[[i]] <- 0 } else { VelocityStreams[[i]] <- VelocityStream}
}

RunType <- vector(mode = "character", length = nrow(RActivites))
for(i in 1:length(RunType)) {
  if(i %in% steadyRuns) { RunType[[i]] <- "Steady"}
  else if(i %in% intenseRuns) { RunType[[i]] <- "Intense" }
  else if(i %in% tempoRuns) { RunType[[i]] <- "Tempo" }
  else if(i %in% trackRuns) { RunType[[i]] <- "Track" }
  else if (i %in% longRuns) { RunType[[i]] <- "Long"}
  else if(i %in% workRuns) { RunType[[i]] <- 'Work' }
  else if(i %in% restRuns) { RunType[[i]] <- "Rest" }
  else if(i %in% WarmupRuns) { RunType[[i]] <- "Warm-up"}
  else if(i %in% CooldownRuns) { RunType[[i]] <- "Cool-down"}
  else { RunType[[i]] <- "Misc"}
}

Pace <- RActivites$moving_time/RActivites$distance/60*1000
Pace <- paste("<b>", floor(Pace), ":", str_pad(round((Pace - floor(Pace))*60), 2, pad = "0"), "/km", "</b>", sep = "")

RunningData <- data.frame(RActivites$start_date_local, RActivites$distance/1000.0, RActivites$moving_time, RunType, Pace, Polylines)
RunningData <- plyr::rename(RunningData, c("RActivites.start_date_local" = "Date", "RActivites.distance.1000" = "Distance", "RActivites.moving_time" = "Time"))
RunningData$Day <- weekdays(as.Date(RunningData$Date))
i <- rep(RunningData$Date[[1]], nrow(RunningData))
RunningData$Week <- as.numeric(floor(difftime(RunningData$Date, i, units = "weeks") + 0.01) + 1)
RunningData$DistanceStreams <- DistanceStreams
RunningData$TimeStreams <- TimeStreams
RunningData$VelocityStreams <- VelocityStreams
OriginalRunningData <- RunningData
RunningDataList <- split(RunningData, RunningData$Week)
save(RunningDataList, file = "data.RData")
rmarkdown::render_site()
for (weekOfPlan in c(unique(OriginalRunningData$Week))){
  rmarkdown::render('/Users/Samir Rehmtulla/Documents/Training-Data-Website/Week1AdvancedStatistics.Rmd',
                    output_file =  paste("report_", weekOfPlan, ".html", sep=''), 
                    output_dir = '/Users/Samir Rehmtulla/Documents/Training-Data-Website')
}
