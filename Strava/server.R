library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(stringr)
library(plyr)
library(sp)
library(googleway)
library(dplyr)
library(DT)
server <- function(session, input, output) { 
  map_key <- 'AIzaSyC2_ZQpyvUMYMm2FZh3HouwqavsnF8QRG4'
  
  RActivites <- fromJSON(rawToChar(GET(url = "https://www.strava.com/api/v3/athlete/activities?after=1519601753&per_page=200&access_token=738ba7d3a2a53c870f699ae5a297383eef11f537&page=1")$content))
  RActivites2 <- fromJSON(rawToChar(GET(url = "https://www.strava.com/api/v3/athlete/activities?after=1519601753&per_page=200&access_token=738ba7d3a2a53c870f699ae5a297383eef11f537&page=2")$content))
  RActivites = rbind_pages(list(RActivites, RActivites2))
  RActivites$type[grep('soccer', RActivites$name, ignore.case = TRUE)] = 'Soccer'
  RActivites$type[grep('tennis', RActivites$name, ignore.case = TRUE)] = 'Tennis'
  RActivites$start_date_local <- strtrim(RActivites$start_date_local, 10)

  RActivites$PaceNumeric <- RActivites$moving_time/RActivites$distance/60*1000
  RActivites$Pace <- paste("<b>", floor(RActivites$PaceNumeric), ":", str_pad(round((RActivites$PaceNumeric - floor(RActivites$PaceNumeric))*60), 2, pad = "0"), "/km", "\nHR:", RActivites$average_heartrate, "</b>", sep = "")
  RActivites$distance <- RActivites$distance/1000.0
  
  RunningData <- RActivites[,c(3,4,5,6,7,8,14,47,49,50,51,10)]
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
  
  updateSelectizeInput(session, 'Week', choices = rev(RunningData$WeekStart), server = TRUE, selected = rev(RunningData$WeekStart)[1])
  
   weekStats = reactive({
     RunningData[RunningData$WeekStart == input$Week,]
    })

   output$currentWeekActivities = plotly::renderPlotly({
     stats = weekStats()
     dates <- seq(stats$WeekStart[[1]], stats$WeekStart[[1]] + 6, by = "day")
     p <- plot_ly(data = stats, type = "bar", hoverinfo = "all")
     
     p <- p %>% add_trace(p, x = dates, y = integer(length(dates)), showlegend = F)  %>%
       layout(title = paste("Week starting", format(stats$WeekStart[[1]], format="%A, %B %d %Y")), xaxis = list(title = "Day"), yaxis = list(title = "Distance (km)"), barmode='stack')
     runTypes = c('warm', 'interval', 'tempo', 'fartlek', 'long', 'easy', 'cool')
     colours = c('rgba(160,70, 255,0.6)', 'rgba(100,255,100,0.6)', 'rgba(255, 100, 100, 0.6)', 'rgba(255, 255, 100, 0.6)',
                 'rgba(255, 100, 255, 0.6)', 'rgba(50, 150, 255, 0.6)', 'rgba(100, 255, 255, 0.6)')
     names = c('Warm-up', 'Intervals', 'Tempo Run', 'Fartlek', 'Long Run', 'Easy Run', 'Cool-down')
     for(i in 1:length(runTypes)) {
       runsOfType = stats[grep(runTypes[[i]], stats$name, ignore.case=TRUE),]
       print(runsOfType$Pace)
       if(nrow(runsOfType) > 0) {
         p <- p %>% add_trace (p, x=runsOfType$start_date_local, y=runsOfType$distance, type='bar', text = runsOfType$Pace,
                              textposition = 'auto', textfont = list(size = 11),
                              marker = list(line = list(color = 'rgb(8,48,107)', width = 2), 
                              color = colours[[i]]), name = names[[i]])
       }
     }
     p
  })
   
   output$pieChart = renderPlotly({
     stats=weekStats()
     colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
     
     p <- plot_ly(stats, labels = ~type, values = ~round(elapsed_time/3600, digits=2), type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent+value',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  #The 'pull' attribute can also be used to create space between the sectors
                  showlegend = FALSE) %>%
       layout(title = 'Time per activity',
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
   })
   
   output$kmsBox <- renderInfoBox({
     stats = weekStats()
     infoBox(
       "Km's Run", round(sum(stats$distance), digits = 2), icon = icon("child", lib = "font-awesome"),
       color = "yellow"
     )
   })
   
   output$ActiveTimeBox <- renderInfoBox({
     stats = weekStats()
     totalTimeInSeconds = sum(stats$elapsed_time)
     infoBox(
       "Active Time", paste(floor(totalTimeInSeconds/3600), "hours,", round((totalTimeInSeconds/3600-floor(totalTimeInSeconds/3600))*60),"min"), icon = icon("time", lib = "glyphicon"),
       color = "green"
     )
   })
   
   output$runPieChart <- renderPlotly({
     stats = weekStats()
     runTypes = c('warm', 'interval', 'tempo', 'fartlek', 'long', 'easy', 'cool')
     names = c('Warm-up', 'Intervals', 'Tempo Run', 'Fartlek', 'Long Run', 'Easy Run', 'Cool-down')
     distancePerType = c()
     for(i in 1:length(runTypes)) {
       distancePerType[[i]] = sum(stats[grep(runTypes[[i]], stats$name, ignore.case=TRUE),]$distance)
     }
     pieChartData = data.frame(names, distancePerType)
     p <- plot_ly(pieChartData, labels=~names, values = ~round(distancePerType, digits=2), type='pie', 
                  textposition = 'inside',
                  textinfo = 'label+percent+value',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  #The 'pull' attribute can also be used to create space between the sectors
                  showlegend = FALSE) %>% layout(title = "Distance per run type")
     
   })
   
   output$WeekSummary = DT::renderDataTable({
     stats = weekStats()
     stats$PaceNumeric = paste(floor(stats$PaceNumeric), ":", str_pad(round((stats$PaceNumeric - floor(stats$PaceNumeric))*60),2,pad="0"), sep = "")
     stats[,c(7,1,2,3,9)]
   }, options = list(dom = 'tp'), selection = "single", rownames = FALSE, class = "compact")
   
   observeEvent(input$WeekSummary_rows_selected, {
     stats = weekStats()
     print(stats[input$WeekSummary_rows_selected,])
     stream_url = paste("https://www.strava.com/api/v3/activities/", stats[input$WeekSummary_rows_selected,]$id,
                 "/streams?keys=time,distance,velocity_smooth,heartrate&key_by_type=true&", "access_token=738ba7d3a2a53c870f699ae5a297383eef11f537", sep = "")
     Activity <- fromJSON(rawToChar(GET(url = stream_url)$content))
     DistanceStream <- Activity$distance$data
     TimeStream <- Activity$time$data
     VelocityStream <- Activity$velocity_smooth$data
     HeartStream <- Activity$heartrate$data
     Activity = data.frame(DistanceStream, TimeStream, VelocityStream, HeartStream)
     output$activityPacePlot = renderPlotly({
       ay <- list(
         overlaying = "y",
         side = "right",
         title = "Heart Rate (bpm)"
       )
       p <- plot_ly(data=Activity, x=~TimeStream/60, y=~VelocityStream, type='scatter', mode="line", name="Raw Data") %>%
         layout(xaxis = list(title="Time (minutes)"), yaxis = list(title="Speed (m/s)", range=c(2,7), autorange = F, autorange= "reversed"),
                yaxis2=ay, legend=list(orientation='h'))
       smooth <- smooth.spline(Activity$TimeStream/60, VelocityStream, spar = input$`Smooth factor`)
       smoothData <- data.frame(smooth$x, smooth$y)
       p <- p %>% add_trace(data=Activity, x=~TimeStream/60, y=~HeartStream, type='scatter', mode="line", name="Heart Rate", yaxis="y2")
       p <- p %>% add_trace(smoothData, x=smoothData$smooth.x, y=smoothData$smooth.y, name = 'Smoothened Data')
     })
   })
  
  
}