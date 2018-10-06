library(shiny)
library(raster)
library(grid)
library(jpeg)
library(plotly)
library(jsonlite)
library(httr)
library(htmlwidgets)
library(plyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(reshape2)
library(DT)
library(tuneR)
library(curl)
shinyServer(function(input, output, session) {
  numGoals <- 0
  gameID <- 10
  lastGoalTime <- Sys.time()
  rink <- readJPEG("NHLRink.jpg")
  goal1PosX <- 89
  goal2PosX <- -89
  load("ShotPredictor.RData")
  get_audio_tag <- function(filename) {
    tags$audio(src = filename,
               type = "audio/wav",
               autoplay = NA)
  }
  
  observeEvent(input$Season, {
    seasonResponse <-
      jsonlite::fromJSON(rawToChar(httr::GET(
        url = paste(
          "http://live.nhl.com/GameData/SeasonSchedule-",
          input$Season,
          ".json",
          sep = ""
        )
      )$content))
    updateSelectInput(session,
                      "Team",
                      label = "Pick Team",
                      choices = unique(sort(c(
                        seasonResponse$a
                      ))))

  })
  
  observeEvent(input$Team, {
    if(input$Team != "") {
    seasonResponse <-
      jsonlite::fromJSON(rawToChar(httr::GET(
        url = paste(
          "http://live.nhl.com/GameData/SeasonSchedule-",
          input$Season,
          ".json",
          sep = ""
        )
      )$content))
    seasonResponse <-
      seasonResponse[((seasonResponse$a == input$Team) |
                        (seasonResponse$h == input$Team)),]
    updateSelectInput(
      session,
      "GameNumber",
      label = paste("Select a game"),
      choices = paste(
        seasonResponse$id,
        seasonResponse$a,
        "at",
        seasonResponse$h,
        seasonResponse$est
      )
    )
    }
  })
  
  plays = reactive({
    print(lastGoalTime)
    print(input$tab)
    originalTime <- Sys.time()
    if(input$tab == 'dashBoard') {
    invalidateLater(input$Refresh * 1000, session)
    }
    gameData <-
      jsonlite::fromJSON(rawToChar(httr::GET(
        url = paste(
          "http://statsapi.web.nhl.com/api/v1/game/",
          if (input$GameNumber == "") {
            "2017021208"
          } else {
            substr(input$GameNumber, 0, 10)
          },
          "/feed/live",
          sep = ""
        )
      )$content))
    
    if (input$GameNumber != gameID) {
      src = paste(
        "//www-league.nhlstatic.com/nhl.com/builds/site-core/33f4bcacaa52eed691a6f0671c4cde69850f3c31_1521478002/images/logos/team/current/team-",
        gameData$gameData$teams$home$id,
        "-dark.svg",
        sep = ""
      )
      output$picture <-
        renderText({
          c('<img src="', src, '", width = 100, height = 100>')
        })
      src2 = paste(
        "//www-league.nhlstatic.com/nhl.com/builds/site-core/33f4bcacaa52eed691a6f0671c4cde69850f3c31_1521478002/images/logos/team/current/team-",
        gameData$gameData$teams$away$id ,
        "-dark.svg",
        sep = ""
      )
      output$picture2 <-
        renderText({
          c('<img src="', src2, '", width = 100, height = 100>')
        })
    }
    if (as.numeric(difftime(Sys.time(), lastGoalTime, units = 'secs')) > 15 &&
        length(gameData) > 2) {
      #need to fix this for when you aren't in a game
      if (numGoals != gameData$liveData$boxscore$teams$home$teamStats$teamSkaterStats$goals + gameData$liveData$boxscore$teams$away$teamStats$teamSkaterStats$goals)
      {
        output$audiotag <- renderUI(get_audio_tag("a.mp3"))
        lastGoalTime <<- Sys.time()
      } else {
        output$audiotag <- renderUI(get_audio_tag("none.mp3"))
      }
    }
    
    numGoals <<-
      gameData$liveData$boxscore$teams$home$teamStats$teamSkaterStats$goals + gameData$liveData$boxscore$teams$away$teamStats$teamSkaterStats$goals
    gameID <<- input$GameNumber
    PBP <- gameData$liveData
    print("Time for plays: ")
    print(originalTime - Sys.time())
    if (length(PBP) > 0) {
      list(PBP, gameData$gameData)
    } else {
      list()
    }
  })
  
  statistics <- reactive({
    if(input$tab == 'dashBoard') {
    invalidateLater(input$Refresh * 1000, session)
    }
    PBP <- plays()[[1]]
    originalTime <- Sys.time()
    if (!(is.null(PBP)) &&
        length(PBP) > 0 && is.data.frame(PBP$plays$allPlays))
    {
      PBP <- flatten(PBP$plays$allPlays)
      shotPrediction <- numeric(nrow(PBP))
      if (nrow(PBP) > 0) {
        for (i in 1:nrow(PBP)) {
          if (PBP$result.event[[i]] != "Shot" &&
              PBP$result.event[[i]] != "Goal") {
            shotPrediction[[i]] <- 0
          }
          else {
            distX <- 89 - abs(PBP$coordinates.x[[i]])
            distY <- PBP$coordinates.y[[i]]
            distances <- sqrt(distX ^ 2 + distY ^ 2)
            tmp.result.secondaryType <-
              PBP$result.secondaryType[[i]]
            angles <- abs(atan(distY / distX))
            predictor <-
              data.frame(distances, angles, tmp.result.secondaryType)
            shotPrediction[[i]] <-
              predict(shotModel, predictor, type = "response")
          }
        }
      }
      if (length(PBP$team.triCode) == 0) {
        PBP$team.triCode <- character(length = length(PBP$result.event))
      }
      shotPrediction <-
        paste(signif(shotPrediction, 4) * 100, "%", sep = "")
      w <-
        data.frame(
          PBP$about.periodTimeRemaining,
          PBP$team.triCode,
          PBP$result.event,
          PBP$result.description,
          shotPrediction
        )
      w <-
        rename(
          w,
          c(
            "PBP.about.periodTimeRemaining" = "Period Time",
            "PBP.team.triCode" = "Team",
            "PBP.result.event" = "Event",
            "PBP.result.description" = "Description",
            "shotPrediction" = "Goal Probability"
          )
        )
      w <- w[dim(w)[1]:1, ]
      x <- data.frame(w)
      print("Statistics Time")
      print(Sys.time() - originalTime)
      x
    }
    else {
      data.frame()
    }
  })
  
  output$PlaySummary = DT::renderDataTable({
    isolate(statistics())
  }, options = list(dom = 'tp'))
  
  proxy = dataTableProxy('PlaySummary')
  
  observe({
    replaceData(proxy, statistics(), resetPaging = FALSE)
  })
  
  
  output$NHLPLot <- renderPlotly({
    gameData1 <- plays()[[1]]
    PBP <- flatten(gameData1$plays$allPlays)
    originalTime <- Sys.time()
    variables <- input$plotVars
    pbpfilter <- PBP[!is.na(PBP$team.triCode), ]
    pbpfilter <- pbpfilter[pbpfilter$result.event %in% variables, ]
    modifiedCoords <-
      pbpfilter$coordinates.x * (2 * (pbpfilter$about.period %% 2) - 1)
    modifiedCoordsY <-
      pbpfilter$coordinates.y * (2 * (pbpfilter$about.period %% 2) - 1)
    Event <- pbpfilter$result.description
    p <-
      ggplot(data = pbpfilter,
             aes(x = modifiedCoords, y = modifiedCoordsY, text = Event)) + annotation_raster(rink,-100, 100,-42.5, 42.5, interpolate = FALSE) +
      geom_point(aes(
        color = pbpfilter$team.triCode,
        shape = pbpfilter$result.event
      ),
      size = 2) +
      scale_shape_manual(values = c(7, 13, 3, 11, 20, 21, 22, 23, 24, 25)) +
      scale_x_continuous(limits = c(-100,100)) +
      scale_y_continuous(limits=c(-43, 43)) +
      theme(
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      ) +
      theme(legend.text = element_text(size = 7)) + theme(legend.key = element_rect(colour = "blue")) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + ggtitle(
        paste(
          gameData1$boxscore$teams$away$team$name,
          "at",
          gameData1$boxscore$teams$home$team$name,
          "-",
          gameData1$linescore$currentPeriodOrdinal,
          "Period",
          "-",
          gameData1$linescore$currentPeriodTimeRemaining
        )
      )
    p <- ggplotly(p, tooltip = "text")
  })
  
  output$TeamStats <- renderTable({
    PBP <- plays()[[1]]
    PBP <- flatten(PBP$plays$allPlays)
    PBP <- PBP[!is.na(PBP$team.triCode), ]
    PBP$result.event <-
      factor(
        PBP$result.event,
        levels = c(
          "Goal",
          "Shot",
          "Penalty",
          "Hit",
          "Faceoff",
          "Blocked Shot",
          "Missed Shot",
          "Takeaway",
          "Giveaway",
          "Expected Goals"
        )
      )
    teams <- unique(PBP$team.triCode)
    PBPTeam1 <- PBP[PBP$team.triCode == teams[[1]], ]
    PBPTeam2 <- PBP[PBP$team.triCode == teams[[2]], ]
    shotPredictionTeam1 <- 0
    if (nrow(PBPTeam1) > 0) {
      for (i in 1:nrow(PBPTeam1)) {
        if (!is.na(PBPTeam1$result.event[[i]]) &&
            (PBPTeam1$result.event[[i]] == "Shot" ||
             PBPTeam1$result.event[[i]] == "Goal")) {
          distX <- 89 - abs(PBPTeam1$coordinates.x[[i]])
          distY <- PBPTeam1$coordinates.y[[i]]
          distances <- sqrt(distX ^ 2 + distY ^ 2)
          tmp.result.secondaryType <-
            PBPTeam1$result.secondaryType[[i]]
          if (is.na(tmp.result.secondaryType)) {
            tmp.result.secondaryType <<- "Wrist Shot"
          }
          angles <- abs(atan(distY / distX))
          predictor <-
            data.frame(distances, angles, tmp.result.secondaryType)
          predictedValue <-
            predict(shotModel, predictor, type = "response")
          if (is.na(predictedValue)) {
            predictedValue = 0.08
          }
          shotPredictionTeam1 <-
            predictedValue + shotPredictionTeam1
        }
      }
    }
    shotPredictionTeam2 <- 0
    if (nrow(PBPTeam2) > 0) {
      for (i in 1:nrow(PBPTeam2)) {
        if (!is.na(PBPTeam2$result.event[[i]]) &&
            (PBPTeam2$result.event[[i]] == "Shot" ||
             PBPTeam2$result.event[[i]] == "Goal")) {
          distX <- 89 - abs(PBPTeam2$coordinates.x[[i]])
          distY <- PBPTeam2$coordinates.y[[i]]
          distances <- sqrt(distX ^ 2 + distY ^ 2)
          tmp.result.secondaryType <-
            PBPTeam2$result.secondaryType[[i]]
          angles <- abs(atan(distY / distX))
          if (is.na(tmp.result.secondaryType)) {
            tmp.result.secondaryType <<- "Wrist Shot"
          }
          predictor <-
            data.frame(distances, angles, tmp.result.secondaryType)
          predictedValue <-
            predict(shotModel, predictor, type = "response")
          if (is.na(predictedValue)) {
            predictedValue = 0.08
          }
          shotPredictionTeam2 <-
            predictedValue + shotPredictionTeam2
        }
      }
    }
    Team1Stats <- table(PBPTeam1$result.event)
    Team2Stats <- table(PBPTeam2$result.event)
    x <- data.frame(Team1Stats, Team2Stats)
    x$Freq[[2]] <- x$Freq[[1]] + x$Freq[[2]]
    x$Freq.1[[2]] <- x$Freq.1[[1]] + x$Freq.1[[2]]
    y <-
      data.frame(x$Var1[x$Var1 != "Expected Goals"], x$Freq[x$Var1 != "Expected Goals"], x$Freq.1[x$Var1 != "Expected Goals"])
    y <-
      rbind(y, c(
        "Expected Goals",
        signif(shotPredictionTeam1, 3),
        signif(shotPredictionTeam2, 3)
      ))
    y <-
      plyr::rename(
        y,
        c(
          "x.Var1.x.Var1.....Expected.Goals.." = "Stat",
          "x.Freq.x.Var1.....Expected.Goals.." = teams[[1]],
          "x.Freq.1.x.Var1.....Expected.Goals.." = teams[[2]]
        )
      )
    y
  }, digits = 3)
  
  output$Player <- renderText({
    dataSet <- plays()[[1]]$boxscore$teams
    PBP <- dataSet$home$onIcePlus
    homeplayers <- "<table align = \"center\"><tr>"
    if (nrow(PBP) > 0) {
      for (i in 1:nrow(PBP)) {
        homeplayers <-
          paste(
            homeplayers,
            "<td><img height = 54 src='https://nhl.bamcontent.com/images/headshots/current/168x168/",
            PBP$playerId[[i]],
            ".jpg'src></td>",
            sep = ""
          )
      }
    }
    homeplayers <- paste(homeplayers, "</tr><tr>")
    if (nrow(PBP) > 0) {
      for (i in 1:nrow(PBP)) {
        homeplayers <- paste(
          homeplayers,
          "
          <td>
          <div class=\"progress\" style=\"width:50px\">
          <div class=\"progress-bar\" role=\"progressbar\" style=\"width:",
          PBP$stamina[[i]],
          "%\">",
          PBP$stamina[[i]],
          "</div>
          </div></td>",
          sep = ""
        )
      }
    }
    homeplayers <- paste(homeplayers, "</tr><tr>")
    PBP <- dataSet$away$onIcePlus
    
    if (nrow(PBP) > 0) {
      for (i in 1:nrow(PBP)) {
        homeplayers <-
          paste(
            homeplayers,
            "<td><img height = 54 src='https://nhl.bamcontent.com/images/headshots/current/168x168/",
            PBP$playerId[[i]],
            ".jpg'src></td>",
            sep = ""
          )
      }
    }
    homeplayers <- paste(homeplayers, "</tr><tr>")
    if (nrow(PBP) > 0) {
      for (i in 1:nrow(PBP)) {
        homeplayers <- paste(
          homeplayers,
          "
          <td>
          <div class=\"progress\" style=\"width:50px\">
          <div class=\"progress-bar\" role=\"progressbar\" style=\"width:",
          PBP$stamina[[i]],
          "%\">",
          PBP$stamina[[i]],
          "</div>
          </div></td>",
          sep = ""
        )
      }
    }
    homeplayers <- paste(homeplayers, "</tr><tr>")
    homeplayers
    })
  
  output$awayRoster <- DT::renderDataTable({
    datatable(isolate(generateTeams()[[2]]), selection = 'single' ,options =  list(dom = 'tp'), class = "compact")
  }, options = list(dom = 'tp'))
  
  output$rosters <- DT::renderDataTable({
    datatable(isolate(generateTeams()[[1]]), selection = 'single' ,options =  list(dom = 'tp'), class = "compact")
  }, options = list(dom = 'tp'))
  
  generateTeams <- reactive({
    PBP <- plays()[[2]]
    homeID <- PBP$teams$home$id
    awayID <- PBP$teams$away$id
    PBP <- PBP$players
    homeRoster <- character(length(PBP))
    homeRosterIds <- character(length(PBP))
    awayRoster <- character(length(PBP))
    awayRosterIds <- character(length(PBP))
    j <- 1
    a <- 1
    if(length(PBP) > 0) {
      for(i in 1:length(PBP)) {
        if(any(names(PBP[[i]]) == "currentTeam")) {
        if(PBP[[i]]$currentTeam$id == homeID) {
          homeRoster[[j]] <- c(PBP[[i]]$fullName)
          homeRosterIds[[j]] <- c(PBP[[i]]$id)
          j <- j+1
        }
          else if(PBP[[i]]$currentTeam$id == awayID) {
            awayRoster[[a]] <- c(PBP[[i]]$fullName)
            awayRosterIds[[a]] <- c(PBP[[i]]$id)
            a <- a+1
          }
        }
      }
    }
    homeRoster <- na.omit(homeRoster[1:(j-1)])
    homeRosterIds <- na.omit(homeRosterIds[1:(j-1)])
    awayRoster <- na.omit(awayRoster[1:(a-1)])
    awayRosterIds <- na.omit(awayRosterIds[1:(a-1)])
    list(data.frame(homeRoster, homeRosterIds), data.frame(awayRoster, awayRosterIds))
  })
  
  proxy1 = dataTableProxy('rosters')
  proxy2 = dataTableProxy('awayRoster')
  
  observe({
    replaceData(proxy1, generateTeams()[[1]], resetPaging = FALSE)
  })
  
  observe({
    replaceData(proxy2, generateTeams()[[2]], resetPaging = FALSE)
  })
  
  output$NHLSite <- renderText({
    "<iframe src=\"https://www.nhl.com\" width = 1300 height=700></iframe>"
  })
  
  observeEvent(input$rosters_rows_selected, {
    PBP <- generateTeams()[[1]]
    link <- paste("https://www.nhl.com/player/", PBP$homeRosterIds[[input$rosters_rows_selected]], sep="")
    output$NHLSite <- renderUI({
      print(link)
      tags$iframe(src = link, width = 1300, height = 800)
    })
  })
  
  observeEvent(input$awayRoster_rows_selected, {
    PBP <- generateTeams()[[2]]
    link <- paste("https://www.nhl.com/player/", PBP$awayRosterIds[[input$awayRoster_rows_selected]], sep="")
    output$NHLSite <- renderUI({
      print(link)
      tags$iframe(src = link, width = 1300, height = 800)
    })
  })
    })


