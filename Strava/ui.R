## ui.R ##
library(shinydashboard)
library(plotly)
library(DT)
title <- tags$a(tags$img(src="strava.png", height = '50', width = '150'), target="_blank")

dashboardPage(
  title = 'Training Stats',
  dashboardHeader(
    title = title
  ),
  dashboardSidebar(
    sidebarMenu(
      id='tab',
      menuItem("All-Time Stats", icon = icon("th"), tabName = "allTime"),
      selectizeInput(
        inputId = "Week",
        label = "Pick Week",
        choices = NULL
      ),
      menuItem("Current Week", icon = icon("th"), tabName = "currWeek"),
      menuItem("Activity Stats", icon = icon("th"), tabName = "activity")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'allTime', box(plotlyOutput("runs", height = 300), width = 12)),
      tabItem(tabName = 'currWeek', 
              fluidRow(
                column(
                  width = 8,
                  box(plotlyOutput("currentWeekActivities"), width = 12)),
              column(
                width = 4,
                  box(plotlyOutput("pieChart"), width = 12)
              )
      ),
      fluidRow(
        column(
          width = 2, offset = 0, style='padding:0px;',
          infoBoxOutput("kmsBox", width = 12),
          infoBoxOutput("ActiveTimeBox", width=12)
        ),
        column(
          width=3, offset = 0, style='padding:0px;',
          box(plotlyOutput("runPieChart", height = 280), width=12)
        ),
        column(
          width = 7, offset = 0, style='padding:0px;',
          box(dataTableOutput("WeekSummary"), width = 12)
        )
    )
  ),
  tabItem(tabName="activity",
          fluidRow(
            column(
              width = 10,
              box(plotlyOutput("activityPacePlot", height=250), width = 12)
            ),
            column(
              width = 2,
              box(sliderInput("Smooth factor", "Smoothening Factor", 0, 2, 1, step = 0.1), width = 12)
            )
          )
  )
  )
)
)