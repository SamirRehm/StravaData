## ui.R ##
library(shinydashboard)
library(plotly)

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
      menuItem("Current Week", icon = icon("th"), tabName = "currWeek")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'allTime', box(plotlyOutput("runs", height = 300), width = 12)),
      tabItem(tabName = 'currWeek', box(plotlyOutput("currentWeekActivities", height = 300), width = 12))
    )
  )
)