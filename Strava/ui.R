## ui.R ##
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      id='tab',
      menuItem("All-Time Stats", icon = icon("th"), tabName = "dashBoard")
    )
  ),
  dashboardBody(
    box(plotlyOutput("runs", height = 300), width = 12)
  )
)