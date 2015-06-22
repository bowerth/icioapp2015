
## app.R ##
library(shiny)
library(shinydashboard)

source("global.R")

header <- dashboardHeader(
  ## title = "VA BSC"
  title = "ICIO Indicator 2015"
  )

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Gross Exports", tabName = "grossexports", icon = icon("th"))
        ,
        menuItem("Final Demand", tabName = "finaldemand", icon = icon("th"))
        ,
        menuItem("Visualize", tabName = "visualize", icon = icon("th"))
       ## ,
        )
)

source(file.path("widgets", "grossexports_ui.R"), local = TRUE)
source(file.path("widgets", "finaldemand_ui.R"), local = TRUE)
source(file.path("widgets", "visualize_ui.R"), local = TRUE)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "grossexports",
                fluidRow(
                    grossexports.input,
                    grossexports.output)
                )
       ,
        tabItem(tabName = "finaldemand",
                fluidRow(
                    finaldemand.input,
                    finaldemand.output)
                )
       ,
        tabItem(tabName = "visualize",
                fluidRow(
                    visualize.input,
                    visualize.output)
                )
    )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
  )

server <- function(input, output) {

    source(file.path("widgets", "grossexports_server.R"), local = TRUE)
    source(file.path("widgets", "finaldemand_server.R"), local = TRUE)
    source(file.path("widgets", "visualize_server.R"), local = TRUE)

}

shinyApp(ui, server)
