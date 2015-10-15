
## app.R ##
library(shiny)
## devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)
## devtools::install_github("rstudio/d3heatmap")
## library(d3heatmap)
## library(threejs)


## devtools::install_github("daattali/shinyjs")
library(shinyjs)

source("global.R")

## include OECD logo in header
## ?dashboardHeader

## layout demo: https://almsaeedstudio.com/AdminLTE

source(file.path("R", "dropdownMenu2.R"), local = TRUE)
source(file.path("R", "dashboardHeader2.R"), local = TRUE)

## use modified function to include link
## header <- dashboardHeader2(titleWidth='17%') # titleWidth ignored
## header <- dashboardHeader2(titleWidth='17%', dropdownMenuOutput("messageMenu")) # titleWidth ignored
header <- dashboardHeader2(dropdownMenuOutput("messageMenu")) # use custom.css for modifications to preserver after window resizing



## header[4]

## header <- dashboardHeader(
##   titleWidth="50%"
##   ## tags$head(
##       ## tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
##       ## )
##   ## title = "VA BSC"
##   ## title = "ICIO Indicator 2015"
##   )



## header[3]$children[[1]]
## header[3]$children[[1]] <- sub("logo", "oecd-logo", header[3]$children[[1]])
## class(header[3]$children[[1]]) <- "shiny.tag"
## class(header)
## header <- sub("logo", "oecd-logo", header)
## class(header) <- "shiny.tag"

sidebar <- dashboardSidebar(
  sidebarMenu(
    ## http://fortawesome.github.io/Font-Awesome/icons/
      ## menuItem("Backward Linkage", tabName = "backlink", icon = icon("th"))
      ## ,
        menuItem("Visualize indicators", tabName = "visualize", icon = icon("th"))
       ,
        menuItem("Gross Exports related indicators", tabName = "grossexports", icon = icon("th"))
        ,
        menuItem("Final Demand related indicators", tabName = "finaldemand", icon = icon("th"))
        ,
        menuItem("About", tabName = "about", icon = icon("info")) # info-circle
        )
)

## source(file.path("widgets", "backlink_ui.R"), local = TRUE)
source(file.path("widgets", "grossexports_ui.R"), local = TRUE)
source(file.path("widgets", "finaldemand_ui.R"), local = TRUE)
source(file.path("widgets", "visualize_ui.R"), local = TRUE)
source(file.path("widgets", "about_ui.R"), local = TRUE)

body <- dashboardBody(
  ##   tags$head(
  ##     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ## )
  ## ,
  includeCSS("www/custom.css")
  ,
  ## tags$head(tags$style(HTML('
  ##     .main-header .logo {
  ##       font-family: "Georgia", Times, "Times New Roman", serif;
  ##       font-weight: bold;
  ##       font-size: 24px;
  ##     }
  ##   ')))
  ##       <div id="logo"><a href="{{ site.url }}/kit/" data-escp="">ICIO Calculation</a></div>

  tabItems(
    tabItem(tabName = "about",
            about.output)
    ,

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
    ##   ,
    ## tabItem(tabName = "backlink",
    ##             fluidRow(
    ##                 backlink.input,
    ##                 backlink.output)
    ##             )
    ## ,

    )
  )

ui <- dashboardPage(
  ##   tags$head(
  ##     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ## )
  ## ,
  header = header,
  sidebar = sidebar,
  body = body,
  title = "icioapp2015" # title shown in the browser's title bar
  )

server <- function(input, output) {

    ## source(file.path("widgets", "backlink_server.R"), local = TRUE)
    source(file.path("widgets", "grossexports_server.R"), local = TRUE)
    source(file.path("widgets", "finaldemand_server.R"), local = TRUE)
    source(file.path("widgets", "visualize_server.R"), local = TRUE)
    source(file.path("widgets", "message_industry.R"), local = TRUE)

}

shinyApp(ui, server)
