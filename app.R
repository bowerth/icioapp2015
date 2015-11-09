
## app.R ##
library(shiny)
## devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)

## devtools::install_github("rstudio/d3heatmap")
library(d3heatmap)
library(threejs)

library(rcdimple)

library(dplyr)
library(tidyr)

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

PUBSERVER <- "AZ-ICIO-1"
## PUBSERVER <- "7D-OECDAH57"

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


## library(shiny)
## library(shinydashboard)

## sidebarMenu(
##     menuItem("Visualize indicators", tabName = "visualize", icon = icon("th")),
##     menuItem("Visualize indicators (internal)", tabName = "visualize_internal", icon = icon("th"))
## )
## <ul class="sidebar-menu">
##   <li>
##     <a href="#shiny-tab-visualize" data-toggle="tab" data-value="visualize">
##       <i class="fa fa-th"></i>
##       <span>Visualize indicators</span>
##     </a>
##   </li>
##   <li>
##     <a href="#shiny-tab-visualize_internal" data-toggle="tab" data-value="visualize_internal">
##       <i class="fa fa-th"></i>
##       <span>Visualize indicators (internal)</span>
##     </a>
##   </li>
## </ul>

## sidebarmenu <- sidebarMenu(
##         menuItem("Visualize indicators", tabName = "visualize", icon = icon("th"))
## )

## menuitems <- list(menuItem("Visualize indicators", tabName = "visualize", icon = icon("th"))
##                  ## ,
##                   ## menuItem("Visualize indicators (internal)", tabName = "visualize_internal", icon = icon("th"))
##                   )

## menuitems <- c(menuitems,
##                list(menuItem("Visualize indicators (internal)", tabName = "visualize_internal", icon = icon("th")))
##                )
## sidebarMenu(menuitems)


menuitems <- list(
    menuItem("Visualize indicators", tabName = "visualize", icon = icon("th"))
)


if (Sys.info()[["nodename"]]!=PUBSERVER) {
    menuitems <- c(menuitems,
                   list(menuItem("Visualize indicators (internal)", tabName = "visualize_internal", icon = icon("th")))
                   )
}

menuitems <- c(menuitems,
               list(
                   menuItem("Gross Exports related indicators", tabName = "grossexports", icon = icon("th"))
                  ,
                   menuItem("Final Demand related indicators", tabName = "finaldemand", icon = icon("th"))
                  ,
                   menuItem("About", tabName = "about", icon = icon("info")) # info-circle
                   )
               )

sidebar <- dashboardSidebar(
    sidebarMenu(menuitems)
)


## sidebar <- dashboardSidebar(
##     sidebarMenu(
##         ## http://fortawesome.github.io/Font-Awesome/icons/
##         ## menuItem("Backward Linkage", tabName = "backlink", icon = icon("th"))
##         ## ,

##         menuItem("Visualize indicators", tabName = "visualize", icon = icon("th"))
##         ,
##         menuItem("Visualize indicators (internal)", tabName = "visualize_internal", icon = icon("th"))

##        ## ,
##        ##  menuItem("3D Scatterplot", tabName = "scatterplot", icon = icon("th"))
##        ## ,
##        ##  menuItem("D3 Heatmap", tabName = "heatmap", icon = icon("th"))
##        ## ,
##        ##  menuItem("Marimekko Plot", tabName = "dimple", icon = icon("th"))

##        ## ,
##        ##  menuItem("Parameters", tabName = "parameter", icon = icon("th"))
##        ,
##         menuItem("Gross Exports related indicators", tabName = "grossexports", icon = icon("th"))
##        ,
##         menuItem("Final Demand related indicators", tabName = "finaldemand", icon = icon("th"))
##        ,
##         menuItem("About", tabName = "about", icon = icon("info")) # info-circle
##        ## ,

##        ##  selectInput("sidebar_data.coef", "Subject", # "Coefficient Data",
##        ##              choices = c(
##        ##                  "TiVA" = "DATA.ICIOeconCVB" # CVB
##        ##                  ## DATA.ICIOeconB
##        ##              ),
##        ##              selected = "DATA.ICIOeconCVB", multiple = FALSE)
##        ## ,
##        ##  selectInput("sidebar_year", "Year",
##        ##              choices = c(1995, 2000, 2005, 2008, 2009, 2010, 2011),
##        ##              ## selected = 2005,
##        ##              selected = 2011,
##        ##              multiple = FALSE,
##        ##              selectize = TRUE
##        ##              )
##        ## ,
##        ##  selectInput("sidebar_data.demand", "Demand Data",
##        ##              choices = c(
##        ##                  "Gross Exports" = "DATA.ICIOeconGRTR", # GRTR
##        ##                  "Final Demand (excl. inventories)" = "DATA.ICIOeconFDTTLexINVNT" # FDTTLexINVNT
##        ##                  ## ,
##        ##                  ## "Final Demand" = "DATA.ICIOeconFDTTLdisc" # FDTTLdisc
##        ##              ),
##        ##              selected = c("DATA.ICIOeconGRTR"),
##        ##              multiple = FALSE)
##        ## ,
##        ## ##  selectInput("sidebar_method", "Result Dimension", # "Calculation Method",
##        ## ##              choices = c(
##        ## ##                  "Source Country and Source Industry" = "couSindS"
##        ## ##                                  # ,
##        ## ##                                  # "Value-added created by imports" = "couXindS" # Domestic VA in exports
##        ## ##                  ## ,
##        ## ##                  ##  "backlink"
##        ## ##              ),
##        ## ##              selected = c("couSindS"),
##        ## ##              ## selected = c("backlink"),
##        ## ##              multiple = FALSE)
##        ## ## ,

##        ##  uiOutput("global_indX")
##        ## ,
##        ##  uiOutput("global_couX")
##        ## ,
##        ##  uiOutput("global_couD")
##        ## ,
##        ##  downloadButton("global_download_data", "Download Data (csv)")
##        ## ,
##        ##  checkboxInput("sidebar_pivotmatrix", "Swap x- and y-axes", value = FALSE)


##     )
## )

## source(file.path("widgets", "backlink_ui.R"), local = TRUE)
source(file.path("widgets", "grossexports_ui.R"), local = TRUE)
source(file.path("widgets", "finaldemand_ui.R"), local = TRUE)

source(file.path("widgets", "visualize_ui.R"), local = TRUE)
source(file.path("widgets", "visualize_internal_ui.R"), local = TRUE)
## source(file.path("widgets", "parameter_ui.R"), local = TRUE)
## source(file.path("widgets", "dimple_ui.R"), local = TRUE)
## source(file.path("widgets", "scatterplot_ui.R"), local = TRUE)
## source(file.path("widgets", "heatmap_ui.R"), local = TRUE)

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

     ,
      tabItem(tabName = "visualize_internal",
              fluidRow(
                  ## visualize.input.internal,
                  visualize_internal.output)
              )

      ##   ,
    ## tabItem(tabName = "backlink",
    ##             fluidRow(
    ##                 backlink.input,
    ##                 backlink.output)
    ##             )
    ## ,

      ## ,
      ## tabItem(tabName = "parameter",
      ##         fluidRow(
      ##             parameter.output)
      ##         )

     ## ,
     ##  tabItem(tabName = "dimple",
     ##          fluidRow(
     ##              dimple.output)
     ##          )
     ##  ,
     ##  tabItem(tabName = "scatterplot",
     ##          fluidRow(
     ##              scatterplot.output)
     ##          )
     ##  ,
     ##  tabItem(tabName = "heatmap",
     ##          fluidRow(
     ##              heatmap.output)
     ##          )

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

    source(file.path("widgets", "message_industry.R"), local = TRUE)

    ## source(file.path("widgets", "global_server.R"), local = TRUE)
    source(file.path("widgets", "grossexports_server.R"), local = TRUE)
    source(file.path("widgets", "finaldemand_server.R"), local = TRUE)

    source(file.path("widgets", "visualize_server.R"), local = TRUE)
    if (Sys.info()[["nodename"]]!=PUBSERVER) {
        source(file.path("widgets", "visualize_internal_server.R"), local = TRUE)
    }
    ## source(file.path("widgets", "backlink_server.R"), local = TRUE)
    ## source(file.path("widgets", "parameter_server.R"), local = TRUE)
    ## source(file.path("widgets", "dimple_server.R"), local = TRUE)
    ## source(file.path("widgets", "scatterplot_server.R"), local = TRUE)
    ## source(file.path("widgets", "heatmap_server.R"), local = TRUE)


}

shinyApp(ui, server)
