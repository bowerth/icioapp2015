
## app.R ##
library(shiny)
## devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)

source("global.R")

## include OECD logo in header
## ?dashboardHeader

## layout demo: https://almsaeedstudio.com/AdminLTE

dashboardHeader2 <- function(..., title = NULL, titleWidth = NULL, disable = FALSE, .list = NULL) {
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li", class = "dropdown")
##
  titleWidth <- validateCssUnit(titleWidth)
##
  # Set up custom CSS for custom width.
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    # This CSS is derived from the header-related instances of '230px' (the
    # default sidebar width) from inst/AdminLTE/AdminLTE.css. One change is that
    # instead making changes to the global settings, we've put them in a media
    # query (min-width: 768px), so that it won't override other media queries
    # (like max-width: 767px) that work for narrower screens.
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_", titleWidth, fixed = TRUE, '
       @media (min-width: 768px) {
      /*  .main-header > .navbar {
          margin-left: _WIDTH_;
        }
      */
        .main-header .logo {
          /* width: _WIDTH_; */
          background: url(\'OECD_white.png\') no-repeat center center;
          background-size: 100px;
        }

       /* changing .logo:hover seems to have no effect - logo disappears when hovering over... */

        .main-header .navbar-custom-title {
          text-align: center;
          color: white;
          /* copy from .main-header .logo */
         display: block;
         float: left;
         font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
         font-size: 20px;
         font-weight: 300;
         height: 50px;
         line-height: 50px;
         overflow: hidden;
         padding: 0 15px;
        }

      }
    '))))
  }
##
  tags$header(class = "main-header",
              custom_css,
              style = if (disable) "display: none;",
              ## span(class = "logo", title),
              a(href = "http://10.101.26.220/kit/", span(class = "logo")),
              tags$nav(class = "navbar navbar-static-top", role = "navigation",
                       ## Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       ## Sidebar toggle button
                       a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
                         role="button",
                         span(class="sr-only", "Toggle navigation")
                         ),
                       div(class = "navbar-custom-title", "ICIO Indicator 2015"),
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   items
                                   )
                           )
                       )
              )
##
}
header <- dashboardHeader2(titleWidth='17%') # titleWidth ignored
## header <- dashboardHeader2(title = "test", titleWidth='17%')



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
        menuItem("Gross Exports", tabName = "grossexports", icon = icon("th"))
        ,
        menuItem("Final Demand", tabName = "finaldemand", icon = icon("th"))
        ,
        menuItem("Visualize", tabName = "visualize", icon = icon("th"))
       ,
        menuItem("About", tabName = "about", icon = icon("info")) # info-circle
        )
)

source(file.path("widgets", "grossexports_ui.R"), local = TRUE)
source(file.path("widgets", "finaldemand_ui.R"), local = TRUE)
source(file.path("widgets", "visualize_ui.R"), local = TRUE)
source(file.path("widgets", "about_ui.R"), local = TRUE)

body <- dashboardBody(
  ##   tags$head(
  ##     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ## )
  ## ,
  ## includeCSS("www/custom.css")
  ## ,
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

    source(file.path("widgets", "grossexports_server.R"), local = TRUE)
    source(file.path("widgets", "finaldemand_server.R"), local = TRUE)
    source(file.path("widgets", "visualize_server.R"), local = TRUE)
    ## source(file.path("widgets", "about_server.R"), local = TRUE)

}

shinyApp(ui, server)
