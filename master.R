## shiny::runApp(file.path(dbpath, "GitHub", "icioapp2015"), launch.browser = TRUE, port = 3838)
## source(file.path(dbpath, "GitHub", "icioapp2015", "master.R"))

## install.packages("shinydashboard")
## detach("package:shiny", unload = TRUE)
## detach("package:stan", unload = TRUE)
## install.packages("shiny")

## install.packages("DT")
## devtools::install_github("rstudio/DT")
if (Sys.info()[["user"]]%in%c("werth_b", "z930")) {
    setwd(file.path(dbpath, "GitHub", "icioapp2015"))
} else {
    setwd(file.path('c:/Temp/Dropbox/icioapp2015/'))
}
## testingRadiant <- FALSE
require(shiny)
## source("global.R")
## require(shinydashboard)
## require(DT)
## sessionInfo()

source("app.R")

shinyApp(ui, server)

