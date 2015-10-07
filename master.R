## shiny::runApp(file.path(dbpath, "GitHub", "icioapp2015"), launch.browser = FALSE, port = 3838)
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
require(shinydashboard)
## require(DT)
## sessionInfo()

## check md5 sums
## https://stat.ethz.ch/R-manual/R-devel/library/tools/html/md5sum.html
## library(tools)
## tools::md5sum(files = file.path("data", "DATA.ICIOeconCVB.Rdata"))
## tools::md5sum(files = file.path("data", "DATA.ICIOeconFDTTLexINVNT.Rdata"))
## tools::md5sum(file.path("data", "DATA.ICIOeconGRTR.Rdata"))


source("app.R")

shinyApp(ui, server)

