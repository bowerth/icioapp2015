

about.output <- list(
  ## h3("ICIO Indicator 2015")
  ## ,
  shiny::p("Version 1.0 (01/07/2015)")
   ,
  shiny::p("Author:", a(href = "http://www.oecd.org/sti/", "OECD STI"))
  ,
  shiny::p("Contact:", a(href = "mailto:stan.contact@oecd.org?subject=ICIO%20Indicators", "stan.contact@oecd.org"))
  ,
  shiny::p("Source code and documentation on",
    a(href = "https://github.com/bowerth/icioapp2015", "GitHub"))
  ,
  shiny::p("Built using", a(href = "http://www.rstudio.com/shiny/", "shiny"),
    "and", a(href = "http://rstudio.github.io/shinydashboard/", "shinydashboard"),
    "by", a(href = "http://www.rstudio.com/", "RStudio"))
  ,
  ## shiny::p("Featured R packages:", a(href = "https://github.com/rstudio/d3heatmap/", "d3heatmap"))
  ## ,
  shiny::p("Industry list using", a(href = "https://fortawesome.github.io/Font-Awesome/", "Font-Awesome"), "icons")
  )
