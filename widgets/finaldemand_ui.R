finaldemand.input <- column(width = 3,
                   box(
                       width = NULL,
                       ## title = "Controls",
                       ## sliderInput("slider", "Number of observations:", 1, 100, 50)

                      ##  selectInput("finaldemand_year", "Year",
                      ##              choices = c(1995:2011),
                      ##              selected = 2005)
                      ## ,
                     sliderInput("finaldemand_year", "Year",
                                 min = 1995, max = 2011,
                                 value = c(2005),
                                 step = 1, sep="",
                                 animate = TRUE
                                 )
                     ,
                     ## #japanese VA by Japanese FD
                       selectInput("finaldemand_indX", "Demand Industry",
                                   ## choices = c(1:isolate(values[["noind"]])), selected = c(1, 3),
                                   ## multiple = TRUE),
                                   choices = isolate(names(values$indagg)),
                                   selected = "C15T37",
                                   multiple = TRUE)
                       ,
                       ## selectInput("finaldemand_indS", "Source Industry", choices = c(1:noind), selected = c(1:34), multiple = TRUE),
                       selectInput("finaldemand_couX", "Product Origin Country/Region",
                                   choices = isolate(names(values$couagg)),
                                   ## selected = "OECD"), # USA
                                   selected = c("MEX", "CHN"),
                                   multiple = TRUE),
                       selectInput("finaldemand_couD", "Demand Country/Region",
                                   choices = isolate(names(values$couagg)),
                                   ## selected = "WOR"), # JPN
                                   selected = "NAFTA", multiple = TRUE) # CAN
                       ,
                       ## selectInput("finaldemand_couS", "Source Country",
                       ##             choices = isolate(names(values$couagg)),
                       ##             selected = "OECD"), # JPN
                       radioButtons("finaldemand_bysource", "Result Source Dimension",
                                    choices = list(
                                        "Industry" = "ind",
                                        "Country" = "cou"))
                      ,
                       htmlOutput("uiFd_bysource")
                     ,
                       downloadButton('finaldemand_download_data', 'Download Data (csv)')

                   )
                   )

finaldemand.output <- column(width = 9,
                    ## box(plotOutput("plot1", height = 250), width = NULL)
                    ## box(dataTableOutput("finaldemand.datatable"), width = NULL)
                             box(title = "Parameters", verbatimTextOutput("finaldemand.summary"), width = NULL, collapsible = TRUE)
                              ,
                              box(plotOutput("finaldemand.barplot", height = 250), width = NULL, collapsible = TRUE)
                             ,
                             ## box(DT::dataTableOutput("finaldemand.datatable"), width = NULL)
                             box(dataTableOutput("finaldemand.datatable"), width = NULL)
                    )



