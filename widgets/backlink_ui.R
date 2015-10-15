backlink.input <- column(width = 3,
                   box(
                       width = NULL,
                       ## title = "Controls",
                       ## sliderInput("slider", "Number of observations:", 1, 100, 50)

                       ## #japanese VA by Japanese FD

                      ##  selectInput("backlink_year", "Year",
                      ##              choices = c(1995:2011), selected = 2005)
                      ## ,
                     ## sliderInput("backlink_year", "Year",
                     ##             min = 1995, max = 2011,
                     ##             value = c(2005),
                     ##             step = 1, sep="",
                     ##             animate = TRUE
                     ##             )
                     ## ,
                     selectInput("backlink_year", "Year",
                                 choices = c(1995, 2000, 2005, 2008, 2009, 2010, 2011),
                                 selected = 2005,
                                 multiple = FALSE,
                                 selectize = TRUE
                                 )
                     ,
                     selectInput("backlink_indX", "Export Industry",
                                   ## choices = c(1:isolate(values[["noind"]])),
                                   ## selected = c(1, 3),
                                   choices = isolate(names(values$indagg)),
                                   selected = "C15T37",
                                   multiple = TRUE)
                      ,

                       selectInput("backlink_couX", "Export Country/Region",
                                   ## choices = c(isolate(values$couagg)), selected = "OECD", multiple = TRUE) # USA
                                   choices = isolate(names(values$couagg)),
                                   ## selected = "WOR",
                                   selected = c("MEX", "CHN"),
                                   multiple = TRUE) # USA
                       ## class(isolate(values$couagg))
                       ## give only 62 selection,
                       ##   if MEX or CHN selected, create region of 62 + x
                      ,
                       selectInput("backlink_couD", "Demand Country/Region",
                                   ## choices = isolate(values$couagg),
                                   choices = isolate(names(values$couagg)),
                                   selected = "NAFTA", multiple = TRUE) # CAN
                      ,
                       radioButtons("backlink_bysource", "Result Source Dimension",
                                    choices = list(
                                        "Industry" = "ind",
                                        "Country" = "cou"))
                       ,
                     htmlOutput("uiBl_bysource")
                     ,
                       downloadButton('backlink_download_data', 'Download Data (csv)')

                   )
                   )

backlink.output <- column(width = 9,
                              box(title = "Parameters", verbatimTextOutput("backlink.summary"), width = NULL, collapsible = TRUE)
                              ,
                              box(plotOutput("backlink.barplot", height = 250), width = NULL, collapsible = TRUE)
                              ,
                              ## box(DT::dataTableOutput("backlink.datatable"), width = NULL)
                              box(dataTableOutput("backlink.datatable"), width = NULL)
                    )
