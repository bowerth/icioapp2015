grossexports.input <- column(width = 3,
                   box(
                       width = NULL,
                       ## title = "Controls",
                       ## sliderInput("slider", "Number of observations:", 1, 100, 50)

                       ## #japanese VA by Japanese FD

                       selectInput("grossexports_year", "Year",
                                   choices = c(1995:2011), selected = 2005)
                      ,
                       selectInput("grossexports_indX", "Export Industry",
                                   ## choices = c(1:isolate(values[["noind"]])),
                                   ## selected = c(1, 3),
                                   choices = isolate(names(values$indagg)),
                                   selected = "C15T37",
                                   multiple = TRUE)
                      ,

                       selectInput("grossexports_couX", "Export Country/Region",
                                   ## choices = c(isolate(values$couagg)), selected = "OECD", multiple = TRUE) # USA
                                   choices = isolate(names(values$couagg)),
                                   ## selected = "WOR",
                                   selected = c("MEX", "CHN"),
                                   multiple = TRUE) # USA
                       ## class(isolate(values$couagg))
                       ## give only 62 selection,
                       ##   if MEX or CHN selected, create region of 62 + x
                      ,
                       selectInput("grossexports_couD", "Demand Country/Region",
                                   ## choices = isolate(values$couagg),
                                   choices = isolate(names(values$couagg)),
                                   selected = "NAFTA", multiple = TRUE) # CAN
                      ,
                       radioButtons("grossexports_bysource", "Result Source Dimension",
                                    choices = list(
                                        "Industry" = "ind",
                                        "Country" = "cou"))
                       ,
                     htmlOutput("uiGe_bysource")
                     ,
                       downloadButton('grossexports_download_data', 'Download Data (csv)')

                   )
                   )

grossexports.output <- column(width = 9,
                              box(title = "Parameters", verbatimTextOutput("grossexports.summary"), width = NULL, collapsible = TRUE)
                              ,
                              box(plotOutput("grossexports.barplot", height = 250), width = NULL, collapsible = TRUE)
                              ,
                              ## box(DT::dataTableOutput("grossexports.datatable"), width = NULL)
                              box(dataTableOutput("grossexports.datatable"), width = NULL)
                    )

## ###########
## GRTR inputs
## ###########

## ## indX = 2 # exports of product 2 = AUS C10T14 to world
## ## indX = NULL # all 34? industries (51?)
## indX = c(1:34)
## indX <- c(1,3)
## indS = c(1:34)

## couS = 21 # Mexico
## couD = 4 # Canada
## couX = 34 # USA


##     ## head(DATA.ICIOeconGRTR[1,1,])

## ### GRTR

## ### FDTT
## #check global balance

