
visualize.input <- column(width = 3,
                   box(
                       width = NULL,
                       ## title = "Controls",
                     sliderInput("visualize_year", "Year",
                                 min = 1995, max = 2011,
                                 ## value = 1995,
                                 value = c(1995, 2011),
                                 step = 1, sep="", # format="#" is old shiny
                                 animate = TRUE)
                     ,
                     selectInput("visualize_data.coef", "Coefficient Data",
                                   choices = c("CVB" = "DATA.ICIOeconCVB"),
                                 selected = "DATA.ICIOeconCVB", multiple = FALSE)
                     ,
                       selectInput("visualize_data.demand", "Demand Data",
                                   choices = c(
                                     "GRTR" = "DATA.ICIOeconGRTR",
                                     "FDTTLexINVNT" = "DATA.ICIOeconFDTTLexINVNT"),
                                   selected = c("DATA.ICIOeconGRTR"), multiple = FALSE)
                     ,
                       selectInput("visualize_method", "Calculation Method",
                                   choices = c("couSindS", "couXindS"),
                                   selected = c("couSindS"), multiple = FALSE)
                     ,

                       selectInput("visualize_indX", "Export or Demand Industry",
                                   choices = isolate(names(values$indagg)),
                                   selected = "C15T37",
                                   multiple = TRUE)
                     ,


                     selectInput("visualize_couX", "Product Origin or Export Country/Region",
                                 choices = isolate(names(values$couagg)),
                                 selected = c("MEX", "CHN"),
                                 multiple = TRUE)
                     ,
                     selectInput("visualize_couD", "Demand Country/Region",
                                 choices = isolate(names(values$couagg)),
                                 selected = "NAFTA", multiple = TRUE) # CAN
                     ,
                       downloadButton('visualize_download_data', 'Download Data (csv)')
                     ,
                     downloadButton('visualize_download_chart', 'Download Charts (pdf)')
                       ,
                       selectInput('visualize_colorscheme', 'Color Scheme',
                                   choices = c("continuous", "discrete"),
                                   ## selected = "blues",
                                   selected = "discrete",
                                   multiple = FALSE)
                     ,
                       checkboxInput('visualize_cellborder', 'Cell Borders', value = TRUE)
                       ,
                       checkboxInput('visualize_pivotmatrix', 'Swap x- and y-axes', value = FALSE)
                      ,

                     helpText('\nChart download for selected time period')
                   )
                   )

visualize.output <- column(width = 9,
                           box(title = "Parameters", verbatimTextOutput("visualize.summary"), width = NULL, collapsible = TRUE)
                           ,
                           box(plotOutput("visualize.plot", height = 650), width = NULL)
                           )



