
visualize.input <- column(width = 3,
                          box(
                            width = NULL,
                            ## title = "Controls",

                            ##   sliderInput("visualize_year", "Year",
                            ##             min = 1995, max = 2011,
                            ##             ## value = 1995,
                            ##             ## value = c(1995, 2011),
                            ##             value = c(2005),
                            ##             step = 1, sep="", # format="#" is old shiny
                            ##             animate = TRUE
                            ##             )
                            ## ,

                              selectInput("visualize_year", "Year",
                                          choices = c(1995, 2000, 2005, 2008, 2009, 2010, 2011),
                                          ## selected = 2005,
                                          selected = 2011,
                                          multiple = FALSE,
                                          selectize = TRUE
                                          )
                             ,

                              selectInput("visualize_data.coef", "Subject", # "Coefficient Data",
                                        choices = c(
                                          "TiVA" = "DATA.ICIOeconCVB" # CVB 
                                          ## DATA.ICIOeconB
                                          ),
                                        selected = "DATA.ICIOeconCVB", multiple = FALSE)
                            ,
                            selectInput("visualize_data.demand", "Demand Data",
                                        choices = c(
                                          "Gross Exports" = "DATA.ICIOeconGRTR", # GRTR
                                          "Final Demand (excl. inventories)" = "DATA.ICIOeconFDTTLexINVNT" # FDTTLexINVNT
                                          ,
                                          "Final Demand" = "DATA.ICIOeconFDTTLdisc" # FDTTLdisc
                                        ),
                                        selected = c("DATA.ICIOeconGRTR"),
                                        multiple = FALSE)
                            ,
                            selectInput("visualize_method", "Result Dimension", # "Calculation Method",
                                        choices = c(
                                            "Source Country and Source Industry" = "couSindS"
                                            # ,
                                            # "Value-added created by imports" = "couXindS" # Domestic VA in exports
                                           ## ,
                                           ##  "backlink"
                                        ),
                                        selected = c("couSindS"),
                                        ## selected = c("backlink"),
                                        multiple = FALSE)
                            ,

                            selectInput("visualize_indX", "Export or Demand Industry",
                                        choices = isolate(names(values$indagg)),
                                        ## selected = "C15T37",
                                        selected = "CTOTAL",
                                        ## selected = "C65T74",
                                        multiple = TRUE)
                            ,


                            selectInput("visualize_couX", "Product Origin or Export Country/Region",
                                        choices = isolate(names(values$couagg)),
                                        ## selected = c("MEX", "CHN"),
                                        selected = "WOR",
                                        multiple = TRUE)
                            ,
                            selectInput("visualize_couD", "Demand Country/Region",
                                        choices = isolate(names(values$couagg)),
                                        ## selected = "NAFTA",
                                        selected = "WOR",
                                        multiple = TRUE) # CAN
                            ,
                            ## selectInput("visualize_charttype", "Chart Type",
                            ##             choices = c("mosaic", "heatmap"),
                            ##             selected = "mosaic",
                            ##             multiple = FALSE)
                            ## ,
                            downloadButton("visualize_download_data", "Download Data (csv)")
                            ,
                            downloadButton("visualize_download_chart", "Download Chart")
                            ,
                            radioButtons("visualize_download_chart_format", "Select Chart Format",
                                         c("PDF", "PNG", "SVG"),
                                         selected = "PDF",
                                         inline = TRUE)
                            ## ,
                            ## helpText("\nChart download for selected time period")
                            ## helpText("\nUse web browser to display SVG files")
                            ,
                            selectInput("visualize_colorscheme", "Color Scheme",
                                        choices = c("continuous", "discrete"),
                                        ## selected = "discrete",
                                        selected = "continuous",
                                        multiple = FALSE)
                            ,
                            checkboxInput("visualize_highlight_y", "Highlight Selection (Industry)",
                                          value = FALSE)
                            ,
                              #library(shiny)
                              conditionalPanel(
                                  condition = "input.visualize_highlight_y == true",
                                  colourInput("visualize_highlight_col", "Select colour", value = "green")
                              )
                             ,
                            checkboxInput("visualize_logval", "Log Values (>= 1)",
                                          value = FALSE)
                            ,
                            checkboxInput("visualize_cellborder", "Cell Borders", value = TRUE)
                            ,
                            checkboxInput("visualize_pivotmatrix", "Swap x- and y-axes", value = FALSE)
                            )
                          )

visualize.output <- column(width = 9,

                           box(width = NULL, title = "Parameters", collapsible = TRUE, collapsed = TRUE,
                               verbatimTextOutput("visualize_summary"))
                           ,
                           box(width = NULL, collapsible = TRUE, collapsed = FALSE,
                               plotOutput("visualize_plot", height = 650)
                               )

                          ,
                           box(width = NULL, collapsible = TRUE, collapsed = FALSE,
                               dimpleOutput("visualize_dimple", height = "650px"),
                               downloadButton("visualize_download_dimple", "Download HTML")
                               )

                           ,
                           box(width = NULL, collapsible = TRUE, collapsed = TRUE,
                               d3heatmapOutput("visualize_heatmap", height = "650px")
                               )
                           ,
                           box(width = NULL, collapsible = TRUE, collapsed = TRUE,
                               scatterplotThreeOutput("visualize_scatterplot", height = "650px")
                               )

                           )



