## ################## ##
## reactive functions ##
## ################## ##
visualize.couX <- reactive({
  couX <- unique(unname(unlist(values$couagg69[input$visualize_couX])))
  return(couX)
})
visualize.indX <- reactive({
    indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))
    return(indX)
})
visualize.couD <- reactive({
  couD <- unique(unname(unlist(values$couagg[input$visualize_couD])))
  return(couD)
})

visualize.data <- reactive({
  ## year <- as.numeric(input$visualize_year)
  year <- as.numeric(input$visualize_year[1])
  ##
  data.coef <- values[[input$visualize_data.coef]][[1]]
  data.demand <- values[[input$visualize_data.demand]][[1]]
  ##
  couX <- visualize.couX()
  indX <- visualize.indX()
  couD <- visualize.couD()
  ##
  visualize_method <- input$visualize_method
  ##
  visualize.data <- .visualize.data(data.coef = data.coef,
                                    data.demand = data.demand,
                                    year = year,
                                    couX = couX,
                                    indX = indX,
                                    couD = couD,
                                    visualize_method = visualize_method,
                                    convRegCou = convRegCou,
                                    NameInd34_agg = NameInd34_agg)
  ##
  return(visualize.data)
})

visualize.param <- reactive({
  param <- list(
    paste(
      ## Year = input$visualize_year[1],
      'Coefficient =', input$visualize_data.coef,
      '; Demand =', input$visualize_data.demand,
      '; Calculation =', input$visualize_method),
    paste(
      'ExportCou =', toString(input$visualize_couX),
      '; ExportInd =', toString(input$visualize_indX),
      '; DemandCou =', toString(input$visualize_couD))
    )
  return(param)
})

visualize.palette <- reactive({
    if (input$visualize_pivotmatrix == TRUE) { # put countries in rows and industries in columns
        selected_y <- visualize.couD()
        obs_y <- values$nocou
    } else {
        selected_y <- visualize.indX()
        obs_y <- values$noind
    }
    palette <- .visualize.color(colorscheme = input$visualize_colorscheme,
                                highlight_y  = input$visualize_highlight_y,
                                highlight_col = input$visualize_highlight_col,
                                selected_y = selected_y,
                                obs_y = obs_y)
    return(palette)
})

visualize.title <- reactive({
    if (input$visualize_method=="couXindS") {
        ## title <- "Domestic VA in Exports"
        title <- "Value-added created by imports"
    } else if (input$visualize_method=="couSindS") {
        title <- "VA by Source Country and Source Industry"
        ## } else if (input$visualize_method=="backlink") {
        ##     title <- "Backward Linkage weighted by total Final Demand"
    }
    return(title)
})

## ##################### ##
## create output objects ##
##      render UI        ##
## ##################### ##
output$visualize_indX <- renderUI ({
    label <- ifelse(input$visualize_data.demand=="DATA.ICIOeconGRTR",
                    "Export Industry",
                    "Demand Industry")
    selectInput("visualize_indX", label, # "Export or Demand Industry",
                choices = isolate(names(values$indagg)),
                ## selected = "CTOTAL",
                selected = "C15T37",
                multiple = TRUE)
})
output$visualize_couX <- renderUI ({
    label <- ifelse(input$visualize_data.demand=="DATA.ICIOeconGRTR",
                    "Export Country (or Region)",
                    "Product Origin Country (or Region)")
    selectInput("visualize_couX", label, # "Product Origin or Export Country/Region"
                choices = isolate(names(values$couagg)),
                selected = "WOR",
                multiple = TRUE)
})

output$visualize_summary <- renderPrint({
    couD <- visualize.couD()
    couX <- visualize.couX()
    indX <- visualize.indX()
    ##
    blurb <- paste(
        ## paste('Year =', input$visualize_year),
        paste('Year =', input$visualize_year[1]),
        paste('Export or Demand Industry =', toString(indX)),
        paste('Product Origin or Export Country =', toString(couX)),
        paste('Demand Country =', toString(couD)),
        sep = '\n')
    ## if (input$visualize_bysource=="ind") {
    ##   couS <- unique(unname(unlist(values$couagg[input$visualize_couS])))
    ##   blurb <- paste(blurb,
    ##               paste('Source Country =', toString(couS)),
    ##               sep = '\n')
    ## } else if (input$visualize_bysource=="cou") {
    ##   indS <- unique(unname(unlist(values$indagg[input$visualize_indS])))
    ##   blurb <- paste(blurb,
    ##               paste('Source Industry =', toString(indS)),
    ##               sep = '\n')
    ## }
    return(cat(blurb))
})

## visualize_plot <- reactive({
##   if (sum(visualize.data())==0) return()
##   .visualize.plot(input.visualize_method = input$visualize_method,
##                   visualize.data = visualize.data(),
##                   title = visualize.title(),
##                   ## input.visualize_year = input$visualize_year
##                   input.visualize_year = input$visualize_year[1],
##                   input.visualize_cellborder = input$visualize_cellborder,
##                   indX = visualize.indX(),
##                   couD = visualize.couD(),
##                   noind = values$noind,
##                   nocou = values$nocou,
##                   visualize.param = visualize.param(),
##                   input.visualize_highlight_y = input$visualize_highlight_y,
##                   input.visualize_colorscheme = input$visualize_colorscheme,
##                   input.visualize_pivotmatrix = input$visualize_pivotmatrix,
##                   visualize.palette = visualize.palette()
##                   )
## })

output$visualize_plot <- renderPlot({
  if (sum(visualize.data())==0) return()
  .visualize.plot(input.visualize_method = input$visualize_method,
                  visualize.data = visualize.data(),
                  title = visualize.title(),
                  ## input.visualize_year = input$visualize_year
                  input.visualize_year = input$visualize_year[1],
                  input.visualize_cellborder = input$visualize_cellborder,
                  indX = visualize.indX(),
                  couD = visualize.couD(),
                  noind = values$noind,
                  nocou = values$nocou,
                  visualize.param = visualize.param(),
                  input.visualize_highlight_y = input$visualize_highlight_y,
                  input.visualize_colorscheme = input$visualize_colorscheme,
                  input.visualize_pivotmatrix = input$visualize_pivotmatrix,
                  visualize.palette = visualize.palette()
                  )
  ## visualize_plot()
})

## output$visualize_heatmap <- renderD3heatmap({
##     if (sum(visualize.data())==0) return()
##   .visualize.heatmap(visualize.data = visualize.data())
## })

## output$visualize_scatterplot <- renderScatterplotThree({
##     if (sum(visualize.data())==0) return()
##     .visualize.scatterplot(visualize.data = visualize.data(),
##                            visualize.palette = visualize.palette())
## })

## output$visualize_dimple <- renderDimple({
##     ## if (sum(visualize.data())==0) return()
##     ## return(.visualize.dimple(visualize.data = visualize.data()))
##     visualize.data.df <- matrix2df(visualize.data = visualize.data(),
##                                                                numeric = FALSE)
##   return(.visualize.dimple(visualize.data.df = visualize.data.df,
##                            visualize.palette = visualize.palette()))
## })

output$visualize_download_data <- downloadHandler(
    filename = function() {
        paste0(## input$visualize_year,
               'icioapp2015_', # namereg[as.numeric(input$couVA)],
               input$visualize_method,
               '_',
               input$visualize_year[1],
               '.csv')
    },
    content = function(file) {
        ## write.csv(t(round(visualize.data(), 2)), file, row.names = FALSE)
        write.csv(t(round(visualize.data(), 2)), file)
    }
)

output$visualize_download_chart <- downloadHandler(
    filename = function() {
        paste0(## input$visualize_year,
            'icioapp2015_', # namereg[as.numeric(input$couVA)],
            input$visualize_method,
            ## '.pdf'
            paste0('.', tolower(input$visualize_download_chart_format))
        )
    },
    content = function(file) {
        if (input$visualize_download_chart_format=="PDF") {
            pdf(file = file, width = 16, height = 9)
        } else if (input$visualize_download_chart_format=="SVG") {
            svg(file = file, width = 16, height = 9)
        } else if (input$visualize_download_chart_format=="PNG") {
            png(file = file, width = 1152, height = 648)
        }
        ## ## turn off multiplot in favor of svg download
        ## for (yr in c(input$visualize_year[1]:input$visualize_year[2])) {
        yr <- as.numeric(input$visualize_year[1])
        ## data.plot.yr <- .visualize.data(data.coef = values[[input$visualize_data.coef]][[1]],
        ##                                 data.demand = values[[input$visualize_data.demand]][[1]],
        ##                                 year = yr, # loop here
        ##                                 couX = visualize.couX(),
        ##                                 indX = visualize.indX(),
        ##                                 couD = visualize.couD(),
        ##                                 visualize_method = input$visualize_method,
        ##                                 convRegCou = convRegCou,
        ##                                 NameInd34_agg = NameInd34_agg)
        ## .visualize.plot(input.visualize_method = input$visualize_method,
        ##                 visualize.data = visualize.data(),
        ##                 input.visualize_year = yr)
        .visualize.plot(input.visualize_method = input$visualize_method,
                        ## visualize.data = data.plot.yr,
                        title = visualize.title(),
                        visualize.data = visualize.data(),
                        input.visualize_year = yr,
                        input.visualize_cellborder = input$visualize_cellborder,
                        indX = visualize.indX(),
                        couD = visualize.couD(),
                        noind = values$noind,
                        nocou = values$nocou,
                        visualize.param = visualize.param(),
                        input.visualize_colorscheme = input$visualize_colorscheme,
                        input.visualize_highlight_y= input$visualize_highlight_y,
                        input.visualize_pivotmatrix = input$visualize_pivotmatrix,
                        visualize.palette = visualize.palette()
                        )
        ## } # multiplot end

        dev.off()
    }
)
