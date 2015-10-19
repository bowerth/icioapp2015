## load('c:/temp/Dropbox/ICIO2015/Rdata/DATA.ICIOeconGRTR.Rdata')
## load('c:/temp/Dropbox/ICIO2015/Rdata/DATA.ICIOeconFDTTLdisc.Rdata')
## load('c:/temp/Dropbox/ICIO2015/Rdata/DATA.ICIOeconCVB.Rdata')

visualize_couSindS <- function(data.coef=values[["DATA.ICIOeconCVB"]][[1]],
                               data.demand=values[["DATA.ICIOeconGRTR"]][[1]],
                               nocou=values[["nocou"]],
                               cou_add=values[["cou_add"]],
                               noind=values[["noind"]],
                               couX,
                               couD,
                               indX,
                               year)
{

    nocou_add <- nocou + cou_add

    temp = array(0, c((nocou_add * noind), (nocou_add + 1))) # 2346 * 70

    for(i in couD) {
        for(j in couX) {
            for(k in indX) {
                temp [ (j-1) * noind + k, i] <- 1
                ## result <- DATA.ICIOeconCVB[year-1994, , ] %*%
                ##     apply(temp*  DATA.ICIOeconGRTR [year-1994,,] ,1,sum)
            }
        }
    }

    result <- data.coef[year-1994, , ] %*%
        apply(temp * data.demand[year-1994, , ] , 1, sum)

    result.m <- matrix( result, byrow = T, ncol = noind) # 34

    return(result.m)
}

## data.coef=isolate(values[["DATA.ICIOeconCVB"]][[1]])
## data.demand=isolate(values[["DATA.ICIOeconGRTR"]][[1]])
## nocou=isolate(values[["nocou"]])
## cou_add=isolate(values[["cou_add"]])
## noind=isolate(values[["noind"]])
## couD <- 34
## indX <- 18
## year <- 2011
## domestic VA in exports
visualize_couXindS <- function(data.coef=values[["DATA.ICIOeconCVB"]][[1]],
                     data.demand=values[["DATA.ICIOeconGRTR"]][[1]],
                     nocou=values[["nocou"]],
                     cou_add=values[["cou_add"]],
                     noind=values[["noind"]],
                     couD,
                     indX,
                     year) {

    nocou_add <- nocou + cou_add
  result.m <- array(0,c(nocou, noind)) # 62, 34

    for(i in 1:nocou) {
        temp = array(0, c(nocou_add * noind, (nocou_add + 1))) # 2346, 70

        for(j in couD) {
            for(k in indX) {
                temp [ (i-1) * noind + k, j] <- 1
            }
        }

        result.m[i, 1:noind]  <- data.coef[year-1994, ((i-1) * noind + 1):(i * noind), ] %*%
            apply(temp * data.demand [year-1994, , ] , 1, sum)

    }
    return(result.m)
}

## data.coef=isolate(values[["DATA.ICIOeconB"]][[1]])
## data.demand=isolate(values[["DATA.ICIOeconFDTTLdisc"]][[1]])
## year <- 2011
## str(data.demand)
## str(values)
visualize_backlink <- function(
    data.coef=values[["DATA.ICIOeconB"]][[1]],
    data.demand=values[["DATA.ICIOeconFDTTLdisc"]][[1]],
    year) {

    ## F = apply(DATA.ICIOeconFDTTLdisc[2011-1994,,],1,sum)
    F = apply(data.demand[year-1994, , ], 1, sum)
    ## B = DATA.ICIOeconB[2011-1994,,]
    B = data.coef[year-1994, , ]
    cF =  concixei %*% F
    ## str(concixei)
    cBF = apply (concixei %*% B %*% diag(F), 2,sum) %*% t(concixei)
    res <- round(c(cBF) / c(cF),5)
    ## result.m <- matrix(res, nrow = 34, byrow = FALSE)
    result.m <- matrix(res, ncol = 34, byrow = TRUE)
    result.m[is.nan(result.m)] <- 1
    ## matrix(res, ncol = 34, byrow = TRUE)
    return(result.m)
}


.visualize.data <- function(data.coef,
                            data.demand,
                            year,
                            couX,
                            indX,
                            couD,
                            visualize_method,
                            convRegCou,
                            NameInd34_agg) {

  if (visualize_method=="couSindS") {
    result <- visualize_couSindS(data.coef = data.coef,
                                 data.demand = data.demand,
                                 couX = couX,
                                 couD = couD,
                                 indX = indX,
                                 year = year)
  } else if (visualize_method=="couXindS") {
    result <- visualize_couXindS(data.coef = data.coef,
                                 data.demand = data.demand,
                                 ## couX = couX,
                                 couD = couD,
                                 indX = indX,
                                 year = year)

  } else if (visualize_method=="backlink") {
      result <- visualize_backlink(year = year)
  }

  ## dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:34, 1])
  dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:values[["noind"]]])
  return(result)
}

.visualize.couX <- reactive({
  couX <- unique(unname(unlist(values$couagg69[input$visualize_couX])))
  return(couX)
})

.visualize.indX <- reactive({
    indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))
    return(indX)
})

.visualize.couD <- reactive({
  couD <- unique(unname(unlist(values$couagg[input$visualize_couD])))
  return(couD)
})


visualize.data <- reactive({
  ## year <- as.numeric(input$visualize_year)
  year <- as.numeric(input$visualize_year[1])

  data.coef <- values[[input$visualize_data.coef]][[1]]
  data.demand <- values[[input$visualize_data.demand]][[1]]

  couX <- .visualize.couX()
  indX <- .visualize.indX()
  couD <- .visualize.couD()

  visualize_method <- input$visualize_method

  visualize.data <- .visualize.data(data.coef = data.coef,
                                    data.demand = data.demand,
                                    year = year,
                                    couX = couX,
                                    indX = indX,
                                    couD = couD,
                                    visualize_method = visualize_method,
                                    convRegCou = convRegCou,
                                    NameInd34_agg = NameInd34_agg)

  return(visualize.data)

})

## ## debug
## input <- list(year = 2005,
##               couX = icioapp2015_couagg[["OECD"]],
##               couD = icioapp2015_couagg[["WOR"]],
##               indX = icioapp2015_indagg[["Goods"]])

## result.test <- isolate(visualize_couSindS(couX = c(1:34),
##                         couD = c(1:62),
##                         indX = c(1:18),
##                         year = 2010))
## ## dim(result.test)
## mosaicplot(result.test)

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

.visualize.color <- function(colorscheme,
                             ## indX,
                             highlight_y,
                             highlight_col,
                             selected_y,
                             obs_y) {

    if (colorscheme == "continuous") {
        ## palette <- colorRampPalette(c("grey20", twitterblue))(length(c(1:34))) # number of industries
        palette <- colorRampPalette(c("grey20", twitterblue))(length(c(1:obs_y))) # number of industries
        ## ## remove color for all non-selected
        ## palette[setdiff(c(1:obs_y), selected_y)] <- "grey90"
        ## remove color for all selected
        if (highlight_y == TRUE) {
            ## palette[selected_y] <- "grey50"
            ## palette[selected_y] <- highlightcol
            palette[selected_y] <- highlight_col
        }

    } else if (colorscheme == "discrete") {

        ## ## different color hues for 5 aggregates: C01T14 (1, 2), C15T37 (3-18), C40T45 (19, 20), C50T74, C75T95
        ## nameagg <- c("C01T14", "C15T37", "C40T45", "C50T74", "C75T95")
        ## ## convAggInd[ , nameagg]
        ## ## RColorBrewer::brewer.pal(length(nameagg), "Set1")[1:length(nameagg)]
        ## ## palette.base <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
        ## ## palette.base <- colorRampPalette(c("grey20", twitterblue))(length(nameagg))
        ## palette.base <- rev(RColorBrewer::brewer.pal(9, "Blues"))[1:length(nameagg)]
        ## ## request: shades of blue
        ## palette <- rep("white", isolate(values$noind))
        ## for (j in seq(along = nameagg)) {
        ##     for (i in c(1:isolate(values$noind))) {
        ##         if (convAggInd[ , nameagg][i, j]==1) palette[i] <- palette.base[j]
        ##     }
        ## }
        ## ## cat(paste0('c("', gsub(", ", "\", \"", toString(palette)), '")\n'))

        if (obs_y == values$noind) { # 34
            ## palette <- c("#E41A1C", "#E41A1C", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#377EB8", "#4DAF4A", "#4DAF4A", "#984EA3", "#984EA3", "#984EA3", "#984EA3", "#984EA3", "#984EA3", "#984EA3", "#984EA3", "#984EA3", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00")
            ## palette <- c("#333333", "#333333", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#364958", "#3A5F7E", "#3A5F7E", "#3E75A4", "#3E75A4", "#3E75A4", "#3E75A4", "#3E75A4", "#3E75A4", "#3E75A4", "#3E75A4", "#3E75A4", "#428BCA", "#428BCA", "#428BCA", "#428BCA", "#428BCA")
            palette <- c("#08306B", "#08306B", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#2171B5", "#2171B5", "#4292C6", "#4292C6", "#4292C6", "#4292C6", "#4292C6", "#4292C6", "#4292C6", "#4292C6", "#4292C6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6")
        } else if (obs_y == values$nocou) { # 62

            ## ## different color hues for 6 regions:
            ## namereg <- c("EU28", "NAFTA", "EASIA", "ASEAN", "ZEUR", "ZOTH", "ZSCA")
            ## ## convRegCou[ , namereg]
            ## ## RColorBrewer::brewer.pal(length(namereg), "Set1") # 7
            ## ## palette.base <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "lawngreen", "#A65628") # 6 is yellow
            ## ## request: shades of blue
            ## ## palette.base <- colorRampPalette(c("grey20", twitterblue))(length(namereg))
            ## palette.base <- rev(RColorBrewer::brewer.pal(9, "Blues"))[1:length(namereg)]
            ## palette <- rep("white", isolate(values$nocou))
            ## for (j in seq(along = namereg)) {
            ##     for (i in c(1:isolate(values$nocou))) {
            ##         if (convRegCou[ , namereg][i, j]==1) palette[i] <- palette.base[j]
            ##     }
            ## }
            ## ## cat(paste0('c("', gsub(", ", "\", \"", toString(palette)), '")\n'))


            ## palette <- c("lawngreen", "#FF7F00", "#FF7F00", "#377EB8", "#A65628", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "lawngreen", "#FF7F00", "#4DAF4A", "#4DAF4A", "#FF7F00", "#377EB8", "#FF7F00", "lawngreen", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "#FF7F00", "lawngreen", "#FF7F00", "#377EB8", "#A65628", "#FF7F00", "#A65628", "#984EA3", "#4DAF4A", "#A65628", "#A65628", "#FF7F00", "#4DAF4A", "#FF7F00", "#984EA3", "lawngreen", "#984EA3", "#FF7F00", "#FF7F00", "#FF7F00", "#984EA3", "#984EA3", "#FF7F00", "#FF7F00", "lawngreen", "#984EA3", "#984EA3", "lawngreen", "#4DAF4A", "#984EA3", "lawngreen", "lawngreen")
            ## palette <- c("#3F7CB0", "#3D6D97", "#3D6D97", "#35414C", "#428BCA", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3F7CB0", "#3D6D97", "#385065", "#385065", "#3D6D97", "#35414C", "#3D6D97", "#3F7CB0", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3D6D97", "#3F7CB0", "#3D6D97", "#35414C", "#428BCA", "#3D6D97", "#428BCA", "#3A5F7E", "#385065", "#428BCA", "#428BCA", "#3D6D97", "#385065", "#3D6D97", "#3A5F7E", "#3F7CB0", "#3A5F7E", "#3D6D97", "#3D6D97", "#3D6D97", "#3A5F7E", "#3A5F7E", "#3D6D97", "#3D6D97", "#3F7CB0", "#3A5F7E", "#3A5F7E", "#3F7CB0", "#385065", "#3A5F7E", "#3F7CB0", "#3F7CB0")
            palette <- c("#9ECAE1", "#6BAED6", "#6BAED6", "#08519C", "#C6DBEF", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#9ECAE1", "#6BAED6", "#2171B5", "#2171B5", "#6BAED6", "#08519C", "#6BAED6", "#9ECAE1", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#9ECAE1", "#6BAED6", "#08519C", "#C6DBEF", "#6BAED6", "#C6DBEF", "#4292C6", "#2171B5", "#C6DBEF", "#C6DBEF", "#6BAED6", "#2171B5", "#6BAED6", "#4292C6", "#9ECAE1", "#4292C6", "#6BAED6", "#6BAED6", "#6BAED6", "#4292C6", "#4292C6", "#6BAED6", "#6BAED6", "#9ECAE1", "#4292C6", "#4292C6", "#9ECAE1", "#2171B5", "#4292C6", "#9ECAE1", "#9ECAE1")

        }
        if (highlight_y == TRUE) {
            ## palette[selected_y] <- "grey50"
            ## palette[selected_y] <- highlightcol
            palette[selected_y] <- highlight_col
        }
    }

    return(palette)

}

.visualize.palette <- reactive({

    if (input$visualize_pivotmatrix == TRUE) { # put countries in rows and industries in columns

      selected_y <- .visualize.couD()
      obs_y <- values$nocou
  } else {
      selected_y <- .visualize.indX()
      obs_y <- values$noind
  }

  palette <- .visualize.color(colorscheme = input$visualize_colorscheme,
                              highlight_y  = input$visualize_highlight_y,
                              highlight_col = input$visualize_highlight_col,
                              selected_y = selected_y,
                              obs_y = obs_y)

      return(palette)
})


.visualize.title <- reactive({

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

## alternatives for marimekko plot:
## http://timelyportfolio.github.io/docs/_build/html/dimple/gallery.html#example14-marimekko-horiz-r
## http://dimplejs.org/advanced_examples_viewer.html?id=advanced_grouped_mekko

.visualize.plot <- function(input.visualize_method,
                            title,
                            visualize.data,
                            input.visualize_year,
                            input.visualize_cellborder,
                            indX,
                            couD,
                            noind,
                            nocou,
                            visualize.param,
                            input.visualize_colorscheme,
                            input.visualize_highlight_y,
                            input.visualize_pivotmatrix,
                            palette2
                            ) {
  ## if (input.visualize_method=="couXindS") {
  ##   ## title <- "Domestic VA in Exports"
  ##     title <- "Value-added created by imports"
  ## } else if (input.visualize_method=="couSindS") {
  ##     title <- "VA by Source Country and Source Industry"
  ## } else if (input.visualize_method=="backlink") {
  ##     title <- "Backward Linkage weighted by total Final Demand"
  ## }

  ## if (input.visualize_pivotmatrix == TRUE) { # put countries in rows and industries in columns
  ##     visualize.data <- t(visualize.data)

  ##     selected_y <- couD
  ##     obs_y <- nocou
  ## } else {
  ##     selected_y <- indX
  ##     obs_y <- noind
  ## }

  ## palette <- .visualize.color(colorscheme = input.visualize_colorscheme,
  ##                             highlight_y  = input.visualize_highlight_y,
  ##                             selected_y = selected_y,
  ##                             obs_y = obs_y)

  if (input.visualize_pivotmatrix == TRUE) { # put countries in rows and industries in columns
      visualize.data <- t(visualize.data)
  }

    op <- par(mar = c(3, 1, 2, 0)
              )

    mosaicplot(visualize.data,
               main = paste(title, input.visualize_year),
               ## color = palette,
               color = .visualize.palette(),
               las = 2,
               border = input.visualize_cellborder
               )

    mtext(do.call(expression, visualize.param),
          side = 1, # bottom
          line = 1:2 # smaller than par(mar[1])
          )

    par(op)
    ## return(p)

}


.visualize.heatmap <- function(visualize.data) {
  d <- d3heatmap(t(visualize.data),
                 colors = colorRampPalette(c("grey90", twitterblue, "grey20"))(20)
                 )
    return(d)
}
output$visualize.heatmap <- renderD3heatmap({
  .visualize.heatmap(visualize.data = visualize.data())
})


.visualize.scatterplot <- function(visualize.data) {
    ## N <- 100
    ## i <- sample(3, N, replace=TRUE)
    ## x <- matrix(rnorm(N*3),ncol=3)
    ## lab <- c("small", "bigger", "biggest")
    ## d <- scatterplot3js(x, color=rainbow(N), labels=lab[i], size=i, renderer="canvas")
    ## ## d <- scatterplot3js()
    visualize.data <- t(visualize.data())
    if (input$visualize_logval==TRUE) {
        ## visualize.data <- read.csv.matrix(file.path("inst", "extdata", "icioapp2015_couSindS_2011.csv"))
        visualize.data[visualize.data >= 1] <- log(visualize.data[visualize.data >= 1])
    }
    visualize.data.df <- data.frame(columns = c(col(visualize.data)), # industry
                                    rows = c(row(visualize.data)), # country
                                    value = c(visualize.data)
                                    )
    ## visualize.data.df <- data.frame(
    ##     columns = c(colnames(visualize.data)), # industry
    ##     rows = c(rownames(visualize.data)), # country
    ##     value = c(visualize.data)
    ## )
    ## names(visualize.data.df) <- NULL
    ## names(visualize.data.df) <- c("industry", "country", "value")
    names(visualize.data.df) <- c("country", "", "industry")
    ## labels=sprintf(
    ##     "x=%.3s, y=%.6s, z=%.1f",
    ##     visualize.data.df$columns,
    ##     visualize.data.df$rows,
    ##     visualize.data.df$value)
    d <- scatterplot3js(
        x = visualize.data.df,
        ## x = as.numeric(visualize.data.df$columns),
        ## y = as.numeric(visualize.data.df$rows),
        ## x = visualize.data.df$columns,
        ## y = visualize.data.df$rows,
        ## z = visualize.data.df$value,
               color=rep(.visualize.palette(),
                   ## length(colnames(visualize.data))),
                   length(colnames(visualize.data))),
               ## labels = labels,
               renderer="canvas"
               ) # size, label
    return(d)
}
output$visualize.scatterplot <- renderScatterplotThree({
    .visualize.scatterplot(visualize.data = visualize.data())
})

output$visualize.plot <- renderPlot({
  .visualize.plot(input.visualize_method = input$visualize_method,
                  visualize.data = visualize.data(),
                  title = .visualize.title(),
                  ## input.visualize_year = input$visualize_year
                  input.visualize_year = input$visualize_year[1],
                  input.visualize_cellborder = input$visualize_cellborder,
                  indX = .visualize.indX(),
                  couD = .visualize.couD(),
                  noind = values$noind,
                  nocou = values$nocou,
                  visualize.param = visualize.param(),
                  input.visualize_highlight_y = input$visualize_highlight_y,
                  input.visualize_colorscheme = input$visualize_colorscheme,
                  input.visualize_pivotmatrix = input$visualize_pivotmatrix,
                  palette2 = .visualize.palette()
                  )
})

output$visualize.summary <- renderPrint({

    couD <- .visualize.couD()
    couX <- .visualize.couX()
    indX <- .visualize.indX()

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
                         title = .visualize.title(),
                         visualize.data = visualize.data(),
                         input.visualize_year = yr,
                         input.visualize_cellborder = input$visualize_cellborder,
                         indX = .visualize.indX(),
                         couD = .visualize.couD(),
                         noind = values$noind,
                         nocou = values$nocou,
                         visualize.param = visualize.param(),
                         input.visualize_colorscheme = input$visualize_colorscheme,
                         input.visualize_highlight_y= input$visualize_highlight_y,
                         input.visualize_pivotmatrix = input$visualize_pivotmatrix
                         )

  ## } # multiplot end

       dev.off()
    }
)
