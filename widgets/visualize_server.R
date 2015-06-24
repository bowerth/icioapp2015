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

  }

  dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:34, 1])
  return(result)
}


visualize.data <- reactive({
  ## year <- as.numeric(input$visualize_year)
  year <- as.numeric(input$visualize_year[1])

  data.coef <- values[[input$visualize_data.coef]][[1]]
  data.demand <- values[[input$visualize_data.demand]][[1]]

  couX <- unique(unname(unlist(values$couagg69[input$visualize_couX])))
  indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))
  couD <- unique(unname(unlist(values$couagg[input$visualize_couD])))

  visualize_method <- input$visualize_method

  .visualize.data(data.coef = data.coef,
                  data.demand = data.demand,
                  year = year,
                  couX = couX,
                  indX = indX,
                  couD = couD,
                  visualize_method = visualize_method,
                  convRegCou = convRegCou,
                  NameInd34_agg = NameInd34_agg)
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
                             selected_y,
                             obs_y) {

    if (colorscheme == "continuous") {
        ## palette <- colorRampPalette(c("grey20", twitterblue))(length(c(1:34))) # number of industries
        palette <- colorRampPalette(c("grey20", twitterblue))(length(c(1:obs_y))) # number of industries
        ## ## remove color for all non-selected
        ## palette[setdiff(c(1:obs_y), selected_y)] <- "grey90"
        ## remove color for all selected
        palette[selected_y] <- "grey50"

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
        palette[selected_y] <- "grey50"
    }

    return(palette)

}

.visualize.plot <- function(input.visualize_method,
                            visualize.data,
                            input.visualize_year,
                            input.visualize_cellborder,
                            indX,
                            couD,
                            noind,
                            nocou,
                            visualize.param,
                            input.visualize_colorscheme,
                            input.visualize_pivotmatrix) {
  if (input.visualize_method=="couXindS") {
    ## title <- "Domestic VA in Exports"
    title <- "Value-added created by imports"
  } else {
    title <- "VA by Source Country and Source Industry"
  }

  if (input.visualize_pivotmatrix == TRUE) { # put countries in rows and industries in columns
      visualize.data <- t(visualize.data)


      selected_y <- couD
      obs_y <- nocou
  } else {
      selected_y <- indX
      obs_y <- noind
  }

  palette <- .visualize.color(colorscheme = input.visualize_colorscheme,
                              selected_y = selected_y,
                              obs_y = obs_y)

  op <- par(mar = c(3, 1, 2, 0)
            )

  mosaicplot(visualize.data,
             main = paste(title, input.visualize_year),
             color = palette,
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

output$visualize.plot <- renderPlot({
  ## .visualize.plot()
    indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))
    couD <- unique(unname(unlist(values$couagg69[input$visualize_couD])))

  .visualize.plot(input.visualize_method = input$visualize_method,
                  visualize.data = visualize.data(),
                  ## input.visualize_year = input$visualize_year
                  input.visualize_year = input$visualize_year[1],
                  input.visualize_cellborder = input$visualize_cellborder,
                  indX = indX,
                  couD = couD,
                  noind = values$noind,
                  nocou = values$nocou,
                  visualize.param = visualize.param(),
                  input.visualize_colorscheme = input$visualize_colorscheme,
                  input.visualize_pivotmatrix = input$visualize_pivotmatrix
                  )
})

output$visualize.summary <- renderPrint({

  couD <- unique(unname(unlist(values$couagg[input$visualize_couD])))
  couX <- unique(unname(unlist(values$couagg69[input$visualize_couX])))
  indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))

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
               '.pdf')
    },
    content = function(file) {
       pdf(file = file, width = 16, height = 9)
         data.coef <- values[[input$visualize_data.coef]][[1]]
         data.demand <- values[[input$visualize_data.demand]][[1]]
         couX <- unique(unname(unlist(values$couagg69[input$visualize_couX])))
         indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))
         couD <- unique(unname(unlist(values$couagg[input$visualize_couD])))
         visualize_method <- input$visualize_method
       ## for (yr in c(1995, 1996)) {
       for (yr in c(input$visualize_year[1]:input$visualize_year[2])) {
         ## year <- as.numeric(input$visualize_year)
         data.plot.yr <- .visualize.data(data.coef = data.coef,
                                         data.demand = data.demand,
                                         year = yr,
                                         couX = couX,
                                         indX = indX,
                                         couD = couD,
                                         visualize_method = visualize_method,
                                         convRegCou = convRegCou,
                                         NameInd34_agg = NameInd34_agg)

         ## .visualize.plot(input.visualize_method = input$visualize_method,
         ##                 visualize.data = visualize.data(),
         ##                 input.visualize_year = yr)
         .visualize.plot(input.visualize_method = visualize_method,
                         visualize.data = data.plot.yr,
                         input.visualize_year = yr)
       }
       dev.off()
    }
)
