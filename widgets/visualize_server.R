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

.visualize.plot <- function(input.visualize_method,
                            visualize.data,
                            input.visualize_year) {
  if (input.visualize_method=="couXindS") {
    ## title <- "Domestic VA in Exports"
    title <- "Value-added created by imports"
  } else {
    title <- "VA by Source Country and Source Industry"
  }

  indX <- unique(unname(unlist(values$indagg[input$visualize_indX])))
  palette <- colorRampPalette(c("grey20", twitterblue))(length(c(1:34))) # number of industries
  palette[setdiff(c(1:34), indX)] <- "white"

  op <- par(mar = c(3, 1, 2, 0)
            )
  mosaicplot(visualize.data,
             main = paste(title, input.visualize_year),
             color = palette,
             las = 2
             )

  mtext(do.call(expression, visualize.param()),
        side = 1, # bottom
        line = 1:2 # smaller than par(mar[1])
        )

  par(op)
  ## return(p)
}

output$visualize.plot <- renderPlot({
  ## .visualize.plot()
  .visualize.plot(input.visualize_method = input$visualize_method,
                  visualize.data = visualize.data(),
                  ## input.visualize_year = input$visualize_year
                  input.visualize_year = input$visualize_year[1]
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
