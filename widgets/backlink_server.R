## use of converters:
## demand data structure:
##
##                couD1  couD2  couD3  couD4 [62]
##   couX1_indX1
##   couX1_indX2
##   couX1_indX3
##   couX2_indX1
##   couX2_indX2
##   couX2_indX3
##     [69 * 34]
##
## result data structure: one column
##
##   couS1_indS1
##   couS1_indS2
##   couS1_indS3
##   couS2_indS1
##   couS2_indS2
##   couS2_indS3
##
## aggregate result by source industry:
##   ind1 ind2 ind3
##    1 1  ...  ...
##    ...  1 1  ...
##    ...  ...  1 1
##    1 1  ...  ...
##    ...  1 1  ...
##    ...  ...  1 1
##
## aggregate result by source country:
##
##   cou1 cou2 cou3
##    1 1  ...  ...
##    1 1  ...  ...
##    1 1  ...  ...
##    ...  1 1  ...
##    ...  1 1  ...
##    ...  1 1  ...
##


backlink.data <- reactive({
    year <- as.numeric(input$backlink_year)

    couX <- unique(unname(unlist(values$couagg69[input$backlink_couX])))
    indX <- unique(unname(unlist(values$indagg[input$backlink_indX])))
    couD <- unique(unname(unlist(values$couagg[input$backlink_couD])))

    DATA.ICIOeconCVB <- values[["DATA.ICIOeconCVB"]][[1]]
    DATA.ICIOeconGRTR <- values[["DATA.ICIOeconGRTR"]][[1]]
    ## DATA.ICIOeconGRTR <- isolate(values[["DATA.ICIOeconGRTR"]][[1]])
    ## DATA.ICIOeconCVB <- isolate(values[["DATA.ICIOeconCVB"]][[1]])

    nocou <- values[["nocou"]]
    cou_add <- values[["cou_add"]]
    noind <- values[["noind"]]

    ## year <- 2005
    cvB <- DATA.ICIOeconCVB[year - 1994, , ]
    ## dim(cvB)

    ## - remove last column (discrepancy)
    ## - "conexc" reduce 69 columns to 62 columns
    ## CHN, MEX: merge 2346 to 2108
    ## DATA.ICIOeconGRTR: [2346, 69]
    ## conexc: [69, 62]
    ## DATA.ICIOeconGRTR.62: [2346, 62]
    DATA.ICIOeconGRTR.62 <- DATA.ICIOeconGRTR[year - 1994, , c(1:(dim(DATA.ICIOeconGRTR)[3]-1))]  %*% conexc

    conv_nocoucouaddnoind_nocou <-
        convCreate(dim=list(row=c(nocou + cou_add,noind),col=c(nocou)),
                   agg.row1=couX,
                   agg.row2=indX,
                   agg.col1=couD,
                   horiz=FALSE,
                   dimnames=NULL)

    ## element-wise multiplication with binary matrix: select elements from demand data
    temp.dmd <- apply(conv_nocoucouaddnoind_nocou * DATA.ICIOeconGRTR.62, 1, sum)

    EXGR_VABSCI <- cvB %*% temp.dmd

    if (input$backlink_bysource=="ind") {

        couS <- unique(unname(unlist(values$couagg[input$backlink_couS])))
        ## overwrite input: select all source industries
        indS <- c(1:noind)

        conv_nocounoind_noind <- convCreateDiag2(dim=list(row=c(nocou,noind),col=c(noind)),
                                                agg.row1=c(couS),
                                                agg.row2=c(indS))
        temp1 <- t(conv_nocounoind_noind) %*% EXGR_VABSCI
        ## add industry aggregates
        temp1 <- c(c(temp1) %*% convAggInd)
        temp2 <- rep(sum(temp.dmd), length(temp1))

        dim_label <- NameInd34_agg
        dim_title <- "Source Industry"
    } else if (input$backlink_bysource=="cou") {

        indS <- unique(unname(unlist(values$indagg[input$backlink_indS])))
        ## overwrite input: select all source countries
        couS <- c(1:nocou)

        conv_nocounoind_nocou <- convCreateDiag(dim=list(row=c(nocou,noind),col=c(nocou)),
                                                agg.row1=c(couS),
                                                agg.row2=c(indS))
        temp1 <- t(conv_nocounoind_nocou) %*% EXGR_VABSCI
        temp1 <- c(c(temp1) %*% convRegCou)

        ## backlink_dim_label <- reactive({
        ##   if (input$backlink_bysource=="ind") {
        ##     NameInd34_agg
        ##   } else if (input$backlink_bysource=="cou") {
        ##     colnames(convRegCou)
        ##   }
        ## })

        ## todo: add country aggregates?
        temp2 <- rep(sum(temp.dmd), length(temp1))
        ## dim_label <- as.character(ui.icioDash.namereg.df[,1])
        dim_label <- colnames(convRegCou)
        dim_title <- "Source Country"
    }

    temp3 <- temp1 / temp2
    ## temp <- cbind(dim_label, round(temp1, 1), round(temp2, 1), round(100 * temp3, 2))
    ## colnames(temp) <- c(dim_title, "VA by Exports", "Exports", "Ratio, in percent")
    temp <- data.frame(
      index = seq(along = temp1),
      dim_title = dim_label,
      "VA_by_Exports" = round(temp1, 1),
      "Exports" = round(temp2, 1),
      "Ratio_percent" = round(100 * temp3, 2))
    names(temp) <- sub("dim_title", dim_title, names(temp))
    ## colnames(temp) <- c(dim_title, "VA by Exports", "Exports", "Ratio, in percent")


    return(temp)
})

output$backlink.summary <- renderPrint({

  couD <- unique(unname(unlist(values$couagg[input$backlink_couD])))
  couX <- unique(unname(unlist(values$couagg69[input$backlink_couX])))
  indX <- unique(unname(unlist(values$indagg[input$backlink_indX])))

  blurb <- paste(
    paste('Year =', input$backlink_year),
    paste('Export Industry =', toString(indX)),
    paste('Export Country =', toString(couX)),
    paste('Demand Country =', toString(couD)),
    sep = '\n')

    if (input$backlink_bysource=="ind") {
      couS <- unique(unname(unlist(values$couagg[input$backlink_couS])))
      blurb <- paste(blurb,
                  paste('Source Country =', toString(couS)),
                  sep = '\n')
    } else if (input$backlink_bysource=="cou") {
      indS <- unique(unname(unlist(values$indagg[input$backlink_indS])))
      blurb <- paste(blurb,
                  paste('Source Industry =', toString(indS)),
                  sep = '\n')
    }

  return(cat(blurb))

})

output$backlink.barplot <-
  renderPlot({

    if (input$backlink_bysource=="ind") {
      dim_length <- values$noind
      dim_label <- NameInd34_agg
      dim_title <- "Source Industry"
    } else if (input$backlink_bysource=="cou") {
      dim_length <- values$nocou
      dim_label <- colnames(convRegCou)
      dim_title <- "Source Country"
    }

    plot_title <- paste0('Value-added share in exports, by ', dim_title, ', ' , input$backlink_year[1], ' percent')

    barplot(
      ## c(backlink.data()[c(1:dim_length), 4]),
      c(backlink.data()[c(1:dim_length), "Ratio_percent"]),
      beside = TRUE,
      las = 2,
      cex.names = .8,
      names = dim_label[c(1:dim_length)],
      col = twitterblue,
      main = plot_title
      )

  })

  output$backlink.datatable <-
    ## DT::renderDataTable({
    renderDataTable({
    backlink.data()
}, options = list(
       scrollY = '550px',
       paging = FALSE))
       ## lengthMenu = c(70, 80),
       ## pageLength = 70)) # 62 countries plus 8 regions

output$backlink_download_data <- downloadHandler(
    filename = function() {
        paste0(
          'icioapp2015_',
          'grtr_',
          input$backlink_bysource,
          '_',
          input$backlink_year[1],
          '.csv')
      },
  content = function(file) {
    write.csv(backlink.data(), file, row.names = FALSE)
  }
  )

output$uiBl_bysource <- renderUI({
    ## show all 34 source industries for selected sources country
    if (input$backlink_bysource=="ind") {
        selectInput("backlink_couS", "Source Country/Region",
                    choices = isolate(names(values$couagg)),
                    ## selected = "NAFTA",
                    selected = c("MEX", "CHN"),
                    multiple = TRUE)
    } else if (input$backlink_bysource=="cou") {
        selectInput("backlink_indS", "Source Industry",
                    choices = isolate(names(values$indagg)),
                    selected = "C15T37",
                    multiple = TRUE)
    }
})

