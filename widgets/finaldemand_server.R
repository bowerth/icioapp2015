
## input <- list(finaldemand_year = 2005,
##               couX = c(4, 21, 63, 64, 65),
##               indX = c(3, 4),
##               couD = c(34, 39),
##               couS = c(21, 39))

## DATA.ICIOeconCVB <- isolate(values[["DATA.ICIOeconCVB"]][[1]])
## DATA.ICIOeconFDTTLexINVNT <- isolate(values[["DATA.ICIOeconFDTTLexINVNT"]][[1]])
## nocou <- isolate(values[["nocou"]])
## cou_add <- isolate(values[["cou_add"]])
## noind <- isolate(values[["noind"]])

finaldemand.data <- reactive({
    year <- as.numeric(input$finaldemand_year)

    ## couX <- as.numeric(input$finaldemand_couX)
    ## indD <- as.numeric(input$finaldemand_indD)
    ## couD <- as.numeric(input$finaldemand_couD)
    couX <- unique(unname(unlist(values$couagg69[input$finaldemand_couX])))
    indX <- unique(unname(unlist(values$indagg[input$finaldemand_indX])))
    couD <- unique(unname(unlist(values$couagg[input$finaldemand_couD])))

    ## couS <- as.numeric(input$finaldemand_couS)
    ## indS <- as.numeric(input$finaldemand_indS)

    DATA.ICIOeconCVB <- values[["DATA.ICIOeconCVB"]][[1]]
    DATA.ICIOeconFDTTLexINVNT <- values[["DATA.ICIOeconFDTTLexINVNT"]][[1]]

    nocou <- values[["nocou"]]
    cou_add <- values[["cou_add"]]
    noind <- values[["noind"]]

    cvB <- DATA.ICIOeconCVB[year - 1994, , ] # ((couS - 1) * noind + c(1:noind)), ]

    DATA.ICIOeconFDTTLexINVNT.62 <- DATA.ICIOeconFDTTLexINVNT[year - 1994, , c(1:(dim(DATA.ICIOeconFDTTLexINVNT)[3]-1))]  %*% conexc # remove last column, result dim [2346 * 62]
    ## dim(DATA.ICIOeconGRTR)



    conv_nocoucouaddnoind_nocou <-
        convCreate(dim=list(row=c(nocou + cou_add,noind),col=c(nocou)),
                   agg.row1=couX,
                   agg.row2=c(1:noind), # all industries
                   agg.col1=couD,
                   horiz=FALSE,
                   dimnames=NULL)

    ## temp.dmd <- DATA.ICIOeconFDTTLexINVNT[year - 1994, 1:((nocou + cou_add) * noind), couD]
    temp.dmd <- apply(conv_nocoucouaddnoind_nocou * DATA.ICIOeconFDTTLexINVNT.62, 1, sum)

    ## total demand: all demand countries
    conv_nocoucouaddnoind_nocou <-
        convCreate(dim=list(row=c(nocou + cou_add,noind),col=c(nocou)),
                   agg.row1=couX,
                   agg.row2=c(1:noind), # all industries
                   agg.col1=c(1:nocou), # all demand countries
                   horiz=FALSE,
                   dimnames=NULL)

    total.dmd <- apply(conv_nocoucouaddnoind_nocou * DATA.ICIOeconFDTTLexINVNT.62, 1, sum)


        ## ## replace previous calculations
        ## temp1 <- DATA.ICIOeconCVB[year - 1994, ((couS - 1) * noind + c(1:noind)), ] %*%
        ##     DATA.ICIOeconFDTTLexINVNT[year - 1994, 1:((nocou + cou_add) * noind), couD]
        ## temp2 <- DATA.ICIOeconCVB[year - 1994, ((couS - 1) * noind + c(1:noind)),] %*%
        ##     apply(DATA.ICIOeconFDTTLexINVNT[year - 1994, 1:((nocou + cou_add) * noind), ], 1, sum)
        ## temp1<-c(c(temp1)%*%convAggInd)
        ## temp2<-c(c(temp2)%*%convAggInd)

    if (input$finaldemand_bysource=="ind") {


      couS <- unique(unname(unlist(values$couagg[input$finaldemand_couS])))
        ## overwrite input: select all source industries
        indS <- c(1:noind)

        conv_nocounoind_noind <- convCreateDiag2(dim=list(row=c(nocou,noind),col=c(noind)),
                                                 agg.row1=c(couS),
                                                 agg.row2=c(indS))

        temp1 <- t(conv_nocounoind_noind) %*% cvB %*% temp.dmd
        temp1 <- c(c(temp1) %*% convAggInd)
        temp2 <- t(conv_nocounoind_noind) %*% cvB %*% total.dmd
        temp2 <- c(c(temp2) %*% convAggInd)

        dim_label <- NameInd34_agg
        dim_title <- "Source Industry"

    } else if  (input$finaldemand_bysource=="cou") {


    ## year = 2005
    ## couX = c(4, 21, 63, 64, 65)
    ## couX = c(1:34)
    ## indX = c(1:18)
    ## couD = c(1:34)
      ## couS = c(21, 39)
      ## indS <- c(3:18)

      indS <- unique(unname(unlist(values$indagg[input$finaldemand_indS])))
        ## overwrite input: select all source industries
        couS <- c(1:nocou)

        conv_nocounoind_nocou <- convCreateDiag(dim=list(row=c(nocou,noind),col=c(nocou)),
                                                 agg.row1=c(couS),
                                                 agg.row2=c(indS))

        temp1 <- t(conv_nocounoind_nocou) %*% cvB %*% temp.dmd
        temp1 <- c(c(temp1) %*% convRegCou)
        temp2 <- t(conv_nocounoind_nocou) %*% cvB %*% total.dmd
        temp2 <- c(c(temp2) %*% convRegCou)
        ## dim_label <- as.character(ui.icioDash.namereg.df[,1])
        dim_label <- colnames(convRegCou)
        dim_title <- "Source Country"

    }

    ## foreign demand = World demand less domestic demand
    temp3 <- temp2 - temp1

    ## combine resulting values
    temp <- cbind(dim_label, round(temp1, 1), round(temp2, 1), round(temp3, 1))
    colnames(temp)<-c(dim_title, "VA by Domestic FD", "VALU", "VA by Foreign FD")
    return(temp)
})

output$finaldemand.datatable <-
    ## DT::renderDataTable({
    renderDataTable({
    finaldemand.data()
}, options = list(
       scrollY = '550px',
       paging = FALSE))
       ## lengthMenu = c(70, 80),
       ## pageLength = 70))

output$finaldemand.summary <- renderPrint({

  couD <- unique(unname(unlist(values$couagg[input$finaldemand_couD])))
  couX <- unique(unname(unlist(values$couagg69[input$finaldemand_couX])))
  indX <- unique(unname(unlist(values$indagg[input$finaldemand_indX])))

  blurb <- paste(
    paste('Year =', input$finaldemand_year),
    paste('Demand Industry =', toString(indX)),
    paste('Product Origin Country =', toString(couX)),
    paste('Demand Country =', toString(couD)),
    sep = '\n')

    if (input$finaldemand_bysource=="ind") {
      couS <- unique(unname(unlist(values$couagg[input$finaldemand_couS])))
      blurb <- paste(blurb,
                  paste('Source Country =', toString(couS)),
                  sep = '\n')
    } else if (input$finaldemand_bysource=="cou") {
      indS <- unique(unname(unlist(values$indagg[input$finaldemand_indS])))
      blurb <- paste(blurb,
                  paste('Source Industry =', toString(indS)),
                  sep = '\n')
    }

  return(cat(blurb))

})

output$uiFd_bysource <- renderUI({
    ## show all 34 source industries for selected sources country
    if (input$finaldemand_bysource=="ind") {
        selectInput("finaldemand_couS", "Source Country/Region",
                    choices = isolate(names(values$couagg)),
                    ## selected = "NAFTA",
                    selected = c("MEX", "CHN"),
                    multiple = TRUE)
    } else if (input$finaldemand_bysource=="cou") {
        selectInput("finaldemand_indS", "Source Industry",
                    choices = isolate(names(values$indagg)),
                    selected = "C15T37",
                    multiple = TRUE)
    }
})

