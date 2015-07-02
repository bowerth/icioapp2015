testingRadiant <- FALSE

setInitValues <- function() {
    ## initialize state list and reactive values
    if(testingRadiant) {
        ## load previous state for testing
    } else {
        state_list <<- list()
        values <<- reactiveValues()

        ## initial plot height and width
        values$plotHeight <- 650
        values$plotWidth <- 650
        values$colors <- c("#4F81BD", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6", "#F79646")

        values$nocou <- 62
        ## isolate(values$nocou)
        values$cou_add <- 7
        ## isolate(values$cou_add)
        values$noind <- 34
        ## isolate(values$nocou)

        ## values$couagg <- couagg

        ## values$couagg <- list(WORLD = c(1:62),
        ##                         OECD = c(1:34)
        ##                         )
        env <- new.env()
        load(file.path("data", "couagg.rda"), envir = env)
        values$couagg <- env$couagg

        ## values$couagg69 <- list(WOR = c(1:69),
        ##                         OECD = c(1:34)
        ##                         )
        env <- new.env()
        load(file.path("data", "couagg69.rda"), envir = env)
        values$couagg69 <- env$couagg69

        ## values$indagg <- list(Goods = c(1:18))
        env <- new.env()
        load(file.path("data", "indagg.rda"), envir = env)
        values$indagg <- env$indagg

        values$datasetlist <- NULL

        ## if (active.icioFddva2013==TRUE) {
        env <- new.env()
        load(file.path("data", "DATA.ICIOeconCVB.Rdata"), envir = env)
        df <- mget(ls(envir = env), envir = env)
        values[["DATA.ICIOeconCVB"]] <- df
        values[["DATA.ICIOeconCVB_descr"]] <- attr(df, "description")
        values$datasetlist <- c(isolate(values$datasetlist), "DATA.ICIOeconCVB")

        ## isolate(dim(values[["DATA.ICIOeconCVB"]][[1]]))
        ## }

        env <- new.env()
        load(file.path("data", "DATA.ICIOeconFDTTLexINVNT.Rdata"), envir = env)
        df <- mget(ls(envir = env), envir = env)
        values[["DATA.ICIOeconFDTTLexINVNT"]] <- df
        values[["DATA.ICIOeconFDTTLexINVNT_descr"]] <- attr(df, "description")
        values$datasetlist <- c(isolate(values$datasetlist), "DATA.ICIOeconFDTTLexINVNT")

        ## isolate(dim(values[["DATA.ICIOeconFDTTLexINVNT"]][[1]]))


        env <- new.env()
        load(file.path("data", "DATA.ICIOeconGRTR.Rdata"), envir = env)
        df <- mget(ls(envir = env), envir = env)
        values[["DATA.ICIOeconGRTR"]] <- df
        values[["DATA.ICIOeconGRTR_descr"]] <- attr(df, "description")
        values$datasetlist <- c(isolate(values$datasetlist), "DATA.ICIOeconGRTR")

    }
}

setInitValues() # using a function here so it can also be called from state.R to reset the app

## ## copy lookup files
## copy.path.io <- file.path(PATH.IO, "2015sut-io", "0_LookupFiles")
## file.list <- c("ConvAggInd.Rdata",
##                "Ind+aggrInd.csv",
##                "Reggrp_ones.csv",
##                )
## for (basename in file.list) {
##     file.copy(from = file.path(copy.path.io, basename),
##               to = file.path("data", basename),
##               overwrite = TRUE)
## }

load(file.path("data", "convRegCou.rda")) # load matrix "convRegCou" to calculate region aggregates for source countries

load(file.path("data", "ConvAggInd.Rdata")) # load matrix "convAggInd" to calculate aggregates for source industries
## ## add aggregate C01T14 and C40T45 for coloring
## C01T14 <- C40T45 <- rep(0, 34);
## C01T14[c(1, 2)] <- 1
## C40T45[c(19, 20)] <- 1
## convAggInd <- cbind(convAggInd, C01T14, C40T45)
## save(convAggInd, file = file.path("data", "ConvAggInd.Rdata"))
##
## ## create indagg list for input field
## nameind <- colnames(convAggInd)
## indagg <- NULL
## for (ind in nameind) {
##   indagg.ind <- seq(along = convAggInd[, ind])[convAggInd[, ind]==1]
##   indagg <- c(indagg, list(indagg.ind))
## }
## names(indagg) <- nameind
## save(indagg, file = file.path(dbpath, "GitHub", "icioapp2015", "data", "indagg.rda"))

NameInd34_agg <- read.csv(file.path("data", "Ind+aggrInd.csv"), header = F) # length 51, corresponds to column names of "convAggInd"
NameInd34_agg <- as.character(NameInd34_agg[, 1])

## create colors
twitterblue <- rgb(red = 66, green = 139, blue = 202, maxColorValue = 255)
## source(file.path("R", "convCreate.R"))

## ## create conversion [2108 * 2346]
## source(file = file.path(copy.path, "0_Initializations.R"))
## save(concixei, file = file.path(dbpath, "GitHub", "icioapp2015", "data", "concixei.rda"))
## save(conexc, file = file.path(dbpath, "GitHub", "icioapp2015", "data", "conexc.rda"))
load(file.path("data", "concixei.rda")) # dim(concixei) = 2108 2346
load(file.path("data", "conexc.rda")) # dim(conexc) = 69 62

## ## copy functions
## copy.path.db <- file.path(dbpath, "GitHub", "stan", "R")
function.list <- c("convCreate.R",
                   "convCreateDiag.R",
                   "convCreateDiag2.R"
                   )
## for (basename in function.list) {
##     file.copy(from = file.path(copy.path.db, basename),
##               to = file.path("R", basename),
##               overwrite = TRUE)
## }
sapply(file.path("R", function.list), source)


ui.icioDash.namereg.df <- read.csv(file.path("data", "Reggrp_ones.csv"), header = T) # required for country names in table

## ## create region and country lists
## ## names(ui.icioDash.namereg.df)
## namereg <- c("WOR", "OECD", "EU28", "NAFTA", "EASIA", "ASEAN", "ZEUR", "ZOTH", "ZSCA")
## ##
## couagg <- NULL
## for (reg in namereg) {
##   couagg.reg <- seq(along = ui.icioDash.namereg.df[[reg]])[ui.icioDash.namereg.df[[reg]]==1]
##   couagg <- c(couagg, list(couagg.reg))
## }
## names(couagg) <- namereg
## ##
## namecou <- ui.icioDash.namereg.df$X
## namecou.list <- as.list(seq(along = namecou))
## names(namecou.list) <- namecou
## couagg <- c(couagg, namecou.list)
## ##
## save(couagg, file = file.path(dbpath, "GitHub", "icioapp2015", "data", "couagg.rda"))

## ## create converter to calculate region aggregates for 62 countries
## convRegCou.temp <- diag(x = 1, nrow(ui.icioDash.namereg.df), nrow(ui.icioDash.namereg.df))
## rownames(convRegCou.temp) <- colnames(convRegCou.temp) <- ui.icioDash.namereg.df[, 1]
## convRegCou <- cbind(convRegCou.temp, as.matrix(subset(ui.icioDash.namereg.df, select = namereg)))

## save(convRegCou, file = file.path(dbpath, "GitHub", "icioapp2015", "data", "convRegCou.rda"))

## ## create region and country list with 69 economies (MEX, CHN)
## DATA.ICIOeconCVB <- isolate(values[["DATA.ICIOeconCVB"]][[1]])
## colnames <- colnames(DATA.ICIOeconCVB[1, , ])
## X <- strsplit(colnames, split = "_")
## couadd.df <- data.frame(X = unique(sapply(X, "[[", 1)))
## couadd.df$MEX <- 0
## couadd.df$CHN <- 0
## couadd.df$MEX[couadd.df$X%in%c("MEX", "MX1", "MX2", "MX3")] <- 1
## couadd.df$CHN[couadd.df$X%in%c("CHN", "CN1", "CN2", "CN3", "CN4")] <- 1

## namereg.add <- c("CHN", "MEX")

## couadd <- NULL
## for (reg in namereg.add) {
##   couadd.reg <- seq(along = couadd.df[[reg]])[couadd.df[[reg]]==1]
##   couadd <- c(couadd, list(couadd.reg))
## }
## names(couadd) <- namereg.add

## ## replace MEX and CHN with new index values
## lookup <- function(x,
##                    index.new) {
##   if (length(intersect(x, index.new)) > 0) x <- sort(unique(c(x, index.new)))
##   return(x)
## }
## ##
## couagg69 <- couagg
## for (cou in names(couadd)) {
##   index.new <- couadd[[cou]] # 39 66, 67 68 69
##   couagg69 <- lapply(couagg69, lookup, index.new)
## }
## ##
## save(couagg69, file = file.path(dbpath, "GitHub", "icioapp2015", "data", "couagg69.rda"))

## create matrix converter to add regions to countries
