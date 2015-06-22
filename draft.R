

require(ggplot2)
require(reshape2)

## function to harmonise industry lists in data and conversion
stripcapcharacter <- function(x) {
    x <- gsub("[A-Z]", "", x)
    x <- gsub("[.]", "", x)
}

data4plot <- function(x,
                      indic=stop("'indic' not specified")) {
    X <- strsplit(rownames(x), split = "_") # x = result.data
    namecou.all <- sapply(X, "[[", 1)
    namecou <- unique(namecou.all)
    ## cat(paste0('c("', namecou, '" = ""),\n'))
    ## merge(data.frame(cou = namecou), namereg, by.x = "cou", by.y = "X")
    nameind.all <- sapply(X, "[[", 2)
    nameind.all <- stripcapcharacter(nameind.all)
    nameind <- unique(nameind.all)
    ##
    data.plot <- data.frame(value = x[ , 1]) # x = EXGR_VABSCI
    data.plot$cou <- namecou.all
    data.plot$ind <- nameind.all
    ##
    data.plot <- merge(data.plot, indagg.m)
    data.plot <- merge(data.plot, namereg.m)
    data.plot$indic <- indic
    return(data.plot)
}

## regions: EU28, ASEAN, NAFTA and E.Asia
namereg <- read.csv(file.path(PATH.IO, "2015sut-io", "0_LookupFiles", "Reggrp_ones.csv"))
names(namereg) <- sub("X", "cou", names(namereg))
namereg <- subset(namereg, select = c("cou", "ZEUR", "ASEAN", "NAFTA", "EASIA", "ZOTH", "ZSCA")) # unused: EU28
## ## test uniqueness and pool missing
## row.names(namereg) <- namereg$cou
## namereg <- namereg[ , -1]
## namereg <- as.matrix(namereg)
## as.factor(rownames(namereg[apply(namereg, 1, sum)==0,]))
## ## missing out on these countries not covered in selected regions: AUS CHL ISL ISR NZL NOR CHE TUR ARG BRA COL CRI IND RUS SAU TUN ZAF ROW
namereg.m <- melt(namereg, id.vars = c("cou"), variable.name = "coureg")
namereg.m <- namereg.m[namereg.m$value!=0,]
namereg.m <- namereg.m[ , !colnames(namereg.m)%in%c("value")]
## namereg.m

## industry aggregates: agriculture, manufacturing, services
indagg <- read.csv(file.path(PATH.IO, "2015sut-io", "0_LookupFiles", "IndGrouping.csv"))
names(indagg) <- sub("X", "indagg", names(indagg))
names(indagg)[c(2:length(indagg))]      # 34
indagg <- subset(indagg, indagg %in% c("C20T22", "C23T26", "C27T28", "C30T33", "C34T35", "C50T55", "C60T64", "C70T74", "C75T95"))
names(indagg) <- stripcapcharacter(names(indagg))
indagg.m <- melt(indagg, id.vars = "indagg", variable.name = "ind")
indagg.m <- indagg.m[indagg.m$value!=0,]
indagg.m <- indagg.m[ , !colnames(indagg.m)%in%c("value")]

## inputs
yr = 1999-1994
noind = 34
## calcratio <- TRUE
calcratio <- FALSE
namedmd <- c("GRTR", "FDDTL")

data.plot <- NULL
if ("GRTR"%in%namedmd) {
    ## gross exports (exports of product 2 = AUS C10T14 to world)

    if (calcratio==TRUE) {
        EXGR_VABSCI <- EXGR_VABSCI / temp.dmd[indX]
    }

    data.plot.indic <- data4plot(EXGR_VABSCI, indic = "EXGR_VABSCI")
    data.plot <- rbind(data.plot, data.plot.indic)

}
if ("FDDTL"%in%namedmd) {

    ## namedmd <- c("GRTR", "FDDTL")
    ## FDVA_BSCI
    coul=1 # couS
    couk=2 # couX

    ## final demand (foreign demand = AUT , foreign demand product = all, va source cou= AUS)
    temp.dmd <- array(0, c(length(DATA.ICIOeconFDTTLdisc.exINVNT[1, , 1])))

    temp.dmd[((couk-1)*noind+1):(couk*noind)] <-
        c(DATA.ICIOeconFDTTLdisc.exINVNT[yr,((couk-1)*noind+1):(couk*noind), coul])

    cvB <- DATA.ICIOeconCVB[yr, , ]            # 2108*2346
    FDVA_BSCI <- cvB %*% temp.dmd
    ## dim(FDVA_BSCI)

    data.plot.indic <- data4plot(FDVA_BSCI, indic = "FDVA_BSCI")
    data.plot <- rbind(data.plot, data.plot.indic)

}

## names(data.plot)

p <- ggplot(data = data.plot, group = coureg) +
    geom_bar(aes(x = indic, y = value, fill = coureg, order = coureg), stat = "identity") +
    theme(axis.text.x = element_text(angle = 90)) +
        facet_grid(~indagg)
p

## write.csv(data.plot, file = file.path(dlpath, "exgr_fdva_bsci.csv"), row.names = FALSE)

data.test <- subset(data.plot, coureg=="ZOTH" & indic=="EXGR_VABSCI" & indagg=="C50T55")
sum(data.test$value)
class(data.test$value)
data.test[-order("value"),]
