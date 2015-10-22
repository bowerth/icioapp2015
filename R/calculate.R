#' .data.indxcou
#'
#' Calculate data for country by industry visualizations
#'
#' Calculate coefficients from demand data and coefficient data with result dimensions source country and source industry.
#'
#' @param data.coef
#' @param data.demand
#' @param nocou
#' @param cou_add
#' @param noind
#' @param couX
#' @param couD
#' @param indX
#' @param year
#'
#' @rdname .data.indxcou
#' @export
.data.indxcou <- function(data.coef,
                            data.demand,
                            year,
                            couX,
                            indX,
                            couD,
                            visualize_method="couSindS",
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

  ## } else if (visualize_method=="backlink") {
  ##     result <- visualize_backlink(year = year)
  }

  ## dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:34, 1])
  dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:values[["noind"]]])
  return(result)
}

#' .visualize.data
#'
#' Calculate data for country by industry visualizations
#'
#' Calculate coefficients from demand data and coefficient data with result dimensions source country and source industry.
#'
#' @param data.coef
#' @param data.demand
#' @param nocou
#' @param cou_add
#' @param noind
#' @param couX
#' @param couD
#' @param indX
#' @param year
#'
#' @rdname .visualize.data
#' @export
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

  ## } else if (visualize_method=="backlink") {
  ##     result <- visualize_backlink(year = year)
  }

  ## dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:34, 1])
  dimnames(result) <- list(rownames(convRegCou), NameInd34_agg[1:values[["noind"]]])
  return(result)
}

#' visualize_couSindS
#'
#' Calculate coefficients by souce country and source industry
#'
#' Calculate embodied VA, CO2 etc. from demand data and coefficient data with result dimensions source country and source industry.
#'
#' @param data.coef
#' @param data.demand
#' @param nocou
#' @param cou_add
#' @param noind
#' @param couX
#' @param couD
#' @param indX
#' @param year
#'
#' @rdname visualize_couSindS
#' @export
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

#' visualize_couXindS
#'
#' Calculate coefficients in exports
#'
#' Calculate domestic VA, CO2 etc. from demand data and coefficient data with result dimensions export country and source industry.
#'
#' @param data.coef
#' @param data.demand
#' @param nocou
#' @param cou_add
#' @param noind
#' @param couD
#' @param indX
#' @param year
#'
#' @rdname visualize_couXindS
#' @export
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

## visualize_backlink <- function(
##     data.coef=values[["DATA.ICIOeconB"]][[1]],
##     data.demand=values[["DATA.ICIOeconFDTTLdisc"]][[1]],
##     year) {

##     ## F = apply(DATA.ICIOeconFDTTLdisc[2011-1994,,],1,sum)
##     F = apply(data.demand[year-1994, , ], 1, sum)
##     ## B = DATA.ICIOeconB[2011-1994,,]
##     B = data.coef[year-1994, , ]
##     cF =  concixei %*% F
##     ## str(concixei)
##     cBF = apply (concixei %*% B %*% diag(F), 2,sum) %*% t(concixei)
##     res <- round(c(cBF) / c(cF),5)
##     ## result.m <- matrix(res, nrow = 34, byrow = FALSE)
##     result.m <- matrix(res, ncol = 34, byrow = TRUE)
##     result.m[is.nan(result.m)] <- 1
##     ## matrix(res, ncol = 34, byrow = TRUE)
##     return(result.m)
## }
