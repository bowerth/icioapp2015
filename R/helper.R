#' read.csv.matrix
#'
#' create matrix from csv file
#'
#' load matrix object from csv file
#'
#' @author OECD STAN
#' @keywords IO
#' @export
#' @examples
#' \dontrun{
#' data_matrix <- read.csv.matrix(file.path(dbpath, "GitHub", "icioapp2015", "inst", "extdata", "icioapp2015_couSindS_2011.csv"))
#' }
read.csv.matrix <- function(...) {
    data <- read.csv(...)
    row.names(data) <- data[,1]
    data <- data[,-1]
    data <- as.matrix(data)
    return(data)
}

#' transform matrix to dataframe
#'
#' Create data frame from matrix
#'
#' Create data frame from matrix with index numbers or row- and column labels.
#'
#' @param visualize.data a data matrix with countries in columns and industries in rows and named dimensions
#' @rdname matrix2df
#' @export
#' @examples
#' \dontrun{
#' visualize.data <- read.csv.matrix(file.path(dbpath, "GitHub", "icioapp2015", "inst", "extdata", "icioapp2015_couSindS_2011.csv"))
#' visualize.data.df <- .visualize.createdf(visualize.data = visualize.data, numeric = FALSE)
#' }
matrix2df <- function(visualize.data,
                                input.visualize_logval=FALSE,
                                numeric=FALSE) {

    visualize.data <- t(visualize.data)
    if (input.visualize_logval==TRUE) {
        visualize.data[visualize.data >= 1] <- log(visualize.data[visualize.data >= 1])
    }
    if (numeric==TRUE) {
      visualize.data.df <- data.frame(columns = c(col(visualize.data)), # industry
                                      rows = c(row(visualize.data)), # country
                                      value = c(visualize.data)
                                      )
    } else {
      visualize.data.df <-
        visualize.data %>%
          as.data.frame() %>%
          add_rownames(var = "industry") %>%
            gather(key = country, value = value, -industry)
    }
    names(visualize.data.df) <- c("industry", "country", "value")
    return(visualize.data.df)

}

#' .visualize.color
#'
#' Create colour palette
#'
#' Create colour palette for visualisations with highlighting of selection
#'
#' @param colorscheme \code{discrete} or \code{continuous}
#' @param highlight_y boolean to activate highlighting of subgroups
#' @param highlight_col hex colour value
#' @param selected_y integer vector to specify highlighted subgroups
#' @param obs_y total number of groups in data
#'
#' @rdname .visualize.color
#' @export
#' @examples
#' \dontrun{
#'  .visualize.color(colorscheme = "continuous", highlight_y  = TRUE, highlight_col = "#00FF00", selected_y = 2, obs_y = 34)
#' }
.visualize.color <- function(colorscheme,
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
        # if (highlight_y == TRUE) {
        #     ## palette[selected_y] <- "grey50"
        #     ## palette[selected_y] <- highlightcol
        #     palette[selected_y] <- highlight_col
        # }

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
        # if (highlight_y == TRUE) {
        #     ## palette[selected_y] <- "grey50"
        #     ## palette[selected_y] <- highlightcol
        #     palette[selected_y] <- highlight_col
        # }
    }

        if (highlight_y == TRUE) {
            ## palette[selected_y] <- "grey50"
            ## palette[selected_y] <- highlightcol
            ## palette[selected_y] <- highlight_col
            highlight_col_dark <- colorRampPalette(c("black", highlight_col))(3)[2]
            palette_highlight_col <- colorRampPalette(c(highlight_col_dark, highlight_col))(length(selected_y))
            palette[selected_y] <- palette_highlight_col
        }

    return(palette)

}
