#' .visualize.plot
#'
#' Create genome chart using mosaicplot function
#'
#' Create  genome chart from matrix data frame with countries in columns and industries in rows.
#'
#' @param input.visualize_method
#' @param title
#' @param visualize.data
#' @param input.visualize_year
#' @param input.visualize_cellborder
#' @param indX
#' @param couD
#' @param noind
#' @param nocou
#' @param visualize.param
#' @param input.visualize_colorscheme
#' @param input.visualize_highlight_y
#' @param input.visualize_pivotmatrix
#' @param palette2
#'
#' @rdname .visualize.plot
#' @export
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
                            visualize.palette=NULL
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
               color = visualize.palette,
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

#' .visualize.dimple
#'
#' Create stained glass chart using rcdimple
#'
#' Create stained glass chart from data frame with country, industry and value columns. Alternatives for marimekko plot: \link{http://timelyportfolio.github.io/docs/_build/html/dimple/gallery.html#example14-marimekko-horiz-r}, \link{http://dimplejs.org/advanced_examples_viewer.html?id=advanced_grouped_mekko}
#'
#' @param visualize.data
#' @param visualize.palette
#'
#' @rdname .visualize.dimple
#' @export
.visualize.dimple <- function(visualize.data.df,
                              visualize.palette=NULL) {
    ## visualize.data.df <- .visualize.createdf(visualize.data = visualize.data,
    ##                                          ## input.visualize_logval = input$visualize_logval,
    ##                                          numeric = FALSE)
    ## print(head(visualize.data.df))
    ## write.csv(visualize.data.df, file = file.path(dlpath, "visualize_data_df.csv"), row.names = FALSE)
    d <-
        visualize.data.df %>%
            dimple(value ~ country,
                   groups = "industry",
                   type = "bar"
                   ## ,
                   ## height = "350px",
                   ## width = "500px"
                   ) %>%
                     ## default_colors(
                     ## colorRampPalette(c(
                     ##   "grey20",
                     ##   ## "#780585",
                     ##   ## "#00F2FF",
                     ##   ## twitterblue
                     ##   input$visualize_highlight_col
                     ##   ))(length(unique(visualize.data.df$industry)))) %>%
                       xAxis(type = "addAxis", measure = "value", showPercent = TRUE) %>%
                         yAxis(type = "addPctAxis")

    if (!is.null(visualize.palette)) d <- d %>% default_colors(visualize.palette)

    ## str(d)
    return(d)
  }

#' .visualize.heatmap
#'
#' Create d3heatmap
#'
#' Create interactive heatmap from data matrix.
#'
#' @param visualize.data
#'
#' @rdname .visualize.heatmap
#' @export
.visualize.heatmap <- function(visualize.data) {
  d <- d3heatmap(t(visualize.data),
                 colors = colorRampPalette(c("grey90", twitterblue, "grey20"))(20)
                 )
    return(d)
}

#' .visualize.scatterplot
#'
#' Create threejs scatterplot
#'
#' Create 3-dimensional scatterplot using threejs library.
#'
#' @param visualize.data
#'
#' @rdname .visualize.scatterplot
#' @export
.visualize.scatterplot <- function(visualize.data,
                                   visualize.palette=NULL) {

    visualize.data.df <- matrix2df(visualize.data = visualize.data,
                                             ## input.visualize_logval = input$visualize_logval,
                                             numeric = TRUE)
    names(visualize.data.df) <- c("country", "", "industry")

    if (!is.null(visualize.palette)) {
        color <- rep(visualize.palette,
                     length(unique(visualize.data.df$industry))
                     )
    } else "steelblue"

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
               color=color,
               ## labels = labels,
               renderer="canvas"
               ) # size, label
    return(d)
}
