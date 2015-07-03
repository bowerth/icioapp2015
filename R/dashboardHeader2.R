dashboardHeader2 <- function(..., title = NULL, titleWidth = NULL, disable = FALSE, .list = NULL) {
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li", class = "dropdown")
##
  titleWidth <- validateCssUnit(titleWidth)
##
  # Set up custom CSS for custom width.
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    # This CSS is derived from the header-related instances of '230px' (the
    # default sidebar width) from inst/AdminLTE/AdminLTE.css. One change is that
    # instead making changes to the global settings, we've put them in a media
    # query (min-width: 768px), so that it won't override other media queries
    # (like max-width: 767px) that work for narrower screens.
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_", titleWidth, fixed = TRUE, '
       @media (min-width: 768px) {
      /*  .main-header > .navbar {
          margin-left: _WIDTH_;
        }
      */
      /*  .skin-blue .main-header .logo {
          /* width: _WIDTH_; */
          background: url(\'OECD_white.png\') no-repeat center center;
          background-color: #367fa9;
          background-size: 100px;
          border-bottom: 0 solid transparent;
        }
      */
      /* changing .logo:hover seems to have no effect - logo disappears when hovering over... */
      
      /* .main-header .navbar-custom-title {
          text-align: center;
          color: white;
          /* copy from .main-header .logo */
         display: block;
         float: left;
         font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
         font-size: 20px;
         font-weight: 300;
         height: 50px;
         line-height: 50px;
         overflow: hidden;
         padding: 0 15px;
        }
      */
      }
    '))))
  }
##
  tags$header(class = "main-header",
              custom_css,
              style = if (disable) "display: none;",
              ## span(class = "logo", title),
              ## a(href = "http://10.101.26.220/kit/", span(class = "logo")),
              a(href = "http://oecd-icio.cloudapp.net/", span(class = "logo")),
              tags$nav(class = "navbar navbar-static-top", role = "navigation",
                       ## Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       ## Sidebar toggle button
                       a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
                         role="button",
                         span(class="sr-only", "Toggle navigation")
                         ),
                       div(class = "navbar-custom-title", "ICIO Indicator 2015"),
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   items
                                   )
                           )
                       )
              )
##
}
