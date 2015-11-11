output$heatmap <- renderD3heatmap({
    if (sum(visualize.data())==0) return()
  ## .visualize.heatmap(visualize.data = global.data.indxcou())
  .visualize.heatmap(visualize.data = visualize.data())
})


output$scatterplot <- renderScatterplotThree({
    if (sum(visualize.data())==0) return()
    ## .visualize.scatterplot(visualize.data = visualize.data())
    ## .visualize.scatterplot(visualize.data = global.data.indxcou(),
    .visualize.scatterplot(visualize.data = visualize.data(),
                           visualize.palette = visualize.palette())

})


output$dimple <- renderDimple({
    if (sum(visualize.data())==0) return()
    ## return(.visualize.dimple(visualize.data = visualize.data()))
    ## visualize.data.df <- matrix2df(visualize.data = global.data.indxcou(),
    visualize.data.df <- matrix2df(visualize.data = visualize.data(),
                                                               numeric = FALSE)
  return(.visualize.dimple(visualize.data.df = visualize.data.df,
                           visualize.palette = visualize.palette()))
})

output$download_dimple <- downloadHandler(
    filename = function() {
        paste0(## input$visualize_year,
            'icioapp2015_', # namereg[as.numeric(input$couVA)],
            input$visualize_method,
            '.html'
        )
    },
    content = function(file) {
            ## visualize.data.df <- matrix2df(visualize.data = global.data.indxcou(),
            visualize.data.df <- matrix2df(visualize.data = visualize.data(),
                                                               numeric = FALSE)

        htmlwidgets:::saveWidget(widget = .visualize.dimple(
                                     visualize.data.df = visualize.data.df,
                                     visualize.palette = visualize.palette()),
                                 file = file, selfcontained = TRUE, libdir = NULL)
    }
)
