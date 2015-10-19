input <- list(
    year = 2011,
    visualize_data.coef = "DATA.ICIOeconCVB",
    visualize_data.demand = "DATA.ICIOeconGRTR",
    visualize_method = "couSindS",
    visualize_indX = "CTOTAL",
    visualize_couX = "WOR",
    visualize_couD = "WOR",
    visualize_logval = FALSE,
    visualize_pivotmatrix = FALSE
    )

year <- input$year

visualize_data.coef = input$visualize_data.coef
visualize_data.demand = input$visualize_data.demand
visualize_method = input$visualize_method
indX = input$visualize_indX
couX = input$visualize_couX
couD = input$visualize_couD
visualize_logval = input$visualize_logval
visualize_pivotmatrix = input$visualize_pivotmatrix

output <- NULL
