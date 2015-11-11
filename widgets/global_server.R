## ################## ##
## reactive functions ##
## ################## ##
global.couX <- reactive({
  couX <- unique(unname(unlist(values$couagg69[input$global_couX])))
  return(couX)
})
global.indX <- reactive({
    indX <- unique(unname(unlist(values$indagg[input$global_indX])))
    return(indX)
})
global.couD <- reactive({
  couD <- unique(unname(unlist(values$couagg[input$global_couD])))
  return(couD)
})

global.data.indxcou <- reactive({
  ## year <- as.numeric(input$visualize_year)
  year <- as.numeric(input$sidebar_year[1])
  ##
  data.coef <- values[[input$sidebar_data.coef]][[1]]
  data.demand <- values[[input$sidebar_data.demand]][[1]]
  ##
  couX <- global.couX()
  indX <- global.indX()
  couD <- global.couD()
  ##
  sidebar_method <- input$sidebar_method
  ##
  global.data.indxcou <- .data.indxcou(data.coef = data.coef,
                                       data.demand = data.demand,
                                       year = year,
                                       couX = couX,
                                       indX = indX,
                                       couD = couD,
                                       ## visualize_method = sidebar_method,
                                       convRegCou = convRegCou,
                                       NameInd34_agg = NameInd34_agg)
  ##
  return(global.data.indxcou)
})

## ##################### ##
## create output objects ##
##      render UI        ##
## ##################### ##
output$global_indX <- renderUI ({
    label <- ifelse(input$sidebar_data.demand=="DATA.ICIOeconGRTR",
                    "Export Industry",
                    "Demand Industry")
    selectInput("global_indX", label, # "Export or Demand Industry",
                choices = isolate(names(values$indagg)),
                selected = "CTOTAL",
                ## selected = "C15T37",
                multiple = TRUE)
})
output$global_couX <- renderUI ({
    label <- ifelse(input$sidebar_data.demand=="DATA.ICIOeconGRTR",
                    "Export Country (or Region)",
                    "Product Origin Country (or Region)")
    selectInput("global_couX", label, # "Product Origin or Export Country/Region"
                choices = isolate(names(values$couagg)),
                selected = "WOR",
                multiple = TRUE)
})
output$global_couD <- renderUI ({
    selectInput("global_couD", "Demand Country/Region",
                choices = isolate(names(values$couagg)),
                ## selected = "NAFTA",
                selected = "WOR",
                multiple = TRUE) # CAN
})

output$global_download_data <- downloadHandler(
    filename = function() {
        paste0(## input$visualize_year,
            'icioapp2015_', # namereg[as.numeric(input$couVA)],
            ## input$sidebar_method,
            "couSindS",
            '_',
            input$sidebar_year[1],
            '.csv')
    },
    content = function(file) {
        ## write.csv(t(round(visualize.data(), 2)), file, row.names = FALSE)
        write.csv(t(round(global.data.indxcou(), 2)), file)
    }
)
