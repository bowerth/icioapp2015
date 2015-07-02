messageData <- data.frame(
  ## from = c("me", "you"),
  from = NameInd34_agg_label_icon$ind,
  ## message = c("message text 1", "message text 2")
  message = NameInd34_agg_label_icon$label
  ,
  ## icon = c("question", "life-saver")
  icon = NameInd34_agg_label_icon$icon
  )

output$messageMenu <- renderMenu({
  # Code to generate each of the messageItems here, in a list. This assumes
  # that messageData is a data frame with two columns, 'from' and 'message'.
  msgs <- apply(messageData, 1, function(row) {
    ## messageItem(from = row[["from"]], message = row[["message"]], icon = icon(""))
    ## messageItem(from = row[["from"]], message = row[["message"]], icon = icon("cube"))
    messageItem(from = row[["from"]], message = row[["message"]], icon = icon(row[["icon"]]))
  })

  # This is equivalent to calling:
  #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  ## dropdownMenu(type = "messages", .list = msgs)
  dropdownMenu2(type = "messages", .list = msgs)
})
## shinydashboard::dropdownMenu
## require(shinydashboard)
## require(shiny)
