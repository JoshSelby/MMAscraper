options(shiny.trace = F)  # cahnge to T for trace
require(shiny)
require(shinysky)

shinyServer(function(input, output, session) {
  
  # typeahead
  observe({
    input$thti
    showshinyalert(session, "shinyalert3", sprintf("Typeahead Text Input Value: '%s'", 
                                                   input$thti), "error")
  })
  
  
  
}) 