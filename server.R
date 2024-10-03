
## backend server code


# toplevel ----------------------------------------------------------------

server <- function(input, output, session) {
  
# debug
  output$sessionInputVariables <- renderPrint({
    reactiveValuesToList(input)
  })
  
} #end server
