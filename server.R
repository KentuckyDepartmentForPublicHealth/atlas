# define server -----
server <- function(input, output, session) {
  # debug
  output$sessionInputVariables <- renderPrint({
    reactiveValuesToList(input)
  })
} # end server
