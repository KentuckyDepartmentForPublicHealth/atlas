library(shiny)
library(plotly)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("t-SNE Visualization with Data Table"),
  sidebarLayout(
    sidebarPanel = NA,
    mainPanel(
      plotlyOutput("tsnePlot"),
      DT::dataTableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$tsnePlot <- renderPlotly({
    # Define your own color palette
    custom_colors <- setNames(rainbow(length(unique(atlasDataClean$diagnosisFinal))), unique(atlasDataClean$diagnosisFinal))
    
    p <- plot_ly(data = atlasDataClean, 
                 x = ~tsne1, y = ~tsne2, 
                 type = 'scatter', mode = 'markers',
                 color = ~diagnosisFinal,
                 colors = custom_colors,
                 marker = list(size = 2, opacity = 0.7),
                 text = ~diagnosisFinal,  
                 hoverinfo = 'text',
                 source = "tsnePlot") %>%
      layout(title = "t-SNE Visualization of Tumor Types",
             xaxis = list(title = "t-SNE 1"),
             yaxis = list(title = "t-SNE 2"),
             dragmode = "select")  # Enable box selection
    
    # Register the click and selection events
    event_register(p, 'plotly_click')
    event_register(p, 'plotly_selected')
    
    p
  })
  
  # Reactive expression to capture selected data
  selected_points <- reactive({
    event_data("plotly_selected", source = "tsnePlot")
  })
  
  # Render the data table based on the selected points
  output$dataTable <- DT::renderDataTable({
    select_data <- selected_points()
    if (!is.null(select_data)) {
      # Get the indices of the selected points
      point_indices <- select_data$pointNumber + 1  # plotly uses 0-based indexing
      
      # Display the data for the selected points
      atlasDataClean[point_indices, ]
    } else {
      # If no points are selected, show an empty data frame
      data.frame()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)