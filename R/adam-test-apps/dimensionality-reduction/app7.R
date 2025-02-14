library(shiny)
library(plotly)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("t-SNE Dimensionality Reduction"),
      plotlyOutput("tsnePlot"),
      DT::dataTableOutput("dataTable")
)


server <- function(input, output) {
  # Data loading or definition here.
  # For example, if you have 'atlasDataClean' and 'filteredData'
  
  # Add key to atlasDataClean
  atlasDataClean$key <- seq_len(nrow(atlasDataClean))
  
  # Assuming filteredData() is already defined; if not, define it or use atlasDataClean directly.
  filteredData <- reactive({
    atlasDataClean
  })
  
  output$tsnePlot <- renderPlotly({
    data <- filteredData()
    validate(need(nrow(data) > 0, "No data available to plot."))
    
    num_classes <- length(unique(data$diagnosisClass))
    custom_colors <- setNames(rainbow(num_classes), unique(data$diagnosisClass))
    
    p <- plot_ly(data = data,
                 x = ~tsne1, y = ~tsne2,
                 type = 'scatter', mode = 'markers',
                 color = ~diagnosisFinal, colors = custom_colors,
                 marker = list(size = 5, opacity = 0.8),
                 text = ~paste("Sample:", sampleID, "<br>Diagnosis Final:", diagnosisFinal),
                 hoverinfo = 'text',
                 key = ~key,  # Include key mapping
                 source = "A") %>%
      layout(
        title = "t-SNE Dimensionality Reduction",
        xaxis = list(title = "t-SNE 1", color = 'white'),
        yaxis = list(title = "t-SNE 2", color = 'white'),
        plot_bgcolor = '#111111', 
        paper_bgcolor = '#111111',
        font = list(color = 'white'),
        dragmode = 'select'  # Enable selection
      )
    
    # Register the event if necessary
    p <- event_register(p, "plotly_selected")
    
    return(p)
  })
  
  selected_points <- reactive({
    event_data("plotly_selected", source = "A")
  })
  
  output$dataTable <- DT::renderDataTable({
    select_data <- selected_points()
    if (!is.null(select_data) && nrow(select_data) > 0) {
      point_keys <- as.numeric(select_data$key)
      selected_data <- atlasDataClean[atlasDataClean$key %in% point_keys, ]
      if(nrow(selected_data) > 0) {
        return(DT::datatable(selected_data))
      } else {
        return(DT::datatable(data.frame(Message = "No valid points selected")))
      }
    } else {
      return(DT::datatable(data.frame(Message = "No points selected")))
    }
  })
}

shinyApp(ui, server)