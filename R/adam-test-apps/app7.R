library(shiny)
library(plotly)
library(dplyr)
library(DT)

ui <- fluidPage(
  tags$style(HTML("
    body { background-color: #111111; color: white; }
    .dataTable { color: #00FF00; font-family: 'Courier New', monospace; font-size: 14px; }
  ")),
  titlePanel("t-SNE Dimensionality Reduction"),
  mainPanel(
    plotlyOutput("tsnePlot"),
    DT::dataTableOutput("dataTable")
  )
)

server <- function(input, output) {
  output$tsnePlot <- renderPlotly({
    num_classes <- length(unique(atlasDataClean$diagnosisClass))
    custom_colors <- setNames(rainbow(num_classes), unique(atlasDataClean$diagnosisClass))
    
    centroids <- atlasDataClean %>%
      group_by(diagnosisClass) %>%
      summarise(tsne1 = median(tsne1), tsne2 = median(tsne2))
    
    plot_ly(data = atlasDataClean, 
            x = ~tsne1, y = ~tsne2, 
            type = 'scatter', mode = 'markers',
            color = ~diagnosisClass, colors = custom_colors,
            marker = list(size = 5, opacity = 0.8),
            text = ~diagnosisClass, hoverinfo = 'text') %>%
      layout(title = "t-SNE Dimensionality Reduction",
             xaxis = list(title = "t-SNE 1", color = 'white'),
             yaxis = list(title = "t-SNE 2", color = 'white'),
             plot_bgcolor = '#111111', 
             paper_bgcolor = '#111111',
             font = list(color = 'white'))
  })
  
  selected_points <- reactive({
    event_data("plotly_selected")
  })
  
  output$dataTable <- DT::renderDataTable({
    select_data <- selected_points()
    if (!is.null(select_data)) {
      point_indices <- select_data$pointNumber + 1
      # Select only specific columns
      selected_data <- atlasDataClean[point_indices, c(1:7, 13:17)]
      if(nrow(selected_data) > 0) {
        return(selected_data)
      } else {
        return(data.frame(Message = "No valid points selected"))
      }
    } else {
      return(data.frame(Message = "No points selected"))
    }
  })
}

shinyApp(ui = ui, server = server)