library(shiny)
library(plotly)
library(dplyr)
library(DT)

# Define the colors list
colors <- c("#6a0033", "#e4016e", "#ff527b", "#360e15", "#ff757c", "#dc002d", "#ff9170", "#7a2000", "#ff7e40", 
            "#ac5300", "#da6c00", "#401f00", "#ffb173", "#d79600", "#6a4800", "#d6c6b2", "#ddaf00", "#d6ca6f", 
            "#6e6900", "#1d1c10", "#8db600", "#1b7a00", "#8ddb76", "#00b861", "#9ad5b1", "#005d3f", "#01bbb7", 
            "#00444d", "#01afc7", "#54d8f9", "#0189dd", "#8ab5ff", "#5292ff", "#004690", "#00317f", "#4263ff", 
            "#240a4e", "#271332", "#fa63ff", "#760078", "#ff77f6", "#8a005f", "#ffa5ca", 
            "black","grey10","grey20","gray80","grey40","grey50","grey60","grey70","grey30")

ui <- fluidPage(
  # Use a dark theme for the overall UI
  tags$style(HTML("
    body {
      background-color: #111111;
      color: white;
    }
  ")),
  titlePanel("t-SNE Dimensionality Reduction"),
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
    # Define color palette using the provided colors list
    num_classes <- length(unique(atlasDataClean$diagnosisClass))
    custom_colors <- setNames(colors[1:num_classes], unique(atlasDataClean$diagnosisClass))
    
    # Calculate centroids for each diagnosisClass
    centroids <- atlasDataClean %>%
      group_by(diagnosisClass) %>%
      summarise(tsne1 = median(tsne1), tsne2 = median(tsne2))
    
    # Creating the plotly plot with dark background
    p <- plot_ly(data = atlasDataClean, 
                 x = ~tsne1, y = ~tsne2, 
                 type = 'scatter', mode = 'markers',
                 color = ~diagnosisClass,
                 colors = custom_colors,
                 marker = list(size = 5, opacity = 0.8),
                 text = ~diagnosisClass,  
                 hoverinfo = 'text',
                 source = "tsnePlot") %>%
      layout(title = "t-SNE Dimensionality Reduction (7375 samples)",
             xaxis = list(title = "t-SNE 1", color = 'white'),
             yaxis = list(title = "t-SNE 2", color = 'white'),
             annotations = lapply(1:nrow(centroids), function(i) {
               list(x = centroids$tsne1[i], y = centroids$tsne2[i], 
                    text = centroids$diagnosisClass[i], 
                    showarrow = FALSE, 
                    font = list(size = 10, color = 'white'))
             }),
             plot_bgcolor = '#111111',
             paper_bgcolor = '#111111',
             font = list(color = 'white'),
             dragmode = "zoom",
             hovermode = "closest") %>%
      config(displayModeBar = TRUE, scrollZoom = TRUE)
    
    # Register the click and selection events
    event_register(p, 'plotly_click')
    event_register(p, 'plotly_selected')
    
    p
  })
  
  # Reactive expression to capture selected data
  selected_points <- reactive({
    event_data("plotly_selected", source = "tsnePlot")
  })
  
  # Render the data table based on the selected points with error handling
  output$dataTable <- DT::renderDataTable({
    tryCatch({
      select_data <- selected_points()
      if (!is.null(select_data)) {
        # Get the indices of the selected points
        point_indices <- select_data$pointNumber + 1  # plotly uses 0-based indexing
        # Check if point_indices is within bounds of atlasDataClean
        if (max(point_indices) <= nrow(atlasDataClean)) {
          # Display the data for the selected points
          atlasDataClean[point_indices, ]
        } else {
          data.frame(Message = "Selected points out of range")
        }
      } else {
        # If no points are selected, show a message
        data.frame(Message = "No points selected")
      }
    }, error = function(e) {
      # If an error occurs, print it to console and show a message in the table
      print(paste("Error in dataTable rendering:", e$message))
      data.frame(Message = paste("Error:", e$message))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)