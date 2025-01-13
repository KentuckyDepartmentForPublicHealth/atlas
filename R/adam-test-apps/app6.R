library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(colorspace)

# Function to generate colors dynamically based on the number of categories
generate_colors <- function(num_categories) {
  rainbow_hcl(num_categories, start = 10, end = 350)
}

ui <- fluidPage(
  # Use a dark theme for the overall UI
  tags$style(HTML("
    body {
      background-color: #111111;
      color: white;
    }
    .dataTable {
      color: #00FF00; /* Bright green for text */
      font-family: 'Courier New', Courier, monospace; /* Use Courier or a similar monospaced font */
      font-size: 14px; /* Adjust size as needed */
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
    # Generate colors dynamically based on the number of unique classes
    num_classes <- length(unique(atlasDataClean$diagnosisClass))
    custom_colors <- setNames(generate_colors(num_classes), unique(atlasDataClean$diagnosisClass))
    
    # Calculate centroids for each diagnosisClass
    centroids <- atlasDataClean %>%
      group_by(diagnosisClass) %>%
      summarise(tsne1 = median(tsne1), tsne2 = median(tsne2))
    
    # Creating the plotly plot with dark background
    plot_ly(data = atlasDataClean, 
            x = ~tsne1, y = ~tsne2, 
            type = 'scatter', mode = 'markers',
            color = ~diagnosisClass,
            colors = custom_colors,
            marker = list(size = 5, opacity = 0.8),
            text = ~diagnosisClass,  
            hoverinfo = 'text',
            source = "tsnePlot") %>%
      layout(title = "t-SNE Dimensionality Reduction",
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
  })
  
  # Reactive expression to capture selected data
  selected_points <- reactive({
    event_data("plotly_selected", source = "tsnePlot")
  })
  
  # Render the data table based on the selected points with error handling
  output$dataTable <- DT::renderDataTable({
    select_data <- selected_points()
    if (!is.null(select_data)) {
      point_indices <- select_data$pointNumber + 1  # Convert to 1-based indexing
      if (max(point_indices) <= nrow(atlasDataClean)) {
        atlasDataClean[point_indices, ]
      } else {
        data.frame(Message = "Selected points out of range")
      }
    } else {
      data.frame(Message = "No points selected")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
