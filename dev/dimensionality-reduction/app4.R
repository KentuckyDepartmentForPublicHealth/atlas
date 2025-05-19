library(shiny)
library(plotly)
library(dplyr)
library(DT)

# Assuming 'atlasData' is already loaded in your environment
# If not, you'll need to load it or create it before running the app

ui <- fluidPage(
  titlePanel("t-SNE Visualization with Data Table"),
  sidebarLayout(
    sidebarPanel(
      # Removed the selectInput since we're going for a simpler interaction
    ),
    mainPanel(
      plotlyOutput("tsnePlot"),
      DT::dataTableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$tsnePlot <- renderPlotly({
    # Define your own color palette
    custom_colors <- setNames(rainbow(length(unique(atlasData$`Final diagnosis`))), unique(atlasData$`Final diagnosis`))
    
    p <- plot_ly(data = atlasData, 
                 x = ~tsne1, y = ~tsne2, 
                 type = 'scatter', mode = 'markers',
                 color = ~`Final diagnosis`,
                 colors = custom_colors,
                 marker = list(size = 8, opacity = 0.7),
                 text = ~`Final diagnosis`,  
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
      atlasData[point_indices, ]
    } else {
      # If no points are selected, show an empty data frame
      data.frame()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)