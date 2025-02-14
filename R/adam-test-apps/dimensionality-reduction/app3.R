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
                 source = "tsnePlot") %>%  # Add the source ID here
      layout(title = "t-SNE Visualization of Tumor Types",
             xaxis = list(title = "t-SNE 1"),
             yaxis = list(title = "t-SNE 2"))
    
    # Register the click event with the correct source ID
    event_register(p, 'plotly_click')
    
    p
  })
  
  # Reactive expression to capture click data
  clicked_point <- reactive({
    event_data("plotly_click", source = "tsnePlot")
  })
  
  # Render the data table based on the clicked point
  output$dataTable <- DT::renderDataTable({
    click_data <- clicked_point()
    if (!is.null(click_data)) {
      # Get the index of the clicked point
      point_index <- click_data$pointNumber + 1  # plotly uses 0-based indexing
      
      # Display the data for the clicked point
      atlasData[point_index, ]
    } else {
      # If no point is clicked, show an empty data frame
      data.frame()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)