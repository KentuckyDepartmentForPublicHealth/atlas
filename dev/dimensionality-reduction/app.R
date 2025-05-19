library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)

# Assuming 'atlasData' is already loaded in your environment
ui <- fluidPage(
  titlePanel("t-SNE Visualization of Tumor Types"),
  sidebarLayout(
    sidebarPanel(
      selectInput("diagnosis", "Select Diagnosis:", 
                  choices = c("All", unique(atlasData$`Final diagnosis`))),
      checkboxInput("showAll", "Show All Labels", value = TRUE)
    ),
    mainPanel(
      plotOutput("tsnePlot")
    )
  )
)

server <- function(input, output) {
  output$tsnePlot <- renderPlot({
    # Filter data based on selected diagnosis
    filtered_data <- if (input$diagnosis == "All") {
      atlasData
    } else {
      atlasData %>% filter(`Final diagnosis` == input$diagnosis)
    }
    
    # Compute the position for labels
    diagnosis_positions <- filtered_data %>%
      group_by(`Final diagnosis`) %>%
      summarise(
        tsne1 = median(tsne1),
        tsne2 = median(tsne2)
      )
    
    # Create the plot
    p <- ggplot(filtered_data, aes(x = tsne1, y = tsne2)) +
      geom_point(aes(color = `Final diagnosis`), alpha = 0.7, size = 2)
    
    # Add labels if 'showAll' is checked or only for the selected diagnosis
    if(input$showAll) {
      p <- p + 
        geom_text_repel(data = diagnosis_positions, 
                        aes(label = `Final diagnosis`, x = tsne1, y = tsne2),
                        size = 3, 
                        max.overlaps = Inf,  
                        box.padding = 0.5, 
                        point.padding = 0.5, 
                        segment.color = 'grey50')
    } else {
      p <- p + 
        geom_text_repel(data = diagnosis_positions, 
                        aes(label = `Final diagnosis`, x = tsne1, y = tsne2),
                        size = 3, 
                        max.overlaps = Inf,  
                        box.padding = 0.5, 
                        point.padding = 0.5, 
                        segment.color = 'grey50')
    }
    
    p + 
      theme_minimal() +
      labs(title = "t-SNE Visualization of Tumor Types",
           x = "t-SNE 1",
           y = "t-SNE 2") +
      theme(legend.position = "none")  # Remove legend since we're using labels
  })
}

# Run the application 
shinyApp(ui = ui, server = server)