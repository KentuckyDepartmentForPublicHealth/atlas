library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)

# Define colors for each diagnosis as per the legend in the image
diagnosis_colors <- c(
  "Angiocentric" = "red", "Diffuse Glioma" = "pink", "G34 Mutant" = "darkred", "IDH Mutant" = "lightpink", "Midline" = "firebrick",
  # Add all other colors here matching the image's legend
  "Choroid Plexus Papilloma" = "darkgreen",
  # ... and so on for all diagnoses
  "Non-tumor" = "grey"
)

ui <- fluidPage(
  titlePanel("t-SNE Dimensionality Reduction"),
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
      geom_point(aes(color = `Final diagnosis`), alpha = 0.7, size = 2) +
      scale_color_manual(values = diagnosis_colors) +
      theme_minimal() +
      labs(title = "t-SNE Dimensionality Reduction (5248 samples)",
           x = "t-SNE 1",
           y = "t-SNE 2") +
      theme(legend.position = "right")  # Position legend on the right
    
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
    
    # Add legend entries
    p + guides(color = guide_legend(override.aes = list(size = 3)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)