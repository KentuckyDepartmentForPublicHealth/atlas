library(shiny)
library(ggplot2)
library(dplyr)

# Assuming atlasDataClean and geneExpressionData are already loaded
# atlasDataClean should have 'sampleID' and geneExpressionData should have columns for each sample

ui <- fluidPage(
  titlePanel("Gene Expression Boxplot by Sample"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sample", "Choose a Sample:", choices = colnames(geneExpressionData))
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

server <- function(input, output) {
  output$boxplot <- renderPlot({
    # Convert the selected sample column to long format for plotting
    plotData <- geneExpressionData %>%
      select(!!input$sample) %>%
      mutate(gene = row_number()) %>%
      rename(expression = !!input$sample)
    
    # Create the boxplot
    ggplot(plotData, aes(x = factor(1), y = expression)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(title = paste("Gene Expression for Sample:", input$sample),
           x = "Sample", y = "Expression Level") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)