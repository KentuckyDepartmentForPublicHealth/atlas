library(shiny)
library(plotly)
library(dplyr)
library(DT)

# Preloaded gene choices for better performance
geneChoices <- unique(c("All Genes", gene_annotations$SYMBOL))

# UI
ui <- fluidPage(
  tags$style(HTML("
    body { background-color: #111111; color: white; }
    .dataTable { color: #00FF00; font-family: 'Courier New', monospace; font-size: 14px; }
  ")),
  titlePanel("t-SNE Dimensionality Reduction with Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("gene", "Select Gene", choices = NULL, options = list(
        placeholder = 'Select a gene',
        onInitialize = I('function() { this.setValue(""); }')
      )),
      verbatimTextOutput("geneInfo")
    ),
    mainPanel(
      plotlyOutput("tsnePlot"),
      DTOutput("dataTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Update selectizeInput with preloaded gene choices
  updateSelectizeInput(session, 'gene', choices = geneChoices, server = TRUE)
  
  # Reactive to filter data based on the selected gene
  filteredData <- reactive({
    req(input$gene)  # Ensure input is available
    if (input$gene == "All Genes") {
      atlasDataClean
    } else {
      # Filter data based on gene expression logic
      genes_in_sample <- vapply(atlasDataClean$filename, function(x) {
        any(geneExpressionData[[x]] > median(unlist(geneExpressionData)))
      }, logical(1))
      atlasDataClean[genes_in_sample, , drop = FALSE]
    }
  })
  
  # Reactive to handle selected points from the plot
  selectedPoints <- reactive({
    event_data("plotly_selected", source = "A")
  })
  
  # Render the t-SNE plot
  output$tsnePlot <- renderPlotly({
    data <- filteredData()
    validate(need(nrow(data) > 0, "No data available to plot."))
    
    num_classes <- length(unique(data$diagnosisClass))
    custom_colors <- setNames(rainbow(num_classes), unique(data$diagnosisClass))
    
    p <- plot_ly(data = data,
                 x = ~tsne1, y = ~tsne2,
                 type = 'scatter', mode = 'markers',
                 color = ~diagnosisClass, colors = custom_colors,
                 marker = list(size = 5, opacity = 0.8),
                 text = ~paste("Sample:", sampleID, "<br>Diagnosis:", diagnosisClass, "<br>Gene:", input$gene),
                 hoverinfo = 'text',
                 source = "A") %>%
      layout(title = "t-SNE Dimensionality Reduction",
             xaxis = list(title = "t-SNE 1", color = 'white'),
             yaxis = list(title = "t-SNE 2", color = 'white'),
             plot_bgcolor = '#111111', 
             paper_bgcolor = '#111111',
             font = list(color = 'white'))
    
    # Register the 'plotly_selected' event
    event_register(p, "plotly_selected")
    p
  })
  
  # Render the DataTable for selected points
  output$dataTable <- renderDT({
    selected <- selectedPoints()
    req(selected)  # Ensure selection exists
    data <- filteredData()
    
    # Extract rows corresponding to selected points
    selectedData <- data[selected$pointNumber + 1, , drop = FALSE]
    if (nrow(selectedData) > 0) {
      datatable(selectedData, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(data.frame(Message = "No points selected"), options = list(pageLength = 10))
    }
  })
  
  # Render gene information based on the selected gene
  output$geneInfo <- renderPrint({
    req(input$gene)
    if (input$gene != "All Genes") {
      gene_info <- gene_annotations[gene_annotations$SYMBOL == input$gene, ]
      if (nrow(gene_info) > 0) {
        cat(paste0("Gene Symbol: ", gene_info$SYMBOL, "\n",
                   "Gene Name: ", gene_info$GENENAME, "\n",
                   "Entrez ID: ", gene_info$ENTREZID, "\n"))
      } else {
        cat("No information available for this gene.")
      }
    } else {
      cat("Displaying all genes.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
