library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(plotly)  # Added library for Plotly

# Convert rownames of geneExpressionData to a column
# geneExpressionData <- geneExpressionData %>%
#   rownames_to_column(var = "ENTREZID")

# Pre-filter the data to include only the relevant genes (to be dynamically selected later)
selected_genes <- unique(gene_annotations$SYMBOL) |> sort()

# Shiny app
ui <- fluidPage(
  titlePanel("mRNA Expression Boxplots with Gene Annotations (Facet Toggle)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_gene", 
        "Select a Gene", 
        multiple = TRUE,
        size = 15,
        selectize = FALSE,
        choices = selected_genes
      ),
      selectInput(
        "group_by",
        "Group By",
        choices = c("grade", "ageGroup", "tumorType", "sex", "compartment", "fullName", "country", "diagnosisFinal"),
        selected = "grade"
      ),
      checkboxInput(
        "use_facet",
        "Use Facet Wrap",
        value = FALSE
      ),
      width = 3
    ),
    mainPanel(
      plotlyOutput("boxplot"),  # Changed to plotlyOutput
      tableOutput("gene_info"),
      width = 9
    )
  )
)

server <- function(input, output) {
  # Filter data dynamically to reduce memory usage
  filtered_data <- reactive({
    req(input$selected_gene)  # Ensure a gene is selected
    
    # Select the ENTREZID corresponding to the selected genes
    entrez_ids <- gene_annotations %>%
      filter(SYMBOL %in% input$selected_gene) %>%
      pull(ENTREZID)
    
    # Subset gene expression data for the selected genes
    gene_data <- geneExpressionData %>%
      filter(ENTREZID %in% entrez_ids) %>%
      pivot_longer(cols = -ENTREZID, names_to = "filename", values_to = "expression")
    
    # Join only necessary columns from gene_annotations and atlasDataClean
    gene_data %>%
      left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
      left_join(atlasDataClean %>% select(filename, grade, ageGroup, tumorType, sex, compartment, fullName, country, diagnosisFinal), by = "filename")
  })
  
  # Render the boxplot with optional faceting using Plotly
  output$boxplot <- renderPlotly({
    req(filtered_data())
    p <- ggplot(filtered_data(), aes(x = SYMBOL, y = expression, color = SYMBOL)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(
        title = "Expression of Selected Genes",
        x = "Gene Symbol",
        y = "Expression Level"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Add faceting if use_facet is TRUE
    if (input$use_facet) {
      p <- p +
        facet_wrap(~ .data[[input$group_by]], scales = "free") +
        theme(strip.text = element_text(size = 12, face = "bold"))
    }
    
    # Convert ggplot to plotly object
    ggplotly(p)
  })
  
  # Render the gene information table
  output$gene_info <- renderTable({
    req(filtered_data())
    filtered_data() %>%
      select(SYMBOL, GENENAME) %>%
      distinct()
  })
}

shinyApp(ui, server)