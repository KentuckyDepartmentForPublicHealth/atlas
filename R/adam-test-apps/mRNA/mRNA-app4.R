library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# Convert rownames of geneExpressionData to a column
geneExpressionData <- geneExpressionData %>%
  rownames_to_column(var = "ENTREZID")

# Pre-filter the data to include only the relevant genes (to be dynamically selected later)
selected_genes <- unique(gene_annotations$SYMBOL)

# Shiny app
ui <- fluidPage(
  titlePanel("mRNA Expression Boxplots with Gene Annotations (Multiple Genes)"),
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
        choices = c("grade", "ageGroup", "tumorType", "sex", "compartment", "fullName", "country"),
        selected = "grade"
      ),
      width = 3
    ),
    mainPanel(
      plotOutput("boxplot"),
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
      left_join(atlasDataClean %>% select(filename, grade, ageGroup, tumorType, sex, compartment, fullName, country), by = "filename")
  })
  
  # Render the boxplot with faceting
  output$boxplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = SYMBOL, y = expression, color = SYMBOL)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      facet_wrap(~ .data[[input$group_by]], scales = "free") +  # Add dynamic faceting
      labs(
        title = paste("Expression of Selected Genes"),
        x = "Gene Symbol",
        y = "Expression Level"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12, face = "bold")
      )
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
