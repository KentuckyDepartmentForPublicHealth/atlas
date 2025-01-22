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
  titlePanel("mRNA Expression Boxplots with Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_gene", "Select a Gene", choices = selected_genes),
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
    # Select the ENTREZID corresponding to the selected gene
    entrez_id <- gene_annotations %>%
      filter(SYMBOL == input$selected_gene) %>%
      pull(ENTREZID)
    
    # Subset gene expression data for the selected gene
    gene_data <- geneExpressionData %>%
      filter(ENTREZID == entrez_id) %>%
      pivot_longer(cols = -ENTREZID, names_to = "filename", values_to = "expression")
    
    # Join only necessary columns from gene_annotations and atlasDataClean
    gene_data %>%
      left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
      left_join(atlasDataClean %>% select(filename, grade, ageGroup, tumorType, sex, compartment, fullName, country), by = "filename")
  })
  
  # Render the boxplot
  output$boxplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = .data[[input$group_by]], y = expression)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(
        title = paste("Expression of", input$selected_gene),
        x = input$group_by,
        y = "Expression Level"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
