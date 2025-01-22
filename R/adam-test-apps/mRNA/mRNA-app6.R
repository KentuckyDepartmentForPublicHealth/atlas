library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# Pivot geneExpressionData to make column names (filenames) a column
geneExpressionData <- geneExpressionData %>%
  as_tibble(rownames = "ENTREZID") %>%
  pivot_longer(cols = -ENTREZID, names_to = "filename", values_to = "expression")

# Pre-filter the data to include only the relevant genes
selected_genes <- sort(unique(gene_annotations$SYMBOL))

# Shiny app
ui <- fluidPage(
  titlePanel("mRNA Expression Boxplots with Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "diagnosis_final",
        "Select Diagnosis Final",
        choices = unique(atlasDataClean$diagnosisFinal),
        selected = unique(atlasDataClean$diagnosisFinal)[1]
      ),
      selectInput(
        "selected_gene",
        "Select Gene(s)",
        multiple = TRUE,
        selectize = TRUE,
        choices = selected_genes
      ),
      selectInput(
        "group_by",
        "Group By",
        choices = c("grade", "ageGroup", "tumorType", "sex", "compartment", "fullName", "country"),
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
      plotOutput("boxplot"),
      tableOutput("gene_info"),
      width = 9
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    req(input$selected_gene, input$diagnosis_final)
    
    # Filter atlasDataClean by selected diagnosis_final
    atlas_filtered <- atlasDataClean %>%
      filter(diagnosisFinal == input$diagnosis_final)
    
    # Match genes and filenames
    entrez_ids <- gene_annotations %>%
      filter(SYMBOL %in% input$selected_gene) %>%
      pull(ENTREZID)
    
    geneExpressionData %>%
      filter(ENTREZID %in% entrez_ids, filename %in% atlas_filtered$filename) %>%
      left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
      left_join(atlas_filtered, by = "filename")
  })
  
  # Render the boxplot with optional faceting
  output$boxplot <- renderPlot({
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$use_facet) {
      p <- p +
        facet_wrap(as.formula(paste("~", input$group_by)), scales = "free") +
        theme(strip.text = element_text(size = 12, face = "bold"))
    }
    
    p
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