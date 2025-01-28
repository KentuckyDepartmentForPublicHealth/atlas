library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Ensure these data frames are loaded before running the app:
# - `gene_annotations` should contain `SYMBOL`, `ENTREZID`, and `GENENAME`.
# - `geneExpressionData` should be a data frame where rownames are `ENTREZID` and columns are filenames.
# - `atlasDataClean` should contain metadata with columns such as `filename`, `grade`, `ageGroup`, `tumorType`, etc.

# Shiny App
ui <- fluidPage(
  titlePanel("mRNA Expression Boxplots with Gene Annotations (Facet Toggle)"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("max_options_slider"),  # Dynamic slider UI
      selectizeInput(
        "selected_gene", 
        "Select a Gene", 
        multiple = TRUE,
        choices = NULL,
        options = list(maxOptions = 5)  # Default value
      ),
      selectInput(
        "group_by",
        "Group By",
        choices = c("grade", "ageGroup", "tumorType", "sex", "compartment", "fullName", "country", "diagnosisFinal", "histologyOriginal", "diagnosisClass") |> sort(),
        selected = NA
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

server <- function(input, output, session) {
  # Validate datasets at the start
  validate(
    need(exists("gene_annotations"), "Error: `gene_annotations` is not loaded."),
    need(exists("geneExpressionData"), "Error: `geneExpressionData` is not loaded."),
    need(exists("atlasDataClean"), "Error: `atlasDataClean` is not loaded.")
  )
  
  # Remove rows with NA ENTREZID in gene_annotations
  gene_annotations <- gene_annotations %>% filter(!is.na(ENTREZID))
  
  # Calculate the total number of unique genes
  total_unique_genes <- rownames(geneExpressionData) %>% unique() %>% length()
  
  # Filter SYMBOLs from gene_annotations that match ENTREZID in geneExpressionData
  selected_genes <- gene_annotations %>%
    filter(ENTREZID %in% rownames(geneExpressionData)) %>%
    pull(SYMBOL) %>%
    unique() %>%
    sort()
  
  validate(
    need(length(selected_genes) > 0, "Error: No matching genes in the dataset.")
  )
  
  # Render sliderInput
  output$max_options_slider <- renderUI({
    sliderInput(
      "max_options",
      "Total unique genes",
      min = 1,
      max = total_unique_genes,
      value = min(5, total_unique_genes),
      step = 1
    )
  })
  
  # Dynamically update selectizeInput
  observe({
    updateSelectizeInput(
      session,
      'selected_gene',
      choices = selected_genes,
      options = list(maxOptions = input$max_options),
      server = TRUE
    )
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$selected_gene)
    
    entrez_ids <- gene_annotations %>%
      filter(SYMBOL %in% input$selected_gene) %>%
      pull(ENTREZID)
    
    validate(
      need(length(entrez_ids) > 0, "Error: No matching genes found for the selected input.")
    )
    
    gene_data <- geneExpressionData[rownames(geneExpressionData) %in% entrez_ids, ]
    
    validate(
      need(nrow(gene_data) > 0, "Error: No data found after filtering.")
    )
    
    # Convert to long format and join metadata
    gene_data <- gene_data %>%
      as.data.frame() %>%
      mutate(ENTREZID = rownames(.)) %>%
      pivot_longer(cols = -ENTREZID, names_to = "filename", values_to = "expression") %>%
      left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
      left_join(atlasDataClean, by = "filename")
    
    gene_data
  })
  
  # Render the boxplot
  output$boxplot <- renderPlot({
    data <- filtered_data()
    count <- nrow(data)
    
    # Count for each facet or overall counts
    if(input$use_facet) {
      facet_counts <- data %>%
        group_by(SYMBOL, .data[[input$group_by]]) %>%
        summarise(n = n(), .groups = 'drop') 
    } else {
      facet_counts <- data %>%
        group_by(SYMBOL) %>%
        summarise(n = n(), .groups = 'drop') 
    }
    
    # Modify plot according to facet usage
    plot_base <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      geom_text(data = facet_counts, aes(label = paste0("n=", n), y = Inf), vjust = 1.5, size = 3) +
      labs(
        title = "Expression of Selected Genes",
        subtitle = paste0("Total values contributing: n=", count),
        x = "Gene Symbol",
        y = "Expression Level"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    if(input$use_facet) {
      plot_base <- plot_base +
      facet_wrap(~ .data[[input$group_by]], scales = "free")
    }
    
    plot_base
  })
  
  # Render gene info table
  output$gene_info <- renderTable({
    req(filtered_data())
    filtered_data() %>%
      select(SYMBOL, GENENAME) %>%
      distinct()
  })
}

shinyApp(ui, server)