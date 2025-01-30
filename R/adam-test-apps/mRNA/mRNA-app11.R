library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("mRNA Expression Boxplots with Gene Ontology and Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("go_term", "Select GO Term", choices = c("", names(go_to_genes_list))),
      uiOutput("gene_selector"),
      uiOutput("max_options_slider"),
      hidden(
        checkboxInput("use_group_by", "Use Group By", value = FALSE)
      ),
      hidden(
        selectInput(
          "group_by",
          "Group By",
          choices = c("isCancerous", "grade", "ageGroup", "tumorType", "sex", "compartment", "fullName", "country", "diagnosisFinal", "histologyOriginal", "diagnosisClass") |> sort(),
          selected = "sex"
        )
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
    need(exists("atlasDataClean"), "Error: `atlasDataClean` is not loaded."),
    need(exists("go_to_genes_list"), "Error: `go_to_genes_list` is not loaded.")
  )
  
  gene_annotations <- gene_annotations %>% filter(!is.na(ENTREZID))
  
  # Dynamic total unique genes based on GO term selection
  total_unique_genes <- reactive({
    req(input$go_term)
    if(input$go_term == "") {
      return(0)  # No GO term selected, no genes to show
    }
    
    go_genes <- go_to_genes_list[[input$go_term]]
    gene_symbols <- gene_annotations %>%
      filter(ENTREZID %in% go_genes) %>%
      pull(SYMBOL) %>%
      unique()
    
    return(length(gene_symbols))
  })
  
  # Dynamic gene selector based on GO term selection
  output$gene_selector <- renderUI({
    req(input$go_term)
    if(input$go_term == "") {
      choices <- c()
    } else {
      go_genes <- go_to_genes_list[[input$go_term]]
      gene_symbols <- gene_annotations %>%
        filter(ENTREZID %in% go_genes) %>%
        pull(SYMBOL) %>%
        unique()
      choices <- gene_symbols
    }
    
    selectizeInput(
      "selected_gene", 
      "Select Genes", 
      multiple = TRUE,
      choices = choices,
      options = list(maxOptions = min(input$max_options, length(choices)))
    )
  })
  
  # Render sliderInput for max options, dynamically based on the number of genes for the selected GO term
  output$max_options_slider <- renderUI({
    sliderInput(
      "max_options",
      "Total unique genes",
      min = 1,
      max = total_unique_genes(),
      value = min(5, total_unique_genes()),
      step = 1
    )
  })
  
  # Show/hide group_by inputs
  observe({
    if(!is.null(input$selected_gene) && length(input$selected_gene) > 0) {
      shinyjs::show("use_group_by")
      if(input$use_group_by) shinyjs::show("group_by") else shinyjs::hide("group_by")
    } else {
      shinyjs::hide("use_group_by")
      shinyjs::hide("group_by")
    }
  })
  
  filtered_data <- reactive({
    req(input$selected_gene)
    
    entrez_ids <- gene_annotations %>%
      filter(SYMBOL %in% input$selected_gene) %>%
      pull(ENTREZID)
    
    if(length(entrez_ids) == 0) return(data.frame())
    
    gene_data <- geneExpressionData[rownames(geneExpressionData) %in% entrez_ids, ]
    if(nrow(gene_data) == 0) return(data.frame())
    
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
    if(is.null(data) || nrow(data) == 0) {
      plot(NULL, xlab = "", ylab = "", main = "No data to display")
    } else {
      count <- nrow(data)
      plot_base <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
        geom_boxplot() +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(
          title = "Expression of Selected Genes",
          subtitle = paste0("Total values contributing: n=", count),
          x = "Gene Symbol",
          y = "Expression Level"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if(input$use_group_by) {
        facet_counts <- data %>%
          group_by(SYMBOL, .data[[input$group_by]]) %>%
          summarise(n = n(), .groups = 'drop') 
        plot_base <- plot_base +
          facet_wrap(~ .data[[input$group_by]], scales = "free") +
          geom_text(data = facet_counts, aes(label = paste0("n=", n), y = Inf), vjust = 1.5, size = 3, fontface = 'bold', color = 'gray20')
      } else {
        symbol_counts <- data %>%
          group_by(SYMBOL) %>%
          summarise(n = n(), .groups = 'drop') 
        plot_base <- plot_base +
          geom_text(data = symbol_counts, aes(label = paste0("n=", n), y = Inf), vjust = 1.5, size = 3, fontface = 'bold', color = 'gray20')
      }
      plot_base
    }
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