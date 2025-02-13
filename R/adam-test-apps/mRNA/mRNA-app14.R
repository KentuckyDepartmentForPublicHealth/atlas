library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table) # Faster operations

# Load static data before the app starts
# gene_annotations <- as.data.table(readRDS("gene_annotations.rds"))
# atlasDataClean <- as.data.table(readRDS("atlasDataClean.rds"))
# go_to_genes_list <- readRDS("go_to_genes_list.rds")
#
all_possible_genes <- unique(gene_annotations$SYMBOL) # Precomputed static list

ui <- fluidPage(
  titlePanel("mRNA Expression Boxplots"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("search_mode", "Search Mode:",
        choices = c("Select GO Term", "Select All Genes"),
        selected = "Select GO Term"
      ),

      # GO Term selection
      conditionalPanel(
        condition = "input.search_mode == 'Select GO Term'",
        selectInput("go_term", "Select GO Term", choices = c("", names(go_to_genes_list))),
        selectizeInput("selected_gene", "Select Genes", choices = NULL, multiple = TRUE)
      ),

      # All Genes selection
      conditionalPanel(
        condition = "input.search_mode == 'Select All Genes'",
        selectizeInput("all_genes", "Select Genes", choices = all_possible_genes, multiple = TRUE)
      ),

      # Grouping options
      checkboxInput("use_group_by", "Use Group By", value = FALSE),
      conditionalPanel(
        condition = "input.use_group_by",
        selectInput("group_by", "Group By",
          choices = sort(c(
            "isCancerous", "grade", "ageGroup", "tumorType",
            "sex", "compartment", "fullName", "country",
            "diagnosisFinal", "histologyOriginal", "diagnosisClass"
          )),
          selected = "sex"
        )
      ),
      actionButton("run", "Run"), # Run button
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
  # Prevent unnecessary UI updates when switching search modes
  observeEvent(input$search_mode, {
    if (input$search_mode == "Select GO Term") {
      updateSelectizeInput(session, "all_genes", choices = NULL)
    } else {
      updateSelectizeInput(session, "selected_gene", choices = NULL)
    }
  })

  # Update genes dynamically when a GO term is selected
  observeEvent(input$go_term, {
    if (input$search_mode == "Select GO Term" && input$go_term != "") {
      available_genes <- gene_annotations[ENTREZID %in% go_to_genes_list[[input$go_term]], SYMBOL]

      updateSelectizeInput(session, "selected_gene", choices = available_genes)
    }
  })

  # Reactive storage for selected genes
  selected_genes <- reactive({
    if (input$search_mode == "Select GO Term") {
      return(input$selected_gene %||% character(0))
    } else {
      return(input$all_genes %||% character(0))
    }
  })

  # Event reactive for processing data when 'Run' button is clicked
  processed_data <- eventReactive(input$run, {
    req(selected_genes())

    # Show a progress indicator
    withProgress(message = "Processing data...", value = 0, {
      incProgress(0.3, detail = "Filtering gene data...")

      # Filter data efficiently with data.table
      selected_genes_vector <- selected_genes()
      gene_expression <- data.table(SYMBOL = selected_genes_vector, expression = rnorm(length(selected_genes_vector)))

      incProgress(0.5, detail = "Joining with annotations...")

      # Perform efficient joins using data.table
      full_data <- gene_expression[
        gene_annotations,
        on = .(SYMBOL), nomatch = 0
      ][
        atlasDataClean,
        on = "filename", nomatch = 0
      ]

      incProgress(1, detail = "Data ready.")
      return(full_data)
    })
  })

  # Render the boxplot
  output$boxplot <- renderPlot({
    data <- processed_data()

    if (nrow(data) == 0) {
      plot(NULL, xlab = "", ylab = "", main = "No data to display")
      return()
    }

    plot_base <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(
        title = "Expression of Selected Genes",
        subtitle = paste0("Total values contributing: n=", nrow(data)),
        x = "Gene Symbol",
        y = "Expression Level"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (input$use_group_by) {
      plot_base <- plot_base + facet_wrap(~ get(input$group_by), scales = "free")
    }

    plot_base
  })

  # Render gene info table
  output$gene_info <- renderTable({
    data <- processed_data()
    if (nrow(data) == 0) {
      return(data.frame(SYMBOL = character(0), GENENAME = character(0)))
    }
    unique(data[, .(SYMBOL, GENENAME)])
  })
}

shinyApp(ui, server)
