library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyjs)
library(rlang) # For the %||% operator if needed

ui <- fluidPage(
  useShinyjs(),
  titlePanel("mRNA Expression Boxplots with Gene Ontology and Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      #------------------#
      #   Search Mode    #
      #------------------#
      radioButtons(
        "search_mode", "Search Mode:",
        choices = c("Select GO Term", "Select All Genes"),
        selected = character(0)
      ),

      # Conditionally show GO-term input and gene selector
      conditionalPanel(
        condition = "input.search_mode === 'Select GO Term'",
        selectizeInput("go_term", "Select GO Term", choices = NULL),
        uiOutput("go_gene_selector")
      ),

      # Conditionally show the "All Genes" selector
      conditionalPanel(
        condition = "input.search_mode === 'Select All Genes'",
        selectizeInput(
          "all_genes", "Select Genes",
          choices = NULL, multiple = TRUE,
          options = list(maxOptions = 5)
        )
      ),

      # Slider to set how many choices appear in the dropdown
      uiOutput("max_options_slider"),

      #----------------------#
      #   "Use Group By" UI  #
      #----------------------#
      conditionalPanel(
        # Only show after a search mode has been chosen (you can tweak this condition)
        condition = "input.search_mode !== null && input.search_mode !== ''",

        # A checkbox to turn grouping on/off
        checkboxInput("use_group_by", "Use Group By", value = FALSE),

        # Placeholder for the dynamic group_by selectInput
        uiOutput("group_by_selector")
      ),

      #-------------------------------#
      #    Run and Reset Buttons      #
      #-------------------------------#
      br(), # Add a bit of space
      actionButton("run", "Run Plot", class = "btn-primary"),
      actionButton("reset", "Reset", class = "btn-warning"),
      width = 3
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.search_mode === null || input.search_mode === ''",
        h3("Please select a search mode to proceed.")
      ),
      plotOutput("boxplot"),
      tableOutput("gene_info"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  #---- 1. Validate required data ----#
  validate(
    need(exists("gene_annotations"), "Error: `gene_annotations` is not loaded."),
    need(exists("geneExpressionData"), "Error: `geneExpressionData` is not loaded."),
    need(exists("atlasDataClean"), "Error: `atlasDataClean` is not loaded."),
    need(exists("go_to_genes_list"), "Error: `go_to_genes_list` is not loaded.")
  )

  # Clean gene_annotations to ensure no NA ENTREZID
  gene_annotations <- gene_annotations %>% filter(!is.na(ENTREZID))

  #---- 2. Pre-compute or reactive for the gene lists ----#
  all_valid_genes <- gene_annotations %>%
    filter(ENTREZID %in% rownames(geneExpressionData)) %>%
    pull(SYMBOL) %>%
    unique() %>%
    sort()

  # Populate GO terms in the go_term input (server-side selectize)
  updateSelectizeInput(
    session, "go_term",
    choices = names(go_to_genes_list),
    selected = NULL,
    server = TRUE
  )

  #---- 3. Dynamic total unique genes (for the slider) ----#
  total_unique_genes <- reactive({
    req(input$search_mode)

    if (input$search_mode == "Select GO Term") {
      req(input$go_term)
      if (input$go_term == "") {
        return(0)
      }
      go_genes <- go_to_genes_list[[input$go_term]] %||% character(0)
      gene_symbols <- gene_annotations %>%
        filter(ENTREZID %in% go_genes) %>%
        pull(SYMBOL) %>%
        unique()
      length(gene_symbols)
    } else if (input$search_mode == "Select All Genes") {
      length(all_valid_genes)
    } else {
      0
    }
  })

  #---- 4. Render the slider for max options ----#
  output$max_options_slider <- renderUI({
    req(total_unique_genes() > 0)
    sliderInput(
      "max_options",
      label = "Number of Genes to Display in Dropdown",
      min = 1,
      max = total_unique_genes(),
      value = min(5, total_unique_genes()),
      step = 1
    )
  })

  #---- 5. Update the "all_genes" input if "Select All Genes" mode is chosen ----#
  observe({
    req(input$search_mode)
    if (input$search_mode == "Select All Genes") {
      updateSelectizeInput(
        session,
        "all_genes",
        choices = all_valid_genes,
        options = list(
          maxOptions = min(input$max_options, length(all_valid_genes))
        ),
        server = TRUE
      )
    }
  })

  #---- 6. Dynamically render gene selector for GO Terms ----#
  output$go_gene_selector <- renderUI({
    req(input$search_mode == "Select GO Term", input$go_term)

    # If no GO term is selected, return empty
    if (input$go_term == "") {
      choices <- character(0)
    } else {
      go_genes <- go_to_genes_list[[input$go_term]] %||% character(0)
      choices <- gene_annotations %>%
        filter(ENTREZID %in% go_genes) %>%
        pull(SYMBOL) %>%
        unique() %>%
        sort()
    }

    selectizeInput(
      "selected_gene",
      "Select Genes",
      multiple = TRUE,
      choices = choices,
      options = list(
        maxOptions = min(input$max_options %||% 5, length(choices))
      )
    )
  })

  #---- 7. Reactive expression for the final set of selected genes ----#
  selected_genes <- reactive({
    req(input$search_mode)
    if (input$search_mode == "Select GO Term") {
      input$selected_gene %||% character(0)
    } else if (input$search_mode == "Select All Genes") {
      input$all_genes %||% character(0)
    } else {
      character(0)
    }
  })

  #---- 8. Hide or show the "Use Group By" checkbox based on selected genes ----#
  # observe({
  #   if (length(selected_genes()) > 0) {
  #     shinyjs::show("use_group_by")
  #   } else {
  #     shinyjs::hide("use_group_by")
  #     updateCheckboxInput(session, "use_group_by", value = FALSE)
  #   }
  # })

  #---- 9. Dynamically render the group_by dropdown (only if checkbox is TRUE) ----#
  output$group_by_selector <- renderUI({
    if (input$use_group_by) {
      selectInput(
        "group_by",
        "Group By",
        choices = c(
          "isCancerous", "grade", "ageGroup", "tumorType", "sex",
          "compartment", "fullName", "country", "diagnosisFinal",
          "histologyOriginal", "diagnosisClass"
        ) |> sort(),
        selected = "sex"
      )
    }
  })

  #---------------------------------------------------------------------#
  #  NEW: Use eventReactive() to delay data filtering until "Run" click  #
  #---------------------------------------------------------------------#
  filtered_data_event <- eventReactive(input$run, {
    # Isolate all relevant inputs so the data only updates upon "Run"
    # (not each time one of the inputs changes)
    isolate({
      req(input$search_mode)
      req(selected_genes())

      if (length(selected_genes()) == 0) {
        return(data.frame())
      }

      entrez_ids <- gene_annotations %>%
        filter(SYMBOL %in% selected_genes()) %>%
        pull(ENTREZID)
      if (length(entrez_ids) == 0) {
        return(data.frame())
      }

      # Subset the gene expression matrix
      gene_data <- geneExpressionData[rownames(geneExpressionData) %in% entrez_ids, ]
      if (nrow(gene_data) == 0) {
        return(data.frame())
      }

      # Reshape and merge with annotations
      gene_data <- gene_data %>%
        as.data.frame() %>%
        mutate(ENTREZID = rownames(.)) %>%
        pivot_longer(
          cols = -ENTREZID,
          names_to = "filename",
          values_to = "expression"
        ) %>%
        left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
        left_join(atlasDataClean, by = "filename")

      gene_data
    })
  })

  #---- 11. Boxplot Output (Uses eventReactive data) ----#
  output$boxplot <- renderPlot({
    data <- filtered_data_event()
    if (nrow(data) == 0) {
      plot(NULL, xlab = "", ylab = "", main = "No data to display")
      return()
    }

    count <- nrow(data)
    p <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
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

    # Only facet if "Use Group By" is on and a valid input$group_by is chosen
    if (input$use_group_by && !is.null(input$group_by)) {
      facet_counts <- data %>%
        group_by(SYMBOL, .data[[input$group_by]]) %>%
        summarise(n = n(), .groups = "drop")

      p <- p +
        facet_wrap(~ .data[[input$group_by]], scales = "free") +
        geom_text(
          data = facet_counts,
          aes(label = paste0("n=", n), y = Inf),
          vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
        )
    } else {
      # Single group (no facet), but add n labels per symbol
      symbol_counts <- data %>%
        group_by(SYMBOL) %>%
        summarise(n = n(), .groups = "drop")

      p <- p +
        geom_text(
          data = symbol_counts,
          aes(label = paste0("n=", n), y = Inf),
          vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
        )
    }

    p
  })

  #---- 12. Render gene info table (Uses eventReactive data) ----#
  output$gene_info <- renderTable({
    data <- filtered_data_event()
    if (nrow(data) == 0) {
      return(NULL)
    }
    data %>%
      select(SYMBOL, GENENAME) %>%
      distinct()
  })

  #--------------------------------------------------#
  #    NEW: Reset Button to Clear/Undo All Inputs    #
  #--------------------------------------------------#
  observeEvent(input$reset, {
    # Clear everything: radio buttons, selectize inputs, sliders, checkboxes, etc.
    updateRadioButtons(session, "search_mode", selected = character(0))
    updateSelectizeInput(session, "go_term", selected = NULL)
    updateSelectizeInput(session, "selected_gene", selected = NULL)
    updateSelectizeInput(session, "all_genes", selected = NULL)
    if (!is.null(total_unique_genes())) {
      # reset slider to 5 or 1, whichever is more appropriate as a "default"
      updateSliderInput(session, "max_options", value = 5)
    }
    updateCheckboxInput(session, "use_group_by", value = FALSE)

    # Optionally, hide or reset group_by selector
    shinyjs::hide("group_by_selector")

    # (Optional) You could also force the plot to display empty if you want:
    # by clearing or invalidating the eventReactive. But typically, if no gene is
    # selected, it will show "No data to display" anyway.
  })
}

shinyApp(ui = ui, server = server)
