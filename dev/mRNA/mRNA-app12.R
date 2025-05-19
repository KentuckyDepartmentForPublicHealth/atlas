library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyjs)
# remotes::install_github("daattali/shinyjs")
library(shinyalert)
library(rlang) # For %||% operator if needed

ui <- fluidPage(
  useShinyjs(), # Enable shinyjs
  titlePanel("mRNA Expression Boxplots with Gene Ontology and Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      # ----------------------- #
      #     Search Mode Radio   #
      # ----------------------- #
      radioButtons(
        "search_mode", "Search Mode:",
        choices = c("Select GO Term", "Select All Genes"),
        selected = character(0) # Start unselected
      ),

      # ------------------------------------- #
      #   Conditional UI for "Select GO Term" #
      # ------------------------------------- #
      conditionalPanel(
        condition = "input.search_mode === 'Select GO Term'",
        selectizeInput(
          "go_term",
          "Select GO Term",
          choices = NULL
        ),
        uiOutput("go_gene_selector") # Dynamically rendered below
      ),

      # -------------------------------------- #
      #   Conditional UI for "Select All Genes"#
      # -------------------------------------- #
      conditionalPanel(
        condition = "input.search_mode === 'Select All Genes'",
        selectizeInput(
          "all_genes", "Select Genes",
          choices = NULL, multiple = TRUE,
          options = list(maxOptions = 5) # will update in server
        )
      ),

      # ----------------------------- #
      #   Dynamic Slider for Choices  #
      # ----------------------------- #
      uiOutput("max_options_slider"),

      # -------------------------------------------------- #
      #   "Use Group By" Checkbox (Initially Hidden via JS) #
      # -------------------------------------------------- #
      checkboxInput("use_group_by", "Use Group By", value = FALSE),

      # --------------------------------- #
      #   UI Output: Group Selector       #
      #   (Visible only if checkbox is on)#
      # --------------------------------- #
      uiOutput("group_by_selector"),
      br(),
      fluidRow(
        column(
          12,
          div(
            style = "text-align: center;",
            actionButton("run", "Run Plot",
              icon = icon("play"),
              class = "btn-primary btn-lg",
              style = "width: 90%; margin: 10px;"
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            style = "text-align: center;",
            actionButton("reset", "Clear",
              icon = icon("eraser"),
              class = "btn-warning btn-lg",
              style = "width: 90%; margin: 10px;"
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            style = "text-align: center;",
            actionButton("reload", "Reload",
              icon = icon("sync"),
              class = "btn-danger btn-lg",
              style = "width: 90%; margin: 10px;"
            )
          )
        )
      ),
      width = 3
    ),
    mainPanel(
      # If no search mode chosen, show a help message
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
  # 1. Validate that required data objects exist
  validate(
    need(exists("gene_annotations"), "Error: `gene_annotations` is not loaded."),
    need(exists("geneExpressionData"), "Error: `geneExpressionData` is not loaded."),
    need(exists("atlasDataClean"), "Error: `atlasDataClean` is not loaded."),
    need(exists("go_to_genes_list"), "Error: `go_to_genes_list` is not loaded.")
  )

  # Ensure no NA ENTREZID
  gene_annotations <- gene_annotations %>% filter(!is.na(ENTREZID))

  # 2. Precompute all valid genes
  all_valid_genes <- gene_annotations %>%
    filter(ENTREZID %in% rownames(geneExpressionData)) %>%
    pull(SYMBOL) %>%
    unique() %>%
    sort()

  # Hide "Use Group By" by default on load
  shinyjs::hide("use_group_by")
  # Reload Page on Reload Button
  observeEvent(input$reload, {
    shinyjs::refresh()
  })

  # 3. Populate GO-Term dropdown, removing 1000-item limit
  updateSelectizeInput(
    session, "go_term",
    choices = names(go_to_genes_list),
    selected = NULL,
    server = TRUE,
    options = list(maxOptions = 999999) # show all GO terms, not just first 1000
  )

  # ------------------------ #
  #   Reactive: # of genes   #
  # ------------------------ #
  total_unique_genes <- reactive({
    req(input$search_mode) # Must have a non-empty search_mode to proceed

    if (input$search_mode == "Select GO Term") {
      req(input$go_term)
      if (input$go_term == "") {
        return(0)
      }
      go_genes <- go_to_genes_list[[input$go_term]] %||% character(0)
      length(
        gene_annotations %>%
          filter(ENTREZID %in% go_genes) %>%
          pull(SYMBOL) %>%
          unique()
      )
    } else if (input$search_mode == "Select All Genes") {
      length(all_valid_genes)
    } else {
      0
    }
  })

  # ----------------------------- #
  #  Render the "max_options" UI  #
  # ----------------------------- #
  output$max_options_slider <- renderUI({
    req(total_unique_genes() > 0)
    sliderInput(
      "max_options",
      "Number of Genes to Display in Dropdown",
      min = 1,
      max = total_unique_genes(),
      value = min(5, total_unique_genes()),
      step = 1
    )
  })

  # --------------------- #
  #  Update all_genes UI  #
  # --------------------- #
  # ## FIX ## use isTRUE(...) to avoid length zero warnings
  observeEvent(input$search_mode, {
    if (isTRUE(input$search_mode == "Select All Genes")) {
      updateSelectizeInput(
        session,
        "all_genes",
        choices = all_valid_genes,
        options = list(maxOptions = 5),
        server = TRUE
      )
    }
  })

  observeEvent(input$max_options, {
    # ## FIX ##
    if (isTRUE(input$search_mode == "Select All Genes")) {
      updateSelectizeInput(
        session,
        "all_genes",
        choices = all_valid_genes,
        options = list(maxOptions = min(input$max_options, length(all_valid_genes))),
        server = TRUE
      )
    }
  })

  # ----------------------- #
  #  Render GO gene select  #
  # ----------------------- #
  output$go_gene_selector <- renderUI({
    req(input$search_mode == "Select GO Term", input$go_term)

    # If no GO term is selected, choices is empty
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

  # -------------------- #
  # Reactive for # Genes #
  # -------------------- #
  selected_genes_now <- reactive({
    # If user has not selected a search mode yet => zero-length => return empty
    if (is.null(input$search_mode) || length(input$search_mode) == 0) {
      return(character(0))
    }

    if (input$search_mode == "Select GO Term") {
      input$selected_gene %||% character(0)
    } else if (input$search_mode == "Select All Genes") {
      input$all_genes %||% character(0)
    } else {
      character(0)
    }
  })

  # ----------------------------------------------------- #
  # Observe # Genes; show/hide "Use Group By" accordingly #
  # ----------------------------------------------------- #
  observe({
    if (length(selected_genes_now()) > 0) {
      shinyjs::show("use_group_by")
    } else {
      shinyjs::hide("use_group_by")
      updateCheckboxInput(session, "use_group_by", value = FALSE)
    }
  })

  # ------------------------------------- #
  #  UI Output for the group_by selector  #
  # ------------------------------------- #
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

  # ---------------------------------------------------------------- #
  #  eventReactive: Actually filter data and store group-by choices  #
  #  Triggered only by "Run Plot"                                    #
  # ---------------------------------------------------------------- #
  final_data <- eventReactive(input$run, {
    # Get the currently selected genes using the same function
    # that's used in the data processing
    chosen_genes <- selected_genes_now()
    
    if (length(chosen_genes) == 0) {
      shinyalert::shinyalert(
        "Oops!", "You must select at least one gene to run the plot.", 
        type = "error",
        confirmButtonCol = '#0B3B60',
        size = 'm'
      )
      return(NULL)
    }

    isolate({
      # Remove this redundant check since we already did it above
      # chosen_genes <- selected_genes_now()
      # if (length(chosen_genes) == 0) {
      #   return(list(df = data.frame(), doGroup = FALSE, grpVar = NULL))
      # }

      # 2) Convert chosen SYMBOLs to ENTREZID
      entrez_ids <- gene_annotations %>%
        filter(SYMBOL %in% chosen_genes) %>%
        pull(ENTREZID)

      # If no valid ENTREZID, no data
      if (length(entrez_ids) == 0) {
        return(list(df = data.frame(), doGroup = FALSE, grpVar = NULL))
      }

      # 3) Subset gene expression matrix
      gene_data <- geneExpressionData[rownames(geneExpressionData) %in% entrez_ids, ]
      if (nrow(gene_data) == 0) {
        return(list(df = data.frame(), doGroup = FALSE, grpVar = NULL))
      }

      # 4) Pivot longer and join metadata
      gene_data <- gene_data %>%
        as.data.frame() %>%
        mutate(ENTREZID = rownames(.)) %>%
        pivot_longer(
          cols = -ENTREZID,
          names_to = "filename",
          values_to = "expression"
        ) %>%
        left_join(
          gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME),
          by = "ENTREZID"
        ) %>%
        left_join(atlasDataClean, by = "filename")

      # 5) Also capture whether user wants to group, and which grouping var
      do_group <- input$use_group_by
      grp_var <- input$group_by

      list(
        df      = gene_data,
        doGroup = do_group,
        grpVar  = grp_var
      )
    })
  })

  # ----------------------- #
  #  Render the Box Plot    #
  # ----------------------- #
output$boxplot <- renderPlot({
  # Get the data
  all_stuff <- final_data()
  
  # Check if all_stuff is NULL or data is empty
  if (is.null(all_stuff) || nrow(all_stuff$df) == 0) {
    plot.new()
    text(0.5, 0.5, "No data to display", cex = 1.2)
    return()
  }
  
  # If we get here, we know all_stuff exists and has data
  data <- all_stuff$df
  gflag <- all_stuff$doGroup
  gvar <- all_stuff$grpVar
  # Create the plot

    p <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(
        title    = "Expression of Selected Genes",
        subtitle = paste0("Total values: n=", nrow(data)),
        x        = "Gene Symbol",
        y        = "Expression Level"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # If user wants grouping, add facet wrap
    if (gflag && !is.null(gvar) && gvar != "") {
      # Summarize n per facet
      facet_counts <- data %>%
        group_by(SYMBOL, .data[[gvar]]) %>%
        summarise(n = n(), .groups = "drop")

      p <- p +
        facet_wrap(as.formula(paste("~", gvar)), scales = "free") +
        geom_text(
          data = facet_counts,
          aes(label = paste0("n=", n), y = Inf),
          vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
        )
    } else {
      # Otherwise, just label total n for each gene
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

  # ---------------------------- #
  #  Render Gene Info Table      #
  # ---------------------------- #
output$gene_info <- renderTable({
  # Get the data
  all_stuff <- final_data()
  
  # Check if all_stuff is NULL before trying to access $df
  if (is.null(all_stuff)) {
    return(NULL)
  }
  
  data <- all_stuff$df
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  data %>%
    select(SYMBOL, GENENAME) %>%
    distinct()
})
  # ----------------------------- #
  #   Reset Button to Clear All   #
  # ----------------------------- #
  observeEvent(input$reset, {
    # 1. Reset search mode to no selection
    updateRadioButtons(session, "search_mode", selected = character(0))

    # 2. Clear GO term & selected genes
    updateSelectizeInput(session, "go_term", selected = NULL)
    updateSelectizeInput(session, "selected_gene", selected = NULL)

    # 3. Clear "All Genes"
    updateSelectizeInput(session, "all_genes", selected = NULL)

    # 4. Reset slider to 5
    if (!is.null(total_unique_genes())) {
      updateSliderInput(session, "max_options", value = 5)
    }

    # 5. Uncheck & hide "Use Group By"
    shinyjs::hide("use_group_by")
    updateCheckboxInput(session, "use_group_by", value = FALSE)

    # 6. Reset group_by
    updateSelectInput(session, "group_by", selected = "sex")
  })
}

shinyApp(ui = ui, server = server)
