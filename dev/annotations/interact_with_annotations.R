# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard) # For UI components

# Load gene_annotations data from RData file
load("../../dat/annotations.RData")

# Define UI for application
ui <- fluidPage(
    # Use a more informative title with styling
    titlePanel(div(
        h1("Gene Annotations Explorer", style = "color: #3474A7;"),
        p("An interactive tool to explore and learn about gene annotations", style = "font-style: italic;")
    )),

    # Add a brief intro/help section at the top
    wellPanel(
        h4("Welcome to the Gene Annotations Explorer!"),
        p("This tool helps you explore gene annotation data interactively. Use the sidebar controls to filter,
          search, and sort genes. The tabs provide different ways to visualize and understand the data."),
        actionButton("show_tutorial", "Show Tutorial", icon = icon("question-circle"))
    ),
    sidebarLayout(
        sidebarPanel(
            # Group related inputs in collapsible panels for cleaner UI
            wellPanel(
                h4("Data Selection", style = "margin-top: 0;"),
                numericInput("sample_size", "Sample Size:", value = 10, min = 1, max = nrow(gene_annotations)),
                actionButton("sample", "Get Sample",
                    icon = icon("random"),
                    class = "btn-primary"
                ),
                checkboxInput("show_all_genes", "Show All Genes", value = FALSE),
                downloadButton("downloadData", "Download Sampled Data")
            ),
            wellPanel(
                h4("Search & Filter", style = "margin-top: 0;"),
                textInput("search_symbol", "Search by SYMBOL or GENENAME:", ""),
                sliderInput("name_length", "Filter by GENENAME Length:",
                    min = 0, max = max(nchar(gene_annotations$GENENAME)),
                    value = c(0, max(nchar(gene_annotations$GENENAME)))
                ),
                selectInput("sort_by", "Sort Table By:",
                    choices = c("SYMBOL", "GENENAME", "Gene Name Length", "ENTREZID"),
                    selected = "SYMBOL"
                )
            ),
            wellPanel(
                h4("GO Categories", style = "margin-top: 0;"),
                selectInput("go_category", "Select GO Category:",
                    choices = names(go_to_genes_list),
                    selected = names(go_to_genes_list)[1]
                ),
                textOutput("go_category_description")
            ),

            # Add a helpful info box
            tags$div(
                class = "alert alert-info",
                tags$b("Tip:"), " Click on any row in the table to view detailed information about that gene."
            ),
            width = 3
        ),
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel(
                    "Table",
                    div(
                        style = "margin-top: 20px",
                        DT::dataTableOutput("gene_table"),
                        tags$div(class = "small text-muted", "Click on a row to see gene details")
                    )
                ),
                tabPanel(
                    "Visualizations",
                    h4("Explore Gene Data Visually"),
                    fluidRow(
                        column(
                            6,
                            plotlyOutput("gene_length_plot"),
                            p(class = "text-muted", "This histogram shows the distribution of gene name lengths.")
                        ),
                        column(
                            6,
                            plotlyOutput("gene_distribution_plot"),
                            p(class = "text-muted", "This bar chart compares gene symbols by their name length.")
                        )
                    ),
                    hr(),
                    plotlyOutput("go_category_plot"),
                    p(class = "text-muted", "This plot shows gene distributions in the selected GO category.")
                ),
                tabPanel(
                    "Educational Resources",
                    fluidRow(
                        column(
                            6,
                            h4("Fun Facts About Genes"),
                            verbatimTextOutput("fun_fact"),
                            actionButton("new_fact", "New Fun Fact", icon = icon("lightbulb"))
                        ),
                        column(
                            6,
                            h4("Glossary of Terms"),
                            tags$dl(
                                tags$dt("SYMBOL"), tags$dd("The official gene symbol assigned by HGNC."),
                                tags$dt("GENENAME"), tags$dd("The full name of the gene."),
                                tags$dt("ENTREZID"), tags$dd("Unique identifier used in NCBI's gene database."),
                                tags$dt("GO Categories"), tags$dd("Gene Ontology terms describing gene function, process, or location.")
                            )
                        )
                    ),
                    hr(),
                    h4("Learn More About Bioinformatics"),
                    fluidRow(
                        column(
                            4,
                            wellPanel(
                                h5("NCBI Resources"),
                                tags$ul(
                                    tags$li(tags$a(href = "https://www.ncbi.nlm.nih.gov/gene/", "NCBI Gene Database", target = "_blank")),
                                    tags$li(tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK21091/", "NCBI Handbook", target = "_blank"))
                                )
                            )
                        ),
                        column(
                            4,
                            wellPanel(
                                h5("Gene Ontology"),
                                tags$ul(
                                    tags$li(tags$a(href = "http://geneontology.org/", "GO Consortium", target = "_blank")),
                                    tags$li(tags$a(href = "http://amigo.geneontology.org/", "AmiGO Browser", target = "_blank"))
                                )
                            )
                        ),
                        column(
                            4,
                            wellPanel(
                                h5("Interactive Tutorials"),
                                tags$ul(
                                    tags$li(tags$a(href = "https://bioinformaticsworkbook.org/", "Bioinformatics Workbook", target = "_blank")),
                                    tags$li(tags$a(href = "https://www.coursera.org/learn/bioinformatics", "Coursera Bioinformatics", target = "_blank"))
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Gene Details",
                    uiOutput("gene_details_ui")
                ),
                tabPanel(
                    "GO Categories",
                    h4("Genes in Selected GO Category"),
                    p("Gene Ontology (GO) categories group genes by their molecular functions, biological processes, or cellular components."),
                    DT::dataTableOutput("go_category_table")
                )
            ),
            width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Store selected row data
    selected_gene_data <- reactiveVal(NULL)

    # Reactive expression for sampled data
    sampled_data <- reactive({
        input$sample

        isolate({
            filtered_data <- gene_annotations %>%
                filter(nchar(GENENAME) >= input$name_length[1] & nchar(GENENAME) <= input$name_length[2])

            if (input$search_symbol != "") {
                # Make the search case-insensitive and partial match
                search_term <- tolower(input$search_symbol)
                filtered_data <- filtered_data %>%
                    filter(grepl(search_term, tolower(SYMBOL)) | grepl(search_term, tolower(GENENAME)))
            }

            if (!input$show_all_genes) {
                # Only sample if we have more rows than the sample size
                if (nrow(filtered_data) > input$sample_size) {
                    filtered_data <- sample_n(filtered_data, input$sample_size)
                }
            }

            filtered_data
        })
    })

    # Render gene table using DT for interactive features
    output$gene_table <- DT::renderDataTable({
        data <- if (input$sample > 0 || input$show_all_genes) sampled_data() else head(gene_annotations, 10)

        # Sort the data based on user selection
        if (input$sort_by == "Gene Name Length") {
            data <- data %>% arrange(desc(nchar(GENENAME)))
        } else {
            data <- data %>% arrange(!!sym(input$sort_by))
        }

        DT::datatable(
            data,
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All"))
            ),
            selection = "single",
            rownames = FALSE,
            caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color: black; font-size: 150%;",
                "Gene Annotations Data"
            )
        ) %>%
            formatStyle("SYMBOL", fontWeight = "bold")
    })

    # Handle row selection in the table
    observeEvent(input$gene_table_rows_selected, {
        req(input$gene_table_rows_selected)
        data <- if (input$sample > 0 || input$show_all_genes) sampled_data() else head(gene_annotations, 10)
        selected_gene_data(data[input$gene_table_rows_selected, ])
        updateTabsetPanel(session, "main_tabs", selected = "Gene Details")
    })

    # Render gene length plot
    output$gene_length_plot <- renderPlotly({
        req(input$sample > 0 || input$show_all_genes)
        data <- sampled_data()

        if (nrow(data) == 0) {
            return(plot_ly() %>% add_annotations(text = "No data to display", showarrow = FALSE))
        }

        p <- ggplot(data, aes(x = nchar(GENENAME))) +
            geom_histogram(binwidth = 5, fill = "#3474A7", color = "white", alpha = 0.8) +
            labs(
                title = "Distribution of Gene Name Lengths",
                x = "Gene Name Length (characters)",
                y = "Number of Genes"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold"),
                axis.title = element_text(face = "bold")
            )

        ggplotly(p, tooltip = c("x", "y")) %>%
            layout(margin = list(l = 50, r = 20, b = 50, t = 50))
    })

    # Render gene distribution plot
    output$gene_distribution_plot <- renderPlotly({
        req(input$sample > 0 || input$show_all_genes)
        data <- sampled_data()

        if (nrow(data) == 0) {
            return(plot_ly() %>% add_annotations(text = "No data to display", showarrow = FALSE))
        }

        # Limit to top 25 genes by name length for better readability
        top_data <- data %>%
            arrange(desc(nchar(GENENAME))) %>%
            head(25)

        p <- ggplot(top_data, aes(x = reorder(SYMBOL, nchar(GENENAME)), y = nchar(GENENAME))) +
            geom_bar(stat = "identity", fill = "#5CB85C", color = "white", alpha = 0.8) +
            coord_flip() +
            labs(
                title = "Top Genes by Name Length",
                x = "Gene Symbol",
                y = "Gene Name Length (characters)"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold"),
                axis.title = element_text(face = "bold")
            )

        ggplotly(p, tooltip = c("x", "y")) %>%
            layout(margin = list(l = 100, r = 20, b = 50, t = 50))
    })

    # Render GO category plot
    output$go_category_plot <- renderPlotly({
        req(input$go_category)
        genes_in_category <- go_to_genes_list[[input$go_category]]

        if (length(genes_in_category) == 0) {
            return(plot_ly() %>% add_annotations(text = "No genes in this category", showarrow = FALSE))
        }

        go_category_data <- gene_annotations %>%
            filter(ENTREZID %in% genes_in_category) %>%
            select(SYMBOL, GENENAME, ENTREZID)

        # Create a summary of gene name lengths
        name_length_data <- go_category_data %>%
            mutate(name_length = nchar(GENENAME)) %>%
            group_by(name_length) %>%
            summarize(count = n())

        p <- ggplot(name_length_data, aes(x = name_length, y = count)) +
            geom_bar(stat = "identity", fill = "#F0AD4E", color = "white", alpha = 0.8) +
            labs(
                title = paste("Gene Name Lengths in", input$go_category),
                x = "Gene Name Length (characters)",
                y = "Number of Genes"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = "bold"),
                axis.title = element_text(face = "bold")
            )

        ggplotly(p, tooltip = c("x", "y")) %>%
            layout(margin = list(l = 50, r = 20, b = 50, t = 50))
    })

    # Enhanced fun facts with citations
    fun_facts <- c(
        "The human genome contains approximately 20,000-25,000 genes, far fewer than initially expected during the Human Genome Project. (Source: NHGRI)",
        "Only about 1-2% of the human genome encodes for proteins. The rest includes regulatory sequences, non-coding RNA genes, introns, and sequences with unknown functions. (Source: Genome Research)",
        "The longest human gene is Dystrophin, which is about 2.4 million base pairs long. Mutations in this gene cause Duchenne muscular dystrophy. (Source: OMIM)",
        "The shortest human gene is TRNAH-GTG, a transfer RNA gene that is only 71 base pairs long. (Source: Genomics)",
        "The human genome is made up of about 3 billion base pairs, which would stretch about 2 meters if laid out in a straight line. (Source: NHGRI)",
        "Humans and chimpanzees share about 98.8% of their DNA, making them our closest living relatives. (Source: Nature)",
        "Your genome contains approximately 3 billion base pairs, but if two unrelated people are compared, their DNA sequences are 99.9% identical. (Source: NIH)",
        "About 8% of the human genome consists of viral DNA from ancient infections that became integrated into our genome. (Source: Cell)",
        "Each cell in your body contains about 2 meters of DNA tightly coiled into the nucleus, which is only about 6 micrometers in diameter. (Source: Scientific American)",
        "Some genes can be read in both directions, producing different proteins depending on which strand is read. These are called 'bidirectional genes'. (Source: Genome Biology)"
    )

    # Render fun fact
    output$fun_fact <- renderText({
        req(fun_facts)
        fun_facts[sample(length(fun_facts), 1)]
    })

    # New fun fact button
    observeEvent(input$new_fact, {
        output$fun_fact <- renderText({
            req(fun_facts)
            fun_facts[sample(length(fun_facts), 1)]
        })
    })

    # Download handler
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("gene_annotations_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            data <- if (input$sample > 0 || input$show_all_genes) sampled_data() else head(gene_annotations, 10)
            write.csv(data, file, row.names = FALSE)
        }
    )

    # Render gene details UI
    output$gene_details_ui <- renderUI({
        gene <- selected_gene_data()

        if (is.null(gene)) {
            return(
                div(
                    class = "alert alert-info",
                    h4("No gene selected"),
                    p("Please select a gene from the table to view detailed information."),
                    p("Click on any row in the Table tab to select a gene.")
                )
            )
        }

        # Create a more educational gene details page
        div(
            h3(paste("Details for Gene:", gene$SYMBOL), style = "color: #3474A7;"),
            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4("Basic Information", style = "margin-top: 0;"),
                        tags$dl(
                            tags$dt("Gene Symbol"), tags$dd(gene$SYMBOL),
                            tags$dt("Gene Name"), tags$dd(gene$GENENAME),
                            tags$dt("Entrez ID"), tags$dd(gene$ENTREZID),
                            tags$dt("Description"), tags$dd(gene$DESCRIPTION)
                        )
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h4("Additional Metrics", style = "margin-top: 0;"),
                        tags$dl(
                            tags$dt("Gene Name Length"), tags$dd(paste(nchar(gene$GENENAME), "characters")),
                            tags$dt("GO Categories"),
                            tags$dd(
                                renderUI({
                                    categories <- names(go_to_genes_list)[sapply(go_to_genes_list, function(x) gene$ENTREZID %in% x)]
                                    if (length(categories) > 0) {
                                        tags$ul(
                                            lapply(categories[1:min(5, length(categories))], function(cat) {
                                                tags$li(cat)
                                            })
                                        )
                                    } else {
                                        "No GO categories found"
                                    }
                                })
                            )
                        )
                    )
                )
            ),
            wellPanel(
                h4("External Resources", style = "margin-top: 0;"),
                p("Learn more about this gene from these scientific databases:"),
                tags$ul(
                    tags$li(tags$a(
                        href = paste0("https://www.ncbi.nlm.nih.gov/gene/?term=", gene$ENTREZID),
                        "NCBI Gene Database", target = "_blank"
                    )),
                    tags$li(tags$a(
                        href = paste0("https://www.uniprot.org/uniprot/?query=gene:", gene$SYMBOL, "+AND+organism:human"),
                        "UniProt Protein Database", target = "_blank"
                    )),
                    tags$li(tags$a(
                        href = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", gene$SYMBOL),
                        "GeneCards", target = "_blank"
                    )),
                    tags$li(tags$a(
                        href = paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", gene$SYMBOL),
                        "Ensembl Genome Browser", target = "_blank"
                    ))
                )
            )
        )
    })

    # Update sample size based on show_all_genes checkbox
    observeEvent(input$show_all_genes, {
        updateNumericInput(session, "sample_size",
            value = if (input$show_all_genes) nrow(gene_annotations) else 10
        )
    })

    # Render GO category table
    output$go_category_table <- DT::renderDataTable({
        selected_category <- input$go_category
        genes_in_category <- go_to_genes_list[[selected_category]]

        # Filter gene_annotations based on selected GO category
        go_category_data <- gene_annotations %>%
            filter(ENTREZID %in% genes_in_category) %>%
            select(SYMBOL, GENENAME, ENTREZID, DESCRIPTION)

        DT::datatable(go_category_data,
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All"))
            ),
            selection = "single",
            rownames = FALSE,
            caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color: black; font-size: 150%;",
                paste("Genes in GO Category:", selected_category)
            )
        ) %>%
            formatStyle("SYMBOL", fontWeight = "bold")
    })

    # Handle row selection in the GO category table
    observeEvent(input$go_category_table_rows_selected, {
        req(input$go_category_table_rows_selected)
        selected_category <- input$go_category
        genes_in_category <- go_to_genes_list[[selected_category]]

        go_category_data <- gene_annotations %>%
            filter(ENTREZID %in% genes_in_category) %>%
            select(SYMBOL, GENENAME, ENTREZID, DESCRIPTION)

        selected_gene_data(go_category_data[input$go_category_table_rows_selected, ])
        updateTabsetPanel(session, "main_tabs", selected = "Gene Details")
    })

    # Add GO category description
    output$go_category_description <- renderText({
        paste("This GO category contains", length(go_to_genes_list[[input$go_category]]), "genes.")
    })

    # Show tutorial
    observeEvent(input$show_tutorial, {
        showModal(modalDialog(
            title = "Gene Annotations Explorer Tutorial",
            h4("How to Use This App"),
            tags$ol(
                tags$li("Use the 'Data Selection' panel to choose how many genes to view"),
                tags$li("Use 'Search & Filter' to find specific genes or types of genes"),
                tags$li("Explore GO Categories to see genes grouped by function"),
                tags$li("Click on any gene in the table to see detailed information"),
                tags$li("Use the visualization tabs to understand gene patterns")
            ),
            h4("Key Features"),
            tags$ul(
                tags$li(strong("Table:"), "View and sort gene data in a searchable table"),
                tags$li(strong("Visualizations:"), "See gene name length distributions and comparisons"),
                tags$li(strong("Educational Resources:"), "Learn interesting facts about genes and genomics"),
                tags$li(strong("Gene Details:"), "Get comprehensive information about selected genes"),
                tags$li(strong("GO Categories:"), "Explore genes grouped by biological function")
            ),
            easyClose = TRUE,
            footer = modalButton("Got it!")
        ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)