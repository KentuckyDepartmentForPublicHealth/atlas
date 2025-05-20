# Define UI using bslib's page_navbar
ui <- page_navbar(
    id = "navBar",
    theme = bs_theme(
        version = 5, # bootswatch = "default",
        primary = "#6a0033", # using one of your tumor colors
        secondary = "#ff757c",
        success = "#377EB8",
        info = "#17a2b8",
        warning = "#ffc107",
        danger = "#dc3545",
        base_font = c("Montserrat", "Helvetica", "sans-serif"),
        code_font = c("Source Code Pro", "monospace"),
        heading_font = c("Impact", "Arial", "sans-serif")
    ),
    title = div(
        img(src = "icons8-brain-tumor-100.png", width = "100px", height = "100px"),
        "Atlas of Nervous System Tumors",
        style = "display: inline-block; vertical-align: middle;"
    ),
    sidebar = NULL,
    header = NULL,
    footer = NULL,
    nav_spacer(),
    nav_item(
        span( # bsicons::bs_icon("wrench"),
            "BETA VERSION",
            # "Beta Version",
            class = "badge bg-warning ms-2",
            title = "This app is in beta. Expect frequent updates and improvements as we continue development. Some features may be under construction.",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "bottom",
            id = "beta-badge",
            onclick = "Shiny.setInputValue('beta_badge_clicked', Math.random())"
        )
    ),
    # Home -----
    nav_panel(
        title = "Home", icon = icon("house"),
        tags$head(
            #   tags$link(rel = "stylesheet", type = "text/css", href = "atlas.css"),
            tags$link(rel = "shortcut icon", href = "favicon.ico"),
            tags$style(HTML("
            /* Custom animations for img hover */
            .bouncy  {
                transition: transform 0.2s ease, box-shadow 0.2s ease;
                border-radius: 10px;
            }
            .bouncy:hover {
                transform: translateY(-5px);
                box-shadow: 0 4px 15px rgba(0,0,0,0.2) !important;
            }
            
        /* Target headers and cell content in dark mode */
        body.dark-mode .dark-mode-table-container .dataTable th,
        body.dark-mode .dark-mode-table-container .dataTable td {
            color: white !important;
        }

        /* Table control elements - consistent in both modes */
        .dark-mode-table-container .dataTables_length, 
        .dark-mode-table-container .dataTables_filter,
        .dark-mode-table-container .dataTables_info,
        .dark-mode-table-container .dataTables_paginate {
            background-color: white !important;
            color: black !important;
            padding: 3px 8px !important;
            border-radius: 4px !important;
        }
        
        /* Pagination buttons */
        .dark-mode-table-container .paginate_button {
            color: black !important;
        }
        
        /* Current page button */
        .dark-mode-table-container .paginate_button.current {
            background: #e6e6e6 !important;
            color: black !important;
        }        "))
        ), # end of tags$head
        span(img(src = "main-banner-1400x400-fullglobe.png"), style = 'text-align: center; width = "15%";'),
        # h2('Welcome to the Transcriptomic Atlas of Nervous System Tumors'),
        tags$blockquote(
            "Explore a pioneering resource in neuro-oncology with the Transcriptomic Atlas of Nervous System Tumors. This project aims to bridge critical gaps in understanding the molecular landscape of nervous system tumors by creating a comprehensive, publicly accessible dataset of gene expression profiles. This atlas integrates thousands of samples from diverse sources, offering a powerful tool for researchers, clinicians, and students to investigate tumor biology, refine diagnostics, and uncover new therapeutic insights. Whether you're analyzing survival trends, visualizing tumor heterogeneity, or comparing gene expression patterns, this app provides an interactive gateway to cutting-edge neuro-oncology research."
        ),
        h3(icon("chart-line"), " What This App Offers"),
        p("This app brings the atlas to life with three interactive tools tailored to neuro-oncology exploration:"),

        # Create a responsive row with three columns for the tools
        fluidRow(
            # Survival Analysis Column
            column(
                4,
                div(
                    # Clickable Image
                    actionLink(
                        inputId = "goto_survival",
                        label = tags$img(
                            src = "survival_analysis_banner.png",
                            alt = "Survival Analysis",
                            style = "width:100%; cursor:pointer; margin-bottom: 20px;",
                            class = "bouncy" # Added class for CSS styling
                        )
                    ),
                    tags$b("Survival Analysis:"),
                    p("Use Kaplan-Meier curves to examine survival trends across tumor types, genetic mutations (e.g., IDH, MGMT), or demographic factors (e.g., age, sex). Filter by diagnosis or age group, and generate single-variable or multi-variable grids to compare outcomes."),
                )
            ),

            # t-SNE Dimensionality Reduction Column
            column(
                4,
                div(
                    # Clickable Image
                    actionLink(
                        inputId = "goto_tsne",
                        label = tags$img(
                            src = "tsne_banner.png",
                            alt = "t-SNE Dimensionality Reduction",
                            style = "width:100%; cursor:pointer; margin-bottom: 20px;",
                            class = "bouncy" # Added class for CSS styling
                        )
                    ),
                    tags$b("t-SNE Dimensionality Reduction:"),
                    p("Visualize how 7,375 samples cluster based on their transcriptomic profiles in a 2D plot. Select points to explore sample details, revealing patterns of similarity and difference across diagnoses."),
                )
            ),

            # mRNA Expression Boxplots Column
            column(
                4,
                div(
                    # Clickable Image
                    actionLink(
                        inputId = "goto_mrna",
                        label = tags$img(
                            src = "mrna_expression_banner.png",
                            alt = "mRNA Expression Boxplots",
                            style = "width:100%; cursor:pointer; margin-bottom: 20px;",
                            class = "bouncy" # Added class for CSS styling
                        )
                    ),
                    tags$b("mRNA Expression Boxplots:"),
                    p("Compare gene expression levels across tumors or groups. Search by Gene Ontology terms (e.g., cell cycle, apoptosis) or specific genes, and group results by variables like diagnosis, grade, or sex to uncover molecular insights."),
                )
            )
        ),
        p("Each tool is designed for ease of use—select options, generate plots, and download results to fuel your research or education."),
        h3(icon("bullseye"), " Purpose of the Study"),
        p(
            "The primary goal of this study was to compile a large-scale, clinically annotated transcriptomic atlas that captures the gene expression signatures of nervous system tumors and non-tumor tissues. Unlike previous efforts that focused narrowly on specific tumor types or relied heavily on DNA methylation, this project sought to harmonize a broad spectrum of transcriptomic data from public repositories. By doing so, it addresses the scarcity of resources for studying rare tumor types, pediatric and adult cases, and samples from varied geographic regions. The atlas aims to empower comparative gene expression analyses, support diagnostic refinement, and lay the groundwork for integrating genomic data with clinical outcomes—ultimately advancing personalized medicine in neuro-oncology."
        ),
        h3(icon("tools"), " How It Was Conducted"),
        p(
            "To build this atlas, the team meticulously gathered raw transcriptomic data from thousands of nervous system samples—both neoplastic (tumorous) and non-neoplastic—available in public databases like the Gene Expression Omnibus and ArrayExpress. They standardized the data by using a single platform, the Applied Biosystems™ GeneChip™ Human Genome U133 Plus 2.0 Array, which offers extensive coverage of the human genome. This raw data, spanning years of collection (2003–2018), was reprocessed simultaneously using advanced computational tools in R, including background correction, normalization, and summarization techniques. Machine learning algorithms, such as t-SNE for visualization and classifiers like random forests and gradient boosting, were employed to organize samples into biologically meaningful clusters and refine diagnoses where historical classifications were outdated or unclear. Clinical metadata—age, sex, tumor location, genetic mutations, and survival data—were manually curated to enrich the dataset, ensuring its utility for multifaceted analyses."
        ),
        h3(icon("seedling"), " A Growing Resource"),
        p(
            "This atlas is just the beginning. The methodology developed here—harmonizing diverse transcriptomic data into a unified framework—is designed to be scalable. As new raw data become available in public domains or through collaborations, we plan to expand the atlas to include additional samples, tumor types, and molecular profiles. Future updates will incorporate emerging genomic platforms and integrate other omics data (e.g., proteomics, epigenomics), enhancing the depth and breadth of this resource. Stay tuned for a continually evolving tool that adapts to the latest advancements in neuro-oncology research."
        ),
        h3(icon("brain"), " Scientific and Biological Context"),
        p(
            "Nervous system tumors, including gliomas, medulloblastomas, meningiomas, and rare entities like gangliogliomas, arise from complex interactions between genetic, epigenetic, and environmental factors. Gene expression—the process by which DNA instructions are converted into functional proteins—offers a window into these tumors’ biological behavior, from growth and invasion to treatment response. This app leverages transcriptomic data (mRNA levels across 20,360 genes) alongside clinical variables to explore these dynamics. Key data types include:",
            tags$ul(
                tags$li("Diagnosis: Tumor types and subtypes (e.g., diffuse gliomas, embryonal tumors), reflecting current classifications."),
                tags$li("Clinical Metadata: Age groups (fetus to 106 years), sex, tumor location (e.g., supratentorial, posterior fossa), and survival outcomes (months lived, mortality status)."),
                tags$li("Genetic Features: Mutations (e.g., IDH, H3F3A), amplifications (e.g., MYCN), and methylation status (e.g., MGMT promoter), critical for tumor subclassification."),
                tags$li("Expression Profiles: mRNA levels for thousands of genes, linked to biological processes via Gene Ontology (GO) terms.")
            ),
            "These variables enable you to investigate how molecular signatures correlate with clinical outcomes, tumor heterogeneity, and potential therapeutic targets."
        ),
        h3(icon("rocket"), " Get Started"),
        p(
            "Dive in by navigating the tabs above. Whether you're a researcher seeking novel hypotheses, a clinician refining diagnostic approaches, or a student learning tumor biology, this app offers a hands-on experience with real-world data."
            # tags$a(href = "mailto:axitamm@gmail.com", "axitamm@gmail.com"), "."
        ),
        span(tagList(
            br(),
            img(src = "KY Pediatric Cancer Research - Final.png", class = "bouncy", style = "width:25%; object-fit: contain;"),
            img(src = "DPH and PHAB logo.png", class = "bouncy", style = "width:35%; object-fit: contain;"),
            img(src = "u_of_l.jpg", class = "bouncy", style = "width:20%; object-fit: contain;"),
            br(),
            paste0("Last updated: ", currentDate), br(),
            p(
                "Download",
                tags$a(href = "https://github.com/axitamm/BrainTumorAtlas", target = "_blank", "source data"),
                "and",
                tags$a(href = "https://github.com/KentuckyDepartmentForPublicHealth/atlas", target = "_blank", "application code"), br(),
                "Project funded by the", tags$a(href = "https://www.chfs.ky.gov/agencies/dph/dpqi/cdpb/Pages/pcrtf.aspx", target = "_blank", "Kentucky Pediatric Cancer Research Trust Fund"), br(),
                "ShinyApp powered by the", tags$a(href = "https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx", target = "_blank", "Kentucky Department for Public Health")
            )
        ), style = "display: block; width: 100%; font-size: 1.5em; color: black; background: white; text-align: center; border-radius: 50px; border: 2px solid white; padding: 1em; margin: 1em 0;"),
        br()
    ),
    # Survival Analysis Tab -----
    nav_panel(
        title = "Survival Analysis", icon = icon("heartbeat"), id = "goto_survival",
        sidebarLayout(
            sidebarPanel(
                useShinyjs(),
                width = 3,
                # Set the card text color to use the dynamic variable so it displays well in both modes.

                h4("Survival Analysis", style = "margin-top: 0;"),
                p("Analyze survival outcomes for nervous system tumors using Kaplan-Meier curves."),
                radioButtons("analysis_type", "Analysis Type:",
                    choices = c("Single Variable" = "single", "Multi-Variable Grid" = "grid"),
                    selected = "grid"
                ),
                conditionalPanel(
                    condition = "input.analysis_type == 'single'",
                    selectInput("strat_var", "Stratify By:",
                        choices = c(
                            "Diagnosis" = "diagnosisFinal",
                            "Diagnosis Class" = "diagnosisClass",
                            "Sex" = "sex",
                            "Age Group" = "ageGroup",
                            "Grade" = "grade",
                            "IDH Mutation" = "mutationIDH1/2",
                            "H3 Mutation" = "mutationH3",
                            "1p/19q Codeletion" = "1p/19q-codel",
                            "MGMT Methylation" = "methylationMGMTpromoter",
                            "MYCN Amplification" = "amplificationMCYN"
                        ),
                        selected = "diagnosisFinal"
                    ),
                    checkboxInput("collapse_rare", "Collapse Rare Categories", value = TRUE),
                    conditionalPanel(
                        condition = "input.collapse_rare == true",
                        sliderInput("min_group_size", "Minimum Group Size:",
                            min = 5, max = 50, value = 10, step = 5
                        )
                    )
                ),
                conditionalPanel(
                    condition = "input.analysis_type == 'grid'",
                    checkboxGroupInput("grid_variables", "Select Variables for Grid:",
                        choices = c(
                            "IDH Mutation" = "mutationIDH1/2",
                            "Grade" = "grade",
                            "MGMT Methylation" = "methylationMGMTpromoter",
                            "Medulloblastoma Subtypes" = "MB_subtypes",
                            "MYCN Amplification" = "amplificationMCYN",
                            "AT/RT Subtypes" = "ATRT_subtypes",
                            "Ependymoma Subtypes" = "EPN_subtypes",
                            "Meningioma Grades" = "MEN_grades",
                            "Germ Cell Tumors" = "GCT_subtypes"
                        ),
                        selected = c("mutationIDH1/2", "grade", "methylationMGMTpromoter", "MB_subtypes")
                    ),
                    numericInput("max_months", "Maximum Months to Display:",
                        value = 150, min = 12, max = 250, step = 12
                    )
                ),
                selectInput("diagnosis_filter", "Filter by Diagnosis Class:",
                    choices = c(
                        "All", "DIFFUSE GLIOMA", "EMBRYONAL", "EPENDYMAL",
                        "MESENCHYMAL", "DISCRETE GLIOMA", "GERM CELL", "NERVE"
                    ),
                    selected = "All"
                ),
                selectInput("age_filter", "Filter by Age Group:",
                    choices = c("All", "0-5YRS", "5-10YRS", "10-20YRS", "20-40YRS", "40-60YRS", "60-80YRS"),
                    selected = "All"
                ),
                hr(),
                checkboxInput("show_pvalue", "Show Log-Rank P-value", TRUE),
                checkboxInput("show_ci", "Show Confidence Intervals", FALSE),
                actionButton("generate", "Generate Plot",
                    class = "btn-primary",
                    style = "width: 100%; margin-top: 15px;"
                ),
                downloadButton("download_plot", "Download Plot (PDF)",
                    class = "btn-success",
                    style = "width: 100%; margin-top: 10px;"
                )
            ),
            mainPanel(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Survival Plot",
                        conditionalPanel(
                            condition = "input.analysis_type == 'single'",
                            withSpinner(plotOutput("survival_plot", height = "600px"), type = 4)
                        ),
                        conditionalPanel(
                            condition = "input.analysis_type == 'grid'",
                            withSpinner(plotOutput("grid_plot", height = "800px"), type = 4)
                        )
                    ),
                    tabPanel(
                        "Summary",
                        h4("Group Statistics"),
                        tableOutput("group_stats"),
                        h4("Median Survival Estimates"),
                        tableOutput("median_survival")
                    ),
                    tabPanel(
                        "Data",
                        div(
                            class = "dark-mode-table-container",
                            DTOutput("survival_data")
                        )
                    )
                )
            )
        )
    ),
    # t-SNE Dimensionality Reduction Tab -----
    nav_panel(
        title = "t-SNE Dimensionality Reduction", icon = icon("th"),
        fluidPage(
            div(
                class = "card mt-3 mb-3",
                id = "selection-manager-card", # Add an ID for client-side theming
                div(
                    class = "card-body",
                    uiOutput("selectionManagerUI")
                )
            ),
            withSpinner(plotlyOutput("tsnePlot", height = "600px"), type = 4),
            br(), hr(), # Add space
            div(
                class = "dark-mode-table-container",
                h2(tags$b("Data Table"), style = "font-size: 24px;"), # Bolder, styled title
                DT::dataTableOutput("dataTable")
                #  uiOutput("clearSelectionsUI")
            )
        )
    ),
    # mRNA Expression Boxplots Tab -----
    nav_panel(
        title = "mRNA Expression Boxplots", icon = icon("vial"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                radioButtons(
                    "search_mode", "Search Mode:",
                    choices = c("Select GO Term", "Select All Genes"),
                    selected = character(0)
                ),
                conditionalPanel(
                    condition = "input.search_mode === 'Select GO Term'",
                    selectizeInput("go_term", "Select GO Term", choices = NULL),
                    uiOutput("go_gene_selector")
                ),
                conditionalPanel(
                    condition = "input.search_mode === 'Select All Genes'",
                    selectizeInput("all_genes", "Select Genes",
                        choices = NULL, multiple = TRUE,
                        options = list(maxOptions = 5)
                    )
                ),
                uiOutput("max_options_slider"),
                checkboxInput("use_group_by", "Use Group By", value = FALSE),
                uiOutput("group_by_selector"),
                br(),
                fluidRow(
                    column(12, div(
                        style = "text-align: center;",
                        actionButton("run", "Run Plot",
                            icon = icon("play"),
                            class = "btn-primary btn-lg", style = "width: 90%; margin: 10px;"
                        )
                    ))
                ),
                fluidRow(
                    column(12, div(
                        style = "text-align: center;",
                        actionButton("reset", "Clear",
                            icon = icon("eraser"),
                            class = "btn-warning btn-lg", style = "width: 90%; margin: 10px;"
                        )
                    ))
                ),
                fluidRow(
                    column(12, div(
                        style = "text-align: center;",
                        downloadButton("save_plot", "Save Plot",
                            icon = icon("download"),
                            class = "btn-success btn-lg", style = "width: 90%; margin: 10px;"
                        )
                    ))
                ),
                fluidRow(
                    column(12, div(
                        style = "text-align: center;",
                        actionButton("reload", "Reload",
                            icon = icon("sync"),
                            class = "btn-danger btn-lg", style = "width: 90%; margin: 10px;"
                        )
                    ))
                )
            ),
            mainPanel(
                width = 9,
                conditionalPanel(
                    condition = "input.search_mode === null || input.search_mode === ''",
                    h3("Please select a search mode to proceed.")
                ),
                plotOutput("boxplot", width = "100%", height = "1000px"),
                tableOutput("gene_info")
            )
        )
    ),

    # Info Tab -----
    nav_menu(
        title = "Info", icon = icon("info-circle"),
        # align = "right",
        nav_item(link_atlas_source),
        nav_item(link_atlas_data)
    ),

    # Contact Tab -----


    nav_panel(
        title = "Contact", icon = icon("envelope"),
        fluidPage(
            # h2("Contact Information"),
            # p("This section will contain contact information and ways to get in touch with the project team."),

            # Layout without styling
            fluidRow(
                column(
                    8,
                    wellPanel(
                        h3("Enter Your Information"),
                        textInput("name", "Name:", ""),
                        textInput("email", "Email:", ""),
                        textAreaInput("message", "Message:", "",
                            rows = 6, # Increased height
                            resize = "vertical", # Allow vertical resizing
                            width = "100%"
                        ), # Full width of container
                        div(
                            style = "display: flex; justify-content: space-between; margin-top: 15px;",
                            actionButton("submit", "Submit",
                                class = "btn-primary",
                                icon = icon("right-to-bracket")
                            ),
                            actionButton("clear_form", "Clear",
                                class = "btn-warning",
                                icon = icon("eraser")
                            )
                        )
                    )
                ),
                column(
                    4,
                    wellPanel(
                        h3("Submission Status:"),
                        uiOutput("status")
                        # icon("paper-plane", style = "font-size: 80px; opacity: 0.2;")
                    )
                )
            )
        )
    ),
    # Remove the Debug tab here.
    # Dark mode toggle in the navbar (placed as a nav_item)
    nav_item(
        input_dark_mode(id = "mode_toggle", mode = "dark")
    )
)
