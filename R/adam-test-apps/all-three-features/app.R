# Load libraries
library(shiny)
library(bslib)
library(survival)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(tidyr)
library(shinyalert)
library(rlang)
library(forcats)

# Create a reactive value to track if gene expression data is loaded
gene_data_loaded <- reactiveVal(FALSE)

# Create a placeholder for the gene expression data
geneExpressionData <- NULL

# Load the data (assuming it's available in the environment)
# load("../../../dat/atlasDataClean.RData")
# load("../../../dat/geneExpressionData.RData")
# load("../../../dat/annotations.RData")
load("dat/atlasDataClean.RData")
load("dat/geneExpressionData.RData")
load("dat/annotations.RData")
# Assume geneExpressionData, gene_annotations, and go_to_genes_list are also loaded

# Define tumor colors for t-SNE plot
tumor_colors <- c(
    "#6a0033", "#e4016e", "#ff527b", "#360e15", "#ff757c", "#dc002d",
    "#ff9170", "#7a2000", "#ff7e40", "#ac5300", "#da6c00", "#401f00",
    "#ffb173", "#d79600", "#6a4800", "#d6c6b2", "#ddaf00", "#d6ca6f",
    "#6e6900", "#1d1c10", "#8db600", "#1b7a00", "#8ddb76", "#00b861",
    "#9ad5b1", "#005d3f", "#01bbb7", "#00444d", "#01afc7", "#54d8f9",
    "#0189dd", "#8ab5ff", "#5292ff", "#004690", "#00317f", "#4263ff",
    "#240a4e", "#271332", "#fa63ff", "#760078", "#ff77f6", "#8a005f",
    "#ffa5ca"
)

# Define UI using bslib's page_navbar and remove debug tab.
ui <- page_navbar(
    id = "navBar",
    theme = bs_theme(
        version = 5, bootswatch = "cosmo",
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
# Home -----
nav_panel(
    title = 'Home', icon = icon('house'),
    tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "atlas.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    span(img(src = "main-banner-1400x400.png"), style = 'text-align: center; width = "15%";'),
    # h2('Welcome to the Transcriptomic Atlas of Nervous System Tumors'),
    tags$blockquote(
        "Explore a pioneering resource in neuro-oncology with the Transcriptomic Atlas of Nervous System Tumors. This project aims to bridge critical gaps in understanding the molecular landscape of nervous system tumors by creating a comprehensive, publicly accessible dataset of gene expression profiles. This atlas integrates thousands of samples from diverse sources, offering a powerful tool for researchers, clinicians, and students to investigate tumor biology, refine diagnostics, and uncover new therapeutic insights. Whether you're analyzing survival trends, visualizing tumor heterogeneity, or comparing gene expression patterns, this app provides an interactive gateway to cutting-edge neuro-oncology research."
    ),
h3(icon('chart-line'), ' What This App Offers'),

p("This app brings the atlas to life with three interactive tools tailored to neuro-oncology exploration:"),

# Create a responsive row with three columns for the tools
fluidRow(
    # Survival Analysis Column
    column(4,
           div(
               # Clickable Image
               actionLink(
                   inputId = "goto_survival",
                   label = tags$img(
                       src = "survival_analysis_banner.png",
                       alt = "Survival Analysis",
                       style = "width:100%; cursor:pointer; margin-bottom: 20px;"
                   )
               ),
               tags$b("Survival Analysis:"),
               p("Use Kaplan-Meier curves to examine survival trends across tumor types, genetic mutations (e.g., IDH, MGMT), or demographic factors (e.g., age, sex). Filter by diagnosis or age group, and generate single-variable or multi-variable grids to compare outcomes."),
           )
    ),
    
    # t-SNE Dimensionality Reduction Column
    column(4,
           div(
               # Clickable Image
               actionLink(
                   inputId = "goto_tsne",
                   label = tags$img(
                       src = "tsne_banner.png",
                       alt = "t-SNE Dimensionality Reduction",
                       style = "width:100%; cursor:pointer; margin-bottom: 20px;"
                   )
               ),
               tags$b("t-SNE Dimensionality Reduction:"),
               p("Visualize how 7,375 samples cluster based on their transcriptomic profiles in a 2D plot. Select points to explore sample details, revealing patterns of similarity and difference across diagnoses."),
           )
    ),
    
    # mRNA Expression Boxplots Column
    column(4,
           div(
               # Clickable Image
               actionLink(
                   inputId = "goto_mrna",
                   label = tags$img(
                       src = "mrna_expression_banner.png",
                       alt = "mRNA Expression Boxplots",
                       style = "width:100%; cursor:pointer; margin-bottom: 20px;"
                   )
               ),
               tags$b("mRNA Expression Boxplots:"),
               p("Compare gene expression levels across tumors or groups. Search by Gene Ontology terms (e.g., cell cycle, apoptosis) or specific genes, and group results by variables like diagnosis, grade, or sex to uncover molecular insights."),
           )
    )
),

p("Each tool is designed for ease of use—select options, generate plots, and download results to fuel your research or education."),


    h3(icon('bullseye'), ' Purpose of the Study'),
    p(
        "The primary goal of this study was to compile a large-scale, clinically annotated transcriptomic atlas that captures the gene expression signatures of nervous system tumors and non-tumor tissues. Unlike previous efforts that focused narrowly on specific tumor types or relied heavily on DNA methylation, this project sought to harmonize a broad spectrum of transcriptomic data from public repositories. By doing so, it addresses the scarcity of resources for studying rare tumor types, pediatric and adult cases, and samples from varied geographic regions. The atlas aims to empower comparative gene expression analyses, support diagnostic refinement, and lay the groundwork for integrating genomic data with clinical outcomes—ultimately advancing personalized medicine in neuro-oncology."
    ),
    h3(icon('tools'), ' How It Was Conducted'),
    p(
        "To build this atlas, the team meticulously gathered raw transcriptomic data from thousands of nervous system samples—both neoplastic (tumorous) and non-neoplastic—available in public databases like the Gene Expression Omnibus and ArrayExpress. They standardized the data by using a single platform, the Applied Biosystems™ GeneChip™ Human Genome U133 Plus 2.0 Array, which offers extensive coverage of the human genome. This raw data, spanning years of collection (2003–2018), was reprocessed simultaneously using advanced computational tools in R, including background correction, normalization, and summarization techniques. Machine learning algorithms, such as t-SNE for visualization and classifiers like random forests and gradient boosting, were employed to organize samples into biologically meaningful clusters and refine diagnoses where historical classifications were outdated or unclear. Clinical metadata—age, sex, tumor location, genetic mutations, and survival data—were manually curated to enrich the dataset, ensuring its utility for multifaceted analyses."
    ),
    h3(icon('seedling'), ' A Growing Resource'),
    p(
        "This atlas is just the beginning. The methodology developed here—harmonizing diverse transcriptomic data into a unified framework—is designed to be scalable. As new raw data become available in public domains or through collaborations, we plan to expand the atlas to include additional samples, tumor types, and molecular profiles. Future updates will incorporate emerging genomic platforms and integrate other omics data (e.g., proteomics, epigenomics), enhancing the depth and breadth of this resource. Stay tuned for a continually evolving tool that adapts to the latest advancements in neuro-oncology research."
    ),
    h3(icon('brain'), ' Scientific and Biological Context'),
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

    # h3(icon('chart-line'), ' What This App Offers'),
    # p(
    #     "This app brings the atlas to life with three interactive tools tailored to neuro-oncology exploration:",
    #     tags$ul(
    #         tags$li(
    #             tags$b("Survival Analysis:"),
    #             " Use Kaplan-Meier curves to examine survival trends across tumor types, genetic mutations (e.g., IDH, MGMT), or demographic factors (e.g., age, sex). Filter by diagnosis or age group, and generate single-variable or multi-variable grids to compare outcomes.",
    #             span(img(src = "survival_analysis_banner.png"), style = 'text-align: center; width = "100%"; margin-bottom: 20px;')
    #         ),
    #         tags$li(
    #             tags$b("t-SNE Dimensionality Reduction:"),
    #             " Visualize how 7,375 samples cluster based on their transcriptomic profiles in a 2D plot. Select points to explore sample details, revealing patterns of similarity and difference across diagnoses.",
    #             span(img(src = "tsne_banner.png"), style = 'text-align: center; width = "100%"; margin-bottom: 20px;')
    #         ),
    #         tags$li(
    #             tags$b("mRNA Expression Boxplots:"),
    #             " Compare gene expression levels across tumors or groups. Search by Gene Ontology terms (e.g., cell cycle, apoptosis) or specific genes, and group results by variables like diagnosis, grade, or sex to uncover molecular insights.",
    #             span(img(src = "mrna_expression_banner.png"), style = 'text-align: center; width = "100%"; margin-bottom: 20px;')
    #         )
    #     ),
    #     "Each tool is designed for ease of use—select options, generate plots, and download results to fuel your research or education."
    # ),
    h3(icon('rocket'), ' Get Started'),
    p(
        "Dive in by navigating the tabs above. Whether you're a researcher seeking novel hypotheses, a clinician refining diagnostic approaches, or a student learning tumor biology, this app offers a hands-on experience with real-world data."
        # tags$a(href = "mailto:axitamm@gmail.com", "axitamm@gmail.com"), "."
    ),
    span(tagList(
        br(),
        img(src = "KY Pediatric Cancer Research - Final.png", style = "width:25%; object-fit: contain;"),
        img(src = "DPH and PHAB logo.png", style = "width:35%; object-fit: contain;"),
        img(src = "u_of_l.jpg", style = "width:20%; object-fit: contain;"),
        br(),
        paste0('Last updated: ', currentDate),br(),
        p('Download',
            tags$a(href = "https://github.com/axitamm/BrainTumorAtlas", "source data"), 
            'and',
            tags$a(href = "https://github.com/KentuckyDepartmentForPublicHealth/atlas", "application code"), br(),
            "This project was funded by the Kentucky Pediatric Cancer Research Trust Fund",br(), "ShinyApp powered by the Kentucky Department for Public Health"
        )
    ), style = 'display: block; width: 100%; font-size: 0.75em; color: black; background: white; text-align: center; border-radius: 50px; border: 2px solid white; padding: 1em; margin: 1em 0;'),
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
                            withSpinner(plotOutput("survival_plot", height = "600px"))
                        ),
                        conditionalPanel(
                            condition = "input.analysis_type == 'grid'",
                            withSpinner(plotOutput("grid_plot", height = "800px"))
                        )
                    ),
                    tabPanel(
                        "Summary",
                        h4("Group Statistics"),
                        tableOutput("group_stats"),
                        h4("Median Survival Estimates"),
                        tableOutput("median_survival")
                    ),
                    tabPanel("Data", DTOutput("survival_data"))
                )
            )
        )
    ),
# t-SNE Dimensionality Reduction Tab -----

    nav_panel(
        title = "t-SNE Dimensionality Reduction", icon = icon("th"),
        fluidPage(
            withSpinner(plotlyOutput("tsnePlot", height = "600px")),
            DT::dataTableOutput("dataTable")
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
                plotOutput("boxplot"),
                tableOutput("gene_info")
            )
        )
    ),
    # Remove the Debug tab here.
    # Dark mode toggle in the navbar (placed as a nav_item)
    nav_item(
        input_dark_mode(id = "mode_toggle", mode = "dark")
    )
)

# Server logic remains unchanged, same as before
server <- function(input, output, session) {

    observeEvent(input$navBar, {
        # Check if user selected the mRNA Expression tab and data not yet loaded
        if (input$navBar == "mRNA Expression Boxplots" && !gene_data_loaded()) {
            # Show a loading message
            showNotification("Loading gene expression data...",
                type = "message",
                duration = NULL,
                id = "loading_notification"
            )

            # Load the large dataset only when needed
            load("dat/geneExpressionData.RData", envir = .GlobalEnv)

            # Mark data as loaded
            gene_data_loaded(TRUE)

            # Remove loading notification
            removeNotification(id = "loading_notification")
            showNotification("Gene expression data loaded successfully!",
                type = "message",
                duration = 3
            )
        }
    })

    # Observe clicks on Survival Analysis image and switch to the corresponding tab
    observeEvent(input$goto_survival, {
        updateNavbarPage(session, "navBar", selected = "Survival Analysis")
    })

    # Observe clicks on t-SNE Dimensionality Reduction image and switch to the corresponding tab
    observeEvent(input$goto_tsne, {
        updateNavbarPage(session, "navBar", selected = "t-SNE Dimensionality Reduction")
    })

    # Observe clicks on mRNA Expression Boxplots image and switch to the corresponding tab
    observeEvent(input$goto_mrna, {
        updateNavbarPage(session, "navBar", selected = "mRNA Expression Boxplots")
    })

    # Survival Analysis functions and logic *************************************
    prepare_data <- function(data) {
        data <- data %>%
            mutate(MB_subtypes = case_when(
                diagnosisFinal == "MB-WNT" ~ "MB-WNT",
                diagnosisFinal == "MB-SHH" ~ "MB-SHH",
                diagnosisFinal == "MB-GP4" ~ "MB-GP4",
                diagnosisFinal == "MB-GP3" ~ "MB-GP3",
                TRUE ~ NA_character_
            )) %>%
            mutate(ATRT_subtypes = case_when(
                diagnosisFinal == "AT/RT-MYC" ~ "AT/RT-MYC",
                diagnosisFinal == "AT/RT-SHH" ~ "AT/RT-SHH",
                diagnosisFinal == "AT/RT-TYR" ~ "AT/RT-TYR",
                TRUE ~ NA_character_
            )) %>%
            mutate(EPN_subtypes = case_when(
                diagnosisFinal == "PFA" ~ "PFA",
                diagnosisFinal == "PFB" ~ "PFB",
                diagnosisFinal == "RELA" ~ "RELA",
                TRUE ~ NA_character_
            )) %>%
            mutate(MEN_grades = case_when(
                diagnosisFinal == "MENINGIOMA" & grade == "GRADE 1" ~ "MEN Grade 1",
                diagnosisFinal == "MENINGIOMA" & grade == "GRADE 2" ~ "MEN Grade 2",
                diagnosisFinal == "MENINGIOMA" & grade == "GRADE 3" ~ "MEN Grade 3",
                TRUE ~ NA_character_
            )) %>%
            mutate(GCT_subtypes = case_when(
                diagnosisClass == "GERM CELL" ~ "GCT",
                diagnosisFinal == "PA" ~ "PA",
                diagnosisFinal == "PXA" ~ "PXA",
                TRUE ~ NA_character_
            ))
        return(data)
    }

filtered_data <- reactive({
    req(input$generate)
    data <- atlasDataClean %>%
        filter(!is.na(survivalMonths), !is.na(mortality)) %>%
        prepare_data()
    if (input$diagnosis_filter != "All") {
        data <- data %>% filter(diagnosisClass == input$diagnosis_filter)
    }
    if (input$age_filter != "All") {
        data <- data %>% filter(ageGroup == input$age_filter)
    }
    
    # Validate necessary columns exist
    required_columns <- c("diagnosisFinal", "diagnosisClass", "sex", "ageGroup", "grade",
                          "mutationIDH1/2", "mutationH3", "1p/19q-codel",
                          "methylationMGMTpromoter", "amplificationMCYN")
    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns) > 0) {
        showNotification(paste("Warning: Missing columns after filtering:", paste(missing_columns, collapse = ", ")), type = "error")
    }
    
    return(data)
})
# Replace the existing strat_data reactive expression with this:
strat_data <- reactive({
    req(input$generate, input$analysis_type == "single")
    data <- filtered_data()
    if (nrow(data) == 0) {
        return(NULL)
    }

    # Convert 'strat_group' to a factor and explicitly handle NA values as "Unknown"
    data$strat_group <- fct_na_value_to_level(factor(data[[input$strat_var]]), "Unknown")

    if (input$collapse_rare) {
        group_counts <- table(data$strat_group)
        large_groups <- names(group_counts[group_counts >= input$min_group_size])
        if (length(large_groups) < length(group_counts)) {
            data <- data %>%
                mutate(strat_group = ifelse(strat_group %in% large_groups,
                    as.character(strat_group),
                    "Other (Small Groups)"
                )) %>%
                mutate(strat_group = factor(strat_group))
        }
    }
    return(data)
})

    create_survival_plot <- function(data, strat_var, title = NULL, show_pvalue = TRUE, show_ci = FALSE, max_time = NULL) {
        if (is.null(data) || nrow(data) == 0 || !strat_var %in% names(data)) {
            plot.new()
            text(0.5, 0.5, "No data available", cex = 1.2)
            return()
        }
        data <- data %>% filter(!is.na(!!sym(strat_var)))
        if (nrow(data) == 0) {
            plot.new()
            text(0.5, 0.5, "No valid data for this variable", cex = 1.2)
            return()
        }
        data$plot_group <- factor(data[[strat_var]])
        data$plot_group <- droplevels(data$plot_group)
        levels <- levels(data$plot_group)
        if (length(levels) == 0) {
            plot.new()
            text(0.5, 0.5, "No groups to display", cex = 1.2)
            return()
        }
        surv_obj <- Surv(data$survivalMonths, data$mortality)
        surv_fit <- survfit(surv_obj ~ plot_group, data = data)
        colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")
        if (length(levels) > length(colors)) {
            colors <- rep(colors, length.out = length(levels))
        } else {
            colors <- colors[1:length(levels)]
        }
        lighten_color <- function(color, factor = 0.5) {
            rgb_val <- col2rgb(color)
            light_rgb <- pmin(rgb_val + (255 - rgb_val) * factor, 255)
            rgb(light_rgb[1], light_rgb[2], light_rgb[3], maxColorValue = 255)
        }
        ci_colors <- sapply(colors, lighten_color, factor = 0.7)
        if (is.null(title)) {
            title <- paste("Survival by", strat_var)
        }
        xlim <- if (!is.null(max_time) && max_time > 0) c(0, max_time) else NULL
        plot(surv_fit,
            col = colors,
            lwd = 2,
            xlab = "Time (Months)",
            ylab = "Survival Probability",
            main = title,
            mark.time = TRUE,
            xlim = xlim
        )
        if (show_ci) {
            for (i in seq_along(levels)) {
                surv_summary <- summary(surv_fit[i])
                time <- surv_summary$time
                lower <- surv_summary$lower
                upper <- surv_summary$upper
                valid <- !is.na(lower) & !is.na(upper)
                time <- time[valid]
                lower <- lower[valid]
                upper <- upper[valid]
                if (length(time) > 0) {
                    polygon(c(time, rev(time)), c(lower, rev(upper)),
                        col = adjustcolor(ci_colors[i], alpha.f = 0.3),
                        border = NA
                    )
                }
            }
        }
        lines(surv_fit, col = colors, lwd = 2)
        group_counts <- table(data$plot_group)
        legend_labels <- paste0(levels, " (n=", group_counts[levels], ")")
        legend("topright",
            legend = legend_labels,
            col = colors,
            lty = 1,
            lwd = 2,
            cex = 0.7,
            bty = "n"
        )
        if (show_pvalue && length(levels) > 1) {
            log_rank <- survdiff(surv_obj ~ plot_group, data = data)
            p_val <- 1 - pchisq(log_rank$chisq, length(levels) - 1)
            p_text <- if (p_val < 0.001) "p < 0.001" else paste("p =", format(round(p_val, 3), nsmall = 3))
            text_x <- if (!is.null(xlim)) xlim[2] * 0.7 else max(data$survivalMonths, na.rm = TRUE) * 0.7
            text(text_x, 0.1, p_text, cex = 0.8)
        }
    }

    output$survival_plot <- renderPlot({
        req(input$generate, input$analysis_type == "single")
        data <- strat_data()
        if (is.null(data) || nrow(data) == 0) {
            plot.new()
            text(0.5, 0.5, "No data available for the selected criteria.", cex = 1.5)
            return()
        }
        create_survival_plot(
            data, "strat_group", paste("Survival by", input$strat_var),
            input$show_pvalue, input$show_ci
        )
    })

    output$grid_plot <- renderPlot({
        req(input$generate, input$analysis_type == "grid")
        data <- filtered_data()
        if (nrow(data) == 0 || length(input$grid_variables) == 0) {
            plot.new()
            text(0.5, 0.5, if (nrow(data) == 0) "No data available for the selected criteria." else "Please select at least one variable for the grid.", cex = 1.5)
            return()
        }
        n_plots <- length(input$grid_variables)
        n_cols <- if (n_plots <= 3) n_plots else 3
        n_rows <- if (n_plots <= 3) 1 else ceiling(n_plots / 3)
        par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
        for (var in input$grid_variables) {
            title <- switch(var,
                "mutationIDH1/2" = "IDH Mutation",
                "grade" = "Tumor Grade",
                "methylationMGMTpromoter" = "MGMT Methylation",
                "MB_subtypes" = "Medulloblastoma",
                "amplificationMCYN" = "MYCN Amplification",
                "ATRT_subtypes" = "AT/RT",
                "EPN_subtypes" = "Ependymoma",
                "MEN_grades" = "Meningioma",
                "GCT_subtypes" = "Germ Cell Tumors",
                var
            )
            create_survival_plot(data, var, title, input$show_pvalue, input$show_ci, input$max_months)
        }
    })

    output$group_stats <- renderTable(
        {
            req(input$generate)
            data <- if (input$analysis_type == "single") strat_data() else filtered_data()
            if (is.null(data) || nrow(data) == 0) {
                return(data.frame(Variable = "N/A", Group = "No data available", Count = NA))
            }
            if (input$analysis_type == "single") {
                group_var <- sym("strat_group")
                data %>%
                    filter(!is.na(!!group_var)) %>%
                    group_by(!!group_var) %>%
                    summarise(
                        Count = n(),
                        `Mean Survival (months)` = round(mean(survivalMonths, na.rm = TRUE), 1),
                        `Events (Deaths)` = sum(mortality),
                        `Mortality Rate (%)` = round(sum(mortality) / n() * 100, 1)
                    ) %>%
                    arrange(desc(Count)) %>%
                    rename(Group = !!group_var) %>%
                    mutate(Variable = input$strat_var) %>%
                    select(Variable, Group, Count, `Mean Survival (months)`, `Events (Deaths)`, `Mortality Rate (%)`)
            } else {
                stats_list <- lapply(input$grid_variables, function(var) {
                    group_var <- sym(var)
                    data %>%
                        filter(!is.na(!!group_var)) %>%
                        group_by(!!group_var) %>%
                        summarise(
                            Count = n(),
                            `Mean Survival (months)` = round(mean(survivalMonths, na.rm = TRUE), 1),
                            `Events (Deaths)` = sum(mortality),
                            `Mortality Rate (%)` = round(sum(mortality) / n() * 100, 1)
                        ) %>%
                        arrange(desc(Count)) %>%
                        rename(Group = !!group_var) %>%
                        mutate(Variable = var)
                })
                if (length(stats_list) == 0) {
                    return(data.frame(Variable = "N/A", Group = "No variables selected", Count = NA))
                }
                bind_rows(stats_list) %>%
                    select(Variable, Group, Count, `Mean Survival (months)`, `Events (Deaths)`, `Mortality Rate (%)`)
            }
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE
    )

# Replace the existing output$median_survival renderTable with this:

output$median_survival <- renderTable(
    {
        req(input$generate)
        data <- if (input$analysis_type == "single") strat_data() else filtered_data()

        if (is.null(data) || nrow(data) == 0) {
            return(data.frame(Variable = "N/A", Group = "No data available", `Median Survival` = NA_character_))
        }

        if (input$analysis_type == "single") {
            group_var <- "strat_group"
            summary_table <- data %>%
                group_by(!!sym(group_var)) %>%
                summarise(
                    Count = n(),
                    `Mean Survival (months)` = round(mean(survivalMonths, na.rm = TRUE), 1),
                    `Events (Deaths)` = sum(mortality),
                    `Mortality Rate (%)` = round(mean(mortality, na.rm = TRUE) * 100, 1),
                    `Median Survival` = if_else(
                        sum(mortality) >= n() / 2,
                        round(median(survivalMonths[mortality == 1], na.rm = TRUE), 1),
                        NA_real_
                    )
                ) %>%
                mutate(
                    `Median Status` = if_else(
                        is.na(`Median Survival`),
                        "Not reached",
                        paste0(`Median Survival`, " months")
                    )
                ) %>%
                arrange(desc(Count)) %>%
                rename(Group = !!sym(group_var)) %>%
                mutate(Variable = input$strat_var) %>%
                select(Variable, Group, Count, `Mean Survival (months)`, `Events (Deaths)`, `Mortality Rate (%)`, `Median Status`)

            return(summary_table)
        } else {
            # For 'grid' analysis
            stats_list <- lapply(input$grid_variables, function(var) {
                group_var <- sym(var)
                if (!(var %in% names(data))) {
                    # Handle missing columns gracefully
                    showNotification(paste("Warning: Column", var, "does not exist in the data. Skipping."), type = "warning")
                    return(NULL)
                }
                data %>%
                    group_by(!!sym(var)) %>%
                    summarise(
                        Count = n(),
                        `Mean Survival (months)` = round(mean(survivalMonths, na.rm = TRUE), 1),
                        `Events (Deaths)` = sum(mortality),
                        `Mortality Rate (%)` = round(mean(mortality, na.rm = TRUE) * 100, 1),
                        `Median Survival` = if_else(
                            sum(mortality) >= n() / 2,
                            round(median(survivalMonths[mortality == 1], na.rm = TRUE), 1),
                            NA_real_
                        )
                    ) %>%
                    mutate(
                        `Median Status` = if_else(
                            is.na(`Median Survival`),
                            "Not reached",
                            paste0(`Median Survival`, " months")
                        )
                    ) %>%
                    arrange(desc(Count)) %>%
                    rename(Group = !!sym(var)) %>%
                    mutate(Variable = var) %>%
                    select(Variable, Group, Count, `Mean Survival (months)`, `Events (Deaths)`, `Mortality Rate (%)`, `Median Status`)
            })

            # Remove NULL elements resulting from missing columns
            stats_list <- stats_list[!sapply(stats_list, is.null)]

            if (length(stats_list) == 0) {
                return(data.frame(Variable = "N/A", Group = "No variables selected or available", `Median Survival` = NA_character_))
            }

            # Bind the rows, ensuring consistent column types
            summary_table <- bind_rows(stats_list)

            return(summary_table)
        }
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
)
    output$survival_data <- renderDT({
        req(input$generate)
        data <- if (input$analysis_type == "single") strat_data() else filtered_data()
        if (is.null(data) || nrow(data) == 0) {
            return(NULL)
        }
        datatable(data %>% select(survivalMonths, mortality, diagnosisFinal, diagnosisClass, sex, ageGroup, grade),
            options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE
        )
    })

    output$download_plot <- downloadHandler(
        filename = function() {
            paste("survival_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep = "")
        },
        content = function(file) {
            pdf(file, width = 10, height = 8)
            if (input$analysis_type == "single") {
                data <- strat_data()
                if (is.null(data) || nrow(data) == 0) {
                    plot.new()
                    text(0.5, 0.5, "No data available for the selected criteria.", cex = 1.5)
                } else {
                    create_survival_plot(
                        data, "strat_group", paste("Survival by", input$strat_var),
                        input$show_pvalue, input$show_ci
                    )
                }
            } else {
                data <- filtered_data()
                if (nrow(data) == 0 || length(input$grid_variables) == 0) {
                    plot.new()
                    text(0.5, 0.5, if (nrow(data) == 0) "No data available for the selected criteria." else "Please select at least one variable for the grid.", cex = 1.5)
                } else {
                    n_plots <- length(input$grid_variables)
                    n_cols <- if (n_plots <= 2) n_plots else min(3, n_plots)
                    n_rows <- ceiling(n_plots / n_cols)
                    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
                    for (var in input$grid_variables) {
                        title <- switch(var,
                            "mutationIDH1/2" = "IDH Mutation",
                            "grade" = "Tumor Grade",
                            "methylationMGMTpromoter" = "MGMT Methylation",
                            "MB_subtypes" = "Medulloblastoma",
                            "amplificationMCYN" = "MYCN Amplification",
                            "ATRT_subtypes" = "AT/RT",
                            "EPN_subtypes" = "Ependymoma",
                            "MEN_grades" = "Meningioma",
                            "GCT_subtypes" = "Germ Cell Tumors",
                            var
                        )
                        create_survival_plot(data, var, title, input$show_pvalue, input$show_ci, input$max_months)
                    }
                }
            }
            dev.off()
        }
    )

    # t-SNE Dimensionality Reduction Logic ***************************************
    if (!"key" %in% names(atlasDataClean)) {
        atlasDataClean$key <- seq_len(nrow(atlasDataClean))
    }
    filteredData <- reactive({
        atlasDataClean
    })
    output$tsnePlot <- renderPlotly({
        data <- filteredData()
        validate(need(nrow(data) > 0, "No data available to plot."))
        data$diagnosisClass <- factor(data$diagnosisClass)
        data$diagnosisFinal <- factor(data$diagnosisFinal)
        non_tumor_data <- data[data$diagnosisClass == "NON-TUMOR", ]
        tumor_data <- data[data$diagnosisClass != "NON-TUMOR", ]
        non_tumor_diagnoses <- levels(factor(non_tumor_data$diagnosisFinal))
        tumor_diagnoses <- levels(factor(tumor_data$diagnosisFinal))
        non_tumor_colors <- if (length(non_tumor_diagnoses) > 0) {
            setNames(gray.colors(length(non_tumor_diagnoses), start = 0.2, end = 0.8), non_tumor_diagnoses)
        } else {
            NULL
        }
        tumor_color_mapping <- if (length(tumor_diagnoses) > 0) {
            setNames(tumor_colors[1:length(tumor_diagnoses)], tumor_diagnoses)
        } else {
            NULL
        }
        all_colors <- c(non_tumor_colors, tumor_color_mapping)
        centroids <- data %>%
            group_by(diagnosisFinal) %>%
            summarise(tsne1 = median(tsne1), tsne2 = median(tsne2))
        p <- plot_ly(source = "A")
        for (class in levels(data$diagnosisClass)) {
            class_data <- data[data$diagnosisClass == class, ]
            if (nrow(class_data) == 0) next
            for (diagnosis in levels(factor(class_data$diagnosisFinal))) {
                diag_data <- class_data[class_data$diagnosisFinal == diagnosis, ]
                if (nrow(diag_data) == 0) next
                p <- add_trace(p,
                    data = diag_data, x = ~tsne1, y = ~tsne2, type = "scatter", mode = "markers",
                    name = diagnosis, legendgroup = class, showlegend = TRUE,
                    legendgrouptitle = if (diagnosis == levels(factor(class_data$diagnosisFinal))[1]) {
                        list(text = class)
                    } else {
                        NULL
                    },
                    marker = list(size = 5, opacity = 0.8, color = all_colors[diagnosis]),
                    text = ~ paste(
                        "Sample:", sampleID, "<br>Diagnosis Class:", diagnosisClass,
                        "<br>Diagnosis Final:", diagnosisFinal
                    ),
                    hoverinfo = "text", key = ~key
                )
            }
        }
        p <- layout(p,
            title = "t-SNE Dimensionality Reduction",
            xaxis = list(title = "t-SNE 1", color = "black"),
            yaxis = list(title = "t-SNE 2", color = "black"),
            plot_bgcolor = "white",
            paper_bgcolor = "white",
            font = list(color = "black"),
            dragmode = "select",
            legend = list(itemclick = "toggleothers", itemdoubleclick = FALSE),
            annotations = lapply(1:nrow(centroids), function(i) {
                list(
                    x = centroids$tsne1[i],
                    y = centroids$tsne2[i],
                    text = centroids$diagnosisFinal[i],
                    showarrow = FALSE, font = list(size = 10, color = "gray10")
                )
            })
        )
        p <- event_register(p, "plotly_selected")
        return(p)
    })
    selected_points <- reactive({
        event_data("plotly_selected", source = "A")
    })
    output$dataTable <- DT::renderDataTable({
        select_data <- selected_points()
        validate(need(!is.null(select_data) && nrow(select_data) > 0, "Must select at least one data point."))
        point_keys <- as.numeric(select_data$key)
        selected_data <- atlasDataClean[atlasDataClean$key %in% point_keys, ]
        validate(need(nrow(selected_data) > 0, "No valid points selected."))
        DT::datatable(selected_data)
    })

    # mRNA Expression Boxplots Logic **********************************************
    validate(
        need(exists("gene_annotations"), "Error: `gene_annotations` is not loaded."),
        # need(exists("geneExpressionData"), "Error: `geneExpressionData` is not loaded."),
        need(exists("atlasDataClean"), "Error: `atlasDataClean` is not loaded."),
        need(exists("go_to_genes_list"), "Error: `go_to_genes_list` is not loaded.")
    )
    
    gene_annotations <- gene_annotations %>% filter(!is.na(ENTREZID))
    # all_valid_genes <- gene_annotations %>%
    #     filter(ENTREZID %in% rownames(geneExpressionData)) %>%
    #     pull(SYMBOL) %>%
    #     unique() %>%
    #     sort()
    shinyjs::hide("use_group_by")
    observeEvent(input$reload, {
        shinyjs::refresh()
    })
    updateSelectizeInput(session, "go_term",
        choices = names(go_to_genes_list), selected = NULL,
        server = TRUE, options = list(maxOptions = 999999)
    )
    total_unique_genes <- reactive({
        req(input$search_mode)
        if (input$search_mode == "Select GO Term") {
            req(input$go_term)
            if (input$go_term == "") {
                return(0)
            }
            go_genes <- go_to_genes_list[[input$go_term]] %||% character(0)
            length(gene_annotations %>% filter(ENTREZID %in% go_genes) %>% pull(SYMBOL) %>% unique())
        } else if (input$search_mode == "Select All Genes") {
            length(all_valid_genes)
        } else {
            0
        }
    })
    output$max_options_slider <- renderUI({
        req(total_unique_genes() > 0)
        sliderInput("max_options", "Number of Genes to Display in Dropdown",
            min = 1, max = total_unique_genes(), value = min(5, total_unique_genes()), step = 1
        )
    })
    observeEvent(input$search_mode, {
        if (isTRUE(input$search_mode == "Select All Genes")) {
            updateSelectizeInput(session, "all_genes",
                choices = all_valid_genes,
                options = list(maxOptions = 5), server = TRUE
            )
        }
    })
    observeEvent(input$max_options, {
        if (isTRUE(input$search_mode == "Select All Genes")) {
            updateSelectizeInput(session, "all_genes",
                choices = all_valid_genes,
                options = list(maxOptions = min(input$max_options, length(all_valid_genes))),
                server = TRUE
            )
        }
    })
    output$go_gene_selector <- renderUI({
        req(input$search_mode == "Select GO Term", input$go_term)
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
        selectizeInput("selected_gene", "Select Genes",
            multiple = TRUE, choices = choices,
            options = list(maxOptions = min(input$max_options %||% 5, length(choices)))
        )
    })
    selected_genes_now <- reactive({
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
    observe({
        if (length(selected_genes_now()) > 0) {
            shinyjs::show("use_group_by")
        } else {
            shinyjs::hide("use_group_by")
            updateCheckboxInput(session, "use_group_by", value = FALSE)
        }
    })
    output$group_by_selector <- renderUI({
        if (input$use_group_by) {
            selectInput("group_by", "Group By",
                choices = sort(c(
                    "isCancerous", "grade", "ageGroup", "tumorType", "sex",
                    "compartment", "fullName", "country", "diagnosisFinal",
                    "histologyOriginal", "diagnosisClass"
                )),
                selected = "sex"
            )
        }
    })
    final_data <- eventReactive(input$run, {
        chosen_genes <- selected_genes_now()
        if (length(chosen_genes) == 0) {
            shinyalert("Oops!", "You must select at least one gene to run the plot.",
                type = "error",
                confirmButtonCol = "#0B3B60", size = "m"
            )
            return(NULL)
        }
        isolate({
            entrez_ids <- gene_annotations %>%
                filter(SYMBOL %in% chosen_genes) %>%
                pull(ENTREZID)
            if (length(entrez_ids) == 0) {
                return(list(df = data.frame(), doGroup = FALSE, grpVar = NULL))
            }
            gene_data <- geneExpressionData[rownames(geneExpressionData) %in% entrez_ids, ]
            if (nrow(gene_data) == 0) {
                return(list(df = data.frame(), doGroup = FALSE, grpVar = NULL))
            }
            gene_data <- gene_data %>%
                as.data.frame() %>%
                mutate(ENTREZID = rownames(.)) %>%
                pivot_longer(cols = -ENTREZID, names_to = "filename", values_to = "expression") %>%
                left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
                left_join(atlasDataClean, by = "filename")
            list(df = gene_data, doGroup = input$use_group_by, grpVar = input$group_by)
        })
    })
    plot_cleared <- reactiveVal(FALSE)
    output$boxplot <- renderPlot({
        if (plot_cleared()) {
            ggplot() +
                geom_text(aes(x = 0.5, y = 0.5, label = "Plot data has been cleared. Make a new selection"),
                    size = 6, hjust = 0.5, vjust = 0.5
                ) +
                theme_void() +
                coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
        } else {
            all_stuff <- final_data()
            if (is.null(all_stuff) || nrow(all_stuff$df) == 0) {
                plot.new()
                text(0.5, 0.5, "No data to display", cex = 1.2)
                return()
            }
            data <- all_stuff$df
            gflag <- all_stuff$doGroup
            gvar <- all_stuff$grpVar
            p <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
                geom_boxplot() +
                geom_jitter(width = 0.2, alpha = 0.5) +
                labs(
                    title = "Expression of Selected Genes", subtitle = paste0("Total values: n=", nrow(data)),
                    x = "Gene Symbol", y = "Expression Level"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            if (gflag && !is.null(gvar) && gvar != "") {
                facet_counts <- data %>%
                    group_by(SYMBOL, .data[[gvar]]) %>%
                    summarise(n = n(), .groups = "drop")
                p <- p + facet_wrap(as.formula(paste("~", gvar)), scales = "free") +
                    geom_text(
                        data = facet_counts, aes(label = paste0("n=", n), y = Inf),
                        vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
                    )
            } else {
                symbol_counts <- data %>%
                    group_by(SYMBOL) %>%
                    summarise(n = n(), .groups = "drop")
                p <- p + geom_text(
                    data = symbol_counts, aes(label = paste0("n=", n), y = Inf),
                    vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
                )
            }
            p
        }
    })
    output$gene_info <- renderTable({
        all_stuff <- final_data()
        if (is.null(all_stuff) || nrow(all_stuff$df) == 0) {
            return(NULL)
        }
        all_stuff$df %>%
            select(SYMBOL, GENENAME) %>%
            distinct()
    })
    observeEvent(input$reset, {
        updateRadioButtons(session, "search_mode", selected = character(0))
        updateSelectizeInput(session, "go_term", selected = NULL)
        updateSelectizeInput(session, "selected_gene", selected = NULL)
        updateSelectizeInput(session, "all_genes", selected = NULL)
        if (!is.null(total_unique_genes())) {
            updateSliderInput(session, "max_options", value = 5)
        }
        shinyjs::hide("use_group_by")
        updateCheckboxInput(session, "use_group_by", value = FALSE)
        updateSelectInput(session, "group_by", selected = "sex")
        plot_cleared(TRUE)
    })
    observeEvent(input$run, {
        if (!is.null(final_data()) && nrow(final_data()$df) > 0) {
            plot_cleared(FALSE)
        }
    })
    output$save_plot <- downloadHandler(
        filename = function() {
            paste("mrna_expression_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep = "")
        },
        content = function(file) {
            if (plot_cleared()) {
                ggsave(file,
                    plot = ggplot() +
                        geom_text(aes(x = 0.5, y = 0.5, label = "Plot data has been cleared. Make a new selection"),
                            size = 6, hjust = 0.5, vjust = 0.5
                        ) +
                        theme_void() +
                        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)),
                    device = "pdf", width = 10, height = 8
                )
            } else {
                all_stuff <- final_data()
                if (is.null(all_stuff) || nrow(all_stuff$df) == 0) {
                    ggsave(file,
                        plot = ggplot() +
                            geom_text(aes(x = 0.5, y = 0.5, label = "No data to display"),
                                size = 6, hjust = 0.5, vjust = 0.5
                            ) +
                            theme_void() +
                            coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)),
                        device = "pdf", width = 10, height = 8
                    )
                } else {
                    data <- all_stuff$df
                    gflag <- all_stuff$doGroup
                    gvar <- all_stuff$grpVar
                    p <- ggplot(data, aes(x = SYMBOL, y = expression, color = SYMBOL)) +
                        geom_boxplot() +
                        geom_jitter(width = 0.2, alpha = 0.5) +
                        labs(
                            title = "Expression of Selected Genes", subtitle = paste0("Total values: n=", nrow(data)),
                            x = "Gene Symbol", y = "Expression Level"
                        ) +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    if (gflag && !is.null(gvar) && gvar != "") {
                        facet_counts <- data %>%
                            group_by(SYMBOL, .data[[gvar]]) %>%
                            summarise(n = n(), .groups = "drop")
                        p <- p + facet_wrap(as.formula(paste("~", gvar)), scales = "free") +
                            geom_text(
                                data = facet_counts, aes(label = paste0("n=", n), y = Inf),
                                vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
                            )
                    } else {
                        symbol_counts <- data %>%
                            group_by(SYMBOL) %>%
                            summarise(n = n(), .groups = "drop")
                        p <- p + geom_text(
                            data = symbol_counts, aes(label = paste0("n=", n), y = Inf),
                            vjust = 1.5, size = 3, fontface = "bold", color = "gray20"
                        )
                    }
                    ggsave(file, plot = p, device = "pdf", width = 10, height = 8)
                }
            }
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
