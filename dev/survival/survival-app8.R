# Load necessary libraries
library(shiny)
library(survival)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(shinyjs)
library(shinycssloaders)

# Load the data
load("../../../dat/atlasDataClean.RData")

# Define UI
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Brain Tumor Survival Analysis"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            # Info box
            div(
                style = "padding: 10px; background-color: #f0f7fa; border-left: 5px solid #17a2b8; margin-bottom: 15px; border-radius: 3px;",
                h4("About This Tool", style = "margin-top: 0;"),
                p("Analyze survival outcomes for brain tumors using Kaplan-Meier curves."),
                p("This dataset contains survival data for", tags$b(HTML("2,074 patients")), ".")
            ),
            # Analysis type selection (single vs grid)
            radioButtons("analysis_type", "Analysis Type:",
                choices = c(
                    "Single Variable" = "single",
                    "Multi-Variable Grid" = "grid"
                ),
                selected = "grid"
            ),
            # Single variable controls
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
            # Grid view controls
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
            # Filters
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
            # Plot controls
            hr(),
            checkboxInput("show_pvalue", "Show Log-Rank P-value", TRUE),
            checkboxInput("show_ci", "Show Confidence Intervals", FALSE),
            # Buttons
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
)

# Server
server <- function(input, output, session) {
    # Create derived variables for plotting
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

    # Filter data based on user inputs
    filtered_data <- reactive({
        req(input$generate)
        data <- atlasDataClean %>%
            filter(!is.na(survivalMonths), !is.na(mortality))
        data <- prepare_data(data)
        if (input$diagnosis_filter != "All") {
            data <- data %>% filter(diagnosisClass == input$diagnosis_filter)
        }
        if (input$age_filter != "All") {
            data <- data %>% filter(ageGroup == input$age_filter)
        }
        return(data)
    })

    # Process data for single variable view
    strat_data <- reactive({
        req(input$generate, input$analysis_type == "single")
        data <- filtered_data()
        if (nrow(data) == 0) {
            return(NULL)
        }
        data$strat_group <- data[[input$strat_var]]
        data$strat_group[is.na(data$strat_group)] <- "Unknown"
        if (input$collapse_rare) {
            group_counts <- table(data$strat_group)
            large_groups <- names(group_counts[group_counts >= input$min_group_size])
            if (length(large_groups) < length(group_counts)) {
                data <- data %>%
                    mutate(strat_group = ifelse(strat_group %in% large_groups,
                        as.character(strat_group),
                        "Other (Small Groups)"
                    ))
            }
        }
        data$strat_group <- factor(data$strat_group)
        return(data)
    })

    # Function to create a single survival plot
    create_survival_plot <- function(data, strat_var, title = NULL,
                                     show_pvalue = TRUE, show_ci = FALSE, max_time = NULL) {
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
        if (is.null(title)) {
            title <- paste("Survival by", strat_var)
        }
        xlim <- NULL
        if (!is.null(max_time) && max_time > 0) {
            xlim <- c(0, max_time)
        }
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
            for (i in 1:length(levels)) {
                lines(surv_fit[i], conf.int = TRUE, col = colors[i], lty = 2)
            }
        }
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
            if (p_val < 0.001) {
                p_text <- "p < 0.001"
            } else {
                p_text <- paste("p =", format(round(p_val, 3), nsmall = 3))
            }
            if (!is.null(xlim)) {
                text_x <- xlim[2] * 0.7
            } else {
                text_x <- max(data$survivalMonths, na.rm = TRUE) * 0.7
            }
            text(text_x, 0.1, p_text, cex = 0.8)
        }
    }

    # Single variable survival plot
    output$survival_plot <- renderPlot({
        req(input$generate, input$analysis_type == "single")
        data <- strat_data()
        if (is.null(data) || nrow(data) == 0) {
            plot.new()
            text(0.5, 0.5, "No data available for the selected criteria.", cex = 1.5)
            return()
        }
        create_survival_plot(
            data = data,
            strat_var = "strat_group",
            title = paste("Survival by", input$strat_var),
            show_pvalue = input$show_pvalue,
            show_ci = input$show_ci
        )
    })

    # Multi-plot grid for different variables
    output$grid_plot <- renderPlot({
        req(input$generate, input$analysis_type == "grid")
        data <- filtered_data()
        if (nrow(data) == 0) {
            plot.new()
            text(0.5, 0.5, "No data available for the selected criteria.", cex = 1.5)
            return()
        }
        selected_vars <- input$grid_variables
        if (length(selected_vars) == 0) {
            plot.new()
            text(0.5, 0.5, "Please select at least one variable for the grid.", cex = 1.5)
            return()
        }
        n_plots <- length(selected_vars)
        if (n_plots <= 3) {
            n_cols <- n_plots
            n_rows <- 1
        } else if (n_plots <= 6) {
            n_cols <- 3
            n_rows <- 2
        } else {
            n_cols <- 3
            n_rows <- 3
        }
        par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
        for (i in 1:length(selected_vars)) {
            var <- selected_vars[i]
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
            create_survival_plot(
                data = data,
                strat_var = var,
                title = title,
                show_pvalue = input$show_pvalue,
                show_ci = input$show_ci,
                max_time = input$max_months
            )
        }
    })

    # Function to compute group statistics
    compute_group_stats <- function(data, strat_var) {
        group_var <- sym(strat_var)
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
            rename(Group = !!group_var)
    }

    # Function to compute median survival
    compute_median_survival <- function(data, strat_var) {
        group_var <- sym(strat_var)
        data %>%
            filter(!is.na(!!group_var)) %>%
            group_by(!!group_var) %>%
            summarise(
                n = n(),
                events = sum(mortality),
                `Median Survival` = if (sum(mortality) >= n() / 2) {
                    round(quantile(survivalMonths[mortality == 1], probs = 0.5, na.rm = TRUE), 1)
                } else {
                    NA
                }
            ) %>%
            mutate(
                `Median Status` = ifelse(is.na(`Median Survival`), "Not reached", paste0(`Median Survival`, " months"))
            ) %>%
            select(!!group_var, n, events, `Median Status`) %>%
            rename(Group = !!group_var, N = n, Events = events)
    }

    # Group statistics table
    output$group_stats <- renderTable({
        req(input$generate)
        if (input$analysis_type == "single") {
            data <- strat_data()
            strat_col <- "strat_group"
            return(compute_group_stats(data, strat_col))
        } else {
            data <- filtered_data()
            selected_vars <- input$grid_variables
            if (length(selected_vars) == 0) {
                return(data.frame(Group = "No variables selected", Count = NA))
            }
            stats_list <- lapply(selected_vars, function(var) {
                stats <- compute_group_stats(data, var)
                stats$Variable <- var
                return(stats)
            })
            return(do.call(rbind, stats_list))
        }
    })

    # Median survival table
    output$median_survival <- renderTable({
        req(input$generate)
        if (input$analysis_type == "single") {
            data <- strat_data()
            strat_col <- "strat_group"
            return(compute_median_survival(data, strat_col))
        } else {
            data <- filtered_data()
            selected_vars <- input$grid_variables
            if (length(selected_vars) == 0) {
                return(data.frame(Group = "No variables selected", `Median Survival` = NA))
            }
            median_list <- lapply(selected_vars, function(var) {
                median <- compute_median_survival(data, var)
                median$Variable <- var
                return(median)
            })
            return(do.call(rbind, median_list))
        }
    })

    # Data table
    output$survival_data <- renderDT({
        req(input$generate)
        if (input$analysis_type == "single") {
            data <- strat_data()
        } else {
            data <- filtered_data()
        }
        if (is.null(data) || nrow(data) == 0) {
            return(NULL)
        }
        datatable(
            data %>% select(survivalMonths, mortality, diagnosisFinal, diagnosisClass, sex, ageGroup, grade),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Download handler for plot
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
                        data = data,
                        strat_var = "strat_group",
                        title = paste("Survival by", input$strat_var),
                        show_pvalue = input$show_pvalue,
                        show_ci = input$show_ci
                    )
                }
            } else {
                # Grid mode
                data <- filtered_data()
                if (nrow(data) == 0) {
                    plot.new()
                    text(0.5, 0.5, "No data available for the selected criteria.", cex = 1.5)
                } else {
                    selected_vars <- input$grid_variables
                    if (length(selected_vars) == 0) {
                        plot.new()
                        text(0.5, 0.5, "Please select at least one variable for the grid.", cex = 1.5)
                    } else {
                        # Layout for PDF
                        n_plots <- length(selected_vars)
                        if (n_plots <= 2) {
                            n_cols <- n_plots
                            n_rows <- 1
                        } else if (n_plots <= 4) {
                            n_cols <- 2
                            n_rows <- 2
                        } else {
                            n_cols <- 3
                            n_rows <- ceiling(n_plots / 3)
                        }
                        par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
                        for (i in 1:length(selected_vars)) {
                            var <- selected_vars[i]
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
                            create_survival_plot(
                                data = data,
                                strat_var = var,
                                title = title,
                                show_pvalue = input$show_pvalue,
                                show_ci = input$show_ci,
                                max_time = input$max_months
                            )
                        }
                    }
                }
            }
            dev.off()
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
