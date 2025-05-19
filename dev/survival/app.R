library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(shiny)
library(gt)
library(ggsurvfit)
library(plotly)
library(broom)
library(gtsummary)
library(cardx)

message("inside global.R")
# data
# ajb load
load("/home/adam/Sandbox/Shiny/atlas/dat/atlasDataClean.RData")

# windows load
# load("~/atlas/dat/atlasDataClean.RData")

# mac load
# load("~/Desktop/Atlas/Survival/atlas/dat/atlasDataClean.RData")


atlasDataClean <- subset(atlasDataClean, atlasDataClean$survivalMonths != "NA")

# fixing NAs so that they are properly reflected as censored observations
atlasDataClean$mortality <- ifelse(is.na(atlasDataClean$mortality), 0, atlasDataClean$mortality)

atlasDataClean$survivalMonths <- as.numeric(atlasDataClean$survivalMonths)


########### strata
allowed_vars <- c("ageGroup", "tumorType", "grade", "sex")

# Create a named vector for mapping strata variables to display names
strata_labels <- c(
    "Age Group" = "ageGroup",
    "Tumor Type" = "tumorType",
    "Sex" = "sex",
    "Grade" = "grade"
)


# ## setting ref level for each strata
# atlasDataClean$ageGroup <- relevel(factor(atlasDataClean$ageGroup), ref = "40-60YRS")
# atlasDataClean$tumorType <- relevel(factor(atlasDataClean$tumorType), ref = "1")
# atlasDataClean$grade <- relevel(factor(atlasDataClean$grade), ref = "1")
# atlasDataClean$sex <- relevel(factor(atlasDataClean$sex), ref = "M")


# new
## setting ref level for each strata
# Create a helper function to safely relevel factors
safe_relevel <- function(x, ref) {
    x_factor <- factor(x)
    if (ref %in% levels(x_factor)) {
        return(relevel(x_factor, ref = ref))
    } else {
        warning(paste(
            "Reference level", ref, "not found in factor. Available levels:",
            paste(levels(x_factor), collapse = ", ")
        ))
        return(x_factor)
    }
}

# Setting ref level for each strata with error handling - FIXED REFERENCE LEVELS
atlasDataClean$ageGroup <- safe_relevel(atlasDataClean$ageGroup, "40-60YRS")
atlasDataClean$tumorType <- safe_relevel(atlasDataClean$tumorType, "PRIMARY") # Changed from "1" to "PRIMARY"
atlasDataClean$grade <- safe_relevel(atlasDataClean$grade, "GRADE 1") # Changed from "1" to "GRADE 1"
atlasDataClean$sex <- safe_relevel(atlasDataClean$sex, "MALE") # Changed from "M" to "MALE"


unique_diagnosis <- c("DIFFUSE GLIOMA", "IDH MUTANT", "PFA", "MB-GP4", "NEUROBLASTOMA", "MENINGIOMA")
unique_histology <- c("GBM", "OD", "OA", "A", "EPN", "MB", "NB", "MEN")
# Option below simply filters out diagnoses with less than 30 patients, one possible approach
# valid_diagnoses <- names(table(atlasDataClean$diagnosisFinal)[table(atlasDataClean$diagnosisFinal) > 30])
# unique_diagnosis <- c("All", valid_diagnoses)

ui <- fluidPage(
        titlePanel("Atlas: Survival Analysis"),
        fluidRow(
            # Left column for all controls
            column(
                3,
                wellPanel(
                    style = "position: sticky; top: 20px;", # Makes sidebar sticky
                    selectInput(
                        inputId = "diagnosis",
                        label = "Select Diagnosis:",
                        choices = c("All", unique_diagnosis),
                        selected = "All"
                    ),
                    selectInput(
                        inputId = "histology",
                        label = "Select Histology:",
                        choices = c("All", unique_histology),
                        selected = "All"
                    ),
                    selectInput(
                        inputId = "Strata",
                        label = "Select strata:",
                        choices = strata_labels,
                        selected = strata_labels[1]
                    ),
                    checkboxInput("show_risk_table", "Show Risk Table", value = FALSE),
                    checkboxInput("show_censoring", "Show Censoring Marks", value = FALSE),
                    checkboxInput("show_pairwise", "Show Pairwise Comparisons", value = FALSE)
                )
            ),

            # Right column for all visualizations
            column(
                9,
                # Kaplan-Meier section
                div(
                    style = "margin-bottom: 40px;",
                    h3("Kaplan-Meier Analysis",
                        style = "color: #ffffff; margin-bottom: 20px;"
                    ),
                    plotlyOutput("kmplt", height = "500px"),
                    div(
                        style = "margin: 15px 0;",
                        downloadButton("download_km_plot", "Download KM Plot")
                    ),
                    verbatimTextOutput("log_rank_results"),
                    conditionalPanel(
                        condition = "input.show_risk_table == true",
                        div(
                            style = "margin-top: 20px;",
                            gt_output("risk_table")
                        )
                    )
                ),

                # Hazard Ratio section
                div(
                    style = "margin-top: 40px; padding-top: 20px; border-top: 1px solid #4d5785;",
                    h3("Hazard Ratio Analysis",
                        style = "color: #ffffff; margin-bottom: 20px;"
                    ),
                    plotlyOutput("hr_plot", height = "500px"),
                    div(
                        style = "margin: 15px 0;",
                        downloadButton("download_hr_plot", "Download HR Plot")
                    ),
                    conditionalPanel(
                        condition = "input.show_hr_table == true",
                        div(
                            style = "margin-top: 20px;",
                            gt_output("hazard_table")
                        )
                    )
                )
            )
        )
    )


#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



server <- function(input, output, session) {
    # package import



    # Separate reactive filtered data for KM tab
    filtered_dat_km <- reactive({
        data <- if (input$diagnosis == "All") {
            atlasDataClean
        } else {
            atlasDataClean %>%
                dplyr::filter(diagnosis == input$diagnosis)
        }

        if (input$histology != "All") {
            data <- data %>%
                dplyr::filter(histologyOriginal == input$histology)
        }

        data
    })

    # Separate reactive filtered data for HR tab
    filtered_dat_hr <- reactive({
        data <- if (input$diagnosis_hr == "All") {
            atlasDataClean
        } else {
            atlasDataClean %>%
                dplyr::filter(diagnosis == input$diagnosis_hr)
        }

        if (input$histology_hr != "All") {
            data <- data %>%
                dplyr::filter(histologyOriginal == input$histology_hr)
        }

        data
    })

    output$kmplt <- renderPlotly({
        data <- filtered_dat_km()
        req(nrow(data) > 0) # Ensure data is not empty

        if (nrow(data) < 30) {
            plot.new()
            text(0.5, 0.5, "Too few observations to display Kaplan-Meier curve.", cex = 0.9, col = "red")
            return()
        }

        data$event_status <- ifelse(is.na(data$mortality), 0, data$mortality)

        strata_var <- input$Strata
        data$strata_factor <- factor(data[[strata_var]])

        clean_labels <- levels(data$strata_factor)
        clean_labels <- gsub("strata_factor=", "", clean_labels)

        fit <- survfit(Surv(survivalMonths, event_status) ~ strata_factor, data = data)

        p <- ggsurvfit(fit) +
            labs(
                x = "Time (Months)",
                y = "Survival Probability",
                title = "Kaplan-Meier Plot"
            ) +
            scale_color_manual(
                name = strata_var,
                values = RColorBrewer::brewer.pal(length(clean_labels), "Set1"),
                labels = clean_labels
            ) +
            theme_minimal() +
            theme(
                panel.background    = element_rect(fill = "white", color = NA),
                plot.background     = element_rect(fill = "white", color = NA),
                panel.grid.major    = element_line(color = "grey"),
                panel.grid.minor    = element_line(color = "grey", linetype = "dotted"),
                axis.text           = element_text(color = "white", size = 14),
                axis.title          = element_text(color = "white"),
                plot.title          = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
                legend.background   = element_rect(fill = "white", color = NA),
                legend.text         = element_text(color = "white"),
                legend.title        = element_text(color = "white")
            )

        if (input$show_censoring) {
            p <- p + add_censor_mark(shape = 3, size = 3, color = "red")
        }

        plotly_obj <- ggplotly(p)

        get_n_at_risk <- function(timepoints, strata_level, data) {
            sapply(timepoints, function(t) {
                sum(data$strata_factor == strata_level & data$survivalMonths >= t, na.rm = TRUE)
            })
        }

        all_data_points <- plotly_obj$x$data

        for (i in seq_along(clean_labels)) {
            trace_data <- all_data_points[[i]]

            if (!(exists("line", where = trace_data) && !is.null(trace_data$x))) {
                next
            }

            time_points <- trace_data$x
            n_at_risk <- get_n_at_risk(time_points, levels(data$strata_factor)[i], data)
            survival_probs <- trace_data$y

            hover_text <- mapply(
                function(t, s, n) {
                    sprintf(
                        "Time: %.1f days<br>Survival: %.3f<br>At risk: %d",
                        t, s, n
                    )
                },
                time_points,
                survival_probs,
                n_at_risk,
                SIMPLIFY = TRUE
            )

            plotly_obj$x$data[[i]]$text <- hover_text
            plotly_obj$x$data[[i]]$hoverinfo <- "text"
            plotly_obj$x$data[[i]]$name <- clean_labels[i]
        }

        plotly_obj %>%
            layout(
                legend = list(
                    title = list(text = strata_var),
                    x = 1, y = 1,
                    traceorder = "normal",
                    font = list(size = 12)
                ),
                hovermode = "closest"
            ) %>%
            config(
                modeBarButtonsToRemove = c(
                    "pan2d", "select2d", "autoscale", "resetScale2d",
                    "toggleSpikelines", "hoverClosestCartesian",
                    "hoverCompareCartesian", "toImage"
                ),
                modeBarButtonsToAdd = c("zoom2d", "lasso2d"),
                displaylogo = FALSE
            )
    })

    # Separate Cox Models for each tab
    cox_model_km <- reactive({
        data <- filtered_dat_km()
        req(nrow(data) > 0)

        if (!is.numeric(data[[input$Strata]])) {
            data[[input$Strata]] <- as.factor(data[[input$Strata]])
        }

        coxph(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata)), data = data)
    })

    cox_model_hr <- reactive({
        data <- filtered_dat_km()
        req(nrow(data) > 0)

        # Ensure we have valid data for analysis
        if (length(unique(data[[input$Strata]])) < 2) {
            return(NULL)
        }

        tryCatch(
            {
                # Create formula for cox model
                formula <- as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata))
                coxph(formula, data = data)
            },
            error = function(e) {
                return(NULL)
            }
        )
    })

    output$risk_table <- render_gt({
        data <- filtered_dat_km()
        req(nrow(data) > 0)
        req(input$show_risk_table)

        strata_label <- switch(input$Strata,
            "diagnosis" = "Diagnosis",
            "histologyOriginal" = "Histology",
            "gender" = "Gender",
            "race" = "Race",
            input$Strata
        )

        data[[input$Strata]] <- gsub("input\\$Strata=", "", data[[input$Strata]])

        fit <- survfit(Surv(survivalMonths, mortality) ~ get(input$Strata), data = data)

        tbl <- gtsummary::tbl_survfit(
            fit,
            times = c(0, 12, 24, 36, 48, 60),
            label_header = "{time} Months"
        ) %>%
            modify_table_body(
                ~ .x %>%
                    dplyr::mutate(
                        label = gsub("input\\$Strata=", "", label)
                    )
            ) %>%
            modify_header(label = strata_label)

        tbl %>%
            gtsummary::as_gt() %>%
            gt::tab_header(title = "Risk Table") %>%
            gt::opt_row_striping() %>%
            gt::tab_options(
                table.font.size = "small",
                data_row.padding = gt::px(2)
            )
    })

    # Update the hazard table output to use the new input
    output$hazard_table <- render_gt({
        req(input$show_hr_table) # Only render when table is requested

        data <- filtered_dat_hr()
        cox_summary <- summary(cox_model_hr())

        hr_vals <- exp(cox_summary$coefficients[, "coef"])
        ci_vals <- exp(confint(cox_model_hr()))
        p_vals <- cox_summary$coefficients[, "Pr(>|z|)"]

        clean_labels <- gsub("^get\\(input\\$Strata_hr\\)", "", rownames(cox_summary$coefficients)) %>%
            gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%
            gsub("([0-9])([A-Za-z])", "\\1 \\2", .) %>%
            gsub("([A-Za-z])([0-9])", "\\1 \\2", .) %>%
            tools::toTitleCase()

        hr_table <- data.frame(
            "Group"         = clean_labels,
            "Hazard Ratio"  = round(hr_vals, 2),
            "95% CI Lower"  = round(ci_vals[, 1], 2),
            "95% CI Upper"  = round(ci_vals[, 2], 2),
            "P-Value"       = signif(p_vals, 3),
            check.names     = FALSE
        )

        gt(hr_table) %>%
            tab_header(
                title = md("**Hazard Ratios**"),
                subtitle = "Estimated from Cox Proportional Hazards Model"
            ) %>%
            fmt_number(
                columns = c("Hazard Ratio", "95% CI Lower", "95% CI Upper"),
                decimals = 2
            ) %>%
            cols_label(
                Group = "Strata Group",
                "Hazard Ratio" = "HR",
                "95% CI Lower" = "Lower CI",
                "95% CI Upper" = "Upper CI",
                "P-Value" = "P-Value"
            ) %>%
            tab_options(
                table.font.size = "14px",
                column_labels.font.weight = "bold"
            )
    })


    output$hr_plot <- renderPlotly({
        req(filtered_dat_km())
        model <- cox_model_hr()

        if (is.null(model)) {
            # Return an empty plot with message
            plot_ly() %>%
                add_annotations(
                    text = "Insufficient data for hazard ratio analysis.\nPlease ensure you have at least two groups for comparison.",
                    showarrow = FALSE,
                    font = list(color = "#ffffff", size = 14)
                ) %>%
                layout(
                    plot_bgcolor = "#353b5e",
                    paper_bgcolor = "#353b5e",
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                )
        } else {
            # Process model results
            hr_data <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
                dplyr::mutate(term = gsub("_", " ", term))

            # Create a reference level mapping
            ref_levels <- list(
                "ageGroup" = "40-60YRS",
                "tumorType" = "1",
                "grade" = "1",
                "sex" = "M"
            )

            # Get human-readable names for strata variables
            strata_names <- list(
                "ageGroup" = "Age Group",
                "tumorType" = "Tumor Type",
                "grade" = "Grade",
                "sex" = "Sex"
            )

            # Create a reference levels text
            ref_text <- paste(
                sapply(names(ref_levels), function(var) {
                    paste0(strata_names[[var]], " = ", ref_levels[[var]])
                }),
                collapse = ", "
            )

            p <- ggplot(hr_data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
                geom_pointrange(color = "lightblue", size = 1) +
                geom_hline(yintercept = 1, linetype = "dashed", color = "#D21F3C") +
                coord_flip() +
                labs(
                    title = "Hazard Ratios from Cox Model",
                    x = "Covariates",
                    y = "Hazard Ratio (95% CI)"
                ) +
                theme(
                    panel.background    = element_rect(fill = "white", color = NA),
                    plot.background     = element_rect(fill = "white", color = NA),
                    panel.grid.major    = element_line(color = "grey"),
                    panel.grid.minor    = element_line(color = "grey", linetype = "dotted"),
                    axis.text           = element_text(color = "white", size = 8),
                    axis.title          = element_text(color = "white"),
                    plot.title          = element_text(color = "black", face = "bold", size = 12),
                    legend.background   = element_rect(fill = "white", color = NA),
                    legend.text         = element_text(color = "white"),
                    legend.title        = element_text(color = "white")
                )

            ggplotly(p) %>%
                layout(
                    annotations = list(
                        list(
                            x = 0.5, # Shifted to the left (was 0.5)
                            y = -0.1, # Keeping the same vertical position
                            xref = "paper",
                            yref = "paper",
                            text = paste("Reference levels:", ref_text),
                            showarrow = FALSE,
                            font = list(size = 10, color = "white"),
                            bgcolor = "rgba(50, 50, 50, 0.5)",
                            bordercolor = "rgba(0, 0, 0, 0)",
                            borderwidth = 0,
                            align = "left", # Left alignment
                            width = 0.9,
                            borderpad = 4
                        )
                    ),
                    margin = list(b = 80),
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)"
                ) %>%
                config(
                    modeBarButtonsToRemove = c(
                        "pan2d", "select2d", "autoscale", "resetScale2d",
                        "toggleSpikelines", "hoverClosestCartesian",
                        "hoverCompareCartesian", "toImage"
                    ),
                    modeBarButtonsToAdd = c("zoom2d", "lasso2d"),
                    displaylogo = FALSE
                )
        }
    })

    output$log_rank_results <- renderText({
        data <- filtered_dat_km()
        req(nrow(data) > 0)

        # Basic log-rank test
        surv_diff <- survdiff(
            Surv(survivalMonths, mortality) ~ get(input$Strata),
            data = data
        )

        p_value <- 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)

        # Start with basic log-rank results
        result <- paste0(
            "Log-rank test results:\n",
            "Chi-square statistic: ", round(surv_diff$chisq, 2), "\n",
            "Degrees of freedom: ", length(surv_diff$n) - 1, "\n",
            "P-value: ", format.pval(p_value, digits = 3)
        )

        # Add pairwise comparisons if toggled
        if (input$show_pairwise) {
            tryCatch(
                {
                    # Create survival object once
                    surv_obj <- Surv(data$survivalMonths, data$mortality)
                    strata_factor <- factor(data[[input$Strata]])

                    # Perform pairwise comparisons
                    pairs <- combn(levels(strata_factor), 2)
                    n_pairs <- ncol(pairs)

                    if (n_pairs > 0) {
                        result <- paste0(result, "\n\nPairwise Log-rank Test Results:\n")

                        # Store all p-values first for BH adjustment
                        all_p_values <- numeric(n_pairs)
                        pair_results <- list()

                        # Calculate all raw p-values first
                        for (i in 1:n_pairs) {
                            pair_data <- data[strata_factor %in% pairs[, i], ]
                            pair_surv <- survdiff(
                                Surv(survivalMonths, mortality) ~ get(input$Strata),
                                data = pair_data
                            )
                            all_p_values[i] <- 1 - pchisq(pair_surv$chisq, df = 1)
                            pair_results[[i]] <- list(
                                group1 = pairs[1, i],
                                group2 = pairs[2, i]
                            )
                        }

                        # Apply Benjamini-Hochberg correction
                        adj_p_values <- p.adjust(all_p_values, method = "BH")

                        # Format and display results
                        for (i in 1:n_pairs) {
                            # Add significance stars
                            stars <- ""
                            if (adj_p_values[i] < 0.001) {
                                stars <- " ***"
                            } else if (adj_p_values[i] < 0.01) {
                                stars <- " **"
                            } else if (adj_p_values[i] < 0.05) stars <- " *"

                            result <- paste0(
                                result,
                                sprintf(
                                    "\n%s vs %s: p = %s%s",
                                    pair_results[[i]]$group1,
                                    pair_results[[i]]$group2,
                                    format.pval(adj_p_values[i], digits = 3),
                                    stars
                                )
                            )
                        }

                        # Update legend to reflect BH method
                        result <- paste0(
                            result,
                            "\n\nSignificance levels (Benjamini-Hochberg adjusted):",
                            "\n* p < 0.05",
                            "\n** p < 0.01",
                            "\n*** p < 0.001"
                        )
                    }
                },
                error = function(e) {
                    result <- paste0(result, "\n\nError in pairwise comparison: ", e$message)
                }
            )
        }

        result
    })
    # Add these at the end of your server function, before the final closing bracket

    output$download_km_plot <- downloadHandler(
        filename = function() {
            paste("kaplan_meier_plot_", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            data <- filtered_dat_km()

            # Recreate the KM plot (without plotly conversion)
            p <- ggsurvfit(
                survfit(Surv(survivalMonths, mortality) ~ get(input$Strata), data = data)
            ) +
                labs(
                    x = "Time (Months)",
                    y = "Survival Probability",
                    title = "Kaplan-Meier Plot"
                ) +
                theme_minimal()

            if (input$show_censoring) {
                p <- p + add_censor_mark(shape = 3, size = 3, color = "red")
            }

            # Save the plot
            ggsave(file, p, width = 10, height = 8)
        }
    )

    output$download_hr_plot <- downloadHandler(
        filename = function() {
            paste("hazard_ratio_plot_", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            # Recreate the HR plot (without plotly conversion)
            hr_data <- broom::tidy(cox_model_hr(), exponentiate = TRUE, conf.int = TRUE) %>%
                dplyr::mutate(term = gsub("_", " ", term))

            # Create a reference level mapping
            ref_levels <- list(
                "ageGroup" = "40-60YRS",
                "tumorType" = "1",
                "grade" = "1",
                "sex" = "M"
            )

            # Get human-readable names for strata variables
            strata_names <- list(
                "ageGroup" = "Age Group",
                "tumorType" = "Tumor Type",
                "grade" = "Grade",
                "sex" = "Sex"
            )

            # Create a reference levels text
            ref_text <- paste(
                sapply(names(ref_levels), function(var) {
                    paste0(strata_names[[var]], " = ", ref_levels[[var]])
                }),
                collapse = ", "
            )

            # Create the base plot with PLOTLY TITLE AND ANNOTATION
            p <- ggplot(hr_data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
                geom_pointrange(color = "blue", size = 1) +
                geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
                coord_flip() +
                labs(
                    title = "Hazard Ratios from Cox Model",
                    x = "Covariates",
                    y = "Hazard Ratio (95% CI)"
                ) +
                theme_minimal()

            # For the downloadable ggplot version, add the reference as a caption
            p <- p + labs(caption = paste("Reference levels:", ref_text))

            # Save the plot
            ggsave(file, p, width = 10, height = 8)
        }
    )
}
shinyApp(ui = ui, server = server)