# Server logic
server <- function(input, output, session) {
    observeEvent(input$beta_badge_clicked, {
        showNotification(
            "This app is in beta. Expect frequent updates and improvements as we continue development. Some features may be under construction.",
            type = "message",
            duration = 5
        )
    })

    # dark mode table headers
    observe({
        if (input$mode_toggle == "dark") {
            shinyjs::addClass(selector = "body", class = "dark-mode")
        } else {
            shinyjs::removeClass(selector = "body", class = "dark-mode")
        }
    })

    # Create a reactive value to store gene expression data
    gene_expression_store <- reactiveVal(NULL)

    # Load gene expression data when the mRNA Expression tab is selected
    observeEvent(input$navBar, {
        # Check if user selected the mRNA Expression tab and data not yet loaded
        if (input$navBar == "mRNA Expression Boxplots" && is.null(gene_expression_store())) {
            # Show a loading message
            showNotification("Loading gene expression data...",
                type = "message",
                duration = NULL,
                id = "loading_notification"
            )

            # Load the data
            load_result <- try({
                temp_env <- new.env()
                load("dat/geneExpressionData.RData", envir = temp_env)
                gene_expression_store(temp_env$geneExpressionData)
            })

            if (inherits(load_result, "try-error")) {
                showNotification("Error loading gene expression data",
                    type = "error",
                    duration = 5
                )
            } else {
                # Remove loading notification
                removeNotification(id = "loading_notification")
                showNotification("Gene expression data loaded successfully!",
                    type = "message",
                    duration = 3
                )
            }
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
    ## Function to prepare data for survival analysis **************************
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

    ## Update the filtered_data reactive expression *****************************
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
        required_columns <- c(
            "diagnosisFinal", "diagnosisClass", "sex", "ageGroup", "grade",
            "mutationIDH1/2", "mutationH3", "1p/19q-codel",
            "methylationMGMTpromoter", "amplificationMCYN"
        )
        missing_columns <- setdiff(required_columns, names(data))
        if (length(missing_columns) > 0) {
            showNotification(paste("Warning: Missing columns after filtering:", paste(missing_columns, collapse = ", ")), type = "error")
        }

        return(data)
    })

    ## Replace the existing strat_data reactive expression **********************
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

    ## Create survival plot **********************
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
        # Set larger margins for the plot
        par(mar = c(5, 5, 4, 2) + 1) # Bottom, left, top, right margins
        plot(surv_fit,
            col = colors,
            lwd = 2,
            xlab = "Time (Months)",
            ylab = "Survival Probability",
            main = title,
            mark.time = TRUE,
            xlim = xlim,
            cex.lab = 2, # Increase axis label size
            cex.main = 2, # Increase main title size
            cex.axis = 1.5, # Increase axis text size
            cex = 1.5 # Increase legend and other text elements
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
            cex = 1.3, # Increase legend text size
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

    ## Single group survival ******************************************************************
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

    ## Grid survival ************************************************************************
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
        # overrides the margins set in create_survival_plot for this output object only
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

    ## Table for group statistics ******************************************************************
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

    ## Median survival table ******************************************************************

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

    ## Abbreviated survival data table *****************************************
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

    ## Download survival plot *************************************************
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

    ## Primary dimensionality reduction plot ***************************************
    output$tsnePlot <- renderPlotly({
        data <- filteredData()
        # validate(need(nrow(data) > 0, "No data available to plot."))
        data$diagnosisClass <- factor(data$diagnosisClass)
        data$diagnosisFinal <- factor(data$diagnosisFinal)
        non_tumor_data <- data[data$diagnosisClass == "NON-TUMOR", ]
        tumor_data <- data[data$diagnosisClass != "NON-TUMOR", ]
        non_tumor_diagnoses <- levels(factor(non_tumor_data$diagnosisFinal))
        tumor_diagnoses <- levels(factor(tumor_data$diagnosisFinal))

        # Choose colors based on mode more concisely
        is_dark_mode <- input$mode_toggle == "dark"
        # Get non-tumor colors
        non_tumor_colors <- if (length(non_tumor_diagnoses) > 0) {
            gray_start <- if (is_dark_mode) 0.5 else 0.2
            gray_end <- if (is_dark_mode) 0.9 else 0.8
            setNames(
                gray.colors(length(non_tumor_diagnoses), start = gray_start, end = gray_end),
                non_tumor_diagnoses
            )
        } else {
            NULL
        }

        # Get tumor colors dynamically using our function
        tumor_color_mapping <- if (length(tumor_diagnoses) > 0) {
            tumor_colors <- get_color_palette(input$mode_toggle, length(tumor_diagnoses))
            setNames(tumor_colors, tumor_diagnoses)
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

        ## Update plot colors based on dark mode *************************************
        bg_color <- if (is_dark_mode) "black" else "white"
        text_color <- if (is_dark_mode) "white" else "black"
        grid_color <- if (is_dark_mode) "#444444" else "#dddddd"

        p <- layout(p,
            title = list(
                text = "t-SNE Dimensionality Reduction (interactive)",
                font = list(size = 26), # Increased title font size
                y = 0.95, # Position from top (0-1 range)
                pad = list(t = 20) # Add top padding
            ),
            margin = list(t = 80),
            xaxis = list(title = "t-SNE 1", color = text_color, gridcolor = grid_color),
            yaxis = list(title = "t-SNE 2", color = text_color, gridcolor = grid_color),
            plot_bgcolor = bg_color,
            paper_bgcolor = bg_color,
            font = list(color = text_color),
            dragmode = "select",
            legend = list(itemclick = "toggleothers", itemdoubleclick = FALSE),
            annotations = lapply(1:nrow(centroids), function(i) {
                list(
                    x = centroids$tsne1[i],
                    y = centroids$tsne2[i],
                    text = centroids$diagnosisFinal[i],
                    showarrow = FALSE,
                    font = list(size = 10, color = if (is_dark_mode) "gray90" else "gray10")
                )
            })
        )
        p <- event_register(p, "plotly_selected")
        return(p)
    })

    ## t-SNE acculumations and selections logic *************************************
    accumulated_selections <- reactiveVal(list())
    selection_counter <- reactiveVal(0)

    selected_points <- reactive({
        event_data("plotly_selected", source = "A")
    })

    ### Handle new selections ********************************************************
    observeEvent(selected_points(), {
        select_data <- selected_points()

        if (!is.null(select_data) && is.data.frame(select_data) && nrow(select_data) > 0 && "key" %in% names(select_data)) {
            # Get current accumulated selections
            current_selections <- accumulated_selections()

            # Increment selection counter
            new_count <- selection_counter() + 1
            selection_counter(new_count)

            # Generate a color that contrasts with background
            is_dark_mode <- input$mode_toggle == "dark"
            color_palette <- if (is_dark_mode) {
                c("#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
            } else {
                c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22")
            }
            selection_color <- color_palette[(new_count - 1) %% length(color_palette) + 1]

            # Add new selection with a default name
            new_selection <- list(
                id = paste0("Selection_", new_count),
                name = paste("Selection", new_count),
                keys = as.numeric(select_data$key),
                color = selection_color
            )

            # Add to accumulated selections
            current_selections <- c(current_selections, list(new_selection))
            accumulated_selections(current_selections)

            # Show naming modal
            showModal(modalDialog(
                title = "Name Your Selection",
                textInput("selectionName", "Selection Name:", paste("Selection", new_count)),
                footer = tagList(
                    actionButton("saveSelectionName", "Save", class = "btn-primary"),
                    modalButton("Cancel")
                )
            ))
        }
    })

    ### Save selection name *******************************************************************
    observeEvent(input$saveSelectionName, {
        if (nchar(input$selectionName) > 0) {
            current_selections <- accumulated_selections()
            if (length(current_selections) > 0) {
                # Update the name of the most recent selection
                current_selections[[length(current_selections)]]$name <- input$selectionName
                accumulated_selections(current_selections)
            }
        }
        removeModal()
    })

    ### Selection Manager UI ********************************************************************
    output$selectionManagerUI <- renderUI({
        selections <- accumulated_selections()
        is_dark_mode <- input$mode_toggle == "dark"

        if (length(selections) == 0) {
            return(div(
                class = if (is_dark_mode) "text-light" else "text-dark",
                "No selections yet. Select points on the plot to create a selection group."
            ))
        }

        # Create UI for each selection
        selection_items <- lapply(seq_along(selections), function(i) {
            selection <- selections[[i]]
            div(
                class = "selection-item mb-2 p-2 border rounded",
                style = paste0("border-left: 5px solid ", selection$color, " !important;"),
                div(
                    class = "d-flex justify-content-between align-items-center",
                    tags$strong(selection$name, class = "mr-2"),
                    span(paste0("(", length(selection$keys), " points)"), class = "text-muted"),
                    div(
                        actionButton(
                            inputId = paste0("renameSelection_", i),
                            label = icon("pencil-alt"),
                            class = "btn-sm btn-outline-secondary mr-1",
                            onclick = sprintf("Shiny.setInputValue('renameBtn', {id: %d, ts: Date.now()})", i)
                        ),
                        actionButton(
                            inputId = paste0("deleteSelection_", i),
                            label = icon("trash"),
                            class = "btn-sm btn-outline-danger",
                            onclick = sprintf("Shiny.setInputValue('deleteBtn', {id: %d, ts: Date.now()})", i)
                        )
                    )
                )
            )
        })
        # btn_class <- if(is_dark_mode) "btn-outline-light" else "btn-outline-dark"
        div(
            h4("Selection Groups", class = if (is_dark_mode) "text-light" else "text-dark"),
            div(class = "selection-list", selection_items),
            div(
                class = "mt-3",
                actionButton("clearSelections", "Clear All Selections",
                    class = "btn-warning"
                ), # btn_class),
                actionButton("compareSelections", "Compare Selections",
                    class = "btn-primary ml-2",
                    disabled = length(selections) < 2
                )
            )
        )
    })
    ### JavaScript renames events **********************************************
    observeEvent(input$renameBtn, {
        if (!is.null(input$renameBtn)) {
            i <- input$renameBtn$id
            selections <- accumulated_selections()

            if (i <= length(selections)) {
                showModal(modalDialog(
                    title = "Rename Selection",
                    textInput(
                        "newSelectionName", "New Name:",
                        selections[[i]]$name
                    ),
                    footer = tagList(
                        actionButton("saveNewName", "Save",
                            class = "btn-primary",
                            onclick = sprintf("Shiny.setInputValue('saveRename', {id: %d, ts: Date.now()})", i)
                        ),
                        modalButton("Cancel")
                    ),
                    easyClose = TRUE
                ))
            }
        }
    })

    ### Handle save rename events **********************************************
    observeEvent(input$saveRename, {
        if (!is.null(input$saveRename) && nchar(input$newSelectionName) > 0) {
            i <- input$saveRename$id
            current_selections <- accumulated_selections()
            if (i <= length(current_selections)) {
                current_selections[[i]]$name <- input$newSelectionName
                accumulated_selections(current_selections)
            }
        }
        removeModal()
    })

    ### Handle delete events *************************************************
    observeEvent(input$deleteBtn, {
        if (!is.null(input$deleteBtn)) {
            i <- input$deleteBtn$id
            current_selections <- accumulated_selections()
            if (i <= length(current_selections)) {
                accumulated_selections(current_selections[-i])
            }
        }
    })

    ### Clear all selections **************************************************
    observeEvent(input$clearSelections, {
        accumulated_selections(list())
        selection_counter(0)
    })

    ### Compare selections ***************************************************
    observeEvent(input$compareSelections, {
        selections <- accumulated_selections()
        if (length(selections) >= 2) {
            showModal(modalDialog(
                title = "Compare Selections",
                size = "xl",
                easyClose = TRUE,
                selectInput("compareVariable", "Compare by variable:",
                    choices = setdiff(names(atlasDataClean), c("key", "tsne1", "tsne2", "age", "sampleID", "filename", "dataID")) |> sort(),
                    selected = "diagnosisFinal"
                ),
                checkboxGroupInput("selectionsToCompare", "Selections to compare:",
                    choices = sapply(selections, function(s) s$name),
                    selected = sapply(selections, function(s) s$name)
                ),
                plotOutput("comparisonPlot", height = "800px"),
                tableOutput("comparisonTable"),
                footer = modalButton("Close")
            ))
        }
    })

    ### Comparison plot ***************************************************
    output$comparisonPlot <- renderPlot({
        req(input$compareVariable, input$selectionsToCompare)
        selections <- accumulated_selections()
        selected_names <- input$selectionsToCompare

        if (length(selected_names) < 1) {
            return(NULL)
        }

        # Prepare data
        plot_data <- NULL
        for (name in selected_names) {
            selection_idx <- which(sapply(selections, function(s) s$name == name))
            if (length(selection_idx) == 1) {
                selection <- selections[[selection_idx]]
                selected_data <- atlasDataClean[atlasDataClean$key %in% selection$keys, ]
                selected_data$selection_group <- selection$name
                plot_data <- rbind(plot_data, selected_data)
            }
        }

        if (is.null(plot_data) || nrow(plot_data) == 0) {
            return(NULL)
        }

        var_name <- input$compareVariable

        # Define a base font size that works well across platforms
        base_size <- 16

        # Check variable type and create appropriate plot
        if (is.numeric(plot_data[[var_name]])) {
            # For numeric variables - boxplot
            ggplot(plot_data, aes(x = selection_group, y = .data[[var_name]], fill = selection_group)) +
                geom_boxplot() +
                labs(
                    title = paste("Comparison of", var_name, "across selection groups"),
                    x = "Selection Group", y = var_name
                ) +
                theme_minimal(base_size = base_size) +
                theme(
                    legend.position = "none",
                    plot.title = element_text(size = base_size * 1.2, face = "bold"),
                    axis.title = element_text(size = base_size * 1.1),
                    axis.text = element_text(size = base_size),
                    plot.margin = margin(15, 15, 15, 15) # Added margin: top, right, bottom, left

                )
        } else {
            # For categorical variables - bar chart
            ggplot(plot_data, aes(x = .data[[var_name]], fill = selection_group)) +
                geom_bar(position = "dodge") +
                labs(
                    title = paste("Comparison of", var_name, "distributions"),
                    x = var_name, y = "Count", fill = "Selection Group"
                ) +
                theme_minimal(base_size = base_size) +
                theme(
                    plot.title = element_text(size = base_size * 1.2, face = "bold"),
                    axis.title = element_text(size = base_size * 1.1),
                    axis.text = element_text(size = base_size),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = base_size),
                    legend.title = element_text(size = base_size),
                    legend.text = element_text(size = base_size * 0.9),
                    plot.margin = margin(15, 15, 15, 15) # Added margin: top, right, bottom, left
                )
        }
    })

    ### Comparison table **************************************************************
    output$comparisonTable <- renderTable({
        req(input$compareVariable, input$selectionsToCompare)
        selections <- accumulated_selections()
        selected_names <- input$selectionsToCompare

        if (length(selected_names) < 1) {
            return(NULL)
        }

        # Create summary stats
        summary_data <- data.frame(
            Selection = character(),
            Count = integer(),
            stringsAsFactors = FALSE
        )

        for (name in selected_names) {
            selection_idx <- which(sapply(selections, function(s) s$name == name))
            if (length(selection_idx) == 1) {
                selection <- selections[[selection_idx]]
                selected_data <- atlasDataClean[atlasDataClean$key %in% selection$keys, ]

                if (is.numeric(selected_data[[input$compareVariable]])) {
                    # For numeric variables
                    stats <- data.frame(
                        Selection = selection$name,
                        Count = nrow(selected_data),
                        Mean = round(mean(selected_data[[input$compareVariable]], na.rm = TRUE), 2),
                        Median = round(median(selected_data[[input$compareVariable]], na.rm = TRUE), 2),
                        SD = round(sd(selected_data[[input$compareVariable]], na.rm = TRUE), 2),
                        Min = min(selected_data[[input$compareVariable]], na.rm = TRUE),
                        Max = max(selected_data[[input$compareVariable]], na.rm = TRUE)
                    )
                    summary_data <- if (nrow(summary_data) == 0) stats else rbind(summary_data, stats)
                } else {
                    # For categorical variables, get the top categories
                    freq_table <- table(selected_data[[input$compareVariable]])
                    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:min(3, length(freq_table))])

                    stats <- data.frame(
                        Selection = selection$name,
                        Count = nrow(selected_data),
                        TopCategories = paste(top_categories, collapse = ", "),
                        TopCategoryCounts = paste(freq_table[top_categories], collapse = ", ")
                    )
                    summary_data <- if (nrow(summary_data) == 0) stats else rbind(summary_data, stats)
                }
            }
        }
        summary_data
    })
    ### Modified data table to show selections ******************************************************************
    output$dataTable <- DT::renderDataTable({
        selections <- accumulated_selections()

        if (length(selections) == 0) {
            DT::datatable(
                data.frame(Message = "Please select at least one data point in the t-SNE plot."),
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE
            )
        } else {
            # Combine all selected keys with selection info
            all_selected_data <- data.frame()
            for (selection in selections) {
                if (length(selection$keys) > 0) {
                    selection_data <- atlasDataClean[atlasDataClean$key %in% selection$keys, ]
                    selection_data$selection_group <- selection$name
                    all_selected_data <- rbind(all_selected_data, selection_data)
                }
            }

            if (nrow(all_selected_data) == 0) {
                DT::datatable(
                    data.frame(Message = "No valid points selected."),
                    options = list(pageLength = 5, scrollX = TRUE),
                    rownames = FALSE
                )
            } else {
                dt <- DT::datatable(
                    all_selected_data,
                    options = list(
                        pageLength = 5,
                        scrollX = TRUE,
                        columnDefs = list(list(targets = "selection_group", searchable = TRUE))
                    ),
                    rownames = FALSE
                )

                # Create style mapping for each selection group
                color_mapping <- sapply(selections, function(s) s$color)
                names(color_mapping) <- sapply(selections, function(s) s$name)

                # Apply the styling
                dt <- DT::formatStyle(
                    dt,
                    "selection_group",
                    backgroundColor = DT::styleEqual(names(color_mapping), color_mapping)
                )
                dt
            }
        }
    })
    # mRNA Expression Boxplots Logic ************************************************************************************************************
    gene_annotations <- gene_annotations %>% filter(!is.na(ENTREZID))
    # all_valid_genes <- gene_annotations %>% # currently loaded with annotations.RData at launch
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
    ## Select search mode: GO or Gene ****************
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
    ## mRNA selectors for UI *************************
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
                    "histologyOriginal", "diagnosisClass", "diagnosis"
                )),
                selected = "sex"
            )
        }
    })
    ## Final data for mRNA selection ****************************************************************
    final_data <- eventReactive(input$run, {
        chosen_genes <- selected_genes_now()
        if (length(chosen_genes) == 0) {
            shinyalert("Oops!", "You must select at least one gene to run the plot.",
                type = "error",
                confirmButtonCol = "#0B3B60", size = "m"
            )
            return(NULL)
        }

        # Get the gene expression data from our reactive store
        geneExpressionData <- gene_expression_store()

        # Check if data is available
        if (is.null(geneExpressionData)) {
            shinyalert("Data Not Loaded", "The gene expression data is not yet loaded. Please wait or try reloading the page.",
                type = "warning",
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

    # Contact form logic ****************************************************************************************

    status_type <- reactiveVal("ready")
    status_message <- reactiveVal("Ready to submit")

    output$status <- renderUI({
        div(
            class = "status-message",
            style = paste0(
                "padding: 15px; border-radius: 8px; ",
                "border-left: 5px solid ", if (grepl("Error", status_message())) "#f44336" else "#4caf50", "; ",
                "margin-top: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);"
            ),
            tags$div(
                style = "display: flex; align-items: center;",
                tags$i(
                    class = if (grepl("Error", status_message())) "fa fa-exclamation-circle" else "fa fa-check-circle",
                    style = paste0(
                        "margin-right: 10px; font-size: 24px; color: ",
                        if (grepl("Error", status_message())) "#f44336" else "#4caf50"
                    )
                ),
                h3(style = "margin: 0; font-weight: 500;", status_message())
            )
        )
    })
    ## Form Validation and Submission ***************************************************************
    observeEvent(input$submit, {
        # Validate inputs
        if (input$name == "" || input$email == "" || input$message == "") {
            showNotification(
                ui = div(
                    tags$b("Error:"),
                    "Please fill in all required fields."
                ),
                type = "error",
                duration = 5
            )
            status_type("error")
            status_message("Error: All fields are required!")
            return()
        }

        # Validate name for safe characters
        if (!grepl("^[A-Za-z0-9 '-]+$", input$name)) {
            showNotification(
                ui = div(
                    tags$b("Error:"),
                    "Name contains invalid characters. Only letters, numbers, spaces, apostrophes, and hyphens are allowed."
                ),
                type = "error",
                duration = 5
            )
            status_type("error")
            status_message("Error: Invalid characters in name!")
            return()
        }

        # Validate message isn't empty or only whitespace
        if (nchar(trimws(input$message)) == 0) {
            showNotification(
                ui = div(
                    tags$b("Error:"),
                    "Message cannot be empty or only whitespace."
                ),
                type = "error",
                duration = 5
            )
            status_type("error")
            status_message("Error: Message cannot be empty!")
            return()
        }

        status_type("ready")
        status_message("Processing submission...")

        # Get current date in NYC timezone
        current_date <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%Y-%m-%d")

        # Escape input$message for JSON and GraphQL
        escaped_message <- gsub('(["\\])', "\\\\\\1", input$message) # Escape quotes and backslashes
        escaped_message <- gsub("\n", "\\n", escaped_message) # Explicitly escape newlines
        escaped_message <- gsub("\t", "\\t", escaped_message) # Explicitly escape tabs
        cat("Escaped message:", escaped_message, "\n")

        column_values <- paste0(
            "{",
            '"text_mkq6vaar": "', current_date, '",',
            '"text_mkq6awc2": "', escaped_message, '",',
            '"text_mkq6cbxg": "', input$email, '"',
            "}"
        )

        # Log column_values for debugging
        cat("Column values:", column_values, "\n")

                query <- paste0("mutation {
            create_item (
            board_id: ", board_id, ',
            item_name: "', input$name, '",
            column_values: "', gsub('"', '\\\\"', column_values), '"
            ) {
            id
            }
        }')

        # Log query for debugging
        cat("Query:", query, "\n")

        # Send API request with error handling
        response <- tryCatch(
            {
                POST(
                    url = "https://api.monday.com/v2",
                    add_headers("Authorization" = api_token, "Content-Type" = "application/json"),
                    body = list(query = query),
                    encode = "json"
                )
            },
            error = function(e) {
                showNotification(
                    ui = div(
                        tags$b("Error:"),
                        "Network issue connecting to Monday.com"
                    ),
                    type = "error",
                    duration = 5
                )
                status_type("error")
                status_message("Error: Network issue connecting to Monday.com")
                return(NULL)
            }
        )

        if (is.null(response)) {
            return()
        }

        # Parse response
        response_body <- content(response, "parsed")
        cat("Response:", toJSON(response_body, pretty = TRUE), "\n")

        if (status_code(response) == 200 && !is.null(response_body$data$create_item$id)) {
            item_id <- response_body$data$create_item$id
            showNotification(
                ui = div(
                    tags$b("Success!"),
                    paste("Your message has been submitted. Item ID:", item_id)
                ),
                type = "message",
                duration = 5
            )
            updateTextInput(session, "message", value = "")
            updateTextInput(session, "name", value = "")
            updateTextInput(session, "email", value = "")
            status_type("success")
            status_message(paste("Successfully submitted! Item ID:", item_id))
        } else {
            error_msg <- if (!is.null(response_body$errors)) toJSON(response_body$errors, pretty = TRUE) else "Unknown error"
            showNotification(
                ui = div(
                    tags$b("Error!"),
                    paste("Status code:", status_code(response), "Details:", error_msg)
                ),
                type = "error",
                duration = 5
            )
            status_type("error")
            status_message(paste("Error submitting. Status code:", status_code(response), "Details:", error_msg))
        }
    })

    observeEvent(input$clear_form, {
        updateTextInput(session, "message", value = "")
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "email", value = "")
    })
} # end of server function
