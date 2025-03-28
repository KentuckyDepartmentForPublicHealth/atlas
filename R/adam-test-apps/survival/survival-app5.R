# Survival Analysis Explorer App

# Load required libraries
library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Load dataset with error handling
tryCatch(
    {
        load("/home/adam/Sandbox/Shiny/atlas/dat/atlasDataClean.RData")
        atlasDataCleanSurvival <- atlasDataClean %>% filter(!is.na(survivalMonths) & survivalMonths > 0)
        message("Data loaded successfully. ", nrow(atlasDataCleanSurvival), " rows available for analysis.")
    },
    error = function(e) {
        message("Error loading data: ", e$message)
    }
)

# Define UI
ui <- fluidPage(
    titlePanel("Survival Analysis Explorer"),
    sidebarLayout(
        sidebarPanel(
            # Group selection dropdown
            selectInput("groups", "Select Group Variable:",
                choices = c(
                    "Diagnosis" = "diagnosisFinal",
                    "Grade" = "grade",
                    "IDH Mutation" = "mutationIDH1/2",
                    "Age Group" = "ageGroup",
                    "Sex" = "sex"
                ),
                selected = "sex"
            ),

            # Plot customization options
            checkboxInput("riskTable", "Show Risk Table", value = FALSE),
            checkboxInput("censorMarks", "Show Censoring Marks", value = TRUE),
            checkboxInput("pairwise", "Show Pairwise Comparisons", value = FALSE),
            checkboxInput("confInt", "Show Confidence Intervals", value = FALSE),

            # Additional options
            numericInput("minGroupSize", "Minimum Group Size",
                value = 1, min = 1, max = 50
            ),

            # Filter for diagnosis (visible only when diagnosis is selected)
            conditionalPanel(
                condition = "input.groups == 'diagnosisFinal'",
                checkboxInput("combineRare", "Combine Rare Diagnoses (<5 cases)", value = TRUE)
            ),

            # Action button to trigger analysis
            actionButton("runAnalysis", "Run Kaplan-Meier Analysis",
                class = "btn-primary", icon = icon("chart-line")
            )
        ),
        mainPanel(
            # Tabset panel for plot and diagnostics
            tabsetPanel(
                tabPanel(
                    "Survival Plot",
                    plotOutput("survivalPlot", height = "600px"),
                    textOutput("groupWarning")
                ),
                tabPanel("Group Details", verbatimTextOutput("groupDetails")),
                tabPanel("Statistics", verbatimTextOutput("survStats")),
                tabPanel(
                    "About"
                    # includeMarkdown("about.md")
                )
            )
        )
    )
)

# Create an about.md file with information about the app
writeLines(
    "## Survival Analysis Explorer

This app allows you to analyze survival data using Kaplan-Meier curves.

### Key Features:
- Stratify survival by different grouping variables
- View risk tables and survival statistics
- Perform pairwise comparisons between groups
- Visualize confidence intervals

### Interpreting the Plot:
- **Solid lines**: Survival probability curves for each group
- **Vertical tick marks**: Censored observations
- **Dashed vertical lines**: Median survival times
- **Risk table**: Number of patients at risk at each time point

### Notes:
- Groups with very few observations may produce unreliable estimates
- The log-rank test evaluates whether there is a difference in survival between groups
- Pairwise comparisons may not be valid with many small groups

For questions or support, contact the developer.
", "about.md"
)

# Define server logic
server <- function(input, output, session) {
    # Create mapping for display names to actual column names
    var_mapping <- list(
        "diagnosisFinal" = "diagnosisFinal",
        "grade" = "grade",
        "mutationIDH1/2" = "mutationIDH1/2",
        "ageGroup" = "ageGroup",
        "sex" = "sex"
    )

    # Create reverse mapping for display labels
    label_mapping <- c(
        "diagnosisFinal" = "Diagnosis",
        "grade" = "Grade",
        "mutationIDH1/2" = "IDH Mutation",
        "ageGroup" = "Age Group",
        "sex" = "Sex"
    )

    # Reactive value to store the analysis trigger
    rv <- reactiveValues(run = FALSE)

    # Update rv$run when the button is clicked
    observeEvent(input$runAnalysis, {
        rv$run <- TRUE
    })

    # Prepare the data with proper handling of variables
    plotData <- reactive({
        # Start with the base survival dataset
        data <- atlasDataCleanSurvival

        # Ensure survival data is present
        data <- data %>%
            filter(!is.na(survivalMonths), !is.na(mortality), survivalMonths > 0)

        # Get the actual column name based on the selected value
        selected_var <- input$groups
        actual_column <- var_mapping[[selected_var]]

        # Create a new column for the grouping variable with proper handling
        data$groupVar <- ifelse(is.na(data[[actual_column]]),
            "Unknown",
            as.character(data[[actual_column]])
        )

        # Apply minimum group size filtering
        if (input$minGroupSize > 1) {
            group_counts <- table(data$groupVar)
            small_groups <- names(group_counts[group_counts < input$minGroupSize])

            if (length(small_groups) > 0) {
                if (input$combineRare && selected_var == "diagnosisFinal") {
                    # For diagnosis, combine rare groups
                    data$groupVar <- ifelse(data$groupVar %in% small_groups,
                        "Other Rare Types",
                        data$groupVar
                    )
                } else {
                    # Otherwise filter them out
                    data <- data %>% filter(!(groupVar %in% small_groups))
                }
            }
        }

        # Convert to factor to preserve all levels
        data$groupVar <- factor(data$groupVar)

        return(data)
    })

    # Render the survival plot
    output$survivalPlot <- renderPlot({
        # Only run if the button has been clicked
        if (!rv$run) {
            plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
            text(1, 1, "Click 'Run Kaplan-Meier Analysis' to generate the plot", cex = 1.5)
            return()
        }

        # Get the prepared data
        data <- plotData()

        # Check if data is empty
        if (nrow(data) == 0) {
            plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
            text(1, 1, "No data available for selected criteria", cex = 1.5)
            return()
        }

        # Get the display label for the selected variable
        selected_var <- input$groups
        display_label <- label_mapping[selected_var]

        # Create the survival fit
        fit <- survfit(Surv(survivalMonths, mortality) ~ groupVar, data = data)

        # Check number of groups for color palette
        group_count <- length(levels(data$groupVar))

        # Choose appropriate color palette based on group count
        if (group_count > 10) {
            color_palette <- colorRampPalette(brewer.pal(min(9, group_count), "Set1"))(group_count)
        } else {
            color_palette <- "jco"
        }

        # Create the Kaplan-Meier plot
        p <- ggsurvplot(
            fit,
            data = data,
            risk.table = input$riskTable,
            risk.table.pos = "out",
            tables.height = 0.3,
            censor = input$censorMarks,
            pval = input$pairwise,
            pval.method = input$pairwise,
            conf.int = input$confInt,
            xlab = "Time (Months)",
            ylab = "Survival Probability",
            title = paste("Kaplan-Meier Survival Curves by", display_label),
            palette = color_palette,
            legend = if (group_count > 15) "none" else "right",
            legend.title = display_label,
            ggtheme = theme_bw(),
            break.time.by = 12,
            risk.table.y.text.col = TRUE,
            surv.median.line = "none", # h hv v
            fontsize = 4 # Smaller font for risk table with many groups
        )

        # Add note if legend is hidden due to too many groups
        if (group_count > 15 && !input$riskTable) {
            p$plot <- p$plot +
                annotate("text",
                    x = max(data$survivalMonths, na.rm = TRUE) / 2,
                    y = 0.1, label = "Too many groups to display legend",
                    size = 5, color = "darkred"
                )
        }

        # Return the plot
        return(p)
    })

    # Warning message for small groups
    output$groupWarning <- renderText({
        if (!rv$run) {
            return(NULL)
        }

        data <- plotData()
        group_counts <- table(data$groupVar)
        small_groups <- names(group_counts[group_counts < 5])

        if (length(small_groups) > 0) {
            return(paste(
                "Warning: The following groups have fewer than 5 observations,",
                "which may lead to unreliable estimates:",
                paste(small_groups, collapse = ", ")
            ))
        } else {
            return(NULL)
        }
    })

    # Display group details
    output$groupDetails <- renderPrint({
        if (!rv$run) {
            return("Run the analysis to see group details")
        }

        data <- plotData()

        # Print group distribution
        cat("Group Distribution:\n")
        group_table <- table(data$groupVar, useNA = "ifany")
        group_df <- data.frame(
            Group = names(group_table),
            Count = as.numeric(group_table),
            Percentage = sprintf("%.1f%%", 100 * as.numeric(group_table) / sum(group_table))
        )
        print(group_df, row.names = FALSE)

        # Print sample size info
        cat("\nTotal sample size:", nrow(data), "patients\n")

        # Print events info (mortality)
        events <- sum(data$mortality)
        cat("Total events (deaths):", events, "\n")
        cat("Censored observations:", nrow(data) - events, "\n")
        cat("Censoring rate:", sprintf("%.1f%%", 100 * (nrow(data) - events) / nrow(data)), "\n")
    })

    # Display survival statistics
    output$survStats <- renderPrint({
        # Only show if analysis has been run
        if (!rv$run) {
            return("Run the analysis to see survival statistics")
        }

        data <- plotData()

        # Skip if no data
        if (nrow(data) == 0) {
            return("No data available for selected criteria")
        }

        fit <- survfit(Surv(survivalMonths, mortality) ~ groupVar, data = data)

        # Print summary statistics
        cat("Survival Statistics:\n\n")

        # Calculate and print median survival times
        median_surv <- summary(fit)$table[, "median"]
        median_df <- data.frame(
            Group = names(median_surv),
            "Median Survival (Months)" = median_surv
        )
        print(median_df, row.names = FALSE)

        # Print log-rank test if pairwise comparison is requested
        if (input$pairwise) {
            cat("\nLog-Rank Test:\n")
            print(survdiff(Surv(survivalMonths, mortality) ~ groupVar, data = data))

            # Add interpretation
            cat("\nInterpretation:\n")
            lr_test <- survdiff(Surv(survivalMonths, mortality) ~ groupVar, data = data)
            p_val <- 1 - pchisq(lr_test$chisq, length(lr_test$n) - 1)

            if (p_val < 0.05) {
                cat("The log-rank test shows a statistically significant difference in survival\n")
                cat("between at least two groups (p-value =", format(p_val, digits = 3), ")\n")
            } else {
                cat("The log-rank test does not show a statistically significant difference\n")
                cat("in survival between the groups (p-value =", format(p_val, digits = 3), ")\n")
            }
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)