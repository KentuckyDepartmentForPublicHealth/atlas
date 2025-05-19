# Survival Analysis Explorer

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
        load("atlasDataClean.RData")
        atlasDataCleanSurvival <- atlasDataClean %>% filter(!is.na(survivalMonths) & survivalMonths > 0)
        message("Data loaded successfully. ", nrow(atlasDataCleanSurvival), " rows available for analysis.")
    },
    error = function(e) {
        message("Error loading data: ", e$message)
    }
)

# Extract unique diagnoses from the actual data
diagnosis_list <- sort(unique(atlasDataCleanSurvival$diagnosisFinal))
diagnosis_list <- diagnosis_list[!is.na(diagnosis_list)]
diagnosis_choices <- c("All Diagnoses" = "all", setNames(diagnosis_list, diagnosis_list))

# Define UI
ui <- fluidPage(
    titlePanel("Survival Analysis Explorer"),
    sidebarLayout(
        sidebarPanel(
            # Driver selector for diagnosis - populated from actual data
            selectInput("diagnosisFilter", "Filter by Diagnosis:",
                choices = diagnosis_choices,
                selected = "all"
            ),

            # Grouping variable selector
            selectInput("groups", "Group By:",
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

            # Button to run analysis
            actionButton("runAnalysis", "Run Kaplan-Meier Analysis",
                class = "btn-primary"
            )
        ),
        mainPanel(
            # Output
            plotOutput("survivalPlot", height = "600px"),
            hr(),
            verbatimTextOutput("groupDetails")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Reactive value to store the analysis trigger
    rv <- reactiveValues(run = FALSE)

    # Update rv$run when the button is clicked
    observeEvent(input$runAnalysis, {
        rv$run <- TRUE
    })

    # Filter data based on selected diagnosis
    filteredData <- reactive({
        data <- atlasDataCleanSurvival

        # If a specific diagnosis is selected (not "all")
        if (input$diagnosisFilter != "all") {
            data <- data %>%
                filter(diagnosisFinal == input$diagnosisFilter)
        }

        return(data)
    })

    # Prepare data for plotting
    plotData <- reactive({
        # Get filtered data
        data <- filteredData()

        # Ensure survival data is present
        data <- data %>% filter(!is.na(survivalMonths), !is.na(mortality))

        # Convert the selected grouping variable to factor
        group_var <- input$groups
        data[[group_var]] <- as.factor(ifelse(is.na(data[[group_var]]), "Unknown", data[[group_var]]))

        return(data)
    })

    # Render survival plot when button is clicked
    output$survivalPlot <- renderPlot({
        # Only run if the button has been clicked
        if (!rv$run) {
            plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
            text(1, 1, "Click 'Run Kaplan-Meier Analysis' to generate the plot", cex = 1.5)
            return()
        }

        # Get prepared data
        data <- plotData()

        # Check if data is empty
        if (nrow(data) == 0) {
            plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
            text(1, 1, "No data available for selected diagnosis", cex = 1.5)
            return()
        }

        # Define survival fit with the selected grouping variable
        formula <- as.formula(paste("Surv(survivalMonths, mortality) ~", input$groups))
        fit <- survfit(formula, data = data)

        # Check number of groups for color palette
        group_count <- length(levels(data[[input$groups]]))

        # Generate Kaplan-Meier plot with survminer
        p <- ggsurvplot(
            fit,
            data = data,
            risk.table = input$riskTable,
            risk.table.pos = "out",
            censor = input$censorMarks,
            pval = if (input$pairwise) TRUE else FALSE,
            pval.method = if (input$pairwise) TRUE else FALSE,
            conf.int = FALSE,
            xlab = "Time (Months)",
            ylab = "Survival Probability",
            title = paste("Kaplan-Meier Survival Curves by", names(which(c(
                "Diagnosis" = "diagnosisFinal",
                "Grade" = "grade",
                "IDH Mutation" = "mutationIDH1/2",
                "Age Group" = "ageGroup",
                "Sex" = "sex"
            ) == input$groups))),
            # Handle color palette correctly for many groups
            palette = if (group_count > 10) {
                colorRampPalette(brewer.pal(min(9, group_count), "Set1"))(group_count)
            } else {
                "jco"
            },
            legend = "right",
            legend.title = names(which(c(
                "Diagnosis" = "diagnosisFinal",
                "Grade" = "grade",
                "IDH Mutation" = "mutationIDH1/2",
                "Age Group" = "ageGroup",
                "Sex" = "sex"
            ) == input$groups)),
            ggtheme = theme_minimal(),
            break.time.by = 12,
            risk.table.y.text.col = TRUE,
            surv.median.line = "hv"
        )

        return(p)
    })

    # Display group details
    output$groupDetails <- renderPrint({
        if (!rv$run) {
            return("Run the analysis to see group details")
        }

        data <- plotData()
        group_var <- input$groups

        # Display filtered diagnosis information
        if (input$diagnosisFilter != "all") {
            cat("Filtered diagnosis: ", input$diagnosisFilter, "\n")
            cat("Total patients: ", nrow(data), "\n\n")
        }

        # Display group distribution
        cat("Group Distribution for", names(which(c(
            "Diagnosis" = "diagnosisFinal",
            "Grade" = "grade",
            "IDH Mutation" = "mutationIDH1/2",
            "Age Group" = "ageGroup",
            "Sex" = "sex"
        ) == group_var)), ":\n")

        # Print table of groups
        print(table(data[[group_var]], useNA = "ifany"))

        # Print survival events data
        cat("\nEvents (deaths):", sum(data$mortality), "\n")
        cat("Censored observations:", nrow(data) - sum(data$mortality), "\n")
    })
}

# Run the application
shinyApp(ui = ui, server = server)