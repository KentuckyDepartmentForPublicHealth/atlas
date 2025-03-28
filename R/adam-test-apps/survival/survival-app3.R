# app.R

# Load required libraries
library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

# Load your dataset (replace with your actual data loading method)
# atlasDataCleanSurvival <- read.csv("path/to/your/data.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
    titlePanel("Survival Analysis Explorer"),
    sidebarLayout(
        sidebarPanel(
            # Dynamic dropdown filters using uiOutput
            uiOutput("diagnosisUI"),
            uiOutput("gradeUI"),
            uiOutput("idhUI"),
            uiOutput("ageGroupUI"),
            uiOutput("sexUI"),

            # Grouping variable (static, as it defines plot stratification)
            selectInput("groupBy", "Group By",
                choices = c("None", "Diagnosis", "Grade", "IDH Mutation", "Age Group", "Sex"),
                selected = "None"
            ),

            # Toggles for plot customization
            checkboxInput("riskTable", "Show Risk Table", value = FALSE),
            checkboxInput("censorMarks", "Show Censoring Marks", value = TRUE),
            checkboxInput("pairwise", "Show Pairwise Comparisons", value = FALSE)
        ),
        mainPanel(
            # Output survival plot
            plotOutput("survivalPlot", height = "600px")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Reactive data filtering
    filteredData <- reactive({
        data <- atlasDataCleanSurvival

        # Apply filters based on current selections
        if (!is.null(input$diagnosis) && input$diagnosis != "All") {
            data <- data %>% filter(`diagnosisFinal` == input$diagnosis)
        }
        if (!is.null(input$grade) && input$grade != "All") {
            data <- data %>% filter(`grade` == input$grade)
        }
        if (!is.null(input$idh) && input$idh != "All") {
            if (input$idh == "Unknown/Not Tested") {
                data <- data %>% filter(is.na(`mutationIDH1/2`))
            } else {
                data <- data %>% filter(`mutationIDH1/2` == input$idh)
            }
        }
        if (!is.null(input$ageGroup) && input$ageGroup != "All") {
            data <- data %>% filter(`ageGroup` == input$ageGroup)
        }
        if (!is.null(input$sex) && input$sex != "All") {
            if (input$sex == "Unknown") {
                data <- data %>% filter(is.na(`sex`))
            } else {
                data <- data %>% filter(`sex` == input$sex)
            }
        }

        # Ensure survivalMonths and mortality are present (survivalMonths assumed from context)
        data <- data %>% filter(!is.na(`survivalMonths`), !is.na(`mortality`))

        return(data)
    })

    # Render dynamic UI for each dropdown
    output$diagnosisUI <- renderUI({
        data <- filteredData()
        choices <- c("All", unique(data$`diagnosisFinal`[!is.na(data$`diagnosisFinal`)]))
        selectInput("diagnosis", "Diagnosis", choices = choices, selected = "All")
    })

    output$gradeUI <- renderUI({
        data <- filteredData()
        choices <- c("All", unique(data$`grade`[!is.na(data$`grade`)]))
        selectInput("grade", "Grade", choices = choices, selected = "All")
    })

    output$idhUI <- renderUI({
        data <- filteredData()
        idh_options <- unique(data$`mutationIDH1/2`[!is.na(data$`mutationIDH1/2`)])
        choices <- c("All", idh_options, "Unknown/Not Tested")
        selectInput("idh", "IDH Mutation", choices = choices, selected = "All")
    })

    output$ageGroupUI <- renderUI({
        data <- filteredData()
        choices <- c("All", unique(data$`ageGroup`[!is.na(data$`ageGroup`)]))
        selectInput("ageGroup", "Age Group", choices = choices, selected = "All")
    })

    output$sexUI <- renderUI({
        data <- filteredData()
        sex_options <- unique(data$`sex`[!is.na(data$`sex`)])
        choices <- c("All", sex_options, "Unknown")
        selectInput("sex", "Sex", choices = choices, selected = "All")
    })

    # Render survival plot
    output$survivalPlot <- renderPlot({
        # Get filtered data
        data <- filteredData()

        # Check if data is empty
        if (nrow(data) == 0) {
            plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
            text(1, 1, "No data available for selected filters", cex = 1.5)
            return()
        }

        # Define survival fit based on grouping
        if (input$groupBy == "None") {
            fit <- survfit(Surv(`survivalMonths`, `mortality`) ~ 1, data = data)
        } else {
            group_var <- switch(input$groupBy,
                "Diagnosis" = "diagnosisFinal",
                "Grade" = "grade",
                "IDH Mutation" = "mutationIDH1/2",
                "Age Group" = "ageGroup",
                "Sex" = "sex"
            )
            formula <- as.formula(paste("Surv(`survivalMonths`, `mortality`) ~", group_var))
            fit <- survfit(formula, data = data)
        }

        # Generate Kaplan-Meier plot with survminer
        p <- ggsurvplot(
            fit,
            data = data,
            risk.table = input$riskTable,
            censor = input$censorMarks,
            pval = input$pairwise, # Show p-value for pairwise comparisons if grouped
            pval.method = TRUE, # Show log-rank test method
            conf.int = FALSE, # No confidence intervals for clarity
            xlab = "Time (Months)",
            ylab = "Survival Probability",
            title = "Kaplan-Meier Survival Curves",
            palette = "jco", # Color palette
            legend = "right",
            legend.title = input$groupBy,
            ggtheme = theme_minimal()
        )

        # Render the plot
        p
    })
}

# Run the app
shinyApp(ui = ui, server = server)