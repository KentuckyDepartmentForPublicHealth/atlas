# app.R

# Load required libraries
library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

# Load your dataset (replace this with your actual data loading method)
# For this example, I'll assume it's available as 'atlasDataCleanSurvival'
# atlasDataCleanSurvival <- read.csv("path/to/your/data.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
    titlePanel("Survival Analysis Explorer"),
    sidebarLayout(
        sidebarPanel(
            # Dropdown filters
            selectInput("diagnosis", "Diagnosis",
                choices = c("All", unique(atlasDataCleanSurvival$diagnosisFinal[!is.na(atlasDataCleanSurvival$diagnosisFinal)])),
                selected = "All"
            ),
            selectInput("grade", "Grade",
                choices = c("All", "GRADE 1", "GRADE 2", "GRADE 3", "GRADE 4"),
                selected = "All"
            ),
            selectInput("idh", "IDH Mutation",
                choices = c("All", "MUT", "WT", "Unknown/Not Tested"),
                selected = "All"
            ),
            selectInput("ageGroup", "Age Group",
                choices = c("All", unique(atlasDataCleanSurvival$ageGroup[!is.na(atlasDataCleanSurvival$ageGroup)])),
                selected = "All"
            ),
            selectInput("sex", "Sex",
                choices = c("All", "MALE", "FEMALE", "Unknown"),
                selected = "All"
            ),

            # Grouping variable
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
server <- function(input, output) {
    # Reactive data filtering
    filteredData <- reactive({
        data <- atlasDataCleanSurvival

        # Apply filters
        if (input$diagnosis != "All") {
            data <- data %>% filter(diagnosisFinal == input$diagnosis)
        }
        if (input$grade != "All") {
            data <- data %>% filter(grade == input$grade)
        }
        if (input$idh != "All") {
            if (input$idh == "Unknown/Not Tested") {
                data <- data %>% filter(is.na(mutationIDH1.2))
            } else {
                data <- data %>% filter(mutationIDH1.2 == input$idh)
            }
        }
        if (input$ageGroup != "All") {
            data <- data %>% filter(ageGroup == input$ageGroup)
        }
        if (input$sex != "All") {
            if (input$sex == "Unknown") {
                data <- data %>% filter(is.na(sex))
            } else {
                data <- data %>% filter(sex == input$sex)
            }
        }

        # Ensure survivalMonths and mortality are present
        data <- data %>% filter(!is.na(survivalMonths), !is.na(mortality))

        return(data)
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
            fit <- survfit(Surv(survivalMonths, mortality) ~ 1, data = data)
        } else {
            group_var <- switch(input$groupBy,
                "Diagnosis" = "diagnosisFinal",
                "Grade" = "grade",
                "IDH Mutation" = "mutationIDH1.2",
                "Age Group" = "ageGroup",
                "Sex" = "sex"
            )
            formula <- as.formula(paste("Surv(survivalMonths, mortality) ~", group_var))
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