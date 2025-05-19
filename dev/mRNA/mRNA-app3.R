library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load your datasets (Replace with actual loading code)
# atlasDataClean <- read.csv("atlasDataClean.csv")
# geneExpressionData <- read.csv("geneExpressionData.csv")
# gene_annotations <- read.csv("gene_annotations.csv")

# Sample Data - Replace with real data
set.seed(123)
atlasDataClean <- data.frame(
  sampleID = paste0("Sample_", 1:10),
  grade = sample(c("Low", "High"), 10, replace = TRUE),
  ageGroup = sample(c("Child", "Adult"), 10, replace = TRUE),
  tumorType = sample(c("TypeA", "TypeB"), 10, replace = TRUE),
  sex = sample(c("M", "F"), 10, replace = TRUE),
  compartment = sample(c("Peripheral", "Central"), 10, replace = TRUE),
  fullName = sample(c("Diagnosis1", "Diagnosis2"), 10, replace = TRUE),
  country = sample(c("Country1", "Country2"), 10, replace = TRUE),
  diagnosis = sample(c("Type1", "Type2"), 10, replace = TRUE)
)

geneExpressionData <- data.frame(
  ENTREZID = as.character(c(1:5, 10001:10005)),
  Sample_1 = rnorm(10),
  Sample_2 = rnorm(10),
  Sample_3 = rnorm(10)
)

gene_annotations <- data.frame(
  ENTREZID = as.character(c(1:5, 10001:10005)),
  SYMBOL = paste0("Gene_", c(1:5, 10001:10005)),
  GENENAME = paste("Gene Name", c(1:5, 10001:10005))
)

# Join gene expression data with annotations
annotated_gene_expression <- geneExpressionData %>%
  pivot_longer(cols = starts_with("Sample_"), names_to = "sampleID", values_to = "expression") %>%
  left_join(gene_annotations, by = "ENTREZID") %>%
  left_join(atlasDataClean, by = "sampleID")

# Shiny app
ui <- fluidPage(
  titlePanel("mRNA Expression Boxplots with Gene Annotations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_gene", "Select a Gene", choices = unique(annotated_gene_expression$SYMBOL)),
      selectInput(
        "group_by",
        "Group By",
        choices = c("diagnosis", "grade", "ageGroup", "tumorType", "sex", "compartment", "fullName", "country")
      ),
      width = 3
    ),
    mainPanel(
      plotOutput("boxplot"),
      tableOutput("gene_info"),
      width = 9
    )
  )
)

server <- function(input, output) {
  # Filter data based on selected gene
  filtered_data <- reactive({
    annotated_gene_expression %>%
      filter(SYMBOL == input$selected_gene)
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = .data[[input$group_by]], y = expression)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(
        title = paste("Expression of", input$selected_gene),
        x = input$group_by,
        y = "Expression Level"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gene Information Table
  output$gene_info <- renderTable({
    req(filtered_data())
    filtered_data() %>%
      select(SYMBOL, GENENAME) %>%
      distinct()
  })
}

shinyApp(ui, server)
