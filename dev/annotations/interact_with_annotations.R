# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Load gene_annotations data from RData file
load("../../dat/annotations.RData")

# Define UI for application
ui <- fluidPage(
    titlePanel("Gene Annotations Explorer"),
    sidebarLayout(
        sidebarPanel(
            numericInput("sample_size", "Sample Size:", value = 1, min = 1, max = nrow(gene_annotations)),
            actionButton("sample", "Get Sample"),
            textInput("search_symbol", "Search by SYMBOL:", ""),
            sliderInput("name_length", "Filter by GENENAME Length:", min = 0, max = max(nchar(gene_annotations$GENENAME)), value = c(0, max(nchar(gene_annotations$GENENAME)))),
            downloadButton("downloadData", "Download Sampled Data")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Table", tableOutput("gene_table")),
                tabPanel(
                    "Plots",
                    plotlyOutput("gene_length_plot"),
                    plotlyOutput("gene_distribution_plot")
                ),
                tabPanel("Fun Facts", verbatimTextOutput("fun_fact"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$sample, {
        sample_data <- gene_annotations %>%
            filter(nchar(GENENAME) >= input$name_length[1] & nchar(GENENAME) <= input$name_length[2]) %>%
            filter(SYMBOL == input$search_symbol | input$search_symbol == "") %>%
            sample_n(input$sample_size)

        output$gene_table <- renderTable({
            sample_data
        })

        output$gene_length_plot <- renderPlotly({
            ggplot(sample_data, aes(x = nchar(GENENAME))) +
                geom_histogram(binwidth = 1, fill = "blue", color = "black") +
                labs(title = "Distribution of Gene Name Lengths", x = "Gene Name Length", y = "Frequency") +
                theme_minimal()
        })

        output$gene_distribution_plot <- renderPlotly({
            ggplot(sample_data, aes(x = reorder(SYMBOL, -nchar(GENENAME)))) +
                geom_bar(stat = "identity", fill = "green", color = "black") +
                coord_flip() +
                labs(title = "Gene Symbols by Name Length", x = "Gene Symbol", y = "Gene Name Length") +
                theme_minimal()
        })

        output$fun_fact <- renderText({
            fun_facts <- c(
                "Did you know? The human genome contains approximately 20,000-25,000 genes.",
                "Fun fact: Only about 1-2% of the human genome encodes for proteins.",
                "Interesting: The longest human gene is Dystrophin, which is about 2.4 million base pairs long.",
                "Cool fact: The shortest human gene is the Histone H1 gene, which is only about 200 base pairs long.",
                "Amazing: The human genome is made up of about 3 billion base pairs."
            )
            sample(fun_facts, 1)
        })

        output$downloadData <- downloadHandler(
            filename = function() {
                paste("sampled_genes", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                write.csv(sample_data, file, row.names = FALSE)
            }
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)