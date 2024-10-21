#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(bslib)
library(bsicons)
library(shinythemes)
library(plotly)
library(htmltools)
library(leaflet)


ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      h3 {
        font-family: 'Times New Roman';  
        font-size: 40px;  
        color: #87CEFA;  
        font-weight: bold;  
      }
      h4 {
        font-family: 'Times New Roman', sans-serif;  
        font-size: 15px;  
        color: #87CEFA;  
        font-weight: bold;  
      }
    "))
  ),
  
  theme = shinytheme("cyborg"),  
  tabsetPanel(
    tabPanel("Tab 1", 
             column(3,
                    wellPanel(  
                      h4("Gene"),
                      radioButtons("boxplot_choice","Select Gene:",
                                   choices = list("GSM" = "GSM", "JDE" = "JDE"))
                    )
             ),
             tabsetPanel(type = "tabs",
                         
                         
                         tabPanel("mRNA Expression", h4("Contents"),
                                  
                                  column(10,
                                         mainPanel(
                                           h4("Boxplot"),
                                           plotOutput("boxplot")))),
                         
                         tabPanel("Scatter Plot", title = tagList(icon("chart-line"), "Survival Curve"),verbatimTextOutput("summary",),
                                  
                                  column(6,
                                         mainPanel(
                                           h4("Scatterplot"),
                                           ))),
                         tabPanel("Data",
                                           
                                  
                                  mainPanel(
                                    tableOutput("contents")
                                  )
                                  
                         )
                         
             )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  load("C:/Users/henry/OneDrive/Documents/GitHub/atlas/subsets.RData")
  
  output$boxplot <- renderPlot({
    if (input$boxplot_choice == "GSM") {
      boxplot(GSM$colMeans.GSM.atlas. ~ GSM$`Original Histology`,data = GSM )
    } else if (input$boxplot_choice == "JDE") {
      boxplot(jde.data$Mean ~ jde.data$Histology,data = jde.data )
    }
  })
  
  output$contents <- renderTable({
    if (input$boxplot_choice == "GSM") {
      df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/gsmsurv.csv")
    } else if (input$boxplot_choice == "JDE") {
      df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/jdesurv.csv")
    }
    return(df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
