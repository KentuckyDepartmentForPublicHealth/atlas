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
library(survival)
library(DT)


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
             
                    sidebarPanel(  
                      h4("Gene"),
                      radioButtons("boxplot_choice","Select Gene:",
                                   choices = list("GSM" = "GSM", "JDE" = "JDE","AGZ"="AGZ","FB"="FB","AL"="AL","JYD"="JYD","hCA"="hCA","hCR"="hCR","Ho"="Ho","hHA"="hHA",
                                                  "MC"="MC","nbt"="nbt","SG"="SG","SJ"="SJ","X_d"="X_d","X_I"="X_I","X_mb"="X_mb","X_md"="X_md",
                                                  "FDY"="FDY","XS"="XS","X"="X","X_MB"="X_MB"))
                    )
            ,
           mainPanel(  tabsetPanel(type = "tabs",
                         
                         
                         tabPanel("mRNA Expression",
                                   
                                           h4("Boxplot"),
                                           plotOutput("boxplot")),
                         
                         tabPanel("Survival Curve", title = tagList(icon("chart-line"), "Survival Curve"),verbatimTextOutput("summary",),
                                  
                                
                                         (
                                           plotOutput("surv")
                                           )),
                         tabPanel("Data",
                                           
                                  tabsetPanel(type = "tabs", 
                                          tabPanel ("Survival Data",mainPanel(
                                    DT::dataTableOutput("mytable1")
                                  )
                                           ),
                                  tabPanel("mRNA Data",mainPanel(
                                    DT::dataTableOutput("mytable2")
                                  )
                                           ))
                                  
                                  
                         )
                         
             ))
    
    
  
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
      else if (input$boxplot_choice=="XS") {
      boxplot(XS$colMeans.xs_atlas. ~ XS$Original.histology, data=XS)
      }
    else if (input$boxplot_choice=="AGZ") {
      boxplot(agz.data$colMeans.agz. ~ XS$Original.histology, data=XS)
    }
    else if (input$boxplot_choice=="FB") {
      boxplot(FB$colMeans.FB. ~ FB$Original.histology, data=FB)
    }
  })
  
 
  output$mytable1 <- DT::renderDataTable({
    if (input$boxplot_choice == "GSM") {
      df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/gsmsurv.csv")
    DT::datatable(df)
    }
  })
  
  output$mytable2 <- DT::renderDataTable({
    if (input$boxplot_choice == "GSM") {
      DT::datatable(GSM)
    }
  })
  
  output$surv <- renderPlot({
     if (input$boxplot_choice == "JDE") {
      jdesurv<- survival[grep("^JDE", survival[,1]), ]
      jdesurv$Overall.survival..months. <- as.numeric(jdesurv$Overall.survival..months.)
      fit<- survfit(Surv(Overall.survival..months.,Vital.status..1.dead..0.alive.)~1, data=jdesurv)
      plot(fit, xlab="Time", ylab="Survival", main="JDE Data", col=c(1), lty=c(1),conf.int = F)
    }
    else if (input$boxplot_choice == "FDY") {
      fdysurv<- survival[grep("^FDY", survival[,1]), ]
      fdysurv$Overall.survival..months. <- as.numeric(fdysurv$Overall.survival..months.)
      fit<- survfit(Surv(Overall.survival..months.,Vital.status..1.dead..0.alive.)~1, data=fdysurv)
      plot(fit, xlab="Time", ylab="Survival", main="FDY Data", col=c(1), lty=c(1),conf.int = F)
    }
    else if (input$boxplot_choice == "GSM") {
      gsmsurv<- survival[grep("^GSM", survival[,1]), ]
      gsmsurv$Overall.survival..months. <- as.numeric(gsmsurv$Overall.survival..months.)
      fit<- survfit(Surv(Overall.survival..months.,Vital.status..1.dead..0.alive.)~1, data=gsmsurv)
      plot(fit, xlab="Time", ylab="Survival", main="GSM Data", col=c(1), lty=c(1),conf.int = F)
    }
    else if (input$boxplot_choice == "FB") {
      fbsurv<- survival[grep("^FB", survival[,1]), ]
      fbsurv$Overall.survival..months. <- as.numeric(fbsurv$Overall.survival..months.)
      fit<- survfit(Surv(Overall.survival..months.,Vital.status..1.dead..0.alive.)~1, data=fbsurv)
      plot(fit, xlab="Time", ylab="Survival", main="FB Data", col=c(1), lty=c(1),conf.int = F)
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
