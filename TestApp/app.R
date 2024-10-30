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
                      selectInput("boxplot_choice","Select Gene:",
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
      boxplot(GSM$colMeans.GSM.atlas. ~ GSM$`Original Histology`,data = GSM, xlab = "Histology",ylab="mRNA Expression log2" )
      points(x = factor(GSM$`Original Histology`), y = GSM$colMeans.GSM.atlas.)
    } else if (input$boxplot_choice == "JDE") {
      boxplot(jde.data$Mean ~ jde.data$Histology,data = jde.data, xlab = "Histology",ylab="mRNA Expression log2" )
    }
      else if (input$boxplot_choice=="XS") {
      boxplot(XS$colMeans.xs_atlas. ~ XS$Original.histology, data=XS, xlab = "Histology",ylab="mRNA Expression log2" )
      }
    else if (input$boxplot_choice=="AGZ") {
      boxplot(agz.data$colMeans.agz. ~ agz.data$Histology, data=XS, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="FB") {
      boxplot(FB$colMeans.FB. ~ FB$Original.histology, data=FB, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="AL") {
      boxplot(AL$colMeans.AL.atlas. ~ AL$...2, data=AL, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="JYD") {
      boxplot(JYD$colMeans.jyd.atlas. ~ JYD$...2, data=JYD, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="hCA") {
      boxplot(hCA$colMeans.hCA.atlas. ~ hCA$...2, data=hCA, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="hCR") {
      boxplot(hCR$colMeans.hCR.atlas. ~ hCR$...2, data=hCR, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="Ho") {
      boxplot(Ho$colMeans.Ho.atlas. ~ Ho$Original.histology, data=Ho, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="hHA") {
      boxplot(hHA$colMeans.hHA.atlas. ~ hHA$Original.Histology, data=hHA, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="MC") {
      boxplot(MC$colMeans.mc.atlas. ~ MC$...2, data=MC, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="nbt") {
      boxplot(nbt$colMeans.nbt.atlas. ~ nbt$...2, data=nbt, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="SG") {
      boxplot(SG$colMeans.SG.atlas. ~ SG$...2, data=SG, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="SJ") {
      boxplot(SJ$colMeans.SJ.atlas. ~ SJ$Original.histology, data=SJ, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="X_d") {
      boxplot(X_d$colMeans.X_d.atlas. ~ X_d$...2, data=X_d, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="X_I") {
      boxplot(X_I$colMeans.X_I.atlas. ~ X_I$...2, data=X_I, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="X_mb") {
      boxplot(X_mb$colMeans.X_mb.atlas. ~ X_mb$...2, data=X_mb, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="X_md") {
      boxplot(X_md$colMeans.X_md.atlas. ~ X_md$...2, data=X_md, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="FDY") {
      boxplot(FDY$colMeans.fdy_atlas. ~ FDY$Original.histology, data=FDY, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="XS") {
      boxplot(XS$colMeans.xs_atlas. ~ XS$Original.histology, data=XS, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="X") {
      boxplot(X$colMeans.X.atlas. ~ X$Original.histology, data=X, xlab = "Histology",ylab="mRNA Expression log2" )
    }
    else if (input$boxplot_choice=="X_MB") {
      boxplot(X_MB$colMeans.X_MB.atlas. ~ X_MB$...2, data=X_MB, xlab = "Histology",ylab="mRNA Expression log2" )
    }
  })
  
 
  output$mytable1 <- DT::renderDataTable({
    if (input$boxplot_choice == "GSM") {
      df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/gsmsurv.csv")
    DT::datatable(df)
    }
    else if (input$boxplot_choice == "JDE") {
      df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/jdesurv.csv")
      DT::datatable(df)
    }
    else if (input$boxplot_choice == "FB") {
      df<- survival[grep("^FB", survival[,1]), ]
      DT::datatable(df)
    }
    else if (input$boxplot_choice == "FDY") {
      df<- survival[grep("^FDY", survival[,1]), ]
      DT::datatable(df)
    }
    
  })
  
  output$mytable2 <- DT::renderDataTable({
    if (input$boxplot_choice == "GSM") {
      DT::datatable(GSM) }
    else if (input$boxplot_choice == "JDE") {
        DT::datatable(jde)
    }
    else if (input$boxplot_choice == "AGZ") {
      DT::datatable(agz.data)
    }
    else if (input$boxplot_choice == "FB") {
      DT::datatable(FB)
    }
    else if (input$boxplot_choice == "AL") {
      DT::datatable(AL)
    }
    else if (input$boxplot_choice == "JYD") {
      DT::datatable(JYD)
    }
    else if (input$boxplot_choice == "hCA") {
      DT::datatable(hCA)
    }
    else if (input$boxplot_choice == "hCR") {
      DT::datatable(hCR)
    }
    else if (input$boxplot_choice == "Ho") {
      DT::datatable(Ho)
    }
    else if (input$boxplot_choice == "hHA") {
      DT::datatable(hHA)
    }
    else if (input$boxplot_choice == "MC") {
      DT::datatable(MC)
    }
    else if (input$boxplot_choice == "nbt") {
      DT::datatable(nbt)
    }
    else if (input$boxplot_choice == "SG") {
      DT::datatable(SG)
    }
    else if (input$boxplot_choice == "SJ") {
      DT::datatable(SJ)
    }
    else if (input$boxplot_choice == "X_d") {
      DT::datatable(X_d)
    }
    else if (input$boxplot_choice == "X_I") {
      DT::datatable(X_I)
    }
    else if (input$boxplot_choice == "X_mb") {
      DT::datatable(X_mb)
    }
    else if (input$boxplot_choice == "X_md") {
      DT::datatable(X_md)
    }
    else if (input$boxplot_choice == "FDY") {
      DT::datatable(FDY)
    }
    else if (input$boxplot_choice == "XS") {
      DT::datatable(XS)
    }
    else if (input$boxplot_choice == "X") {
      DT::datatable(X)
    }
    else if (input$boxplot_choice == "X_MB") {
      DT::datatable(X_MB)
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
