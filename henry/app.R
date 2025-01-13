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
load(file = '~/Sandbox/Shiny/atlas/henry/subsets.RData')

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
                                           plotlyOutput("boxplot")),
                         
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
  # load("C:/Users/henry/OneDrive/Documents/GitHub/atlas/subsets.RData")
  output$boxplot <- renderPlotly({
    if (input$boxplot_choice == "GSM") {
    p <- plot_ly(data = GSM, 
                 x = ~factor(GSM$`Original Histology`), 
                 y = ~GSM$colMeans.GSM.atlas., 
                 type = 'box', 
                 mode = 'markers', 
                 boxpoints = 'all',
                 text = ~paste("mean:", GSM$colMeans.GSM.atlas., "Histology:", GSM$`Original Histology`),  
                 hoverinfo = 'text') 
    
    p <- p %>% layout(title = "GSM Dataset",
                      xaxis = list(title = "Histology"),
                      yaxis = list(title = "mRNA expression"))
    
    p}
    else if (input$boxplot_choice == "JDE") {
      p <- plot_ly(data = jde, 
                   x = ~factor(jde$Histology), 
                   y = ~jde$mean.frame.1.170..., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", jde$mean.frame.1.170..., "Histology:", jde$Histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "JDE Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "AGZ") {
      p <- plot_ly(data = agz.data, 
                   x = ~factor(agz.data$Histology), 
                   y = ~agz.data$colMeans.agz., 
                   type = 'box', 
                   boxpoints = 'all',
                   mode = 'markers', 
                   text = ~paste("mean:", agz.data$colMeans.agz., "Histology:", agz.data$Histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "AGZ Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "FB") {
      p <- plot_ly(data = FB, 
                   x = ~factor(FB$Original.histology), 
                   y = ~FB$colMeans.FB., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", FB$colMeans.FB., "Histology:", FB$Original.histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "FB Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "AL") {
      p <- plot_ly(data = AL, 
                   x = ~factor(AL$...2), 
                   y = ~AL$colMeans.AL.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", AL$colMeans.AL.atlas., "Histology:", AL$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "AL Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "JYD") {
      p <- plot_ly(data = JYD, 
                   x = ~factor(JYD$...2), 
                   y = ~JYD$colMeans.jyd.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", JYD$colMeans.jyd.atlas., "Histology:", JYD$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "JYD Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "hCA") {
      p <- plot_ly(data = hCA, 
                   x = ~factor(hCA$...2), 
                   y = ~hCA$colMeans.hCA.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", hCA$colMeans.hCA.atlas., "Histology:", hCA$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "hCA Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "hCR") {
      p <- plot_ly(data = hCR, 
                   x = ~factor(hCR$...2), 
                   y = ~hCR$colMeans.hCR.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", hCR$colMeans.hCR.atlas., "Histology:", hCR$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "hCR Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "Ho") {
      p <- plot_ly(data = Ho, 
                   x = ~factor(Ho$Original.histology), 
                   y = ~Ho$colMeans.Ho.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", Ho$colMeans.Ho.atlas., "Histology:", Ho$Original.histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "Ho Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "hHA") {
      p <- plot_ly(data = hHA, 
                   x = ~factor(hHA$Original.Histology), 
                   y = ~hHA$colMeans.hHA.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", hHA$colMeans.hHA.atlas., "Histology:", hHA$Original.Histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "hHA Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "MC") {
      p <- plot_ly(data = MC, 
                   x = ~factor(MC$...2), 
                   y = ~MC$colMeans.mc.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", MC$colMeans.mc.atlas., "Histology:", MC$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "MC Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "nbt") {
      p <- plot_ly(data = nbt, 
                   x = ~factor(nbt$...2), 
                   y = ~nbt$colMeans.nbt.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", nbt$colMeans.nbt.atlas., "Histology:", nbt$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "nbt Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "SG") {
      p <- plot_ly(data = SG, 
                   x = ~factor(SG$...2), 
                   y = ~SG$colMeans.SG.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", SG$colMeans.SG.atlas., "Histology:", SG$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "SG Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "SJ") {
      p <- plot_ly(data = SJ, 
                   x = ~factor(SJ$Original.histology), 
                   y = ~SJ$colMeans.SJ.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", SJ$colMeans.SJ.atlas., "Histology:", SJ$Original.histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "SJ Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X_d") {
      p <- plot_ly(data = X_d, 
                   x = ~factor(X_d$...2), 
                   y = ~X_d$colMeans.X_d.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X_d$colMeans.X_d.atlas., "Histology:", X_d$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X_d Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X_I") {
      p <- plot_ly(data = X_I, 
                   x = ~factor(X_I$...2), 
                   y = ~X_I$colMeans.X_I.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X_I$colMeans.X_I.atlas., "Histology:", X_I$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X_I Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X_d") {
      p <- plot_ly(data = X_d, 
                   x = ~factor(X_d$...2), 
                   y = ~X_d$colMeans.X_d.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X_d$colMeans.X_d.atlas., "Histology:", X_d$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X_d Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X_mb") {
      p <- plot_ly(data = X_d, 
                   x = ~factor(X_mb$...2), 
                   y = ~X_mb$colMeans.X_mb.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X_mb$colMeans.X_mb.atlas., "Histology:", X_mb$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X_mb Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X_md") {
      p <- plot_ly(data = X_md, 
                   x = ~factor(X_md$...2), 
                   y = ~X_md$colMeans.X_md.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X_md$colMeans.X_md.atlas., "Histology:", X_md$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X_md Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "FDY") {
      p <- plot_ly(data = FDY, 
                   x = ~factor(FDY$Original.histology), 
                   y = ~FDY$colMeans.fdy_atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", FDY$colMeans.fdy_atlas., "Histology:", FDY$Original.histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "FDY Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "XS") {
      p <- plot_ly(data = XS, 
                   x = ~factor(XS$Original.histology), 
                   y = ~XS$colMeans.xs_atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", XS$colMeans.xs_atlas., "Histology:", XS$Original.histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "XS Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X") {
      p <- plot_ly(data = X, 
                   x = ~factor(X$Original.histology), 
                   y = ~X$colMeans.X.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X$colMeans.X.atlas., "Histology:", X$Original.histology),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    else if (input$boxplot_choice == "X_MB") {
      p <- plot_ly(data = X_MB, 
                   x = ~factor(X_MB$...2), 
                   y = ~X_MB$colMeans.X_MB.atlas., 
                   type = 'box', 
                   mode = 'markers', 
                   boxpoints = 'all',
                   text = ~paste("mean:", X_MB$colMeans.X_MB.atlas., "Histology:", X_MB$...2),  
                   hoverinfo = 'text') 
      
      p <- p %>% layout(title = "X_MB Dataset",
                        xaxis = list(title = "Histology"),
                        yaxis = list(title = "mRNA expression"))
      
      p}
    
  })
  
 
  output$mytable1 <- DT::renderDataTable({
    if (input$boxplot_choice == "GSM") {
      # df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/gsmsurv.csv")
      df <- read.csv('gsmsurv.csv')
    DT::datatable(df)
    }
    else if (input$boxplot_choice == "JDE") {
      # df<- read.csv("C:/Users/henry/OneDrive/Documents/GitHub/atlas/jdesurv.csv")
      df <- read.csv('jdesurv.csv')
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
