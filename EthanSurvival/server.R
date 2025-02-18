#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(dplyr)
library(survival)
library(survminer)

server <- function(input, output, session) {
  # Reactive filtered data based on user input
  filtered_dat <- reactive({
    atlasDataClean %>%
      dplyr::filter(
        diagnosis %in% input$diagnosis  # Filter by selected diagnosis
      )
  })
  
  # Create KM plot
  output$kmplt <- renderPlot({
    data <- filtered_dat()
    
    # Fit survival model
    fit <- survfit2(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata)), data = data)
    
    # Plot using ggsurvfit
    ggsurvfit(fit) +
      labs(
        x = "Time (Days)",
        y = "Survival Probability",
        title = "Kaplan-Meier Plot"
      ) +
      scale_color_discrete() +  # Use color instead of fill
      theme(
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),   
        panel.grid.major = element_line(color = "grey"),              
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"),
        axis.text = element_text(color = "black", size = 14),                   
        axis.title = element_text(color = "black"),                   
        plot.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),  
        plot.subtitle = element_text(color = "black", size = 12),     
        legend.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(color = "black"),                  
        legend.title = element_text(color = "black")                  
      )
  })
  
  # Configuring output for hazard ratios
  output$hazard_ratios <- renderPrint({
    
    req(input$show_hr)
    
    data <- filtered_dat()
    
    # Creating Cox PH model
    cox_model <- coxph(Surv(survivalMonths, mortality) ~ get(input$Strata), data = data)
    cox_summary <- summary(cox_model)
    
    # Extracting HR, confidence intervals, and p-values
    hr <- exp(cox_summary$coefficients[, "coef"])
    ci <- exp(confint(cox_model))
    p <- cox_summary$coefficients[, "Pr(>|z|)"]
    
    # Create a table with hazard ratio details
    hr_table <- data.frame(
      Group_Comparison = rownames(cox_summary$coefficients),
      Hazard_Ratio = round(hr, 2),
      CI_Lower = round(ci[, 1], 2),
      CI_Upper = round(ci[, 2], 2),
      P_Value = signif(p, 3)
    )
    
    # Display the table
    print(hr_table, row.names = FALSE)
  })
}





