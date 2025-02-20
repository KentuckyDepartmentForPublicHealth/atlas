#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


server <- function(input, output, session) {
  # Reactive filtered data based on user input
  filtered_dat <- reactive({
    if (input$diagnosis == "All") {
      atlasDataClean  
    } else {
      atlasDataClean %>%
        dplyr::filter(diagnosis == input$diagnosis)  # Subset if specific diagnosis selected
    }
  })
  
  
  output$kmplt <- renderPlot({
    data <- filtered_dat()
    req(nrow(data) > 0)
    
    # Ensure mortality is correctly coded: 1 = event, 0 = censored
    data$event_status <- ifelse(is.na(data$mortality), 0, data$mortality)
    
    # Fit survival model
    fit <- survfit(Surv(survivalMonths, event_status) ~ data[[input$Strata]], data = data)
    
    # Create Kaplan-Meier plot
    p <- ggsurvfit(fit) +
      labs(
        x = "Time (Days)",
        y = "Survival Probability",
        title = "Kaplan-Meier Plot"
      ) +
      scale_color_discrete() +  
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
    
    # Apply censoring marks if checkbox is checked
    p <- if (input$show_censoring) p + add_censor_mark(shape = 3, size = 3, color = "red") else p
    
    # Apply risk table if checkbox is checked
    p <- if (input$show_risk_table) p + add_risktable() else p
    
    # Return final plot
    p
  })
  
  
  output$hazard_table <- render_gt({
    req(input$show_hr)  # Ensure checkbox is checked
    
    data <- filtered_dat()
    req(nrow(data) > 0)  # Ensure data isn't empty
    
    # Ensure Strata is categorical if necessary
    if (!is.numeric(data[[input$Strata]])) {
      data[[input$Strata]] <- as.factor(data[[input$Strata]])
    }
    
    # Get reference level
    ref_lvl <- if (is.factor(data[[input$Strata]])) levels(data[[input$Strata]])[1] else "Continuous Variable"
    
    # Create Cox model using correct formula
    cox_model <- coxph(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata)), data = data)
    cox_summary <- summary(cox_model)
    
    # Extract hazard ratios
    hr <- exp(cox_summary$coefficients[, "coef"])
    ci <- exp(confint(cox_model))
    p <- cox_summary$coefficients[, "Pr(>|z|)"]
    
    # Clean row names
    clean_labels <- gsub("^get\\(input\\$Strata\\)", "", rownames(cox_summary$coefficients))
    
    # Build table
    hr_table <- data.frame(
      "Group" = clean_labels,
      "Hazard Ratio" = round(hr, 2),
      "95% CI Lower" = round(ci[, 1], 2),
      "95% CI Upper" = round(ci[, 2], 2),
      "P-Value" = signif(p, 3),
      check.names = FALSE
    )
    
    # Return the gt table
    gt(hr_table) %>%
      tab_header(
        title = md("**Hazard Ratios**"),
        subtitle = paste("Reference Level:", ref_lvl)
      ) %>%
      fmt_number(
        columns = c("Hazard Ratio", "95% CI Lower", "95% CI Upper"),
        decimals = 2
      ) %>%
      cols_label(
        `Group` = "Strata Group",
        `Hazard Ratio` = "HR",
        `95% CI Lower` = "Lower CI",
        `95% CI Upper` = "Upper CI",
        `P-Value` = "P-Value"
      ) %>%
      tab_options(
        table.font.size = "14px",
        column_labels.font.weight = "bold"
      )
  })
  
}





