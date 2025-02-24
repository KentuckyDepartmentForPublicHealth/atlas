#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


server <- function(input, output, session) {
  
  # Reactive filtered data based on the selected diagnosis
  filtered_dat <- reactive({
    if (input$diagnosis == "All") {
      atlasDataClean
    } else {
      atlasDataClean %>%
        dplyr::filter(diagnosis == input$diagnosis)
    }
  })
  
  # Render Kaplan-Meier plot
  output$kmplt <- renderPlot({
    data <- filtered_dat()
    req(nrow(data) > 0)  # Ensure data is not empty
    
    # Count observations for selected diagnosis
    obs_count <- nrow(data)
    
    # If fewer than 30 observations, display a warning message instead of the plot
    if (obs_count < 30) {
      plot.new()  # Create an empty plot area
      text(0.5, 0.5, "Too few observations to display Kaplan-Meier curve.", cex = 0.9, col = "red")
      return()  # Exit the function early
    }
    
    # Recode mortality: NA becomes 0 (censored), 1 remains as event
    data$event_status <- ifelse(is.na(data$mortality), 0, data$mortality)
    
    # Fit survival model using the selected strata
    fit <- survfit(Surv(survivalMonths, event_status) ~ data[[input$Strata]], data = data)
    
    # Create the Kaplan-Meier plot with custom labels and theme
    p <- ggsurvfit(fit) +
      labs(
        x = "Time (Days)",
        y = "Survival Probability",
        title = "Kaplan-Meier Plot"
      ) +
      scale_color_discrete() +
      theme(
        panel.background    = element_rect(fill = "white", color = NA),
        plot.background     = element_rect(fill = "white", color = NA),
        panel.grid.major    = element_line(color = "grey"),
        panel.grid.minor    = element_line(color = "grey", linetype = "dotted"),
        axis.text           = element_text(color = "black", size = 14),
        axis.title          = element_text(color = "black"),
        plot.title          = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
        plot.subtitle       = element_text(color = "black", size = 12),
        legend.background   = element_rect(fill = "white", color = NA),
        legend.text         = element_text(color = "black"),
        legend.title        = element_text(color = "black")
      )
    
    # Conditionally add censoring marks if the checkbox is checked
    p <- if (input$show_censoring) {
      p + add_censor_mark(shape = 3, size = 3, color = "red")
    } else {
      p
    }
    
    # Conditionally add the risk table if the checkbox is checked
    p <- if (input$show_risk_table) {
      p + add_risktable()
    } else {
      p
    }
    
    # Return the final plot object
    p
  })
  
  # Render Hazard Ratios table using gt
  output$hazard_table <- render_gt({
    req(input$show_hr)  # Only show if the checkbox is checked
    
    data <- filtered_dat()
    req(nrow(data) > 0)
    
    # Ensure the strata variable is treated as a factor if not numeric
    if (!is.numeric(data[[input$Strata]])) {
      data[[input$Strata]] <- as.factor(data[[input$Strata]])
    }
    
    # Get the reference level from the factor levels
    ref_lvl <- if (is.factor(data[[input$Strata]])) {
      levels(data[[input$Strata]])[1]
    } else {
      "Continuous Variable"
    }
    
    # Fit a Cox proportional hazards model
    cox_model <- coxph(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata)), data = data)
    cox_summary <- summary(cox_model)
    
    # Extract hazard ratios, confidence intervals, and p-values
    hr_vals <- exp(cox_summary$coefficients[, "coef"])
    ci_vals <- exp(confint(cox_model))
    p_vals  <- cox_summary$coefficients[, "Pr(>|z|)"]
    
    # Clean up the row names for better display
    clean_labels <- gsub("^get\\(input\\$Strata\\)", "", rownames(cox_summary$coefficients)) %>%
      gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%  # Insert space between camelCase words
      gsub("([0-9])([A-Za-z])", "\\1 \\2", .) %>%  # Insert space between numbers and letters
      gsub("([A-Za-z])([0-9])", "\\1 \\2", .) %>%  # Insert space between letters and numbers
      tools::toTitleCase()  # Capitalize first letter of each word
    
    # Build the hazard ratio table
    hr_table <- data.frame(
      "Group"         = clean_labels,
      "Hazard Ratio"  = round(hr_vals, 2),
      "95% CI Lower"  = round(ci_vals[, 1], 2),
      "95% CI Upper"  = round(ci_vals[, 2], 2),
      "P-Value"       = signif(p_vals, 3),
      check.names     = FALSE
    )
    
    # Return the formatted gt table
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
        Group = "Strata Group",
        "Hazard Ratio" = "HR",
        "95% CI Lower" = "Lower CI",
        "95% CI Upper" = "Upper CI",
        "P-Value" = "P-Value"
      ) %>%
      tab_options(
        table.font.size = "14px",
        column_labels.font.weight = "bold"
      )
  })
}





