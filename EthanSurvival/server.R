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
  
  output$kmplt <- renderPlotly({
    data <- filtered_dat()
    req(nrow(data) > 0)  # Ensure data is not empty
    
    # If fewer than 30 observations, display a warning message
    if (nrow(data) < 30) {
      plot.new()
      text(0.5, 0.5, "Too few observations to display Kaplan-Meier curve.", cex = 0.9, col = "red")
      return()
    }
    
    # Recode mortality: NA becomes 0 (censored), 1 remains as event
    data$event_status <- ifelse(is.na(data$mortality), 0, data$mortality)
    
    # Create a clean factor for the strata variable with better labels
    strata_var <- input$Strata
    data$strata_factor <- factor(data[[strata_var]])
    
    # Generate clean labels for legend
    clean_labels <- levels(data$strata_factor)
    clean_labels <- gsub("strata_factor=", "", clean_labels)  # Remove unnecessary prefixes
    
    # Fit survival model using the clean factor
    fit <- survfit(Surv(survivalMonths, event_status) ~ strata_factor, data = data)
    
    # Create the Kaplan-Meier plot
    p <- ggsurvfit(fit) +
      labs(
        x = "Time (Months)",
        y = "Survival Probability",
        title = "Kaplan-Meier Plot"
      ) +
      scale_color_manual(
        name = strata_var, 
        values = RColorBrewer::brewer.pal(length(clean_labels), "Set1"),  # Assign colors manually
        labels = clean_labels  # Ensure legend labels are formatted correctly
      ) +
      theme_minimal() +
      theme(
        panel.background    = element_rect(fill = "white", color = NA),
        plot.background     = element_rect(fill = "white", color = NA),
        panel.grid.major    = element_line(color = "grey"),
        panel.grid.minor    = element_line(color = "grey", linetype = "dotted"),
        axis.text           = element_text(color = "black", size = 14),
        axis.title          = element_text(color = "black"),
        plot.title          = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
        legend.background   = element_rect(fill = "white", color = NA),
        legend.text         = element_text(color = "black"),
        legend.title        = element_text(color = "black")
      )
    
    # Conditionally add censoring marks if the checkbox is checked
    if (input$show_censoring) {
      p <- p + add_censor_mark(shape = 3, size = 3, color = "red")
    }
    
    # Conditionally add the risk table if the checkbox is checked
    if (input$show_risk_table) {
      p <- p + add_risktable()
    }
    
    # Convert to plotly 
    plotly_obj <- ggplotly(p)
    
    # Function to calculate number at risk at specific timepoints
    get_n_at_risk <- function(timepoints, strata_level, data) {
      sapply(timepoints, function(t) {
        sum(data$strata_factor == strata_level & data$survivalMonths >= t)
      })
    }
    
    # Extract time points from the plot data
    all_data_points <- plotly_obj$x$data
    
    # Update each trace with custom hover text including number at risk
    for (i in seq_along(clean_labels)) {
      # Get the current trace data
      trace_data <- all_data_points[[i]]
      
      # Skip if this isn't a line trace (e.g., risk table)
      if (!("line" %in% names(trace_data)) || is.null(trace_data$x)) {
        next
      }
      
      # Get time points for this curve
      time_points <- trace_data$x
      
      # Calculate number at risk at each time point
      n_at_risk <- get_n_at_risk(time_points, levels(data$strata_factor)[i], data)
      
      # Get survival probabilities
      survival_probs <- trace_data$y
      
      # Create custom hover text
      hover_text <- mapply(
        function(t, s, n) {
          sprintf(
            "Time: %.1f days<br>Survival: %.3f<br>At risk: %d", 
            t, s, n
          )
        },
        time_points,
        survival_probs,
        n_at_risk,
        SIMPLIFY = TRUE
      )
      
      # Update the trace with new hover text
      plotly_obj$x$data[[i]]$text <- hover_text
      plotly_obj$x$data[[i]]$hoverinfo <- "text"
      plotly_obj$x$data[[i]]$name <- clean_labels[i]
    }
    
    plotly_obj <- plotly_obj %>%
      layout(
        legend = list(
          title = list(text = strata_var),
          x = 1, y = 1,
          traceorder = "normal",
          font = list(size = 12)
        ),
        hovermode = "closest"
      ) %>%
      config(
        modeBarButtonsToRemove = c("pan2d", "select2d", "autoscale", "resetScale2d", 
                                   "toggleSpikelines", "hoverClosestCartesian", 
                                   "hoverCompareCartesian"),
        modeBarButtonsToAdd = c("zoom2d", "lasso2d", "toImage"),
        displaylogo = FALSE
      )
    
    plotly_obj  # Return fixed plotly object
  })
  
  # Fit Cox Model ONCE
  cox_model <- reactive({
    data <- filtered_dat()
    req(nrow(data) > 0)  # Ensure we have data
    
    # Ensure the strata variable is treated as a factor if not numeric
    if (!is.numeric(data[[input$Strata]])) {
      data[[input$Strata]] <- as.factor(data[[input$Strata]])
    }
    
    # Fit Cox proportional hazards model
    coxph(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata)), data = data)
  })
  
  hr_data <- reactive({
    broom::tidy(cox_model(), exponentiate = TRUE, conf.int = TRUE) %>% 
      dplyr::mutate(term = gsub("_", " ", term))  # Clean term names
  })
  
  # Render Hazard Ratios Table using gt
  output$hazard_table <- render_gt({
    req(input$show_hr)  # Only show if checkbox is checked
    
    data <- filtered_dat()
    req(nrow(data) > 0)
    
    cox_summary <- summary(cox_model())
    
    # Extract hazard ratios, confidence intervals, and p-values
    hr_vals <- exp(cox_summary$coefficients[, "coef"])
    ci_vals <- exp(confint(cox_model()))
    p_vals  <- cox_summary$coefficients[, "Pr(>|z|)"]
    
    # Clean up row names for better display
    clean_labels <- gsub("^get\\(input\\$Strata\\)", "", rownames(cox_summary$coefficients)) %>%
      gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%
      gsub("([0-9])([A-Za-z])", "\\1 \\2", .) %>%
      gsub("([A-Za-z])([0-9])", "\\1 \\2", .) %>%
      tools::toTitleCase()
    
    # Build Hazard Ratio Table
    hr_table <- data.frame(
      "Group"         = clean_labels,
      "Hazard Ratio"  = round(hr_vals, 2),
      "95% CI Lower"  = round(ci_vals[, 1], 2),
      "95% CI Upper"  = round(ci_vals[, 2], 2),
      "P-Value"       = signif(p_vals, 3),
      check.names     = FALSE
    )
    
    # Render table
    gt(hr_table) %>%
      tab_header(
        title = md("**Hazard Ratios**"),
        subtitle = "Estimated from Cox Proportional Hazards Model"
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
  
  # Render Hazard Ratios Plot using Plotly
  output$hr_plot <- renderPlotly({
    p <- ggplot(hr_data(), aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_pointrange(color = "blue", size = 1) +  
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  
      coord_flip() +  
      labs(
        title = "Hazard Ratios from Cox Model",
        x = "Covariates",
        y = "Hazard Ratio (95% CI)"
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      config(
        modeBarButtonsToRemove = c("pan2d", "select2d", "autoscale", "resetScale2d", 
                                   "toggleSpikelines", "hoverClosestCartesian", 
                                   "hoverCompareCartesian"),
        modeBarButtonsToAdd = c("zoom2d", "lasso2d", "toImage"),
        displaylogo = FALSE
      )
  })
}


