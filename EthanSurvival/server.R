#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



server <- function(input, output, session) {
  # Separate reactive filtered data for KM tab
  filtered_dat_km <- reactive({
    data <- if (input$diagnosis == "All") {
      atlasDataClean
    } else {
      atlasDataClean %>%
        dplyr::filter(diagnosis == input$diagnosis)
    }
    
    if (input$histology != "All") {
      data <- data %>%
        dplyr::filter(histologyOriginal == input$histology)
    }
    
    data
  })
  
  # Separate reactive filtered data for HR tab
  filtered_dat_hr <- reactive({
    data <- if (input$diagnosis_hr == "All") {
      atlasDataClean
    } else {
      atlasDataClean %>%
        dplyr::filter(diagnosis == input$diagnosis_hr)
    }
    
    if (input$histology_hr != "All") {
      data <- data %>%
        dplyr::filter(histologyOriginal == input$histology_hr)
    }
    
    data
  })
  
  output$kmplt <- renderPlotly({
    data <- filtered_dat_km()
    req(nrow(data) > 0)  # Ensure data is not empty
    
    if (nrow(data) < 30) {
      plot.new()
      text(0.5, 0.5, "Too few observations to display Kaplan-Meier curve.", cex = 0.9, col = "red")
      return()
    }
    
    data$event_status <- ifelse(is.na(data$mortality), 0, data$mortality)
    
    strata_var <- input$Strata
    data$strata_factor <- factor(data[[strata_var]])
    
    clean_labels <- levels(data$strata_factor)
    clean_labels <- gsub("strata_factor=", "", clean_labels)
    
    fit <- survfit(Surv(survivalMonths, event_status) ~ strata_factor, data = data)
    
    p <- ggsurvfit(fit) +
      labs(
        x = "Time (Months)",
        y = "Survival Probability",
        title = "Kaplan-Meier Plot"
      ) +
      scale_color_manual(
        name = strata_var, 
        values = RColorBrewer::brewer.pal(length(clean_labels), "Set1"),
        labels = clean_labels
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
    
    if (input$show_censoring) {
      p <- p + add_censor_mark(shape = 3, size = 3, color = "red")
    }
    
    plotly_obj <- ggplotly(p)
    
    get_n_at_risk <- function(timepoints, strata_level, data) {
      sapply(timepoints, function(t) {
        sum(data$strata_factor == strata_level & data$survivalMonths >= t)
      })
    }
    
    all_data_points <- plotly_obj$x$data
    
    for (i in seq_along(clean_labels)) {
      trace_data <- all_data_points[[i]]
      
      if (!(exists("line", where = trace_data) && !is.null(trace_data$x))) {
        next
      }
      
      time_points <- trace_data$x
      n_at_risk <- get_n_at_risk(time_points, levels(data$strata_factor)[i], data)
      survival_probs <- trace_data$y
      
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
      
      plotly_obj$x$data[[i]]$text <- hover_text
      plotly_obj$x$data[[i]]$hoverinfo <- "text"
      plotly_obj$x$data[[i]]$name <- clean_labels[i]
    }
    
    plotly_obj %>%
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
  })
  
  # Separate Cox Models for each tab
  cox_model_km <- reactive({
    data <- filtered_dat_km()
    req(nrow(data) > 0)
    
    if (!is.numeric(data[[input$Strata]])) {
      data[[input$Strata]] <- as.factor(data[[input$Strata]])
    }
    
    coxph(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata)), data = data)
  })
  
  cox_model_hr <- reactive({
    data <- filtered_dat_hr()
    req(nrow(data) > 0)
    
    if (!is.numeric(data[[input$Strata_hr]])) {
      data[[input$Strata_hr]] <- as.factor(data[[input$Strata_hr]])
    }
    
    coxph(as.formula(paste("Surv(survivalMonths, mortality) ~", input$Strata_hr)), data = data)
  })
  
  output$risk_table <- render_gt({
    data <- filtered_dat_km()
    req(nrow(data) > 0)
    req(input$show_risk_table)
    
    strata_label <- switch(input$Strata,
                           "diagnosis" = "Diagnosis",
                           "histologyOriginal" = "Histology",
                           "gender" = "Gender",
                           "race" = "Race",
                           input$Strata)
    
    data[[input$Strata]] <- gsub("input\\$Strata=", "", data[[input$Strata]])
    
    fit <- survfit(Surv(survivalMonths, mortality) ~ get(input$Strata), data = data)
    
    tbl <- gtsummary::tbl_survfit(
      fit,
      times = c(0, 12, 24, 36, 48, 60),
      label_header = "{time} Months"
    ) %>%
      modify_table_body(
        ~.x %>%
          dplyr::mutate(
            label = gsub("input\\$Strata=", "", label)
          )
      ) %>%
      modify_header(label = strata_label)
    
    tbl %>% 
      gtsummary::as_gt() %>%
      gt::tab_header(title = "Risk Table") %>%
      gt::opt_row_striping() %>%
      gt::tab_options(
        table.font.size = "small",
        data_row.padding = gt::px(2)
      )
  })
  
  output$hazard_table <- render_gt({
    req(input$show_hr)
    
    data <- filtered_dat_hr()
    req(nrow(data) > 0)
    
    cox_summary <- summary(cox_model_hr())
    
    hr_vals <- exp(cox_summary$coefficients[, "coef"])
    ci_vals <- exp(confint(cox_model_hr()))
    p_vals  <- cox_summary$coefficients[, "Pr(>|z|)"]
    
    clean_labels <- gsub("^get\\(input\\$Strata_hr\\)", "", rownames(cox_summary$coefficients)) %>%
      gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%
      gsub("([0-9])([A-Za-z])", "\\1 \\2", .) %>%
      gsub("([A-Za-z])([0-9])", "\\1 \\2", .) %>%
      tools::toTitleCase()
    
    hr_table <- data.frame(
      "Group"         = clean_labels,
      "Hazard Ratio"  = round(hr_vals, 2),
      "95% CI Lower"  = round(ci_vals[, 1], 2),
      "95% CI Upper"  = round(ci_vals[, 2], 2),
      "P-Value"       = signif(p_vals, 3),
      check.names     = FALSE
    )
    
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
  
  output$hr_plot <- renderPlotly({
    hr_data <- broom::tidy(cox_model_hr(), exponentiate = TRUE, conf.int = TRUE) %>% 
      dplyr::mutate(term = gsub("_", " ", term))
    
    p <- ggplot(hr_data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
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
  
  output$log_rank_results <- renderText({
    data <- filtered_dat_km()
    req(nrow(data) > 0)
    
    surv_diff <- survdiff(
      Surv(survivalMonths, mortality) ~ get(input$Strata), 
      data = data
    )
    
    p_value <- 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
    
    paste0(
      "Log-rank test results:\n",
      "Chi-square statistic: ", round(surv_diff$chisq, 2), "\n",
      "Degrees of freedom: ", length(surv_diff$n) - 1, "\n",
      "P-value: ", format.pval(p_value, digits = 3)
    )
  })
}


