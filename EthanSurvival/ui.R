ui <- tagList(
  tags$head(
    tags$style(HTML("
  /* Modern Dark Mode Navbar */
  .navbar {
    background-color: #1e1e2e !important;
    border-bottom: 1px solid #4d4d69;
    box-shadow: 0 2px 5px rgba(0,0,0,0.2);
    transition: all 0.3s ease;
  }
  .navbar .navbar-brand {
    color: #ffffff !important;
    font-weight: 600;
    letter-spacing: 0.5px;
  }
  .navbar .navbar-nav .nav-link {
    color: #c8c8d2 !important;
    margin: 0 5px;
    padding: 0.5rem 0.8rem;
    border-radius: 4px;
    transition: all 0.2s ease;
  }
  .navbar .navbar-nav .nav-link:hover {
    color: #ffffff !important;
    background-color: #2d2d3e;
    transform: translateY(-2px);
  }
  .navbar .navbar-toggler {
    border-color: rgba(255,255,255,0.3);
  }
  .navbar .navbar-toggler-icon {
    background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' width='30' height='30' viewBox='0 0 30 30'%3e%3cpath stroke='rgba(255, 255, 255, 0.7)' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e\");
  }
  
  /* Matching Sidebar/Input Panel Styling */
  .well, .panel, .shiny-input-panel, .sidebar {
    background-color: #262636 !important;
    border: 1px solid #4d4d69 !important;
    border-radius: 6px !important;
    box-shadow: 0 2px 5px rgba(0,0,0,0.15) !important;
    color: #c8c8d2 !important;
    padding: 15px !important;
    margin-bottom: 20px !important;
  }
  
  /* Input Controls Styling */
  .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background-color: #383852 !important;
    color: #ffffff !important;
    border: 1px solid #4d4d69 !important;
    border-radius: 4px !important;
    transition: all 0.2s ease;
  }
  
  .form-control:focus, .selectize-input.focus {
    border-color: #6272a4 !important;
    box-shadow: 0 0 0 0.2rem rgba(98, 114, 164, 0.25) !important;
  }
  
  /* Labels and Text */
  label, .control-label {
    color: #c8c8d2 !important;
    font-weight: 500 !important;
  }
  
  /* Buttons */
  .btn-default, .btn-primary {
    background-color: #44475a !important;
    color: #ffffff !important;
    border: 1px solid #6272a4 !important;
    border-radius: 4px !important;
    transition: all 0.2s ease;
  }
  
  .btn-default:hover, .btn-primary:hover {
    background-color: #6272a4 !important;
    transform: translateY(-1px);
    box-shadow: 0 2px 3px rgba(0,0,0,0.2);
  }
  
  /* Sliders and Other Controls */
  .js-irs-0 .irs-bar {
    background-color: #6272a4 !important;
    border-color: #6272a4 !important;
  }
  
  .js-irs-0 .irs-handle {
    background-color: #8be9fd !important;
    border-color: #6272a4 !important;
  }
  
  /* Tabs within sidebar if any */
  .nav-tabs {
    border-bottom: 1px solid #4d4d69 !important;
  }
  
  .nav-tabs > li > a {
    color: #c8c8d2 !important;
    background-color: #262636 !important;
    border: 1px solid #4d4d69 !important;
  }
  
  .nav-tabs > li.active > a, 
  .nav-tabs > li.active > a:focus, 
  .nav-tabs > li.active > a:hover {
    color: #ffffff !important;
    background-color: #383852 !important;
    border: 1px solid #6272a4 !important;
    border-bottom-color: transparent !important;
  }
  
  /* For a more fluid experience on different screen sizes */
  @media (max-width: 991.98px) {
    .navbar .navbar-nav .nav-link {
      padding: 0.7rem 1rem;
      margin: 2px 0;
    }
  }
"))
  ),
  
  navbarPage(
    title = "Atlas: Survival Analysis",
    
    # First tab - Kaplan Meier Curves (keeping original inputs)
    tabPanel(
      "Kaplan Meier Curves",
      fluidPage(
        titlePanel("Kaplan Meier Curves with Simulated Dataset"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "diagnosis",
              label = "Select Diagnosis:",
              choices = c("All", unique(atlasDataClean$diagnosis)),
              selected = "All"
            ),
            selectInput(
              inputId = "histology",
              label = "Select Histology:",
              choices = c("All", unique(atlasDataClean$histologyOriginal)),
              selected = "All"
            ),
            selectInput(
              inputId = "Strata",
              label = "Select strata:",
              choices = allowed_vars,
              selected = allowed_vars[1]
            ),
            checkboxInput("show_risk_table", "Show Risk Table", value = FALSE),
            checkboxInput("show_censoring", "Show Censoring Marks", value = FALSE)
          ),
          mainPanel(
            fluidRow(
              column(12, plotlyOutput("kmplt", width = "100%", height = "500px"))
            ),
            fluidRow(
              column(12, 
                     downloadButton("download_km_plot", "Download Plot"),
                     style = "margin-top: 10px; margin-bottom: 10px;"
              )
            ),
            fluidRow(
              column(12,
                     verbatimTextOutput("log_rank_results"),
                     style = "margin-top: 20px; margin-bottom: 20px;"
              )
            ),
            fluidRow(
              column(12, 
                     conditionalPanel(
                       condition = "input.show_risk_table == true",
                       gt_output("risk_table")
                     )
              )
            )
          )
        )
      )
    ),
    
    # Second tab - Hazard Ratios
    tabPanel(
      "Hazard Ratios",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "diagnosis_hr",
              label = "Select Diagnosis:",
              choices = c("All", unique(atlasDataClean$diagnosis)),
              selected = "All"
            ),
            selectInput(
              inputId = "histology_hr",
              label = "Select Histology:",
              choices = c("All", unique(atlasDataClean$histologyOriginal)),
              selected = "All"
            ),
            selectInput(
              inputId = "Strata_hr",
              label = "Select strata:",
              choices = allowed_vars,
              selected = allowed_vars[1]
            ),
            checkboxInput("show_hr_table", "Display Hazard Ratio Table", value = FALSE),
            helpText("This tab displays the hazard ratios from a Cox proportional hazards model.")
          ),
          mainPanel(
            fluidRow(
              column(12, 
                     plotlyOutput("hr_plot", height = "500px"),
                     downloadButton("download_hr_plot", "Download Plot"),
                     style = "margin-top: 10px"
              )
            ),
            fluidRow(
              column(12, 
                     conditionalPanel(
                       condition = "input.show_hr_table == true",
                       gt_output("hazard_table")
                     )
              )
            )
          )
        )
      )
    )  # Close second tabPanel
  )  # Close navbarPage
)  # Close tagList