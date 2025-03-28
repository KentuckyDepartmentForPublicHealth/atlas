library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(shiny)
library(gt)
library(ggsurvfit)
library(plotly)
library(broom)
library(gtsummary)
library(cardx)

message("inside global.R")
# data
# ajb load
load("/home/adam/Sandbox/Shiny/atlas/dat/atlasDataClean.RData")

# windows load
# load("~/atlas/dat/atlasDataClean.RData")

# mac load
# load("~/Desktop/Atlas/Survival/atlas/dat/atlasDataClean.RData")


atlasDataClean <- subset(atlasDataClean, atlasDataClean$survivalMonths != "NA")

# fixing NAs so that they are properly reflected as censored observations
atlasDataClean$mortality <- ifelse(is.na(atlasDataClean$mortality), 0, atlasDataClean$mortality)

atlasDataClean$survivalMonths <- as.numeric(atlasDataClean$survivalMonths)


########### strata
allowed_vars <- c("ageGroup", "tumorType", "grade", "sex")

# Create a named vector for mapping strata variables to display names
strata_labels <- c(
  "Age Group" = "ageGroup",
  "Tumor Type" = "tumorType",
  "Sex" = "sex",
  "Grade" = "grade"
)


# ## setting ref level for each strata
# atlasDataClean$ageGroup <- relevel(factor(atlasDataClean$ageGroup), ref = "40-60YRS")
# atlasDataClean$tumorType <- relevel(factor(atlasDataClean$tumorType), ref = "1")
# atlasDataClean$grade <- relevel(factor(atlasDataClean$grade), ref = "1")
# atlasDataClean$sex <- relevel(factor(atlasDataClean$sex), ref = "M")


# new
## setting ref level for each strata
# Create a helper function to safely relevel factors
safe_relevel <- function(x, ref) {
  x_factor <- factor(x)
  if (ref %in% levels(x_factor)) {
    return(relevel(x_factor, ref = ref))
  } else {
    warning(paste(
      "Reference level", ref, "not found in factor. Available levels:",
      paste(levels(x_factor), collapse = ", ")
    ))
    return(x_factor)
  }
}

# Setting ref level for each strata with error handling - FIXED REFERENCE LEVELS
atlasDataClean$ageGroup <- safe_relevel(atlasDataClean$ageGroup, "40-60YRS")
atlasDataClean$tumorType <- safe_relevel(atlasDataClean$tumorType, "PRIMARY") # Changed from "1" to "PRIMARY"
atlasDataClean$grade <- safe_relevel(atlasDataClean$grade, "GRADE 1") # Changed from "1" to "GRADE 1"
atlasDataClean$sex <- safe_relevel(atlasDataClean$sex, "MALE") # Changed from "M" to "MALE"


unique_diagnosis <- c("DIFFUSE GLIOMA", "IDH MUTANT", "PFA", "MB-GP4", "NEUROBLASTOMA", "MENINGIOMA")
unique_histology <- c("GBM", "OD", "OA", "A", "EPN", "MB", "NB", "MEN")
# Option below simply filters out diagnoses with less than 30 patients, one possible approach
# valid_diagnoses <- names(table(atlasDataClean$diagnosisFinal)[table(atlasDataClean$diagnosisFinal) > 30])
# unique_diagnosis <- c("All", valid_diagnoses)

ui <- tagList(
  tags$head(
    tags$style(HTML("
      /* Global Background and Text */
      body, html {
        background-color: #171723 !important;
        color: #e4e4e6 !important;
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', sans-serif;
        line-height: 1.6;
      }
      
      /* Main content area */
      .container-fluid, .tab-content {
        background-color: #171723 !important;
      }
      
      /* Modern Navbar with Better Contrast */
      .navbar {
        background-color: #13141f !important;
        border-bottom: 2px solid #4361ee !important;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      
      .navbar .navbar-brand {
        color: #ffffff !important;
        font-weight: 700;
        letter-spacing: 0.5px;
      }
      
      .navbar .navbar-nav .nav-link {
        color: #e4e4e6 !important;
        font-weight: 500;
        padding: 8px 16px;
        margin: 0 3px;
        border-radius: 6px;
        transition: all 0.2s ease;
      }
      /* For navbarPage tab headers */
      .navbar-default .navbar-nav > li > a {
        color: white !important;
      }
      
      .navbar .navbar-nav .nav-link:hover {
        color: #ffffff !important;
        background-color: #4361ee;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(67, 97, 238, 0.3);
      }
      
      /* Fix for active navbar tab background */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background: linear-gradient(90deg, #4c6ef5, #63b3ed) !important;
        color: white !important;
        border-radius: 6px !important;
      }
      
      /* Style for inactive navbar tabs */
      .navbar-default .navbar-nav > li > a {
        background-color: transparent !important;
        color: #e4e4e6 !important;
      }
      
      /* Clean Sidebar */
      .sidebar {
        background-color: #1e2030 !important;
        border-right: none !important;
        box-shadow: 2px 0 10px rgba(0, 0, 0, 0.1);
        padding: 20px !important;
      }
      
      /* Panel and Well Elements */
      .well, .panel, .shiny-input-panel {
        background-color: #252a41 !important;
        border: 1px solid #313552 !important;
        border-radius: 10px !important;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1) !important;
        padding: 20px !important;
        margin-bottom: 25px !important;
      }
      
      /* Output Containers - Improved contrast for plots */
      .shiny-plot-output, .html-widget, .shiny-bound-output {
        background-color: #2e3450 !important;
        border-radius: 8px !important;
        border: 1px solid #3b4168 !important;
        padding: 15px !important;
        margin-bottom: 20px !important;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15) !important;
      }
      
      /* Fix for Plotly visibility - with better background contrast */
      .plotly .main-svg {
        background-color: transparent !important;
      }
      
      .plotly .bg {
        fill: #2e3450 !important;
      }
      
      /* Plotly paper (background of the actual plot area) */
      .plotly .plotbg {
        fill: #353b5e !important;
      }
      
      /* Plotly axis and grid lines */
      .plotly .xtick text, .plotly .ytick text, .plotly .gtitle {
        fill: #ffffff !important;
        font-weight: 500 !important;
      }
      
      .plotly .xgrid, .plotly .ygrid {
        stroke: rgba(255, 255, 255, 0.1) !important;
      }
      
      .plotly .zerolinelayer path {
        stroke: rgba(255, 255, 255, 0.3) !important;
        stroke-width: 1px !important;
      }
      
      /* Input Controls with Better Contrast */
      .form-control, .selectize-input, .selectize-control.single .selectize-input {
        background-color: #2d3250 !important;
        color: #ffffff !important;
        border: 1px solid #3b4168 !important;
        border-radius: 6px !important;
        padding: 10px 12px !important;
        box-shadow: inset 0 1px 3px rgba(0, 0, 0, 0.1) !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #4361ee !important;
        box-shadow: 0 0 0 3px rgba(67, 97, 238, 0.25) !important;
      }
      
      /* Enhanced Dropdown Menu Styling */
      .selectize-dropdown, .selectize-dropdown.form-control {
        background-color: #353b5e !important;
        border: 1px solid #4d5785 !important;
        border-radius: 6px !important;
        box-shadow: 0 6px 16px rgba(0, 0, 0, 0.25) !important;
        margin-top: 2px !important;
        z-index: 1000;
      }
      
      .selectize-dropdown .option {
        background-color: #353b5e !important;
        color: #ffffff !important;
        padding: 10px 12px !important;
        border-bottom: 1px solid rgba(255, 255, 255, 0.05) !important;
        transition: all 0.2s ease;
      }
      
      .selectize-dropdown .option:last-child {
        border-bottom: none !important;
      }
      
      .selectize-dropdown .option:hover, 
      .selectize-dropdown .option.active {
        background-color: #4361ee !important;
        color: #ffffff !important;
      }
      
      .selectize-dropdown .option.selected {
        background-color: rgba(67, 97, 238, 0.4) !important;
        color: #ffffff !important;
        font-weight: 500 !important;
      }
      
      /* Improve styling for select dropdown carets */
      .selectize-control.single .selectize-input::after {
        border-color: #ffffff transparent transparent transparent !important;
      }
      
      .selectize-control.single .selectize-input.dropdown-active::after {
        border-color: transparent transparent #ffffff transparent !important;
      }
      
      /* Make placeholder text more visible */
      .selectize-input::placeholder, 
      .form-control::placeholder, 
      .selectize-input input::placeholder {
        color: rgba(255, 255, 255, 0.6) !important;
      }
      
      /* Additional styling for dropdown search input */
      .selectize-dropdown-content {
        max-height: 300px !important;
      }
      
      .selectize-control.plugin-input_autogrow .selectize-input.has-items input {
        color: #ffffff !important;
      }
      
      /* Dropdown headers if any */
      .selectize-dropdown .optgroup-header {
        color: #a3a8c3 !important;
        background-color: #2d3250 !important;
        font-weight: 600 !important;
        padding: 8px 12px !important;
      }
      
      /* Bootstrap Select Styling */
      .bootstrap-select .dropdown-menu {
        background-color: #353b5e !important;
        border: 1px solid #4d5785 !important;
        box-shadow: 0 6px 16px rgba(0, 0, 0, 0.25) !important;
      }
      
      .bootstrap-select .dropdown-menu li a {
        color: #ffffff !important;
      }
      
      .bootstrap-select .dropdown-menu li a:hover,
      .bootstrap-select .dropdown-menu li.selected a {
        background-color: #4361ee !important;
        color: #ffffff !important;
      }
      
      /* Labels with Enhanced Readability */
      label, .control-label {
        color: #ffffff !important;
        font-weight: 500 !important;
        font-size: 14px !important;
        margin-bottom: 8px !important;
        letter-spacing: 0.3px;
      }
      
      /* Modern Buttons */
      .btn-default, .btn-primary {
        background: linear-gradient(135deg, #4361ee, #3a56d4) !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 10px 20px !important;
        font-weight: 500 !important;
        transition: all 0.3s ease;
        box-shadow: 0 4px 6px rgba(67, 97, 238, 0.2);
      }
      
      .btn-default:hover, .btn-primary:hover {
        background: linear-gradient(135deg, #3a56d4, #2a46c4) !important;
        transform: translateY(-2px);
        box-shadow: 0 6px 12px rgba(67, 97, 238, 0.3);
      }
      
      .btn-default:active, .btn-primary:active {
        transform: translateY(1px);
        box-shadow: 0 2px 3px rgba(67, 97, 238, 0.2);
      }
      
   /* For active tab - use the blue color from your buttons/navbar */
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:focus, 
    .nav-tabs > li.active > a:hover {
     color: white !important;
     background: linear-gradient(135deg, #4361ee, #3a56d4) !important;
     border-color: #4361ee !important;
     border-bottom-color: transparent !important;
    }
    
    /* For hover effects on tabs */
  .nav-tabs > li > a:hover {
  background-color: rgba(67, 97, 238, 0.7) !important; /* Semi-transparent version of #4361ee */
  border-color: rgba(67, 97, 238, 0.7) !important;
  }
    /* Tab container bottom border */
    .nav-tabs {
    border-bottom: 1px solid #4361ee !important;
  }
      
      /* Sliders with Better Visibility */
      .js-irs-0 .irs-bar, .irs-bar {
        background-color: #4361ee !important;
        border-color: #4361ee !important;
      }
      
      .js-irs-0 .irs-handle, .irs-handle {
        background-color: #ffffff !important;
        border-color: #4361ee !important;
        box-shadow: 0 0 5px rgba(67, 97, 238, 0.5) !important;
      }
      
      .irs-min, .irs-max, .irs-from, .irs-to, .irs-single {
        background-color: #353b5e !important;
        color: #ffffff !important;
      }
      
      /* Table Improvements */
      .gt_table, .dataTable, table {
        background-color: #252a41 !important;
        color: #e4e4e6 !important;
        border-radius: 8px !important;
        overflow: hidden !important;
        border: 1px solid #313552 !important;
      }
      
      .gt_table th, .dataTable th, table th {
        background-color: #1e2030 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
        border-bottom: 2px solid #3b4168 !important;
        padding: 12px 16px !important;
      }
      
      .gt_table td, .dataTable td, table td {
        padding: 12px 16px !important;
        border-top: 1px solid #313552 !important;
      }
      
      .gt_table tr:hover td, .dataTable tr:hover td, table tr:hover td {
        background-color: #2d3250 !important;
      }
      
      /* General Text Styling */
      h1, h2, h3, h4, h5, h6, p, span, div {
        color: #e4e4e6 !important;
      }
      
      /* Scrollbar Styling */
      ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #1e2030;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #3b4168;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #4361ee;
      }
      
      /* Special fix to force white background elements to dark */
      .shiny-output-error, .dataTables_wrapper {
        color: #e4e4e6 !important;
        background-color: #171723 !important;
      }
    
      
      /* Add custom class for brighter plots if needed */
      .brighter-plot {
        padding: 15px;
        background-color: #414973 !important; 
        border-radius: 10px;
      }
      
      /* Fix for select2 dropdowns if used */
      .select2-dropdown {
        background-color: #353b5e !important;
        border: 1px solid #4d5785 !important;
        color: #ffffff !important;
      }
      
      .select2-results__option {
        color: #ffffff !important;
        padding: 10px 12px !important;
      }
      
      .select2-results__option--highlighted[aria-selected] {
        background-color: #4361ee !important;
        color: #ffffff !important;
      }
      
      .select2-selection__rendered {
        color: #ffffff !important;
      }
      
      /* Fix for ShinyWidgets dropdowns */
      .sw-dropdown-content, .sw-dropdown-in {
        background-color: #353b5e !important;
        border: 1px solid #4d5785 !important;
        color: #ffffff !important;
        box-shadow: 0 6px 16px rgba(0, 0, 0, 0.25) !important;
      }
      
      .sw-dropdown-item {
        color: #ffffff !important;
        padding: 10px 12px !important;
      }
      
      .sw-dropdown-item:hover, .sw-dropdown-item.selected {
        background-color: #4361ee !important;
        color: #ffffff !important;
      }
      /* Log Rank Text Output Styling */
      .shiny-text-output, .shiny-verbatim-text-output {
      color: #ffffff !important;
      background-color: #353b5e !important;
      padding: 15px !important;
      border-radius: 8px !important;
      border: 1px solid #4d5785 !important;
      font-family: 'Consolas', 'Monaco', monospace !important;
      white-space: pre-wrap !important;
      font-size: 14px !important;
      line-height: 1.5 !important;F
      }
      /* Button styling */
    .btn {
      border-radius: 6px;
      font-weight: 500;
      padding: 0.5rem 1.25rem;
      transition: all 0.2s ease;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      font-size: 0.85rem;
    }

    .btn-primary {
      background: linear-gradient(90deg, #4c6ef5, #63b3ed);
      border: none;
      color: #ffffff;
    }

    .btn-primary:hover {
      background: linear-gradient(90deg, #5a78f2, #72c3f9);
      transform: translateY(-1px);
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
    }

    /* Download button styling - fix for double icon */
    .download-button-custom .fa {
      display: none;  /* Hide the default FontAwesome icon */
    }

    .download-button-custom .glyphicon {
      display: none;  /* Hide the default glyphicon */
    }
    
    /* Update tab styling */
    .tabbable > .nav > li > a {
      background-color: #252a41 !important;
      color: #e4e4e6 !important;
      border: 1px solid #313552 !important;
      margin-right: 4px;
    }
    
    .tabbable > .nav > li[class=active] > a {
      background: linear-gradient(90deg, #4c6ef5, #63b3ed) !important;
      color: white !important;
      border-color: #4c6ef5 !important;
    }
    
    .tabbable > .nav > li > a:hover {
      background: rgba(76, 110, 245, 0.5) !important;
      color: white !important;
    }
    
      /* Log Rank Results Styling */
      #log_rank_results, #pairwise_logrank {
        color: #ffffff !important;
        background-color: #353b5e !important;
        padding: 15px !important;
        border-radius: 8px !important;
        border: 1px solid #4d5785 !important;
        font-family: 'Consolas', 'Monaco', monospace !important;
        white-space: pre-wrap !important;
        font-size: 14px !important;
        line-height: 1.5 !important;
        margin: 10px 0 !important;
        width: 100% !important;
        display: block !important;
      }
    "))
  ),
  
fluidPage(
    titlePanel("Atlas: Survival Analysis"),
    
    fluidRow(
      # Left column for all controls
      column(3,
        wellPanel(
          style = "position: sticky; top: 20px;",  # Makes sidebar sticky
          selectInput(
            inputId = "diagnosis",
            label = "Select Diagnosis:",
            choices = c("All", unique_diagnosis),
            selected = "All"
          ),
          selectInput(
            inputId = "histology",
            label = "Select Histology:",
            choices = c("All", unique_histology),
            selected = "All"
          ),
          selectInput(
            inputId = "Strata",
            label = "Select strata:",
            choices = strata_labels,
            selected = strata_labels[1]
          ),
          checkboxInput("show_risk_table", "Show Risk Table", value = FALSE),
          checkboxInput("show_censoring", "Show Censoring Marks", value = FALSE),
          checkboxInput("show_pairwise", "Show Pairwise Comparisons", value = FALSE)
        )
      ),
      
      # Right column for all visualizations
      column(9,
        # Kaplan-Meier section
        div(
          style = "margin-bottom: 40px;",
          h3("Kaplan-Meier Analysis", 
             style = "color: #ffffff; margin-bottom: 20px;"),
          plotlyOutput("kmplt", height = "500px"),
          div(
            style = "margin: 15px 0;",
            downloadButton("download_km_plot", "Download KM Plot")
          ),
          verbatimTextOutput("log_rank_results"),
          conditionalPanel(
            condition = "input.show_risk_table == true",
            div(
              style = "margin-top: 20px;",
              gt_output("risk_table")
            )
          )
        ),
        
        # Hazard Ratio section
        div(
          style = "margin-top: 40px; padding-top: 20px; border-top: 1px solid #4d5785;",
          h3("Hazard Ratio Analysis", 
             style = "color: #ffffff; margin-bottom: 20px;"),
          plotlyOutput("hr_plot", height = "500px"),
          div(
            style = "margin: 15px 0;",
            downloadButton("download_hr_plot", "Download HR Plot")
          ),
          conditionalPanel(
            condition = "input.show_hr_table == true",
            div(
              style = "margin-top: 20px;",
              gt_output("hazard_table")
            )
          )
        )
      )
    )
  )
)
