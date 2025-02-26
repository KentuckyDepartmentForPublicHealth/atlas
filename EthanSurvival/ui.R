#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Update ui.R - Remove the checkbox and reorganize the tabs

ui <- tagList(
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #ADD8E6 !important;
        border-bottom: 2px solid #33c4ff;
      }
      .navbar .navbar-brand {
        color: #000000 !important;
        font-weight: bold;
      }
      .navbar .navbar-nav .nav-link {
        color: #000000 !important;
      }
      .navbar .navbar-nav .nav-link:hover {
        color: #ff5733 !important;
      }
    "))
  ),
  
  navbarPage(
    title = "Atlas: Survival Analysis",
    
    # First tab - Kaplan Meier Curves
    tabPanel(
      "Kaplan Meier Curves",
      fluidPage(
        titlePanel("Kaplan Meier Curves with Simulated Dataset"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "diagnosis",
              label = "Select Diagnosis:",
              choices = c("All", unique_diagnosis),
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
              column(12, gt_output("risk_table"))
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
              choices = c("All", unique_diagnosis),
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
            checkboxInput("show_hr", "Display Hazard Ratios", value = TRUE),
            helpText("This tab displays the hazard ratios from a Cox proportional hazards model.")
          ),
          mainPanel(
            fluidRow(
              column(12, 
                     conditionalPanel(
                       condition = "input.show_hr",
                       gt_output("hazard_table")
                     )
              )
            ),
            fluidRow(
              column(12, 
                     conditionalPanel(
                       condition = "input.show_hr",
                       plotlyOutput("hr_plot")
                     )
              )
            )
          )
        )
      )
    )
  )
)