#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(shiny)

ui <- tagList(
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #ADD8E6 !important; /* Light blue background */
        border-bottom: 2px solid #33c4ff;   /* Optional accent border */
      }
      .navbar .navbar-brand {
        color: #000000 !important; /* Black text for title */
        font-weight: bold; /* Bold title */
      }
      .navbar .navbar-nav .nav-link {
        color: #000000 !important; /* Black text for tabs */
      }
      .navbar .navbar-nav .nav-link:hover {
        color: #ff5733 !important; /* Orange hover effect for links */
      }
    "))
  ),
  
  navbarPage(
    title = "Atlas: Survival Analysis",
    
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
              inputId = "Strata",
              label = "Select strata:",
              choices = allowed_vars,
              selected = allowed_vars[1]
            ),
            checkboxInput("show_hr", "Display Hazard Ratios", value = FALSE),
            checkboxInput("show_risk_table", "Show Risk Table", value = FALSE),
            checkboxInput("show_censoring", "Show Censoring Marks", value = FALSE)
          ),
          mainPanel(
            fluidRow(
              column(8, plotlyOutput("kmplt", width = "100%", height = "500px")),  # Larger KM plot
              column(4, gt_output("hazard_table"))  # Smaller Hazard Ratios table
            ),
            fluidRow(
              column(12, gt_output("risk_table"))  # Risk Table below, spanning full width
            )
          )
        )
      )
    ),
    tabPanel("Hazard Ratios",
             sidebarLayout(
               sidebarPanel(
                 helpText("This tab displays the hazard ratios from a Cox proportional hazards model.")
               ),
               mainPanel(
                 plotlyOutput("hr_plot"),   # Plotly-based hazard ratio plot
                 dataTableOutput("hazard_table") # Table with HR estimates
         )
      )
    )
  )
)