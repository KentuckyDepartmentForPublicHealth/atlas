#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI using navbarPage for two tabs
ui <- navbarPage(
  title = "Atlas: Survival Analysis",
  # CSS to custom color top bar ---- had to generate template w chat gpt
  tags$head(tags$style(HTML("
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
  "))),
  # Tab 1
  tabPanel(
    "Kaplan Meier Curves",
    fluidPage(
      # Title
      titlePanel("Kaplan Meier Curves with Simulated Dataset"),
      
      # Sidebar with input
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = 'diagnosis',
            label = 'Select Diagnosis:', # What user sees
            choices = unique_diagnosis, # Choices are each unique value from the bmi variable
            selected = unique_diagnosis[1] # Default choice set to first index
          ),
          selectInput(
            inputId = 'Strata',
            label = 'Select strata:', # What user sees
            choices = allowed_vars, # Choices are each unique value from the bmi variable
            selected = allowed_vars[1] # Default choice set to first index
          ),
          checkboxInput("show_hr", "Display Hazard Ratios", value = FALSE)
        ),
        mainPanel(
          plotOutput("kmplt"), # Output for Kaplan-Meier plot
          verbatimTextOutput("hazard_ratios") # Output for hazard ratios
        )
      )
    )
  )
)



