


# libs --------------------------------------------------------------------


library(shiny)
library(bslib)
library(dplyr)
library(readr)


# colors ------------------------------------------------------------------

chfs <- list(
  cols2 = c('#95D3F5', '#0C3151'),
  cols3 = c('#62BCF0', '#01203D', '#84BC49'),
  cols4 = c('#5CB2E5', '#0C3151', '#305E4C', '#76AB48'),
  cols5 = c('#95D3F5', '#5CB2E5', '#0C3151', '#517F44', '#9FCA70'),
  cols6 = c('#95D3F5', '#5CB2E5', '#0C3151', '#305E4C', '#517F44', '#9FCA70'),
  cols7 = c('#95D3F5', '#5CB2E5', '#3A7CA6', '#0C3151', '#305E4C', '#517F44', '#9FCA70'),
  cols8 = c('#95D3F5', '#5CB2E5', '#3A7CA6', '#0C3151', '#00060C', '#305E4C', '#517F44', '#9FCA70'),
  cols9 = c('#5CB2E5', '#0C3151', '#305E4C', '#76AB48','#00060C', '#305E4C', '#517F44', '#76AB48', '#9FCA70')
)


# deploy ------------------------------------------------------------------

# currentDate <- format(Sys.time(), '%a, %b %d, %Y at %I:%M %p EDT')
# saveRDS(currentDate, file = 'dat/currentDate.rds')
currentDate <- readRDS(file = 'dat/currentDate.rds')

# rsconnect::deployApp(account = 'kdph', appName = 'atlas', appVisibility = 'private', logLevel = 'verbose')


# bootstrap variables -----------------------------------------------------


# Mytheme <- bs_theme(version = 5, bootswatch = "spacelab")
# bs_get_variables(Mytheme, all_bootstrap_vars)
# all_bootstrap_vars <- c(
#   # Colors
#   "primary", "secondary", "success", "info", "warning", "danger", "light", "dark",
#   "body-bg", "body-color", "link-color", "link-hover-color", "link-hover-decoration",
#   
#   # Typography
#   "font-family-base", "font-size-base", "font-weight-base", "line-height-base",
#   "headings-font-family", "headings-font-weight", "headings-line-height", 
#   "display1-size", "display2-size", "display3-size", "display4-size",
#   
#   # Spacing
#   "spacer", "spacers", "padding-x", "padding-y", "margin-x", "margin-y",
#   
#   # Borders & Radius
#   "border-width", "border-color", "border-radius", "border-radius-sm", "border-radius-lg",
#   
#   # Components
#   "btn-padding-y", "btn-padding-x", "btn-font-size", "btn-border-radius",
#   "input-padding-y", "input-padding-x", "input-font-size", "input-bg", "input-border-color",
#   
#   # Layout & Containers
#   "container-max-widths", "container-padding-x", "container-padding-y",
#   
#   # Breakpoints
#   "grid-breakpoints", "container-max-widths", "media-breakpoint-up", "media-breakpoint-down",
#   
#   # Shadows
#   "box-shadow-sm", "box-shadow", "box-shadow-lg",
#   
#   # Components (Dropdowns, Modals, etc.)
#   "dropdown-bg", "dropdown-border-color", "modal-bg", "modal-header-border-color",
#   
#   # Navbar
#   "navbar-padding-x", "navbar-padding-y", "navbar-bg", "navbar-color", "navbar-brand-font-size",
#   
#   # Cards
#   "card-spacer-y", "card-border-color", "card-border-radius", "card-header-border-color",
#   
#   # Alerts
#   "alert-padding-y", "alert-padding-x", "alert-margin-bottom", "alert-border-width", 
#   "alert-border-radius", "alert-bg-color", "alert-text-color", "alert-border-color"
# )

