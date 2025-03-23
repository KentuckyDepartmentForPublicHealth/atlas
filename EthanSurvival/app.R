library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(gt)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
