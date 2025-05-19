library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(gt)

message('inside app.R')

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
