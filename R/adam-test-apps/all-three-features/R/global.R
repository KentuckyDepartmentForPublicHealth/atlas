# Load libraries
library(shiny)
library(bslib)
library(survival)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(tidyr)
library(shinyalert)
library(rlang)
library(forcats)
library(httr)
library(jsonlite)

# Create a reactive value to track if gene expression data is loaded
# gene_data_loaded <- reactiveVal(FALSE)

# Create a placeholder for the gene expression data
geneExpressionData <- NULL

# Load the data (assuming it's available in the environment)
# load("../../../dat/atlasDataClean.RData")
# load("../../../dat/geneExpressionData.RData")
# load("../../../dat/annotations.RData")
load("dat/atlasDataClean.RData")
# load("dat/geneExpressionData.RData")
load("dat/annotations.RData")
# Assume geneExpressionData, gene_annotations, and go_to_genes_list are also loaded

# Define your color palettes
tumor_colors_during_light_mode <- c(
    "#7a3353", "#d4317e", "#e5688b", "#563a3f", "#e5838c", "#cc3051",
    "#e59580", "#864630", "#e58a5d", "#a86a30", "#ca7c30", "#5d4620",
    "#e5ad83", "#c59b30", "#7a6330", "#c6b8a6", "#ccab30", "#c6bc7f",
    "#7d7a20", "#44442a", "#93b030", "#438620", "#93cb84", "#30b372",
    "#9bc5ab", "#30705d", "#30b3b1", "#306066", "#31aaac", "#6bc7e2",
    "#3191cb", "#92afda", "#6a97da", "#306196", "#305288", "#5f77da",
    "#4a3766", "#4c3d54", "#e277da", "#843084", "#e285d3", "#923072",
    "#e2a5be"
)

# Define a completely different color palette for dark mode to make it obvious
tumor_colors_during_dark_mode <- c(
    "#FF8080", "#80FF80", "#8080FF", "#FFFF00", "#FF00FF", "#00FFFF",
    "#FFA500", "#00FF00", "#FF00FF", "#FFFF00", "#00FFFF", "#FF8000",
    "#80FF00", "#0080FF", "#FF0080", "#FFFF80", "#FF80FF", "#80FFFF",
    "#FFC080", "#80FFC0", "#C080FF", "#FFC0FF", "#C0FFC0", "#C0C0FF",
    "#FFFFC0", "#FFC0FF", "#C0FFFF", "#FFE0C0", "#C0FFE0", "#E0C0FF",
    "#FFE0FF", "#E0FFE0", "#E0E0FF", "#FFFFE0", "#FFE0FF", "#E0FFFF",
    "#FFF0E0", "#E0FFF0", "#F0E0FF", "#FFE0F0", "#F0FFE0", "#E0F0FF"
)

# time --------------------------------------------------------------------


# currentDate <- format(Sys.time(), '%a, %b %d, %Y at %I:%M %p ET')
# saveRDS(currentDate, file = paste0(getwd(), '/R/adam-test-apps/all-three-features/dat/currentDate.rds'))
currentDate <- readRDS(file = "dat/currentDate.rds")
getwd()