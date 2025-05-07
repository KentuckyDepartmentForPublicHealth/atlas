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
library(rlang)

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


# Define a color palette generator function
get_color_palette <- function(mode, n_colors) {
    if (mode == "dark") {
        # Use a palette that works well on dark backgrounds
        # RColorBrewer provides excellent ready-made palettes
        # Combine multiple palettes if you need more colors
        if (n_colors <= 8) {
            return(RColorBrewer::brewer.pal(max(3, n_colors), "Set2"))
        } else if (n_colors <= 12) {
            return(RColorBrewer::brewer.pal(n_colors, "Set3"))
        } else {
            # For many colors, use a rainbow palette with higher saturation and value
            return(colorRampPalette(
                c("#FF8080", "#FFFF80", "#80FF80", "#80FFFF", "#8080FF", "#FF80FF")
            )(n_colors))
        }
    } else {
        # Light mode palette - more subdued colors
        if (n_colors <= 9) {
            return(RColorBrewer::brewer.pal(max(3, n_colors), "Set1"))
        } else if (n_colors <= 12) {
            return(RColorBrewer::brewer.pal(n_colors, "Paired"))
        } else {
            # For many colors, use a customized palette
            return(colorRampPalette(
                c(
                    "#7a3353", "#d4317e", "#e5688b", "#563a3f", "#ca7c30", "#93b030",
                    "#30b372", "#31aaac", "#3191cb", "#6a97da", "#5f77da", "#923072"
                )
            )(n_colors))
        }
    }
}

# deployrment --------------------------------------------------------------------

# setwd('./R/adam-test-apps/all-three-features')
# currentDate <- format(Sys.time(), '%a, %b %d, %Y at %I:%M %p ET')
# saveRDS(currentDate, file = paste0(getwd(), '/R/adam-test-apps/all-three-features/dat/currentDate.rds'))
# saveRDS(currentDate, file = "dat/currentDate.rds")
currentDate <- readRDS(file = "dat/currentDate.rds")

# rsconnect::deployApp(appName = "atlas-dev")