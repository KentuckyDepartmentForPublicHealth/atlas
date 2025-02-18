#package import
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(shiny)
library(ggsurvfit)


#data
load("~/atlas/dat/atlasDataClean.RData")
atlasDataClean <- subset(atlasDataClean,atlasDataClean$survivalMonths != "NA" & atlasDataClean$mortality != "NA")
atlasDataClean$survivalMonths <- as.numeric(atlasDataClean$survivalMonths)
#strata
allowed_vars <- c("ageGroup", "tumorType", "grade", "sex", "locationOriginal","compartment","histologyOriginal")


unique_diagnosis <- unique(atlasDataClean$diagnosis)

