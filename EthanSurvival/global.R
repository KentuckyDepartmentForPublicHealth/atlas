#package import
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(shiny)
library(gt)
library(ggsurvfit)

#data
load("~/atlas/dat/atlasDataClean.RData")
atlasDataClean <- subset(atlasDataClean,atlasDataClean$survivalMonths != "NA")
atlasDataClean$mortality <- ifelse(is.na(atlasDataClean$mortality), 0, atlasDataClean$mortality)

atlasDataClean$survivalMonths <- as.numeric(atlasDataClean$survivalMonths)
#strata
allowed_vars <- c("ageGroup", "tumorType", "grade", "sex", "locationOriginal","compartment","histologyOriginal")


unique_diagnosis <- unique(atlasDataClean$diagnosisFinal)

