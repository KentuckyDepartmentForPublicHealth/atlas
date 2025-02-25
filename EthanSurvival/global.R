#package import
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(shiny)
library(gt)
library(ggsurvfit)
library(plotly)
library(broom)
#data
#windows load
#load("~/atlas/dat/atlasDataClean.RData")

#mac load
load("~/Desktop/Survival Intern Project/Survival/atlas/dat/atlasDataClean.RData")


atlasDataClean <- subset(atlasDataClean,atlasDataClean$survivalMonths != "NA")

#fixing NAs so that they are properly reflected as censored observations
atlasDataClean$mortality <- ifelse(is.na(atlasDataClean$mortality), 0, atlasDataClean$mortality)

atlasDataClean$survivalMonths <- as.numeric(atlasDataClean$survivalMonths)
#strata
allowed_vars <- c("ageGroup", "tumorType", "grade", "sex", "locationOriginal","compartment","histologyOriginal")


unique_diagnosis <- unique(atlasDataClean$diagnosisFinal)

#Option below simply filters out diagnoses with less than 30 patients, one possible approach
#valid_diagnoses <- names(table(atlasDataClean$diagnosisFinal)[table(atlasDataClean$diagnosisFinal) > 30])
#unique_diagnosis <- c("All", valid_diagnoses)


#Another option w warning message display 
