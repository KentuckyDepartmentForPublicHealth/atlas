# #package import
# library(survival)
# library(survminer)
# library(ggplot2)
# library(dplyr)
# library(shiny)
# library(gt)
# library(ggsurvfit)
# library(plotly)
# library(broom)
# library(gtsummary)
# library(cardx)

# message('inside global.R')
# #data
# # ajb load
# load("/home/adam/Sandbox/Shiny/atlas/dat/atlasDataClean.RData")

# #windows load
# # load("~/atlas/dat/atlasDataClean.RData")

# #mac load
# #load("~/Desktop/Atlas/Survival/atlas/dat/atlasDataClean.RData")


# atlasDataClean <- subset(atlasDataClean,atlasDataClean$survivalMonths != "NA")

# #fixing NAs so that they are properly reflected as censored observations
# atlasDataClean$mortality <- ifelse(is.na(atlasDataClean$mortality), 0, atlasDataClean$mortality)

# atlasDataClean$survivalMonths <- as.numeric(atlasDataClean$survivalMonths)


# ###########strata
# allowed_vars <- c("ageGroup", "tumorType", "grade", "sex")

# # Create a named vector for mapping strata variables to display names
# strata_labels <- c(
#   "Age Group" = "ageGroup",
#   "Tumor Type" = "tumorType",
#   "Sex" = "sex", 
#   "Grade" = "grade"
# )


# ##setting ref level for each strata
# atlasDataClean$ageGroup <- relevel(factor(atlasDataClean$ageGroup), ref = "40-60YRS")
# atlasDataClean$tumorType <- relevel(factor(atlasDataClean$tumorType), ref = "1")
# atlasDataClean$grade <- relevel(factor(atlasDataClean$grade), ref = "1")
# atlasDataClean$sex <- relevel(factor(atlasDataClean$sex), ref = "M")


# unique_diagnosis <- c( "DIFFUSE GLIOMA", "IDH MUTANT","PFA","MB-GP4","NEUROBLASTOMA","MENINGIOMA")
# unique_histology <- c("GBM","OD","OA","A","EPN","MB","NB","MEN")
# #Option below simply filters out diagnoses with less than 30 patients, one possible approach
# #valid_diagnoses <- names(table(atlasDataClean$diagnosisFinal)[table(atlasDataClean$diagnosisFinal) > 30])
# #unique_diagnosis <- c("All", valid_diagnoses)


