


# libs --------------------------------------------------------------------


library(shiny)
library(bslib)
library(dplyr)
library(readr)

source('/home/adam/Sandbox/adam-utils/sysOpsLibsFuncs.R')
# wrangling ---------------------------------------------------------------

geneExpressionData <- readRDS('/mnt/KDPH/PQI/PQI Data Analytics/BrainTumorAtlas-main/Gene Expression Data/ExpData.rds')

atlasData <- read_csv('/mnt/KDPH/PQI/PQI Data Analytics/BrainTumorAtlas-main/Atlas_Data.csv')
