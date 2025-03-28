library(dplyr)
library(tidyr)

# Load datasets
ExpData <- readRDS("~/atlas/Atlas/atlas-data/Gene Expression Data/ExpData.rds")
gene_annotations <- load("~/atlas/dat/annotations.RData")
atlasDataClean <- load("~/atlas/dat/atlasDataClean.RData")

# Step 1: Convert gene_exp from wide to long format but keep only necessary info
gene_exp_long <- gene_exp %>%
  tibble::rownames_to_column(var = "Entrez_ID") %>%
  pivot_longer(cols = -Entrez_ID, names_to = "filename", values_to = "Expression_Value") %>%
  select(Entrez_ID, filename) %>%  # Only keep necessary columns
  distinct()  # Remove duplicates

# Step 2: Merge to get Gene Names, avoiding duplication
gene_exp_long <- gene_exp_long %>%
  
library(tibble)
library(DOSE)

ExpData$Entrez_ID <- rownames(ExpData)
rownames(ExpData) <- NULL  # Remove row names





# Ensure Entrez_ID is a column if it's stored as row names
if (!"Entrez_ID" %in% colnames(ExpData)) {
  ExpData <- ExpData %>% rownames_to_column(var = "Entrez_ID")
}

# Convert ExpData from wide to long format
ExpData_long <- ExpData %>%
  pivot_longer(cols = -Entrez_ID, names_to = "filename", values_to = "Expression_Value") %>%
  select(Entrez_ID, filename) %>%  # Keep only necessary columns
  distinct()  # Avoid row explosion

# Merge to get Gene Names
ExpData_long <- ExpData_long %>%
  left_join(gene_annotations, by = "Entrez_ID") %>%
  select(filename, Gene_Name) %>%
  distinct()  # Remove duplicates

# Aggregate gene names per filename
ExpData_agg <- ExpData_long %>%
  group_by(filename) %>%
  summarise(Gene_Names = paste(unique(Gene_Name), collapse = "; "))

# Merge with atlasDataClean
final_data <- atlasDataClean %>%
  left_join(ExpData_agg, by = "filename")

# View the result
head(final_data)
