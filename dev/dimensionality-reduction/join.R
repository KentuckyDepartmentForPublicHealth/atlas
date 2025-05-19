# Load required libraries
library(dplyr)    # For data manipulation
library(tidyr)    # For pivot_longer
library(tibble)   # For rownames_to_column

# Assuming geneExpressionData, gene_annotations, and atlasDataClean are already loaded

# Process the gene expression data efficiently
gene_data_processed <- geneExpressionData %>%
  as.data.frame() %>%
  # Convert row names to a column 'ENTREZID' (matches gene_annotations)
  rownames_to_column(var = "ENTREZID") %>%
  # Rename all column names (except ENTREZID) to uppercase for consistency
  rename_with(toupper, -ENTREZID) %>%
  # Pivot to long format: faster than reshaping with base R
  pivot_longer(
    cols = -ENTREZID,         # All columns except ENTREZID
    names_to = "filename",    # Column names become 'filename' (now in ALL CAPS)
    values_to = "expression", # Expression values in a single column
    values_drop_na = TRUE     # Drop NA values to save memory
  ) %>%
  # Join with gene_annotations using ENTREZID (left join preserves all expression data)
  left_join(gene_annotations, by = "ENTREZID", relationship = "many-to-one") %>%
  # Join with atlasDataClean using filename
  left_join(atlasDataClean, by = "filename", relationship = "many-to-one") %>%
  # Optional: Remove rows with missing critical data (e.g., expression or annotations)
  filter(!is.na(expression))

# View the first few rows of the processed data
head(gene_data_processed)

# Optional: Check memory usage or dimensions for debugging
# print(object.size(gene_data_processed), units = "MB")
# dim(gene_data_processed)


# extra -----

    gene_data <- geneExpressionData %>%
      mutate(ENTREZID = rownames(.)) %>%
      rename_with(toupper, -ENTREZID) %>%
      pivot_longer(cols = -ENTREZID, names_to = "filename", values_to = "expression") %>%
      left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
      left_join(filter(atlasDataClean, !is.na(survivalMonths)), by = "filename")
    

# again -----
gene_data <- geneExpressionData %>%
  # Convert to tibble and preserve rownames as ENTREZID
  as_tibble(rownames = "ENTREZID") %>%
  # Rename columns to uppercase except ENTREZID
  rename_with(toupper, -ENTREZID) %>%
  # Join gene annotations by ENTREZID
  left_join(
    gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME),
    by = "ENTREZID"
  ) %>% 
  left_join(filter(atlasDataClean, !is.na(survivalMonths)), by = "filename")

# rebate -----
gene_data <- geneExpressionData %>%
  mutate(ENTREZID = rownames(.)) %>%
  rename_with(toupper, -ENTREZID) %>%
  pivot_longer(
    cols = -ENTREZID,
    names_to = "filename",
    values_to = "expression"
  ) %>%
  left_join(gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME), by = "ENTREZID") %>%
  left_join(filter(atlasDataClean, !is.na(survivalMonths)), by = "filename")



# 1) Convert your expression matrix to a tibble with a column for ENTREZID 
#    (assuming your rownames store the gene IDs)
gene_data <- geneExpressionData %>%
  tibble::as_tibble(rownames = "ENTREZID") %>% 
  # 2) Append your gene annotations by ENTREZID
  left_join(
    gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME),
    by = "ENTREZID"
  )

gene_data2 <- geneExpressionData %>%
  tibble::as_tibble(rownames = "ENTREZID") %>% 
  # 2) Append your gene annotations by ENTREZID
  inner_join(
    gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME),
    by = "ENTREZID"
  )

  identical(gene_data, gene_data2)


  library(dplyr)
library(tidyr)

gene_data_wide <- geneExpressionData %>%
  # Step 1: Put gene IDs in a proper column
  as_tibble(rownames = "ENTREZID") %>%
  
  # Step 2: Pivot from wide to long to make 'filename' a joinable column
  pivot_longer(
    cols      = -ENTREZID,     # everything except the gene ID
    names_to  = "filename",
    values_to = "expression"
  ) %>%
  
  # Step 3: Join gene annotations on ENTREZID
  left_join(
    gene_annotations %>% select(ENTREZID, SYMBOL, GENENAME),
    by = "ENTREZID"
  ) %>%
  
  # Step 4: Join sample-level metadata on filename
  left_join(
    filter(atlasDataClean, !is.na(survivalMonths)),
    by = "filename"
  ) %>%
  
  # Step 5: Pivot back to wide
  #         So we get one row per gene, and each sample’s expression & metadata
  #         become separate columns.
  pivot_wider(
    # which columns define each row (the keys)?
    id_cols     = c("ENTREZID", "SYMBOL", "GENENAME"),
    # how do we name the new columns?
    names_from  = "filename",
    # which columns become “values” that get spread out per-sample?
    # for example, maybe `expression` plus `survivalMonths`, etc.
    values_from = c("expression", "survivalMonths")
  )


########################### 
gene_data_annotated <- geneExpressionData %>%
  as_tibble(rownames = "ENTREZID") %>%
  inner_join(gene_annotations, by = "ENTREZID")# %>% 
#   left_join(filter(atlasDataClean, !is.na(survivalMonths)), by = "filename")

library(dplyr)
library(tidyr)

join_atlas_on_demand <- function(
  gene_data_wide,
  atlasDataClean,
  pivot_back = FALSE
) {
  # Identify which columns are gene-level metadata
  # so we don't pivot them into long by accident:
  # Assume everything except the expression columns is gene-level metadata.
  # For example, if you know the gene annotation columns by name, list them:
  gene_level_cols <- c("ENTREZID", "SYMBOL", "GENENAME") 
  # or any others that should remain "as-is" in each row when pivoting back
  
  # 1) Pivot your wide data long so `filename` becomes a column
  gene_data_long <- gene_data_wide %>%
    pivot_longer(
      cols         = -all_of(gene_level_cols),
      names_to     = "filename",
      values_to    = "expression"
    ) %>%
    
    # 2) Join the sample-level metadata by 'filename'
    left_join(atlasDataClean, by = "filename")
  
  # 3) If pivot_back=TRUE, return to wide format
  if (pivot_back) {
    gene_data_long %>%
      pivot_wider(
        id_cols      = all_of(gene_level_cols),
        names_from   = "filename",
        values_from  = c("expression", "survivalMonths", "any_other_sample_fields")
      )
  } else {
    # Otherwise, return the long form
    gene_data_long
  }
}

gene_data_long <- join_atlas_on_demand(
  gene_data_annotated,
  atlasDataClean,
  pivot_back = FALSE
)

#############################


# geneExpressionData <- geneExpressionData %>%
#   rename_with(toupper, everything()) 

#   glimpse(gene_data_processed)
#   glimpse(geneExpressionData)




####################################################################
###########################################################
## 1) Create a wide dataset with gene annotations appended
###########################################################
library(dplyr)
library(tidyr)

gene_data_annotated <- geneExpressionData %>%
  # Convert your expression matrix to a tibble, storing rownames as "ENTREZID"
  as_tibble(rownames = "ENTREZID") %>%
  # Join gene-level annotations by ENTREZID
  left_join(gene_annotations, by = "ENTREZID")

# --------------------------------------------------------
# At this point, 'gene_data_annotated' is wide:
#   - One row per gene
#   - Each sample is a column (named like the 'filename' IDs)
#   - Extra columns for SYMBOL, GENENAME, etc. from gene_annotations
# --------------------------------------------------------


###################################################################
## 2) Define a function to join atlasDataClean "on demand"
##    - It pivots your wide data long so 'filename' is a real column
##    - Then merges with atlasDataClean by 'filename'
##    - Optionally pivots back to wide
###################################################################
join_atlas_on_demand <- function(gene_data_annotated, atlasDataClean, pivot_back = FALSE) {
  # List the columns that describe each gene (so we don't pivot them):
  # Adjust if your gene_annotations have different column names
  gene_level_cols <- c("ENTREZID", "SYMBOL", "GENENAME")  
  
  # 1) Pivot from wide to long
  gene_data_long <- gene_data_annotated %>%
    pivot_longer(
      cols         = -all_of(gene_level_cols),  # pivot everything except gene metadata
      names_to     = "filename",
      values_to    = "expression"
    ) %>%
    # 2) Join with atlasDataClean by 'filename'
    left_join(filter(atlasDataClean, !is.na(survivalMonths)), by = "filename")
  
  # 3) Optionally pivot back to wide
  if (pivot_back) {
    # Here you must decide which atlasDataClean columns you want to "spread" back out
    # e.g., "survivalMonths", "someOtherField", etc.
    gene_data_long %>%
      pivot_wider(
        id_cols      = all_of(gene_level_cols),
        names_from   = "filename",
        values_from  = c("expression", "survivalMonths") 
        # Add more fields from atlasDataClean if needed
      )
  } else {
    # If you don't pivot back, you get a tidy (long) table: 1 row per (gene, filename)
    gene_data_long
  }
}

###########################################################
## 3) Use the function whenever you need sample-level data
###########################################################

# A) Get a LONG table (gene x sample), plus atlasDataClean columns
gene_data_long <- join_atlas_on_demand(
  gene_data_annotated,
  atlasDataClean,
  pivot_back = FALSE
)

# B) Or get a WIDE table (1 row per gene) with new columns for sample metadata
gene_data_wide_plus <- join_atlas_on_demand(
  gene_data_annotated,
  atlasDataClean,
  pivot_back = TRUE
)


###############################################################
# Sample Data Analysis

###############################################################
library(dplyr)
library(tidyr)

gene_data_wide_plus <- gene_data_annotated %>%
  # 1) Pivot to long format (one row per gene x sample)
  pivot_longer(
    # pivot all sample columns, i.e. everything except your gene-level columns
    cols = -c("ENTREZID", "SYMBOL", "GENENAME"),
    names_to = "filename",
    values_to = "expression"
  ) %>%
  # 2) Join with atlasDataClean on 'filename'
  left_join(atlasDataClean, by = "filename") %>%
  # 3) (Optional) Pivot back to wide to keep 1 row per gene
  pivot_wider(
    id_cols = c("ENTREZID", "SYMBOL", "GENENAME"),
    names_from = "filename",
    values_from = c("expression", "survivalMonths") 
    # ... or any other columns you want from atlasDataClean
  )



###### 9823579273508728570827387082757823 #################

library(dplyr)

link_sample_by_index <- function(
  gene_data_annotated,
  atlasDataClean,
  sample_index,
  gene_cols = c("ENTREZID", "SYMBOL", "GENENAME")
) {
  # 1) Figure out which columns in gene_data_annotated are "gene-level"
  #    (like ENTREZID, SYMBOL, GENENAME) vs. which are sample columns.
  all_cols    <- colnames(gene_data_annotated)
  sample_cols <- setdiff(all_cols, gene_cols)  # everything not in gene_cols
  
  # 2) Pick the sample column(s) by user-chosen index
  #    (e.g., sample_index = 2 picks the second sample column in sample_cols)
  chosen_sample <- sample_cols[sample_index]
  
  # 3) Build a data frame with the gene info + chosen sample’s expression
  out <- gene_data_annotated %>%
    select(all_of(gene_cols), all_of(chosen_sample)) %>%
    # rename the chosen sample column to something consistent like "expression"
    rename(expression = !!chosen_sample) %>%
    # create a new column "filename" with the sample column name
    mutate(filename = chosen_sample) %>%
    # 4) Join to atlasDataClean by filename
    left_join(atlasDataClean, by = "filename")
  
  return(out)
}


#### USAGE EXAMPLE ####

# Suppose you already created your wide data:
gene_data_annotated <- geneExpressionData %>%
  as_tibble(rownames = "ENTREZID") %>%
  inner_join(gene_annotations, by = "ENTREZID")

# Now, link the SECOND sample column to atlasDataClean
linked_single_sample <- link_sample_by_index(
  gene_data_annotated,
  atlasDataClean,
  sample_index = 233
)

# This yields a data frame with columns:
#   ENTREZID, SYMBOL, GENENAME, expression, filename, [plus columns from atlasDataClean]

link_samples_by_index <- function(
  gene_data_annotated,
  atlasDataClean,
  sample_indices,
  gene_cols = c("ENTREZID", "SYMBOL", "GENENAME")
) {
  all_cols    <- colnames(gene_data_annotated)
  sample_cols <- setdiff(all_cols, gene_cols)
  chosen_samples <- sample_cols[sample_indices]
  
  gene_data_annotated %>%
    select(all_of(gene_cols), all_of(chosen_samples)) %>%
    # Transform the chosen samples from wide to long
    tidyr::pivot_longer(
      cols = all_of(chosen_samples),
      names_to = "filename",
      values_to = "expression"
    ) %>%
    left_join(atlasDataClean, by = "filename")
}

XX <- link_samples_by_index(
  gene_data_annotated,
  atlasDataClean,
  sample_indices = c(2,5)
)