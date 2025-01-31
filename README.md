---
output:
  pdf_document: default
  html_document: default
---
# Transcriptomic Atlas of Nervous System Tumors

This Shiny app is designed to explore and analyze a large, annotated transcriptomic atlas of nervous system tumors and non-tumor entities. It provides three main functionalities: mRNA boxplots, dimensionality reduction, and survival analysis.

## Overview

The underlying dataset consists of 5,402 neoplastic and 1,973 non-neoplastic samples collected from public sources and processed using Applied Biosystems GeneChip for data uniformity. The raw data has been reprocessed, normalized, and harmonized to create a cohesive dataset.

This app draws inspiration from the glioVis R app and aims to provide similar functionality for exploring the nervous system tumor atlas.

## Features

### 1. mRNA Boxplots

- Visualize gene expression levels across different tumor types and non-tumor samples
- Compare expression distributions using boxplots
- Filter samples based on various clinical and demographic factors

### 2. Dimensionality Reduction

- Apply machine learning techniques (FIt-SNE, DBSCAN, OPTICS) to identify clusters by diagnosis
- Visualize high-dimensional data in 2D or 3D space
- Explore clustering patterns and relationships between tumor types

### 3. Survival Analysis

- Perform Kaplan-Meier survival analysis for different tumor groups
- Investigate the impact of gene expression on survival outcomes
- Compare survival curves across various clinical and molecular subgroups

## Usage

1. **Data Selection**: Choose the dataset and specific samples to analyze.
2. **Analysis Type**: Select the desired analysis type (mRNA boxplots, dimensionality reduction, or survival analysis).
3. **Parameters**: Adjust parameters specific to each analysis type.
4. **Visualization**: View the results in interactive plots and charts.
5. **Export**: Download results and figures for further analysis or presentation.

## Key Findings from the Underlying Dataset

- Clustering by diagnosis was achieved, with clusters primarily diagnosis-driven.
- DNA methylation's diagnostic uniqueness extends to transcriptomic data across nervous system neoplasms.
- The dataset includes rare tumors, spans all ages, and integrates samples worldwide, supporting broad comparative analyses.

## Applications

- Enables comparative gene expression analysis among nervous system neoplasms.
- Supports diagnostic refinement, especially in cases like pilocytic astrocytoma and ganglioglioma.
- Useful for exploring biological relationships between tumors and healthy tissues.

## Limitations

- Some diagnostic inconsistencies remain due to surrogate variables like geographic origin.
- High classifier accuracy may reflect potential overfitting; not yet suited for clinical application.

## Requirements

- R version 4.0 or higher
- Shiny package
- Additional R packages for specific analyses (e.g., ggplot2, survival, Rtsne, dbscan)

## Installation

1. Clone the repository:
   ```
   git clone https://github.com/nervous-system-tumor-atlas-shiny-app.git
   ```

2. Install required R packages:
   ```
   install.packages(c("shiny", "ggplot2", "survival", "Rtsne", "dbscan"))
   ```

3. Run the app:
   ```
   shiny::runApp("path/to/your/app")
   ```

## Contributing

Contributions are welcome! Please fork the repository and submit pull requests with your improvements or new features.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

- The transcriptomic atlas of nervous system tumors and non-tumor entities
- The glioVis R app for inspiration and guidance
- All researchers and institutions that contributed to the underlying dataset

