# SIRIUS Merger Tool

[![License](https://img.shields.io/badge/License-MIT%202.0-blue.svg)](https://opensource.org/licenses/MIT)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/froz9/SIRIUS_merger_tool/graphs/commit-activity)
[![GitHub contributors](https://img.shields.io/github/contributors/froz9/SIRIUS_merger_tool.svg)](https://GitHub.com/froz9/SIRIUS_merger_tool/graphs/contributors/)
[![GitHub issues](https://img.shields.io/github/issues/froz9/SIRIUS_merger_tool.svg)](https://GitHub.com/froz9/SIRIUS_merger_tool/issues/)

## Overview

The **SIRIUS Merger Tool** (SIRIUS Output Analyzer) is an interactive R Shiny application developed to organize, merge, and visualize the primary output files from the [SIRIUS](https://bio.informatik.uni-jena.de/software/sirius/) software suite. 

This tool is specifically designed for chemoinformatics and metabolomics workflows, allowing users to effortlessly combine `canopus_structure_summary.tsv` and `structure_identifications.tsv` datasets. It provides rich, interactive visualizations of chemical classifications (ClassyFire and Natural Product Classifier) and generates combined data tables ready for downstream analysis.

🌐 **Live Demo:** [Try the SIRIUS Merger Tool online!](https://5bptzm-alan-hernandez.shinyapps.io/SIRIUS_merger_tool/)

## Key Features

* **Data Merging:** Seamlessly merges CANOPUS structure summaries with structure identifications based on their feature IDs.
* **Interactive Visualizations:** Generates high-quality Pie Charts and Sunburst plots for both **ClassyFire** (Superclass, Class, Subclass) and **NPC** (Pathway, Superclass, Class) ontologies using `plotly`.
* **Customizable Parameters:** * Filter merged datasets by Mass Accuracy Threshold (ppm).
  * Group minor features into an "Others" category to clean up visualizations.
  * Manually customize colors for Sunburst plot root elements.
* **Data Export:** Download fully merged datasets, ppm-filtered datasets, and individual chemical class summary tables as `.csv` files.
* **Custom Adduct Support:** Automatically integrates custom adduct calculations via `MetaboCoreUtils` if provided.

## Inputs Required

The app accepts standard `.tsv` output files directly from SIRIUS:
1. `canopus_structure_summary.tsv` (Required for plots and base summaries).
2. `structure_identifications.tsv` (Optional, required to unlock data merging and ppm filtering).

## Local Installation

If you prefer to run the app locally rather than using the web version, you will need [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed on your machine.
1. **Clone the repository:**
   ```bash
   git clone [https://github.com/froz9/SIRIUS_merger_tool.git](https://github.com/froz9/SIRIUS_merger_tool.git)
   cd SIRIUS_merger_tool

2. **Install required R packages:**
Open R or RStudio and run the following command to install the necessary dependencies:
```bash
install.packages(c("shiny", "data.table", "dplyr", "plotly", "DT", "colourpicker", "BiocManager"))
BiocManager::install("MetaboCoreUtils")
```
3. **Run the App:**
Open the app.R file in RStudio and click Run App, or execute the following in your R console:
```bash
shiny::runApp()
```
**Usage Instructions** 
* Upload Data: Navigate to the sidebar and upload your canopus_structure_summary.tsv file.
* Merge (Optional): Upload your structure_identifications.tsv file to calculate mass accuracy and merge the tables.
* Adjust Parameters: Use the sliders to set your desired ppm mass accuracy threshold and the minimum count for grouping minor features into "Others" on the pie charts.
* Explore: Navigate through the tabs to view interactive ClassyFire and NPC plots. You can adjust the image download resolution scales directly above the plots.
* Download: Go to the "Downloads" tab to retrieve your merged data, filtered data, or individual classification summary tables.

## Acknowledgments
This application was developed by Alan Hernandez.
This work was supported by the Universidad Nacional Autónoma de México (UNAM) Postdoctoral Program (Lab. 125, School of Chemistry).

## License
This project is licensed under the MIT License - see the LICENSE file for details.

For bug reports or feature requests, please open an issue on GitHub or contact the author at f9.alan@gmail.com.
