# PreAOutpatientRCT
This repository contains the code classification analysis and data visualization for the paper: Tao, X., Zhou, S., Ding, K. et al. Integrating LLMs into real-world outpatient workflows for pre-specialist consultation: A randomized controlled clinical trial. Working paper (2025). 

# Prerequisites
## Prerequisite software 
* Python (version 3.7 or higher)
* R (version 4.0 or higher)
## Prerequisite Python packages
* transformers
* torch
* pandas
* numpy
* scipy
* umap-learn
* tqdm
* (All other modules used, such as `itertools`, are part of the Python Standard Library.)*
## Prerequisite R packages
* dplyr
* purrr
* glue
* tidyr
* ggplot2
* ggtext
* cowplot
* grid
* grImport2
* scales

# Descriptions of the files
* **Classification analysis**: Code for classification analysis on physician clinical notes.
  - **across-domain-classification/**:
    Classification analysis across all clinical domains
  - **domain-specific-classification/**:
    Classification analysis for individual clinical domains
* **Source data**: All the data required to reproduce Figures 3 and 4, and Extended Data Fig. 1. 
* **Source plot**: Plotting code for generating the figures listed above. 

# Usage
## Data Visualization
Use the data in **Source data/** with the plotting code in **Source plot/** to regenerate Figures 3, 4, and Extended Data Figure 1.
## Classification Analysis
The **Classification analysis/** folder directory contains the code architecture and methodology used in our analysis. However, the actual clinical notes data are not included due to privacy concerns. Researchers can reference this code structure to understand our analytical approach.


