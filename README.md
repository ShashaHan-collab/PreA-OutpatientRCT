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
* scikit-learn
* datasets
* tqdm
* *(All other modules used, such as `itertools`, are part of the Python Standard Library.)*
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
  
  All datasets should be formatted in `jsonl` format with the following structure:
```jsonl
{"data": "[CLS]Field 1[SEP]Field 2[SEP]Field 3[SEP]...[/CLS]", "label": 0}
{"data": "[CLS]Field 1[SEP]Field 2[SEP]Field 3[SEP]...[/CLS]", "label": 1}
```
 The fields which be considered in training and analysis are combined in BERT format (using [SEP] token to split)
```
[CLS]Field 1[SEP]Field 2[SEP]Field 3[SEP]...[/CLS]
```

  - **across-domain-classification/**:
    Classification analysis across all clinical domains.
    
`train_CLS.py` for training and testing the vanilla BERT classification model;

`train_CLS.py` for training and testing the supervised SimCSE classification model;

`bootstrap.py` and `p_values.py` for one-sided bootstrap tests.

`domain_specific.py` for clinical domain-specific Uniform Manifold Approximation and Projection and Mann-Whitney U tests.

* **Source data**: All the data required to reproduce Figures 3 and 4, and Extended Data Fig. 1. 
* **Source plot**: Plotting code for generating the figures listed above. 

# Usage
## Data Visualization
Use the data in **Source data/** with the plotting code in **Source plot/** to regenerate Figures 3, 4, and Extended Data Figure 1.
## Classification Analysis
The **Classification analysis/** folder directory contains the code architecture and methodology used in our analysis. However, the actual clinical notes data are not included due to privacy concerns. Researchers can reference this code structure to understand our analytical approach.


