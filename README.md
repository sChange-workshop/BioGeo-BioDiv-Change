# BioGeo-BioDiv-Change

This repository contains code necessary to replicate data analysis, figures, and tables for the sChange Working Group manuscript "The geography of biodiversity change in marine and terrestrial assemblages".

It is archived on Zenodo at : https://doi.org/10.5281/zenodo.1473861

We use the new BioTIME database [Dornelas et al. 2018](https://doi.org/10.1111/geb.12729) to analyze community time-series datasets that record species identity, abundance, and richness. Here, we synthesize millions of biodiversity records from assemblages and biomes across the globe, to estimate rates of diversity change (species richness, community composition) through time. We ask:

1. How are the total numbers of species and patterns of community composition within local assemblages changing through time?
2. What are the spatial patterns in the distribution of change? Across realms? Biomes? Latitudinal bands? Taxa?
3. What is the relationship between changes in species richness and changes in the components (i.e., turnover and nestedness) of compositional dissimilarity?

**Disclaimer:** The project and related code in this repository represent one version of the code developed for the project, and may yet undergo changes and revisions.

**Authors:**  This work was developed through collaboration initiated during sChange Working Group meetings funded by iDiv in Leipzig, Germany.

Shane Blowes and Sarah Supp share first authorship. Maria Dornelas is senior author on the paper.

Working group participants: Laura Antao, Amanda Bates, Helge Bruelheide, Jon Chase, Faye Moyes, Anne Magurran, Brian McGill, Isla Myers-Smith, Marten Winter, Anne Bjorkman, Diana Bowler, Jarrett E.K. Byrnes, Andrew Gonzalez, Jes Hines, Forest Isbell, Holly Jones, Laetitia M. Navarro, Patrick Thompson, Mark Vellend, Conor Waldock.

**Contacts:** 
* Shane Blowes - sablowes@gmail.com
* Sarah Supp - sarah@weecology.org
* Maria Dornelas - maadd@st-andrews.ac.uk 

## Data 

The time series analysed were from references found in the BioTIME dataset and in other studies which were used with permission.

Approximately 92% (306 references) of the biodiversity studies analysed here are available as part of the published BioTIME Database28. The data are openly available, and can be accessed on Zenodo (https://doi.org/10.5281/zenodo.1211105) or through the BioTIME website (http://biotime.st-andrews.ac.uk/).

Dornelas, M., L.H. Antao, F. Moyes, A.E. Bates, A.E. Magurran, and BioTIME consortium (200+ authors). 2018. BioTIME: a database of biodiversity time-series for the Anthropocene. Global Ecology and Biogeography. 10.1111/geb.12729 

The remaining 8% (26 references) of biodiversity studies analysed were used with permission. Some of these studies are published and publicly available outside of the BioTIME database, and others are available with permission from the corresponding author on reasonable request. For more details regarding the original citations, corresponding authors, and availability of these datasets, please refer to Table S1 in Dornelas et al. (2018). 


## R Analysis Files 

These scripts prepare the data for analysis, produce plots summarising the data, and simulate time series of compositional dissimilarity components. 

First (01), studies with large extents are broken up into equal area grids, and studies with only one geographical location (or small extents) are similarly gridded. Species in these new cell-level time series are aggregated within years, and the coverage (or sample completeness) for each cell-year combination calculated, and all cell-year combinations with coverage < 0.85 are discarded. Second (02), we apply sample-based rarefaction to these new cell-level time series, and calculate diversity metrics (03) as the median of 199 bootstrap rarefactions. (04) Rejoin rarefied diversity metrics with metadata ready for fitting models. (05) code to make plots for visual inspection of the data (e.g., Fig. S2). (06) Code to be sourced to calculate community composition metrics (to be called from script for simulations). (07) Code to simulate time series of compositional dissimilarity components (i.e., turnover and nestedness) by randomly sampling local assemblages from a constant regional species pool.

Please note that much of the code in this repository was written to run on a HPC cluster.

* 01_Study_to_Grid.R
* 02_rarefy_griddedData_clusterVersion.R 
* 03_collate_rarefied_resamps_median.R
* 04_meta_join.R
* 05_visualise_data.R
* 06_rarefysamplesturnoverbnh.R
* 07_sim_repeat_sampling.R

**bt folder** - biome-taxa model
* **code**

This folder contains scripts that: (00) fit the biome-taxa models, (00a) examined diagnostics of model fit, (01) gets the coefficients from fitted models, (02) wrangles coefficient estimates and metadata ready for plotting, (03) plots maps and posterior densities of parameters of interest, (04) plots sensitivity analyses, (05) plot coefficient estimates from models estimated with Gaussian error as function of models estimated with Beta error, (06) compare coefficients from BT and RCT (all models, BT, RRT and RCT are compared with a script in the RRT folder)

    + 00_HLM_Jne.R
    + 00_HLM_Jtu.R
    + 00_HLM_S.R
    + 00a_nuts_diagnostics.R
    + 00b_bt_diagnostic_plots.R
    + 01_bt_getCoefs_remote.R
    + 02a_bt_coef_wrangle.R
    + 02b_bt_alpha_beta_relationships_wrangle.R
    + 02c_bt_taxa_to_singleLocations.R  
    + 02d_bt_biome_map_wrangle.R
    + 03a_bt_plot_global_estimates.R
    + 03b_bt_biome_map_plot.R
    + 03c_bt_alpha_beta_plots.R
    + 03d_bt_density_ridge_plot.R
    + 04_bt_sensitivity_plots.R
    + 05_bt_beta_vs_norm.R
    + 06_bt_rct_new_models_coefCompare.R
* **model_fits_output**

This Rdata object contains all of the coefficient estimates from the biome-taxa models.

    + BT_model_coef_ranef.Rdata

**rct folder** - realm-climate-taxa model
* **code**

This folder contains scripts that: (00) fit the realm-climate-taxa models, (00a) examined diagnostics of model fit, (01) gets the coefficients from fitted models, (02) wrangle metadata for plotting coefficients, (03) plot posterior densities of parameters of interest, (04) plot sensitivity analyses.

    + 00_HLM_Jne.R			
    + 00_HLM_Jtu.R			
    + 00_HLM_S.R		
    + 00a_nuts_diagnostics.R
    + 01_rct_getCoefs.R
    + 02_rct_coefWrangle.R
    + 03_rct_posterior_density_plots.R
    + 04_rct_sensitivity_plots.R
    
* **model_fits_output**

This Rdata object contains all of the coefficient estimates from the realm-climate-taxa models.

    + rlm_clm_txa_modelCoefs.Rdata

**rrt folder** - realm-region-taxa model
* **code**

This folder contains scripts that: (00) assigned the Terrestrial and Freshwater cell-level time series to continents; retained Biome grouping structure for Marine time series, (01) fit the realm-region-taxa models, (02) gets the coefficients from fitted models, (03) wrangle metadata for plotting coefficients, (03) plot coefficient estimates, (04) compare slope estimates between BT, RRT and RCT models at multiple levels.

    + 00_continent_wrangle.R
    + 01_continent_jne.R
    + 01_continent_jtu.R			
    + 01_continent_s.R
    + 02_get_coef.R
    + 03_coef_plots.R
    + 04_model_coef_compare.R
    
* **model_fits_output**

This Rdata object contains all of the coefficient estimates from the realm-climate-taxa models.

    + rlm_reg_txa_coef_ranef.Rdata


## Requirements
Program R version 3.3.1 or greater

Necessary Packages: bayesplot, brms (version 1.5.1 or greater), broom, dggridR, dplyr, ggplot2, ggridges, ggthemes, iNEXT, lazyeval, maps, maptools, MEOWr, purrr, readr, scales, tibble, tidyr, vegan
