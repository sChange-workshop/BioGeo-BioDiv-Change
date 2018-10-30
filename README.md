# BioGeo-BioDiv-Change

This repository contains code necessary to replicate data analysis, figures, and tables for the sChange Working Group manuscript "Biodiversity trends are stronger in marine than terrestrial assemblages".

It is archived on Zenodo at : https://doi.org/10.5281/zenodo.1473861

We use the new BioTIME database [Dornelas et al. 2018](https://doi.org/10.1111/geb.12729) to analyze community time-series datasets that record species identity, abundance, and richness. Here, we synthesize millions of biodiversity records from taxa and biomes across the globe, to estimate rates of diversity change (species richness, ENSPIE) and species turnover (composition, abundance) through time. We ask:

1. Across the sites, what is the overall of species richness change, and the turnover component of Jaccard's dissimilarity?
2. What are the spatial distribution of patterns of biodiversity change, across realms, biomes, latitudinal bands and taxa?
3. (How) is richness change related to changes in community composition?

**Disclaimer:** The project and related code in this repository represent one version of the code developed for the project, and may yet undergo changes and revisions.

**Authors:** Shane Blowes and Sarah Supp will share first authorship. Maria Dornelas is senior author on the paper. 
Collaborators include the sChange Working Group, funded by iDiv in Leipzig, Germany:
Laura Antao, Amanda Bates, Helge Bruelheide, Jon Chase, Faye Moyes, Anne Magurran, Brian McGill, Isla Myers-Smith, Marten Winter, Anne Bjorkman, Diana Bowler, Jarrett E.K. Byrnes, Andrew Gonzalez, Jes Hines, Forest Isbell, Holly Jones, Laetitia M. Navarro, Patrick Thompson, Mark Vellend, Conor Waldock.

**Contacts:** 
Sarah Supp - sarah@weecology.org
Shane Blowes - sablowes@gmail.com

## Data 

The time series analysed were from 332 unique references found in the BioTIME dataset and in other studies which were used with permission.

Approximately 92% (306 references) of the biodiversity studies analysed here are available as part of the published BioTIME Database28. The data are openly available, and can be accessed on Zenodo (https://doi.org/10.5281/zenodo.1211105) or through the BioTIME website (http://biotime.st-andrews.ac.uk/).

Dornelas, M., L.H. Antao, F. Moyes, A.E. Bates, A.E. Magurran, and BioTIME consortium (200+ authors). 2018. BioTIME: a database of biodiversity time-series for the Anthropocene. Global Ecology and Biogeography. 10.1111/geb.12729 

The remaining 8% (26 references) of biodiversity studies analysed were used with permission. Some of these studies are published and publicly available outside of the BioTIME database, and others are available with permission from the corresponding author on reasonable request. For more details regarding the original citations, corresponding authors, and availability of these datasets, please refer to Table S1 in Dornelas et al. (2018). 


## R Analysis Files 

These scripts prepare the data for analysis. First (01), studies with large extents are consolidated into equal area grids, and studies with only one geographical location (or small extents) are similarly gridded. Species in these new cell-level time series are aggregated within years, and the coverage (or sample completeness) for each cell-year combination calculated, and all cell-year combinations with coverage < 0.85 are discarded. Second (02), we apply sample-based rarefaction to these new cell-level time series, and calculate diversity metrics (03) as the median of 199 bootstrap rarefactions. Finally (04), we rejoin our rarefied diversity metrics with metadata ready for fitting models.

Please note that the majority of the code herein was written to run on a HPC cluster.

* 01Study_to_Grid.R
* 02rarefy_griddedData_clusterVersion.R 
* 03collate_rarefied_resamps_median.R
* 04meta_join.R

**bt folder** - biome-taxa model
* **code**

This folder contains scripts that: (00) fit the biome-taxa models, (00a) examined diagnostics of model fit, (01) gets the coefficients from fitted models, (02) wrangles coefficient estimates and metadata ready for plotting, (03) plots maps and posterior densities of parameters of interest, (04) plots sensitivity analyses, (05) creates table of coefficient estimates.

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
    + 05_SI_tables.R
* **model_fits_output**

This Rdata object contains all of the coefficient estimates from the biome-taxa models.
    + BTRfyID_coef_ranef_160818.Rdata

**rct folder** - realm-climate-taxa model
* **code**

This folder contains scripts that: (00) fit the realm-climate-taxa models, (00a) examined diagnostics of model fit, (01) gets the coefficients from fitted models, (02) plots posterior densities of parameters of interest, (03) plots sensitivity analyses.

    + 00_HLM_Jne.R			
    + 00_HLM_Jtu.R			
    + 00_HLM_S.R		
    + 00a_nuts_diagnostics.R
    + 00b_rct_diagnosticPlots.R
    + 01_rct_getCoefs_remote.R
    + 03_rct_density_plot.R
    + 04_rct_sensitivity_plots.R
* **model_fits_output**

This Rdata object contains all of the coefficient estimates from the realm-climate-taxa models.
    + RCTRfyID_rarefyID_coef.Rdata


## Requirements
Program R version 3.3.1 or greater

Necessary Packages: bayesplot, brms (version 1.5.1 or greater), broom, dggridR, dplyr, ggplot2, ggridges, ggthemes, iNEXT, lazyeval, maps, maptools, MEOWr, purrr, readr, scales, tibble, tidyr, vegan
