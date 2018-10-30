# BioGeo-BioDiv-Change

This repository contains code necessary to replicate data analysis, figures, and tables for the sChange Working Group manuscript "Biodiversity trends are stronger in marine than terrestrial assemblages".

We use the new BioTIME database [Dornelas et al. 2018](https://doi.org/10.1111/geb.12729) to analyze community time-series datasets that record species identity, abundance, and richness. Here, we synthesize millions of biodiversity records from taxa and biomes across the globe, to estimate rates of diversity change (species richness, ENSPIE) and species turnover (composition, abundance) through time. We ask:

    Across the sites, what is the overall of species richness change, and the turnover component of Jaccard's dissimilarity?
    What are the spatial distribution of patterns of biodiversity change, across realms-climate-taxa and across biomes-taxa?
    (How) is richness change related to changes in species turnover?

Disclaimer: The project and related code in this repository represent one version of the code developed for the project, and may yet undergo changes and revisions.

Authors: Shane Blowes and Sarah Supp will share first authorship. Maria Dornelas is senior author on the paper. 
Collaborators include the sChange Working Group, funded by iDiv in Leipzig, Germany:
Laura Antao, Amanda Bates, Helge Bruelheide, Jon Chase, Faye Moyes, Anne Magurran, Brian McGill, Isla Myers-Smith, Marten Winter, Anne Bjorkman, Diana Bowler, Jarrett E.K. Byrnes, Andrew Gonzalez, Jes Hines, Forest Isbell, Holly Jones, Laetitia M. Navarro, Patrick Thompson, Mark Vellend, Conor Waldock.

Contacts: 
Sarah Supp - sarah@weecology.org
Shane Blowes - sablowes@gmail.com

##Data 

The time series analysed were from 332 unique references found in the BioTIME dataset and in other studies which were used with permission.

Approximately 92% (306 references) of the biodiversity studies analysed here are available as part of the published BioTIME Database28. The data are openly available, and can be accessed on Zenodo (https://doi.org/10.5281/zenodo.1211105) or through the BioTIME website (http://biotime.st-andrews.ac.uk/).

Dornelas, M., L.H. Antao, F. Moyes, A.E. Bates, A.E. Magurran, and BioTIME consortium (200+ authors). 2018. BioTIME: a database of biodiversity time-series for the Anthropocene. Global Ecology and Biogeography. 10.1111/geb.12729 

The remaining 8% (26 references) of biodiversity studies analysed were used with permission. Some of these studies are published and publicly available outside of the BioTIME database, and others are available with permission from the corresponding author on reasonable request. For more details regarding the original citations, corresponding authors, and availability of these datasets, please refer to Table S1 in Dornelas et al. (2018). 


##R Analysis Files

    Study_to_Grid.R - inputs BioTIME database, filters on inclusion criteria, separates single location from multi location studies, and places all studies into equal-area icosahedron cell grid (res=12).
    rarefy_griddedData_all_metrics.R - further filters data based on coverage and inclusion criteria, calculates rarefied biodiversity metrics over 200 resamples for each study in each cell in each year. Outputs a new dataframe for time-series analysis of S, N, ENSPIE, and Jaccard's. Also saves subsetted versions of the dataframe for sensitivity analyses.
    rarefied_metrics_lm.R - inputs dataframe of rarefied yearly biodiveristy metrics, calculates biodiversity change (slopes) for each study, visually plots the results, and conducts a biogeographic hierarchical meta-analysis for diversity change patterns in magnitude and direction.
    regionalData_meta_lm.R - inputs dataframe of lm results, annotates with ecoregion datasets from TNC, and plots summary maps.
    meta_analysis1.R - inputs dataframe of lm results, tries a random-effects meta-analysis using metafor package.
    meta_analysis_of_ecoregion_correlations.R - inputs dataframe of lm results annotated with ecoregion data, uses metacor package to test the correlations of change in different metrics, by ecoregion.


Program R version 3.3.1 or greater

Necessary Packages: brms (version 1.5.1 or greater), broom, dggridR, dplyr, ggplot2, ggthemes, iNEXT, lazyeval, maps, maptools, MEOWr, metafor, purrr, readr, scales, tibble, tidyr, vegan

Data: BioTIME (version dated 24 February 2017, from shared folder on Dropbox, BioTIMELatest)
