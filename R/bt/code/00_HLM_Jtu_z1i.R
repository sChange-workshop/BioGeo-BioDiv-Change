##  Hierarchical models: fit to either all studies (S, Jaccard), or only those with
##	count data (N, PIE, etc)
##======================================================================
rm(list=ls())

##	
library(brms)
library(dplyr)
##======================================================================
##	alternatively load averaged metrics
load('/gpfs1/data/sChange/BioTime/data/rarefied_medians.Rdata')
##======================================================================
##      count cells within Biome/taxa hierarchy, add STUDY_ID 
cell_count <- rarefied_medians %>%
        group_by(Biome, taxa_mod) %>%
        dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
        ungroup()

##      rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('Biome','taxa_mod'))

## filter to ecoregions with >3 cells
rarefied_medians <- rarefied_medians %>%
        filter(n_cells > 3)

##	set some priors...
hier_prior <- c(set_prior(prior = 'normal(0,1)', class='b', coef='cYEAR'), 	# global slope
	set_prior(prior = 'normal(0,2)', class='Intercept', coef=''), 		# global intercept
	set_prior(prior = 'cauchy(0,2.5)', class='sd'),							# group-level intercepts and slopes
	set_prior(prior = 'lkj(2)', class='cor'))

##	stan via brms
Jtu_z1i_realm_BTSRfyID_countData <- brm(bf(Jtu_base ~ cYEAR + (cYEAR|Biome/taxa_mod/STUDY_ID/rarefyID),
	zoi ~ REALM, coi ~ REALM, 
        family = brmsfamily('zero_one_inflated_beta')),       
        data= filter(rarefied_medians, BROAD_TYPE=='count'),
        prior=hier_prior,
        inits = '0',
        control = list(adapt_delta=0.99),
        cores = 4)

save(Jtu_z1i_realm_BTSRfyID_countData, file=Sys.getenv('OFILE'))
