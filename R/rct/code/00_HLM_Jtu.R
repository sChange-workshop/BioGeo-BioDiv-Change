##======================================================================
##  Hierarchical models: fit to either all studies (S, Jaccard), or only those with
##	count data (N, PIE, etc)
##======================================================================
rm(list=ls())

##	
withr::with_libpaths(new = '/home/blowes/R/x86_64-pc-linux-gnu-library/3.3/brms', library(brms))
library(dplyr)
library(tidyr)
##======================================================================
##	alternatively load averaged metrics
load('/gpfs1/data/sChange/BioTime/data/rarefied_medians.Rdata')

##======================================================================		
##	remove rarefyID's with only one year
one_year_only <- rarefied_medians %>%
        group_by(rarefyID) %>%
        dplyr::summarise(duration = max(YEAR) - min(YEAR)) %>%
        filter(duration <= 1)

rarefied_medians <- rarefied_medians %>% filter(!(rarefyID %in% one_year_only$rarefyID))

##	create new covariate that is concatenation of realm_climate_taxa
rarefied_medians <- rarefied_medians %>%
	unite(rlm_clm_txa, REALM, climate_mod, taxa_mod, remove=FALSE)

##	count cells within realm/climate/taxa hierarchy
cell_count <- rarefied_medians %>%
	group_by(rlm_clm_txa) %>%
	dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
	ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by='rlm_clm_txa')  

##	filter to ecoregions with >3 cells and count data
rarefied_medians <- rarefied_medians %>%
	filter(n_cells > 3 & BROAD_TYPE=='count')
	
##	set some weakly regularising priors...
hier_prior <- c(set_prior(prior = 'normal(0,2)', class='b', coef='cYEAR'), 	# global slope
                set_prior(prior = 'normal(0,5)', class='Intercept', coef=''), 		        # global intercept
                set_prior(prior = 'cauchy(0, 2.5)', class='sd'),							            # group-level intercepts and slopes
                set_prior(prior = 'lkj(2)', class='cor'))                                 # parameter for covariance matrix

Jtu_norm_RCTRfyID <- brm(bf(Jtu_base ~ cYEAR + (cYEAR|rlm_clm_txa/rarefyID), 
	family = brmsfamily('gaussian')),	
	data= rarefied_medians,
	prior=hier_prior,
	inits = '0',
	control = list(adapt_delta=0.9),
	cores = 4)

save(Jtu_norm_RCTRfyID, file=Sys.getenv('OFILE'))
