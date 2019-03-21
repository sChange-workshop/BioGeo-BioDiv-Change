##======================================================================
##  Hierarchical models: fit to either all studies (S, Jaccard), or only those with
##	count data (N, PIE, etc)
##======================================================================
rm(list=ls())

##	
#withr::with_libpaths(new = '/home/blowes/R/x86_64-pc-linux-gnu-library/3.3/', library(brms))
library(brms)
library(dplyr)
library(tidyr)
##======================================================================
##	alternatively load averaged metrics
load('/gpfs1/data/sChange/BioTime/data/rarefied_medians.Rdata')

##======================================================================		
##	count cells within realm/climate/taxa hierarchy
cell_count <- rarefied_medians %>%
        group_by(Biome, taxa_mod) %>%
        dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
        ungroup()

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, 
	by=c('Biome','taxa_mod'))

# need to set first year to zero dissimilarity for consecutive beta metrics
#rarefied_medians <- rarefied_medians %>%
#        mutate(Jtu_next = ifelse(YEAR==startYear, 0, Jtu_next))

##	filter to ecoregions with >3 cells
rarefied_medians <- rarefied_medians %>%
        filter(n_cells > 3 & BROAD_TYPE == 'count')

##	set some weakly regularising priors...
hier_prior <- c(set_prior(prior = 'normal(0,1)', class='b', coef='cYEAR'), 	# global slope
	set_prior(prior = 'normal(0,2)', class='Intercept', coef=''), 		# global intercept
	set_prior(prior = 'cauchy(0,2)', class='sd'),							# group-level intercepts and slopes
	set_prior(prior = 'lkj(2)', class='cor'))

Jtu_norm_BTSRfyID <- brm(bf(Jtu_base ~ cYEAR + (cYEAR|Biome/taxa_mod/STUDY_ID/rarefyID), 
	family = brmsfamily('gaussian')),	
	data= rarefied_medians,
	prior=hier_prior,
	inits = '0',
	iter = 2000,
	cores = 4,
	chains = 4)

save(Jtu_norm_BTSRfyID, file=Sys.getenv('OFILE'))
