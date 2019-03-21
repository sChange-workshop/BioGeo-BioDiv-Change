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
# cell count for a hierarchy that includes study
cell_count <- rarefied_medians %>%
        group_by(Biome, taxa_mod) %>%
        dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
        ungroup()

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('Biome','taxa_mod'))

##	filter to ecoregions with >3 cells
rarefied_medians <- rarefied_medians %>%
	ungroup() %>%
	filter(BROAD_TYPE == 'count') %>%
        filter(n_cells > 3)

##	set some weakly regularising priors...
hier_prior <- c(set_prior(prior = 'normal(0,0.2)', class='b', coef='cYEAR'), 	# global slope
	set_prior(prior = 'normal(0,1)', class='Intercept', coef=''), 		# global intercept
	#set_prior(prior = 'exponential(1)', class = 'sd'),
	set_prior(prior = 'lkj(1)', class='cor')
)

S_pois_BTSRfyID_countData <- brm(bf(S ~ cYEAR + (cYEAR|Biome/taxa_mod/STUDY_ID/rarefyID), 
	family = brmsfamily('poisson')),	
	data= rarefied_medians,
	prior=hier_prior,
	iter = 2000,
	warmup = 1000,
	inits = '0',
	#init_r = 0.001,
	control = list(adapt_delta=0.95),
	cores = 4,
	chains = 4)

save(S_pois_BTSRfyID_countData, file=Sys.getenv('OFILE'))
