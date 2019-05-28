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
load('/gpfs1/data/sChange/BioTime/data/rarefied_medians_continents.Rdata')

##======================================================================		
##	create new covariate that is concatenation of region and taxa (with or without realm)
rarefied_medians <- rarefied_medians %>%
  unite(rlm_region_taxa, REALM, region, taxa_mod, remove=FALSE)

# rarefied_medians <- rarefied_medians %>%
#   unite(region_taxa, region, taxa_mod, remove=FALSE)

##	count cells within realm/climate/taxa hierarchy
cell_count <- rarefied_medians %>%
  group_by(rlm_region_taxa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by='region_taxa')  

##	filter to ecoregions with >3 cells
rarefied_medians <- rarefied_medians %>%
  filter(n_cells > 3 & BROAD_TYPE=='count')

##	set some weakly regularising priors...
hier_prior <- c(set_prior(prior = 'normal(0,0.2)', class='b', coef='cYEAR'), 	# global slope
	set_prior(prior = 'normal(0,1)', class='Intercept', coef=''), 		# global intercept
	#set_prior(prior = 'exponential(1)', class = 'sd'),
	set_prior(prior = 'lkj(1)', class='cor')
)

S_pois_RTSRfyID_countData <- brm(bf(S ~ cYEAR + (cYEAR|rlm_region_taxa/STUDY_ID/rarefyID), 
	family = brmsfamily('poisson')),	
	data= rarefied_medians,
	prior=hier_prior,
	iter = 4000,
	warmup = 1000,
	#thin = 10,
	inits = '0',
	#init_r = 0.001,
	control = list(adapt_delta=0.95),
	cores = 8,
	chains = 8)

#n_cell = '3+'

save(S_pois_RTSRfyID_countData, file=Sys.getenv('OFILE'))
