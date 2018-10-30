##======================================================================
##  Hierarchical model of turnover component of Jaccard's
#   dissimilarity fit to studies with count data 
##======================================================================
rm(list=ls())

##	
#withr::with_libpaths(new = '/home/blowes/R/x86_64-pc-linux-gnu-library/3.3/', library(brms))
library(brms)
library(dplyr)
library(tidyr)
##======================================================================
##	load the rarefied data
load('/gpfs1/data/sChange/BioTime/data/rarefied_medians.Rdata')

##======================================================================		
##	count cells within biome/taxa hierarchy
cell_count <- rarefied_medians %>%
        group_by(Biome, taxa_mod) %>%
        dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
        ungroup()

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('Biome','taxa_mod'))

##	filter to groups with >3 cells
rarefied_medians <- rarefied_medians %>%
        filter(n_cells > 3)

##	set some weakly regularising priors...
hier_prior <- c(set_prior(prior = 'normal(0,2)', class='b', coef='cYEAR'), 	# global slope
	set_prior(prior = 'normal(0,5)', class='Intercept', coef=''), 		        # global intercept
	set_prior(prior = 'cauchy(0, 2.5)', class='sd'),							            # group-level intercepts and slopes
	set_prior(prior = 'lkj(2)', class='cor'))                                 # parameter for covariance matrix

Jtu_norm_BTRfyID <- brm(bf(Jtu_next ~ cYEAR + (cYEAR|Biome/taxa_mod/rarefyID), 
	family = brmsfamily('gaussian')),	
	data= rarefied_medians,
	prior=hier_prior,
	inits = '0',
	iter = 5000,
	warmup = 1500,
	control = list(adapt_delta=0.99),
	cores = 4,
	chains = 4)

save(Jtu_norm_BTRfyID, file=Sys.getenv('OFILE'))
