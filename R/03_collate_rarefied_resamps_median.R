##============================================================
##	script to combine rarefied resamples and calculate mean
##	for analysis
library(dplyr)
library(tibble)
##============================================================
rm(list=ls())
setwd('/gpfs0/home/blowes/')
##	set the pattern to load the files to be compiled
filelist = dir(pattern="41.+Rdata")
nSimul = length(filelist)

##	initialise data.frames to store all rarefied resamples
rarefy_abund_all <- data_frame()
rarefy_biomass_all <- data_frame()
rarefy_pres_all <- data_frame()

for (iSimul in 1:199) {
	##	load a rarefied sample
	load(filelist[iSimul])
	print(iSimul)
	##	add a column describing measurement type
	rarefy_abund <- rarefy_abund %>% mutate(measurement_type = 'abundance')
	rarefy_biomass <- rarefy_biomass %>% mutate(measurement_type = 'biomass')
	rarefy_pres <- rarefy_pres %>% mutate(measurement_type = 'presence')	
	
	##	collate rarefied samples
	rarefy_abund_all <- bind_rows(rarefy_abund_all, rarefy_abund)
	rarefy_biomass_all <- bind_rows(rarefy_biomass_all, rarefy_biomass)
	rarefy_pres_all <- bind_rows(rarefy_pres_all, rarefy_pres)		
}

##	put them all together
rarefied_metrics <- bind_rows(rarefy_abund_all, rarefy_biomass_all, rarefy_pres_all)

##	pull out new metadata
new_meta <- rarefied_metrics %>%
	distinct(rarefyID, SamplePool, SampleN, num_years, duration, startYear, endYear)


##	calculate the medians for all the metrics
rarefied_medians <- ungroup(rarefied_metrics) %>%
	group_by(rarefyID, YEAR, cell) %>%
    dplyr::summarise(
      N = median(N),
      N_int = round(median(N)),
      S = median(S),	# as above?
      S_int = round(median(S)),
      PIE = median(PIE),
      ENSPIE = median(ENSPIE),
      ENSPIE_int = round(median(ENSPIE)),
      pielou = median(pielou),
      Hill1 = median(Hill1),
      MSA = median(MSA),
      Jaccard_base = median(Jaccard_base),
      Horn_base = median(Horn_base),
      Chao_base = median(Chao_base),
      Pearson_base = median(Pearson_base),
      Gains_base = median(Gains_base),
      Losses_base = median(Losses_base),
      Jbeta_base = median(Jbeta_base),
      Jtu_base = median(Jtu_base),
      Jne_base = median(Jne_base),
      Jaccard_next = median(Jaccard_next),
      Horn_next = median(Horn_next),
      Chao_next = median(Chao_next),
      Pearson_next = median(Pearson_next),
      Gains_next = median(Gains_next),
      Losses_next = median(Losses_next),
      Jbeta_next = median(Jbeta_next),
      Jtu_next = median(Jtu_next),
      Jne_next = median(Jne_next),
      Jaccard_hind = median(Jaccard_hind),
      Horn_hind = median(Horn_hind),
      Chao_hind = median(Chao_hind),
      Pearson_hind = median(Pearson_hind),
      Gains_hind = median(Gains_hind),
      Losses_hind = median(Losses_hind),
      Jbeta_hind = median(Jbeta_hind),
      Jtu_hind = median(Jtu_hind),
      Jne_hind = median(Jne_hind)) %>%
    ungroup()

##	recombine with new metadata
rarefied_medians <- inner_join(new_meta, rarefied_medians, by='rarefyID')

##	save
save(rarefied_medians, file=Sys.getenv('OFILE'))
