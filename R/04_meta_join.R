##--------------Script to be run after rarefaction: join rarefied_medians with:
## 1) biome meta data (these are the new Biomes from Faye)
## 2) old meta data (needed for taxa meta data)
## 3) do some tidying up: classify data types, remove time series with only 1 year,
##                        add covariates - centred year, logN, logENSPIE, logS,
##                        add comparison to first year to dissimilarity metrics, get spatial information
##                        for individual cells,

##--------------join new biome metadata with the rarefied_medians for modelling-----------------

##	libraries
library(tidyverse)
# rarefied metrics ready for modelling
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')
# new meta data
biomes <- read_csv('~/Dropbox/BiogeoBioTIME/rarefIDBiomeEco.csv')

##	filter the new biomes meta data to rarefyIDs that we have for modelling
biomes <- biomes %>% filter(rarefyID %in% rarefied_medians$rarefyID)

# join
rarefied_medians <- inner_join(biomes %>% select(rarefyID, Biome, Ecoregion, X9, eco3, eco4, ECOREGION, Distance, Manual), rarefied_medians, by='rarefyID')

# join with 'old' meta data too
meta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEmetadataJune.csv')
# meta <- read_csv('/home/sarah/Dropbox/BioTIMELatest/bioTIMEmetadataFeb.csv')
#str(meta)

##	merge with 'old' metadata with the rarefied metrics
rarefied_medians <- rarefied_medians %>% 
  separate(rarefyID, c("STUDY_ID", "cell2"), "_", remove=FALSE, convert=TRUE) %>%
  dplyr::select(-cell2)
rarefied_medians <- inner_join(meta, rarefied_medians, by='STUDY_ID')
rarefied_medians <- as_tibble(rarefied_medians)

##================================BEFORE MODEL FITTING======================================
#	group abundance into broad categories (as in the published version metadata)
rarefied_medians <- rarefied_medians %>%
  mutate(BROAD_TYPE = ifelse(ABUNDANCE_TYPE %in% c("Presence/Absence"), "presence",
                             ifelse(ABUNDANCE_TYPE %in% c("Count", "Density", "MeanCount"), "count",
                                    ifelse(is.na(ABUNDANCE_TYPE), "biomass", NA))))

##	remove rarefyID's with only one year
one_year_only <- rarefied_medians %>%
  group_by(rarefyID) %>% 
  summarise(duration = last(YEAR, order_by = YEAR) - first(YEAR, order_by = YEAR)) %>%
  filter(duration == 1)

rarefied_medians <- rarefied_medians %>% filter(!(rarefyID %in% one_year_only$rarefyID))


##	centre year so intercept is mean year of time series
##  add log-transformed responses for modelling
rarefied_medians <- rarefied_medians %>%
  mutate(cYEAR = YEAR - mean(YEAR),
         logS = log(S), 
         logN = log(N),
         logENSPIE = log(ENSPIE))

##	want coordinates of rarefyIDs (cells)
load('~/Dropbox/BiogeoBioTIME/rarefyID_cell_centre_011017.Rdata')

##	join 
rarefied_medians <- inner_join(rarefied_medians, select(rarefyID_cell_centre, -STUDY_ID), by='rarefyID')	


##	new: simplified taxa and climate (latitudinal) bands
rarefied_medians <- rarefied_medians %>%
  mutate(  		
    # simplified taxa
    taxa_mod = ifelse((TAXA=='Marine invertebrates' | TAXA=='Terrestrial invertebrates' | TAXA=='Freshwater invertebrates'),
                      'Invertebrates',
                      ifelse((TAXA=='Marine plants' | TAXA=='Terrestrial plants' | TAXA=='Freshwater plants'),
                             'Plant', TAXA)),
    # simplified climate/latitudinal bands	
    climate_mod = ifelse(abs(rarefyID_y) > 60, 'Polar',
                         ifelse(abs(rarefyID_y) < 23.5, 'Tropical', 'Temperate')))

##	add turnover comparisons of first year to itself: similarity==1, dissimiliarity==0 
##	also, add indicator variable for complete turnover (similarity==0)
rarefied_medians <- rarefied_medians %>%
  group_by(rarefyID) %>%
  mutate(
    ##	for the 'base' comparison, set first year of time series to 1 (or 0)
    Jaccard_base = ifelse(YEAR==startYear, 1, Jaccard_base),
    Horn_base = ifelse(YEAR==startYear, 1, Horn_base),
    Chao_base = ifelse(YEAR==startYear, 1, Chao_base),
    # these are dissimilarity, so first year comparison to itself is zero
    Jbeta_base = ifelse(YEAR==startYear, 0, Jbeta_base),
    Jtu_base = ifelse(YEAR==startYear, 0, Jtu_base),
    Jne_base = ifelse(YEAR==startYear, 0, Jne_base),
    # create some binary covariates (for complete dissimilarity, or a 100% contribution of nestedness or
    # turnover to Jaccard)
    Jsim_01 = ifelse(Jaccard_base==0, 1, 0),			# perfect dissimilarity
    Jbeta_01 = ifelse(Jbeta_base==1, 1, 0), 			# perfect dissimilarity
    Jtu_01 = ifelse(Jtu_base==Jbeta_base, 1, 0),		# 100% contribution of turnover (species replacement)
    Jne_01 = ifelse(Jne_base==Jbeta_base, 1, 0)) %>%	# 100% contribution of nestedness ()
  ungroup()

setwd('~/Dropbox/BiogeoBioTIME/')
save(rarefied_medians, file = 'rarefied_medians.Rdata')