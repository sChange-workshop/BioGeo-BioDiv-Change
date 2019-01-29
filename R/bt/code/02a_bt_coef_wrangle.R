# code to clean model coefficients
##------load data and libraries, wrangling for plotting-----------------------
rm(list=ls())

library(brms)
library(tidyverse)
library(broom)

##-----load coefficient estimates with new biomes--------------
load('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/model_fits_output/BTRfyID_coef_ranef_160818.Rdata')
# plus the coefficients and random effects from the beta regression models 
load('~/Desktop/BT_betaR_coef_ranef.Rdata')

# join beta regression estimates to the others
BT_global_estimates <- bind_rows(BT_global_estimates, BT_betaR_global_estimates)
BT_biome_estimate <- bind_rows(BT_biome_estimate, BT_betaR_biome_estimate)
BT_taxa_estimate <- bind_rows(BT_taxa_estimate, BT_betaR_taxa_estimate)
BTRfyID_rarefyID_coef <- bind_rows(BTRfyID_rarefyID_coef, BT_betaR_rarefyID_coef)

BT_biome_ranef <- bind_rows(BT_biome_ranef, BT_betaR_biome_ranef)
BT_Taxa_ranef <- bind_rows(BT_Taxa_ranef, BT_betaR_taxa_ranef)
BT_rarefyID_ranef <- bind_rows(BT_rarefyID_ranef, BT_betaR_rarefyID_ranef)
##----- get raw data and process for metadata--------
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')  

cell_count <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

biome_cell_count <- rarefied_medians %>%
  group_by(Biome) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('Biome', 'taxa_mod'))

##	filter to count data and biome/taxa combinations with >3 cells
# these are the data that the model was fit to
rarefied_medians <- rarefied_medians %>%
  filter(BROAD_TYPE=='count' & n_cells > 3) %>%
  # simplify realm covariate for plotting
  mutate(Realm2 = ifelse(REALM!='Marine', 'Terrestrial/Freshwater', 'Marine'))

##  get time periods for each level of the model (biome, taxa, etc, for cleaner plots of 
# regression lines with data)
allYears <- rarefied_medians %>%
  filter(BROAD_TYPE=='count' & n_cells>3) %>%
  summarise(start = min(cYEAR),
            stop = max(cYEAR),
            start2 = min(YEAR),
            stop2 = max(YEAR))

# this ignores the time slice models
BT_global_estimates <- BT_global_estimates %>%
  mutate(start = allYears$start,
         stop = allYears$stop,
         start2 = allYears$start2,
         stop2 = allYears$stop2)

biome_years <- rarefied_medians %>%
  filter(BROAD_TYPE=='count') %>%
  group_by(Biome) %>%
  summarise(start = min(cYEAR),
            stop = max(cYEAR),
            start2 = min(YEAR),
            stop2 = max(YEAR)) %>%
  ungroup()

## biome/taxa years
BT_years <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  summarise(start = min(cYEAR),
            stop = max(cYEAR),
            start2 = min(YEAR),
            stop2 = max(YEAR)) %>%
  ungroup()

# years for each cell
cell_years <- rarefied_medians %>%
  filter(BROAD_TYPE=='count' & n_cells>3) %>%
  group_by(Biome, taxa_mod, rarefyID) %>%
  summarise(start = min(cYEAR),
            stop = max(cYEAR),
            start2 = min(YEAR),
            stop2 = max(YEAR)) %>%
  ungroup()


##  get realm and climate (if unique) information for biomes
realm_biome <- rarefied_medians %>% distinct(Biome, REALM)
biome_climate <- rarefied_medians %>% distinct(climate_mod, Biome) # some biomes have multiples climate / latitude bands...
# get unique rarefyIDs for each biome and their geographic centre
coords <- rarefied_medians %>% distinct(Biome, rarefyID, rarefyID_x, rarefyID_y)

##  get realm, biome and climate (if unique) information for individual taxa (within biomes)
realm_biome <- rarefied_medians %>% distinct(Biome, taxa_mod, REALM)
realm_climate_biome <- rarefied_medians %>% distinct(Biome, climate_mod, taxa_mod, REALM)

## realm/biome/taxa/rarefyID metadata
BTRfyID_meta <- rarefied_medians %>%
  distinct(REALM, Biome, taxa_mod, rarefyID, SamplePool, SampleN, num_years, duration, startYear, endYear)


##-------wrangle global level estimates-----------
BT_global_estimates$model_clean <- BT_global_estimates$model
BT_global_estimates$model_clean <- factor(BT_global_estimates$model_clean, 
                                          levels = c('S_pois', 'Jbeta_norm', 'Jtu_norm', 'Jtu_z1i', 'Jne_norm', 'Jne_zi', 'N_lognorm', 'ENSPIE_pois'),
                                          labels = c('Species richness', 'Total dissimilarity',  'Turnover component', 'Turnover component (z1i)', 'Nestedness component',
                                                     'Nestedness component (zi)', 'Abundance (N)', 'ENSPIE'))

##----wrangle biome level-------------
# combine cell countss and realm meta data with biome estimates 
BT_biome_estimate <- inner_join(biome_cell_count, BT_biome_estimate, by='Biome')
BT_biome_estimate <- inner_join(realm_biome, BT_biome_estimate, by='Biome')

##  combine years of biome-level times series with biome coefficients
BT_biome_estimate <- inner_join(biome_years, BT_biome_estimate, by='Biome') 



##  expand the biome_estimates_only to include the coordinates of the cells where we had time series
BT_biome_estimate <- inner_join(BT_biome_estimate, coords, by='Biome')
BT_biome_estimate$model_clean <- BT_biome_estimate$model
BT_biome_estimate$model_clean <- factor(BT_biome_estimate$model_clean, 
                                        levels = c('S_pois', 'Jbeta_norm', 'Jtu_norm', 'Jtu_z1i', 'Jne_norm', 'Jne_zi', 'N_lognorm', 'ENSPIE_pois'),
                                        labels = c('Species richness', 'Total dissimilarity',  'Turnover component', 'Turnover component (z1i)', 'Nestedness component',
                                                   'Nestedness component (zi)', 'Abundance (N)', 'ENSPIE'))


BT_biome_estimate <- BT_biome_estimate %>%
  mutate(realm2 = ifelse(REALM=='Marine', 'Marine', 'Terrestrial/Freshwater'))


##-----wrangle taxa level---------
BT_taxa_estimate <- BT_taxa_estimate %>%
  separate(level, c('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 't'), remove=F, sep='_') %>% 
  mutate(taxa_mod = ifelse(!is.na(t), t,
                           ifelse(!is.na(b7), b7,
                                  ifelse(!is.na(b6), b6,
                                         ifelse(!is.na(b5), b5,
                                                ifelse(!is.na(b4), b4,
                                                       ifelse(!is.na(b3), b3, b2)))))),
         b7 = ifelse(b7==taxa_mod, '', b7),                                           
         Biome = ifelse(is.na(b3), b1,
                        ifelse(is.na(b4), paste(b1, b2, sep='_'),
                               ifelse(is.na(b5), paste(b1, b2, b3, sep='_'),
                                      ifelse(is.na(b6), paste(b1, b2, b3, b4, sep='_'),
                                             ifelse(is.na(b7), paste(b1, b2, b3, b4, b5, sep='_'),
                                                    paste(b1, b2, b3, b4, b5, b6, b7, sep='_'))))))) %>%
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -t)

# fix one name
BT_taxa_estimate$Biome <- ifelse(BT_taxa_estimate$Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests_', 
                                 'Tropical_and_Subtropical_Moist_Broadleaf_Forests', BT_taxa_estimate$Biome)  

##  combine cell counts of biome/taxa with taxa estimates for plotting
BT_taxa_estimate <- inner_join(rarefied_medians %>% distinct(REALM, Biome, taxa_mod, n_cells), 
                               BT_taxa_estimate, by=c('Biome', 'taxa_mod'))

##  new realm to combine freshwater and terrestrial
BT_taxa_estimate <- BT_taxa_estimate %>%
  mutate(Realm2 = ifelse(REALM!='Marine', 'Terrestrial/Freshwater', 'Marine'))

##  combine years with biome/taxa coefficients
BT_taxa_estimate <- inner_join(BT_years, BT_taxa_estimate, by=c('Biome', 'taxa_mod'))   

BT_taxa_estimate$model_clean <- BT_taxa_estimate$model
BT_taxa_estimate$model_clean <- factor(BT_taxa_estimate$model_clean, 
                                       levels = c('S_pois', 'Jbeta_norm', 'Jtu_norm', 'Jtu_z1i', 'Jne_norm', 'Jne_zi', 'N_lognorm', 'ENSPIE_pois'),
                                       labels = c('Species richness', 'Total dissimilarity',  'Turnover component', 'Turnover component (z1i)', 'Nestedness component',
                                                  'Nestedness component (zi)', 'Abundance (N)', 'ENSPIE'))
# repeat for ranef dataframe
BT_Taxa_ranef <- BT_Taxa_ranef %>%
  mutate(level = Taxa) %>%
  select(-Taxa) %>%
  separate(level, c('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 't'), remove=F, sep='_') %>% 
  mutate(taxa_mod = ifelse(!is.na(t), t,
                           ifelse(!is.na(b7), b7,
                                  ifelse(!is.na(b6), b6,
                                         ifelse(!is.na(b5), b5,
                                                ifelse(!is.na(b4), b4,
                                                       ifelse(!is.na(b3), b3, b2)))))),
         b7 = ifelse(b7==taxa_mod, '', b7),                                           
         Biome = ifelse(is.na(b3), b1,
                        ifelse(is.na(b4), paste(b1, b2, sep='_'),
                               ifelse(is.na(b5), paste(b1, b2, b3, sep='_'),
                                      ifelse(is.na(b6), paste(b1, b2, b3, b4, sep='_'),
                                             ifelse(is.na(b7), paste(b1, b2, b3, b4, b5, sep='_'),
                                                    paste(b1, b2, b3, b4, b5, b6, b7, sep='_'))))))) %>%
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -t)

# fix one name
BT_Taxa_ranef$Biome <- ifelse(BT_Taxa_ranef$Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests_', 
                                 'Tropical_and_Subtropical_Moist_Broadleaf_Forests', BT_Taxa_ranef$Biome)  

##  combine cell counts of biome/taxa with taxa estimates for plotting
BT_Taxa_ranef <- inner_join(rarefied_medians %>% distinct(REALM, Biome, taxa_mod, n_cells), BT_Taxa_ranef, by=c('Biome', 'taxa_mod'))

##  new realm to combine freshwater and terrestrial
BT_Taxa_ranef <- BT_Taxa_ranef %>%
  mutate(Realm2 = ifelse(REALM!='Marine', 'Terrestrial/Freshwater', 'Marine'))

##  combine years with biome/taxa coefficients
BT_Taxa_ranef <- inner_join(BT_years, BT_Taxa_ranef, by=c('Biome', 'taxa_mod'))   

BT_Taxa_ranef$model_clean <- BT_Taxa_ranef$model
BT_Taxa_ranef$model_clean <- factor(BT_Taxa_ranef$model_clean, 
                                    levels = c('S_pois', 'Jbeta_norm', 'Jtu_norm', 'Jtu_z1i', 'Jne_norm', 'Jne_zi', 'N_lognorm', 'ENSPIE_pois'),
                                    labels = c('Species richness', 'Total dissimilarity',  'Turnover component', 'Turnover component (z1i)', 'Nestedness component',
                                               'Nestedness component (zi)', 'Abundance (N)', 'ENSPIE'))

##-----need to wrangle the rarefyIDs for plotting--------
BTRfyID_rarefyID_coef <- BTRfyID_rarefyID_coef %>%
  separate(level, c('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 't'), remove=F, sep='_') %>% 
  mutate(cell = ifelse(!is.na(t), t,
                       ifelse(!is.na(b9), b9,
                              ifelse(!is.na(b8), b8,
                                     ifelse(!is.na(b7), b7,
                                            ifelse(!is.na(b6), b6,
                                                   ifelse(!is.na(b5), b5,
                                                          ifelse(!is.na(b4), b4,
                                                                 ifelse(!is.na(b3), b3, b2))))))))) %>%
  mutate(rarefyID = ifelse(is.na(b5), paste(b3, b4, sep = '_'),
                           ifelse(is.na(b6), paste(b4, b5, sep='_'),
                                  ifelse(is.na(b7), paste(b5, b6, sep='_'),
                                         ifelse(is.na(b8), paste(b6, b7, sep='_'),
                                                ifelse(is.na(b9), paste(b7, b8, sep='_'), paste(b8, b9, sep='_'))))))) %>%
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -b8, -b9, -cell, -t)


##  cell-level metadata with coefficient estimates
BTRfyID_rarefyID_coef <- inner_join(BTRfyID_meta, BTRfyID_rarefyID_coef, by='rarefyID')
BTRfyID_rarefyID_coef$model <- factor(BTRfyID_rarefyID_coef$model, level = c('Jbeta_norm', 'Jne_norm', 'Jtu_norm', 'Jne_zi', 'Jtu_z1i', 'N_lognorm', 'ENSPIE_pois', 'S_pois'))
# indicator for colour plotting of directional trend
BTRfyID_rarefyID_coef$change <- factor(BTRfyID_rarefyID_coef$change, level = c('neutral', 'up', 'down'))
# combine years of time series with coefficient estimates
BTRfyID_rarefyID_coef <- inner_join(BTRfyID_rarefyID_coef, cell_years %>% select(-Biome, -taxa_mod), by='rarefyID')
BTRfyID_rarefyID_coef$model_clean <- BTRfyID_rarefyID_coef$model
BTRfyID_rarefyID_coef$model_clean <- factor(BTRfyID_rarefyID_coef$model_clean, 
                                            levels = c('S_pois', 'Jbeta_norm', 'Jtu_norm', 'Jtu_z1i', 'Jne_norm', 'Jne_zi', 'N_lognorm', 'ENSPIE_pois'),
                                            labels = c('Species richness', 'Total dissimilarity',  'Turnover component', 'Turnover component (z1i)', 'Nestedness component',
                                                       'Nestedness component (zi)', 'Abundance (N)', 'ENSPIE'))
BTRfyID_rarefyID_coef <- BTRfyID_rarefyID_coef %>%
  mutate(Realm2 = ifelse(REALM!='Marine', 'Terrestrial/Freshwater', 'Marine'))

BT_rarefyID_ranef <- BT_rarefyID_ranef %>%
  separate(rarefyID, c('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 't'), remove=F, sep='_') %>% 
  mutate(cell = ifelse(!is.na(t), t,
                       ifelse(!is.na(b9), b9,
                              ifelse(!is.na(b8), b8,
                                     ifelse(!is.na(b7), b7,
                                            ifelse(!is.na(b6), b6,
                                                   ifelse(!is.na(b5), b5,
                                                          ifelse(!is.na(b4), b4,
                                                                 ifelse(!is.na(b3), b3, b2))))))))) %>%
  mutate(rarefyID = ifelse(is.na(b5), paste(b3, b4, sep = '_'),
                           ifelse(is.na(b6), paste(b4, b5, sep='_'),
                                  ifelse(is.na(b7), paste(b5, b6, sep='_'),
                                         ifelse(is.na(b8), paste(b6, b7, sep='_'),
                                                ifelse(is.na(b9), paste(b7, b8, sep='_'), paste(b8, b9, sep='_'))))))) %>%
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -b8, -b9, -cell, -t)

# put the rarefyID coordinates into these dataframes
BT_rarefyID_ranef <- inner_join(rarefied_medians %>% distinct(REALM, Biome, taxa_mod, rarefyID, rarefyID_x, rarefyID_y),
                                BT_rarefyID_ranef, by = 'rarefyID')

#-----add Biome-level random effects (departures from global trend) to the taxa and rarefyID dataframes (to match colours of map)----
# taxa first
BT_taxa_estimate <- inner_join(BT_taxa_estimate, select(BT_biome_ranef, Biome, model, Slope_ranef), by = c('Biome', 'model')) 

BT_taxa_estimate <- BT_taxa_estimate %>%
  mutate(Biome_ranef = Slope_ranef,
         Biome_departure = ifelse(Slope_ranef > 0, 'up', 'down')) %>%
  select(-Slope_ranef)

BTRfyID_rarefyID_coef <- inner_join(BTRfyID_rarefyID_coef, select(BT_biome_ranef, Biome, model, Slope_ranef), by = c('Biome', 'model')) 
BTRfyID_rarefyID_coef <- BTRfyID_rarefyID_coef %>%
  mutate(Biome_ranef = Slope_ranef,
         Biome_departure = ifelse(Slope_ranef > 0, 'up', 'down')) %>%
  select(-Slope_ranef)

