# coef wrangle for new models
rm(list = ls())
library(tidyverse)
library(brms)

load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BT_model_coef_ranef.Rdata')


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

##-------wrangle global level estimates-----------
bt_new_global <- BT_global_estimates %>% 
  filter(model=='Jne_new_norm' | model=='Jtu_new_norm' | model=='S_pois_new') 

bt_new_global$model_clean <- bt_new_global$model
bt_new_global$model_clean <- factor(bt_new_global$model_clean, 
                                          levels = c('S_pois_new', 'Jtu_new_norm', 'Jne_new_norm'),
                                          labels = c('Species richness', 'Turnover component', 'Nestedness component'))

##-------wrangle biome level coef and ranef estimates-----------
bt_new_biome <- BT_biome_estimate %>% 
  filter(model=='Jne_new_norm' | model=='Jtu_new_norm' | model=='S_pois_new') %>% 
  inner_join(rarefied_medians %>% distinct(REALM, Biome),
             by = 'Biome')


bt_new_biome_ranef <- BT_biome_ranef %>% 
  filter(model=='Jne_new_norm' | model=='Jtu_new_norm' | model=='S_pois_new') %>% 
  inner_join(rarefied_medians %>% distinct(REALM, Biome),
             by = 'Biome')


##-------wrangle biome-taxa level coef and ranef estimates-----------
bt_new_taxa <- BT_taxa_estimate %>% 
  filter(model=='Jne_new_norm' | model=='Jtu_new_norm' | model=='S_pois_new') %>% 
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
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -t) %>% 
  # fix one name
  mutate(Biome = ifelse(Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests_', 
                        'Tropical_and_Subtropical_Moist_Broadleaf_Forests', Biome)) %>% 
  inner_join(rarefied_medians %>% distinct(REALM, Biome, taxa_mod),
             by = c('Biome', 'taxa_mod'))



##------------wrangle study-level coef-------------
bt_new_study <- BT_studyID_estimate %>% 
  mutate(STUDY_ID2 = str_sub(level, start = -3),
         # drop underscores
         STUDY_ID = sub('_','', STUDY_ID2),
         # remove study
         bt = str_remove(level, STUDY_ID2),
         # remove trailing underscore
         bt = sub('_$', '', bt)) %>% 
  # ok, now use existing hacky code to separate biome and taxa
  separate(bt, c('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 't'), remove=F, sep='_') %>% 
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
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -t) %>% 
  # fix the biome name that causes problems for joins
  mutate(Biome = ifelse(Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests_', 
                        'Tropical_and_Subtropical_Moist_Broadleaf_Forests', Biome)) %>% 
  # now join with the realm meta data
  left_join(rarefied_medians %>% distinct(REALM, Biome, taxa_mod, STUDY_ID) %>% 
               mutate(STUDY_ID = as.character(STUDY_ID)),
             by = c('Biome', 'taxa_mod', 'STUDY_ID'))


##------examine some summary stats for different levels of teh hierarchy---------
# compare departures for marine versus terrestrial
bt_new_biome_ranef %>% 
  group_by(model, REALM) %>% 
  summarise(min_departure = min(Slope_ranef),
            max_departure = max(Slope_ranef),
            n = n_distinct(Biome))

bt_new_biome_ranef %>% 
  filter(lower_slope > 0)

bt_new_biome_ranef %>% 
  filter(upper_slope < 0)

bt_new_biome_ranef %>% 
  filter(Slope_ranef > 0 & REALM!='Marine')

# compare biome estimates for marine versus terrestrial
bt_new_biome %>% 
  group_by(model, REALM) %>% 
  filter(model=='Jtu_new_norm') %>% 
  summarise(min_slope = min(Estimate.cYEAR),
            max_slope = max(Estimate.cYEAR),
            median_slope = median(Estimate.cYEAR),
            n = n_distinct(Biome))


# compare taxa levels estimates marine versus terrestrial
bt_new_taxa %>% 
  group_by(model, REALM) %>% 
  summarise(min_slope = min(Estimate.cYEAR),
            max_slope = max(Estimate.cYEAR),
            median_slope = median(Estimate.cYEAR),
            n = n_distinct(Biome, taxa_mod))

# how many studies show changes different from zero (90% CI)
bt_new_study %>% 
  group_by(model, REALM) %>% 
  summarise(up = sum(change=='up'),
            down = sum(change=='down'),
            neutral = sum(change=='neutral'))

bt_new_study %>% 
  group_by(model, REALM) %>% 
  summarise(min_slope = min(Estimate.cYEAR),
            max_slope = max(Estimate.cYEAR),
            median_slope = median(Estimate.cYEAR),
            n = n_distinct(Biome, taxa_mod, STUDY_ID))

