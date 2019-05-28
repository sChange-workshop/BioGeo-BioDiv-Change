# rm(list=ls())
## plot to study level estimates as a function of latitude
library(tidyverse)
library(brms)
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BT_model_coef_ranef.Rdata')

#--------rarefied_medians.Rdata for meta data not in model object
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')

rarefied_medians <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  mutate(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  filter(n_cells > 3)

# want the centre_lat and studyID to join with study-level estimates
BT_study_estimate <- BT_studyID_estimate %>% 
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
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -t)

BT_study_estimate %>% distinct(model)

# the nested model was fit ALL the data (not the count as per Jtu and S)

meta <- rarefied_medians %>% distinct(REALM, Biome, taxa_mod, STUDY_ID)

##-want study-level estimates-

Jtu_study <- BT_study_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jtu_new_norm' & time_period=='ALL') %>%
  mutate(deltaJtu = Estimate.cYEAR,
         deltaJtu_lower = lower_slope,
         deltaJtu_upper = upper_slope) %>% 
  select(level, deltaJtu, deltaJtu_lower, deltaJtu_upper)

Jne_study <- BT_study_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jne_new_norm' & time_period=='ALL') %>%
  mutate(deltaJne = Estimate.cYEAR,
         deltaJne_lower = lower_slope,
         deltaJne_upper = upper_slope) %>%
  select(deltaJne,
         deltaJne_lower,
         deltaJne_upper) 

S_study <- BT_study_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'S_pois_new' & time_period=='ALL') %>%
  mutate(deltaS = Estimate.cYEAR,
         deltaS_lower = lower_slope,
         deltaS_upper = upper_slope) %>%
  select(deltaS,
         deltaS_lower,
         deltaS_upper) 

study_corr <- bind_cols(
  Jne_study,
  Jtu_study,
  S_study
) %>%
  # separate level into its nested components
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
  select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -t, -STUDY_ID2)
  
# put the meta data in
study_corr <- left_join(study_corr, 
                         meta %>% mutate(STUDY_ID = as.character(STUDY_ID)),
                         by = c('Biome', 'taxa_mod', 'STUDY_ID'))

# some studies have not got there realm information
missing <- study_corr %>% 
  filter(is.na(REALM)) %>% 
  distinct(STUDY_ID)
# check the realm of these studies
meta %>% 
  filter(STUDY_ID %in% missing$STUDY_ID) %>% 
  distinct(REALM) # they are all terrestrial...

# change all the missing realms to terrestrial
study_corr <- study_corr %>% 
  mutate(REALM = ifelse(is.na(REALM), 'Terrestrial', REALM))

study_corr <- study_corr %>%
  mutate(Jtu_grt_Jne = ifelse(deltaJtu > deltaJne, 'Jtu > Jne', 'Jne > Jtu'))

# add another indicator for quadrant conceptual model (four colours: Jne>Jtu & S < 0, Jne>Jtu & S>0, etc)
study_corr <- study_corr %>%
  mutate(quad = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS < 0), 'c1', 
                       ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS > 0), 'c2',
                              ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS < 0), 'c3', 'c4'))),
         quad_sig = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS_upper < 0 & deltaJtu_lower > 0), 'c1_sig',
                           ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS_lower > 0 & deltaJtu_lower > 0), 'c2_sig',
                                  ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS_upper < 0 & deltaJne_lower > 0), 'c3_sig', 
                                         ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS_lower > 0 & deltaJne_lower > 0), 'c4_sig', quad)))))


# create some covariates for plotting whether changes differ from zero
study_corr <- study_corr %>%
  mutate(alphaS = ifelse((deltaS_lower < 0 & deltaS_upper > 0), 0.95, 1),
         alphaJtu = ifelse((deltaJtu_lower < 0 & deltaJtu_upper > 0), 0.95, 1),
         alphaJne = ifelse((deltaJne_lower < 0 & deltaJne_upper > 0), 0.95, 1),
         ab_sig = ifelse(((Jtu_grt_Jne=='Jtu > Jne' & alphaJtu==1 & alphaS==1) |
                            (Jtu_grt_Jne=='Jne > Jtu' & alphaJne==1 & alphaS==1)), 1, 0.95))

study_corr <- study_corr %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse((Jtu_grt_Jne=='Jtu > Jne' & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse((Jtu_grt_Jne=='Jne > Jtu' & alphaS==1 & alphaJne==1), 1.1, 1),
         # make points large if both change in dissimilarity and richness differ from zero
         sizeBLUE = ifelse((Jtu_grt_Jne=='Jtu > Jne' & alphaS==1 & alphaJtu==1), 2, .5),
         sizeGREEN = ifelse((Jtu_grt_Jne=='Jne > Jtu' & alphaS==1 & alphaJne==1), 2, .5))


## jtu > jne?
study_corr %>% 
  group_by(Jtu_grt_Jne) %>% 
  summarise(n = n_distinct(Biome, taxa_mod, STUDY_ID))

study_corr %>% 
  group_by(quad_sig) %>% 
  summarise(n = n_distinct(Biome, taxa_mod, STUDY_ID))

