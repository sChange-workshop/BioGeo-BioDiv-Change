# want to compare the slope estimates for studies and rarefyID's from
# the bt versus the rct models

rm(list=ls())

library(tidyverse)

load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BT_model_coef_ranef.Rdata')
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/rct/model_fits_output/rlm_clm_txa_modelCoefs.Rdata')

#--------rarefied_medians.Rdata for meta data not in model object
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')

rarefied_medians <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  mutate(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  filter(n_cells > 3)

realm_climate_biome <- rarefied_medians %>%
  distinct(REALM, climate_mod, Biome) %>%
  ungroup()

# want unique climate (latitudinal bands for each biome)
# this is to compare like with like: some of the rct study estimates span multiple latitudinal bands
realm_climate_biome <- realm_climate_biome %>%
  # filter(Biome %in% biome_levels_model$Biome) %>%
  mutate(climate_mod = ifelse(Biome=='Arctic', 'Polar', 
                              ifelse(Biome=='Boreal_Forests_Taiga', 'Temperate',
                                     ifelse(Biome=='Cold_Temperate_Northeast_Pacific', 'Temperate',
                                            ifelse(Biome=='Continental_High_Antarctic', 'Polar',
                                                   ifelse(Biome=='Hawaii', 'Tropical', 
                                                          ifelse(Biome=='Northern_European_Seas', 'Temperate',
                                                                 ifelse(Biome=='Subantarctic_Islands', 'Polar',
                                                                        ifelse(Biome=='Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands', 'Temperate',
                                                                               ifelse(Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests', 'Tropical', 
                                                                                      ifelse(Biome=='Tropical_Northwestern_Atlantic', 'Tropical', 
                                                                                             ifelse(Biome=='Northeast_Australian_Shelf', 'Tropical', climate_mod))))))))))))
# get the study level estimates
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

# note there are only 291 study-level estimates for this model (cf. 321 for bt)
rct_study_estimate <- rlm_clm_txa_studyID_estimate %>% 
  separate(level, into = c('Realm', 'Climate', 'Taxa', 'STUDY_ID', 'b5' , 'b6'), sep = '_', remove = FALSE) %>% 
  select(-b5, -b6)

s1 <- BT_study_estimate %>% 
  filter(model!='Jne_new_norm' & model!='Jtu_beta') %>% 
  mutate(bt_slope = Estimate.cYEAR,
         bt_upper = upper_slope,
         bt_lower = lower_slope) %>% 
  select(Biome, STUDY_ID, model, bt_slope, bt_upper, bt_lower) %>% 
  inner_join(realm_climate_biome %>% 
               select(-REALM),
             by = 'Biome')

s2 <- rct_study_estimate %>% 
  filter(model!='Jne_new_norm' & time_period=='ALL') %>% 
  mutate(rct_slope = Estimate.cYEAR,
         rct_lower = lower_slope,
         rct_upper = upper_slope,
         climate_mod = Climate) %>% 
  select(climate_mod, STUDY_ID, model, rct_slope, rct_lower, rct_upper)

unique(s2$STUDY_ID) %in% unique(s1$STUDY_ID)
unique(s2$STUDY_ID)[5]
s1 %>% filter(STUDY_ID==236)

study_compare <- inner_join(s1, s2, by = c("climate_mod", "STUDY_ID", 'model'))  

study_compare %>% 
  ggplot() +
  geom_linerange(aes(x = rct_slope, ymin = bt_lower, ymax = bt_upper, colour = model),
                 alpha = 0.2, size = .8) +
  geom_errorbarh(aes(xmin = rct_lower, xmax = rct_upper, colour = model,
                     y = bt_slope), 
                 height = 0, alpha = 0.2, size = .8) +
  geom_point(aes(x = rct_slope, y = bt_slope, colour = model),
             alpha = 0.5,
             size = 2,
             pch = 19) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_manual(values = c('Jtu_new_norm' = '#d8b365',
                                'S_pois_new' = '#5ab4ac'),
                     labels = c("Turnover component of\nJaccard's dissimilarity", 
                                "Species richness")
                     #guide = '', guide argument is broken
                     ) +
  labs(x = 'Study-level slope (RCT model)',
       y = 'Study-level slope (BT model)') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.title = element_blank())

# ggsave('study_level_model_compare.png', width = 250, height = 200, units = 'mm')
# lets have a look at some of the funky ones: 290
# some problematic points are showing estimates, e.g., of tropical biome with temperate latitude
study_compare %>% 
  filter(bt_slope > 0.15 & rct_slope < -0.1)

BT_study_estimate %>% 
  filter(STUDY_ID==290) %>% 
  distinct(Biome)

rct_study_estimate %>% 
  filter(STUDY_ID==290) 

study_compare %>% 
  filter(rct_slope < -0.2)
study_compare %>% 
  filter(STUDY_ID==292)
BT_study_estimate %>% 
  filter(STUDY_ID==292)
rct_study_estimate %>% 
  filter(STUDY_ID==292)

## rarefyID estimates
bt_rarefyid_coef <- BTRfyID_rarefyID_coef %>%
  filter(model=='Jtu_new_norm' | model=='S_pois_new') %>% 
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

rct_rarefyID_estimate <- bind_rows(
  rlm_clm_txa_rarefyID_estimate %>%
    filter(model=='Jtu_new_norm' | model=='S_pois_new') %>% 
    separate(level, into = c('Realm', 'Climate', 'Taxa', 'STUDY_ID', 'b5' , 'b6'), sep = '_', remove = FALSE) %>%
    unite(rarefyID, b5, b6))

r1 = bt_rarefyid_coef %>% 
  filter(model!='Jne_new_norm') %>% 
  mutate(bt_slope = Estimate.cYEAR,
         bt_upper = upper_slope,
         bt_lower = lower_slope) %>% 
  select(rarefyID, model, bt_slope, bt_upper, bt_lower) %>% 
  inner_join(rarefied_medians %>% distinct(Biome, rarefyID),
             by = 'rarefyID') %>% 
  inner_join(realm_climate_biome %>% 
               select(-REALM),
             by = 'Biome')

r2 = rct_rarefyID_estimate %>% 
  mutate(rct_slope = Estimate.cYEAR,
         rct_lower = lower_slope,
         rct_upper = upper_slope, 
         climate_mod = Climate) %>% 
  select(climate_mod, rarefyID, model, rct_slope, rct_lower, rct_upper)

cell_compare <- inner_join(r1, r2, by = c('model', 'rarefyID', 'climate_mod'))

cell_compare %>% 
  # separate(rarefyID, into = c('STUDY_ID', 'cell'), remove = F) %>% 
  # # want to see single versus multi-celled estimates
  # group_by(STUDY_ID) %>% 
  # mutate(one.cell = ifelse(n_distinct(cell)==1,'1','>1')) %>% 
  # ungroup() %>% 
  ggplot() +
  # facet_wrap(~model, scales = 'free') +
  geom_point(aes(x = rct_slope, y = bt_slope, colour = model),
             alpha = 0.2,
             size = 1,
             pch = 19) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_manual(values = c('Jtu_new_norm' = '#d8b365',
                                'S_pois_new' = '#5ab4ac'),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")
                     #guide = '', guide argument is broken
  ) +
  labs(x = 'Cell-level slope (RCT model)',
       y = 'Cell-level slope (BT model)') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
    legend.position = c(0.1, 0.9),
    legend.title = element_blank())

# ggsave('FigSx_model_compare_cellLevel.png', width = 250, height = 200, units = 'mm')