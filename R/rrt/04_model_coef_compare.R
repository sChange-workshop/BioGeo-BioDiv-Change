# code to compare results of alternative models

# high-level comparison - bt versus rrt: 
# compare the biome/taxon level to the realm-region-taxon levels


# low level comparisons: compare slope estimates for studies and cells that the different models share
# biome-taxon versus realm-climate-taxon and bt versus realm-region-taxon

rm(list=ls())

library(tidyverse)

# biome-taxa model
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BT_model_coef_ranef.Rdata')
# realm-climate-taxa model
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/rct/model_fits_output/rlm_clm_txa_modelCoefs.Rdata')
# realm-region-taxa model
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/rrt/model_coef_ranef/rlm_reg_txa_coef_ranef.Rdata')

#--------rarefied_medians.Rdata for meta data not in model object
load('~/Dropbox/BiogeoBioTIME/rarefied_medians_continents.Rdata')
rarefied_medians_region <- rarefied_medians
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
# this is to compare like with like when comparing bt with rct:
# both rct estimates and bimoe estimates span multiple latitudinal bands, 
# here I constrain biomes and to later make comparisons with rct estimate
# from the same latitude
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


# wrangle study level estimates
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

# repeat for realm-climate-taxa model (note different number of studies due to inclusion criteria)
rct_study_estimate <- rlm_clm_txa_studyID_estimate %>% 
  separate(level, into = c('Realm', 'Climate', 'Taxa', 'STUDY_ID', 'b5' , 'b6'), sep = '_', remove = FALSE) %>% 
  select(-b5, -b6)

# repeat for realm-region-taxa model (note different number of studies due to inclusion criteria)
rrt_study_estimate <- rrt_studyID_estimate %>% 
  # separate on the last (rightmost) underscore: this gets us study
  separate(level, c('realm_region_taxa', 'STUDY_ID'), sep = '_(?!.*_)',
           remove = F) %>% 
  # separate realm_region from taxa
  separate(realm_region_taxa, c('realm_region', 'taxa_mod'), sep = '_(?!.*_)',
           remove = F) %>% 
  # separate realm and region
  separate(realm_region, c('realm' , 'region'), sep = '(^[^_]*_)',
           remove = F) %>% 
  separate(realm_region, c('realm', 'nuu'), sep = '_', remove = F) %>% 
  select(-nuu)

# prepare coefficients for joining
s1 <- BT_study_estimate %>% 
  filter(model!='Jne_new_norm' & model!='Jtu_beta') %>% 
  mutate(bt_slope = Estimate.cYEAR,
         bt_upper = upper_slope,
         bt_lower = lower_slope) %>% 
  select(Biome, STUDY_ID, model, bt_slope, bt_upper, bt_lower) %>% 
  # put the climate/latitude information in for joining with the rct estimates
  inner_join(realm_climate_biome %>% 
               select(-REALM),
             by = 'Biome')
  # and the taxa information for joining with both rct and rrt

s2 <- rct_study_estimate %>% 
  filter(model!='Jne_new_norm' & time_period=='ALL') %>% 
  mutate(rct_slope = Estimate.cYEAR,
         rct_lower = lower_slope,
         rct_upper = upper_slope,
         climate_mod = Climate) %>% 
  select(climate_mod, STUDY_ID, model, rct_slope, rct_lower, rct_upper)


s3 <- rrt_study_estimate %>% 
  filter(model!='Jne_rlm_reg_txa') %>% 
  mutate(rrt_slope = Estimate.cYEAR,
         rrt_lower = lower_slope,
         rrt_upper = upper_slope) %>% 
  # need to rename the models so as we can join using model names
  mutate(model = ifelse(model=='Jtu_rlm_reg_txa', 'Jtu_new_norm', 'S_pois_new')) %>% 
  select(realm, region, taxa_mod, STUDY_ID, model, rrt_slope, rrt_lower, rrt_upper) %>% 
  mutate(Biome = region)


# want to add the estiamtes of the overall trends
# compare  parameter estimates
global_trends <- bind_cols(
  BT_global_estimates %>% 
    filter(model=="Jtu_new_norm" & term=='Slope') %>% 
    mutate(bt_jtu = Estimate,
           bt_jtu_lower = lower,
           bt_jtu_upper = upper) %>% 
    select(bt_jtu, bt_jtu_lower, bt_jtu_upper),
  BT_global_estimates %>% 
    filter(model=="S_pois_new" & term=='Slope') %>% 
    mutate(bt_s = Estimate,
           bt_s_lower = lower,
           bt_s_upper = upper) %>% 
    select(bt_s, bt_s_lower, bt_s_upper),
  rlm_clm_txa_global_estimates %>% 
    filter(model=="Jtu_new_norm" & term=='slope' & time_period=='ALL') %>% 
    mutate(rct_jtu = Estimate,
           rct_jtu_lower = lower,
           rct_jtu_upper = upper) %>% 
    select(rct_jtu, rct_jtu_lower, rct_jtu_upper),
  rlm_clm_txa_global_estimates %>% 
    filter(model=="S_pois_new" & term=='slope' & time_period=='ALL') %>% 
    mutate(rct_s = Estimate,
           rct_s_lower = lower,
           rct_s_upper = upper) %>% 
    select(rct_s, rct_s_lower, rct_s_upper),
  rrt_global_estimates %>% 
    filter(model=="Jtu_rlm_reg_txa" & term=='Slope') %>% 
    mutate(rrt_jtu = Estimate,
           rrt_jtu_lower = lower,
           rrt_jtu_upper = upper) %>% 
    select(rrt_jtu, rrt_jtu_lower, rrt_jtu_upper),
  rrt_global_estimates %>% 
    filter(model=="S_rlm_reg_txa" & term=='Slope') %>% 
    mutate(rrt_s = Estimate,
           rrt_s_lower = lower,
           rrt_s_upper = upper) %>% 
    select(rrt_s, rrt_s_lower, rrt_s_upper))

bt_rct_study_compare <- inner_join(s1, s2,
                            by = c("STUDY_ID", 'model', 'climate_mod')) 

# get the marine biomes for filtering
marine_biomes <- s3 %>% 
  filter(realm=="Marine") %>% 
  distinct(Biome)

bt_rrt_study_compare <- inner_join(s1, s3, by = c('STUDY_ID', 'model', 'Biome')) %>% 
  # and the terrestrial and freshwater studies
  bind_rows(inner_join(s1 %>% filter(!Biome %in% marine_biomes$Biome) %>% select(-Biome),
                       s3 %>% filter(realm!='Marine'),
                       by = c('STUDY_ID', 'model')))


fig_a <- bt_rrt_study_compare %>% 
  ggplot() +
  geom_linerange(aes(x = rrt_slope, ymin = bt_lower, ymax = bt_upper, 
                     colour = model,
                     linetype = model),
                 alpha = 0.2, size = .8) +
  geom_errorbarh(aes(xmin = rrt_lower, xmax = rrt_upper, 
                     colour = model,
                     linetype = model,
                     y = bt_slope),
                 height = 0, alpha = 0.2, size = .8) +
  geom_point(aes(x = rrt_slope, y = bt_slope, 
                 colour = model,
                 shape = model),
             alpha = 0.5,
             size = 2) +
  # overall turnover estimates
  geom_point(data = global_trends,
             aes(x = rrt_jtu, y = bt_jtu, shape = 'Jtu_new_norm'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rrt_jtu, ymin = bt_jtu_lower, ymax = bt_jtu_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_jtu, xmin = rrt_jtu_lower, xmax = rrt_jtu_upper),
                 height = 0) +
  # overall richness estimates
  geom_point(data = global_trends,
             aes(x = rrt_s, y = bt_s, shape = 'S_pois_new'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rrt_s, ymin = bt_s_lower, ymax = bt_s_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_s, xmin = rrt_s_lower, xmax = rrt_s_upper),
                 height = 0) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_shape_manual(values = c('Jtu_new_norm' = 19,
                                'S_pois_new' = 17),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")) +
  scale_color_manual(values = c('Jtu_new_norm' = '#d8b365',
                                'S_pois_new' = '#5ab4ac'),
                     labels = c("Turnover component of\nJaccard's dissimilarity", 
                                "Species richness")) +
  scale_linetype_manual(values = c('Jtu_new_norm' = 1,
                                   'S_pois_new' = 2),
                        labels = c("Turnover component of\nJaccard's dissimilarity", 
                                   "Species richness")) +
  labs(x = 'Study-level slope (RRT model)',
       y = 'Study-level slope (BT model)',
       tag = 'A') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
    legend.position = c(0,1), 
    legend.background = element_blank(),
    legend.justification = c(0,1),
    legend.title = element_blank()) +
  guides(shape = guide_legend(override.aes = list(colour = c('#d8b365','#5ab4ac'))))

fig_b <- bt_rct_study_compare %>% 
  ggplot() +
  geom_linerange(aes(x = rct_slope, ymin = bt_lower, ymax = bt_upper, 
                     colour = model,
                     linetype = model),
                 alpha = 0.2, size = .8) +
  geom_errorbarh(aes(xmin = rct_lower, xmax = rct_upper, 
                     colour = model,
                     linetype = model,
                     y = bt_slope),
                 height = 0, alpha = 0.2, size = .8) +
  geom_point(aes(x = rct_slope, y = bt_slope, 
                 colour = model,
                 shape = model),
             alpha = 0.5,
             size = 2) +
  # overall turnover estimates
  geom_point(data = global_trends,
             aes(x = rct_jtu, y = bt_jtu, shape = 'Jtu_new_norm'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rct_jtu, ymin = bt_jtu_lower, ymax = bt_jtu_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_jtu, xmin = rct_jtu_lower, xmax = rct_jtu_upper),
                 height = 0) +
  # overall richness estimates
  geom_point(data = global_trends,
             aes(x = rct_s, y = bt_s, shape = 'S_pois_new'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rct_s, ymin = bt_s_lower, ymax = bt_s_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_s, xmin = rct_s_lower, xmax = rct_s_upper),
                 height = 0) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_shape_manual(values = c('Jtu_new_norm' = 19,
                                   'S_pois_new' = 17),
                        labels = c("Turnover component of\nJaccard's dissimilarity",
                                   "Species richness")) +
  scale_color_manual(values = c('Jtu_new_norm' = '#d8b365',
                                'S_pois_new' = '#5ab4ac'),
                     labels = c("Turnover component of\nJaccard's dissimilarity", 
                                "Species richness")) +
  scale_linetype_manual(values = c('Jtu_new_norm' = 1,
                                   'S_pois_new' = 2),
                        labels = c("Turnover component of\nJaccard's dissimilarity", 
                                   "Species richness")) +
  labs(x = 'Study-level slope (RCT model)',
       y = 'Study-level slope (BT model)',
       tag = 'B') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
        legend.position = c(0,1), 
        legend.background = element_blank(),
        legend.justification = c(0,1),
        legend.title = element_blank()) +
  guides(shape = guide_legend(override.aes = list(colour = c('#d8b365','#5ab4ac'))))

## cell-level estimates
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
  filter(time_period=='ALL') %>% 
  mutate(rct_slope = Estimate.cYEAR,
         rct_lower = lower_slope,
         rct_upper = upper_slope, 
         climate_mod = Climate) %>% 
  select(climate_mod, rarefyID, model, rct_slope, rct_lower, rct_upper)

bt_rct_cell_compare <- inner_join(r1, r2, by = c('model', 'rarefyID', 'climate_mod'))

# repeat for realm-region-taxa model (note different number of studies due to inclusion criteria)
rrt_cell_estimate <- rrt_rarefyID_coef %>% 
  # separate on the last (rightmost) underscore: this gets us study
  separate(level, c('realm_region_taxa_study_study', 'cell'), sep = '_(?!.*_)',
           remove = F) %>% 
  # get the second study and combine with cell for rarefyID
  separate(realm_region_taxa_study_study, c('realm_region_taxa_study', 'study'), sep = '_(?!.*_)',
           remove = F) %>% 
  unite(rarefyID, study, cell) %>% 
  # separate study from realm_region_taxa
  # separate on the last (rightmost) underscore: this gets us study
  separate(realm_region_taxa_study, c('realm_region_taxa', 'STUDY_ID'), sep = '_(?!.*_)',
           remove = F) %>% 
  # separate realm_region from taxa
  separate(realm_region_taxa, c('realm_region', 'taxa_mod'), sep = '_(?!.*_)',
           remove = F) %>% 
  # separate realm and region
  separate(realm_region, c('realm' , 'region'), sep = '(^[^_]*_)',
           remove = F) %>% 
  separate(realm_region, c('realm', 'nuu'), sep = '_', remove = F) %>% 
  select(-nuu)

r3 = rrt_cell_estimate %>% 
  filter(model!='Jne_rlm_reg_txa') %>% 
  mutate(rrt_slope = Estimate.cYEAR,
         rrt_lower = lower_slope,
         rrt_upper = upper_slope) %>% 
  # need to rename the models so as we can join using model names
  mutate(model = ifelse(model=='Jtu_rlm_reg_txa', 'Jtu_new_norm', 'S_pois_new')) %>% 
  select(realm, region, taxa_mod, STUDY_ID, rarefyID, model, rrt_slope, rrt_lower, rrt_upper) %>% 
  mutate(Biome = region)


bt_rrt_cell_compare <- right_join(r1, r3, by = c('rarefyID', 'model', 'Biome')) %>% 
  # and the terrestrial and freshwater studies
  bind_rows(inner_join(r1 %>% filter(!Biome %in% marine_biomes$Biome) %>% select(-Biome),
                       r3 %>% filter(realm!='Marine'),
                       by = c('rarefyID', 'model')))

fig_c <- bt_rrt_cell_compare %>% 
  ggplot() +
  # geom_linerange(aes(x = rrt_slope, ymin = bt_lower, ymax = bt_upper, 
  #                    colour = model,
  #                    linetype = model),
  #                alpha = 0.2, size = .8) +
  # geom_errorbarh(aes(xmin = rrt_lower, xmax = rrt_upper, 
  #                    colour = model,
  #                    linetype = model,
  #                    y = bt_slope),
  #                height = 0, alpha = 0.2, size = .8) +
  geom_point(aes(x = rrt_slope, y = bt_slope, 
                 colour = model,
                 shape = model),
             alpha = 0.5,
             size = 2) +
  # overall turnover estimates
  geom_point(data = global_trends,
             aes(x = rrt_jtu, y = bt_jtu, shape = 'Jtu_new_norm'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rrt_jtu, ymin = bt_jtu_lower, ymax = bt_jtu_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_jtu, xmin = rrt_jtu_lower, xmax = rrt_jtu_upper),
                 height = 0) +
  # overall richness estimates
  geom_point(data = global_trends,
             aes(x = rrt_s, y = bt_s, shape = 'S_pois_new'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rrt_s, ymin = bt_s_lower, ymax = bt_s_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_s, xmin = rrt_s_lower, xmax = rrt_s_upper),
                 height = 0) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_shape_manual(values = c('Jtu_new_norm' = 19,
                                'S_pois_new' = 17),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")) +
  scale_color_manual(values = c('Jtu_new_norm' = '#d8b365',
                                'S_pois_new' = '#5ab4ac'),
                     labels = c("Turnover component of\nJaccard's dissimilarity", 
                                "Species richness")) +
  scale_linetype_manual(values = c('Jtu_new_norm' = 1,
                                   'S_pois_new' = 2),
                        labels = c("Turnover component of\nJaccard's dissimilarity", 
                                   "Species richness")) +
  labs(x = 'Study-level slope (RRT model)',
       y = 'Study-level slope (BT model)',
       tag = 'C') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
    legend.position = c(0,1), 
    legend.background = element_blank(),
    legend.justification = c(0,1),
    legend.title = element_blank()) +
  guides(shape = guide_legend(override.aes = list(colour = c('#d8b365','#5ab4ac'))))

fig_d <- bt_rct_cell_compare %>% 
  ggplot() +
  # geom_linerange(aes(x = rct_slope, ymin = bt_lower, ymax = bt_upper, 
  #                    colour = model,
  #                    linetype = model),
  #                alpha = 0.2, size = .8) +
  # geom_errorbarh(aes(xmin = rct_lower, xmax = rct_upper, 
  #                    colour = model,
  #                    linetype = model,
  #                    y = bt_slope),
  #                height = 0, alpha = 0.2, size = .8) +
  geom_point(aes(x = rct_slope, y = bt_slope, 
                 colour = model,
                 shape = model),
             alpha = 0.5,
             size = 2) +
  # overall turnover estimates
  geom_point(data = global_trends,
             aes(x = rct_jtu, y = bt_jtu, shape = 'Jtu_new_norm'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rct_jtu, ymin = bt_jtu_lower, ymax = bt_jtu_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_jtu, xmin = rct_jtu_lower, xmax = rct_jtu_upper),
                 height = 0) +
  # overall richness estimates
  geom_point(data = global_trends,
             aes(x = rct_s, y = bt_s, shape = 'S_pois_new'), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rct_s, ymin = bt_s_lower, ymax = bt_s_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_s, xmin = rct_s_lower, xmax = rct_s_upper),
                 height = 0) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_shape_manual(values = c('Jtu_new_norm' = 19,
                                'S_pois_new' = 17),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")) +
  scale_color_manual(values = c('Jtu_new_norm' = '#d8b365',
                                'S_pois_new' = '#5ab4ac'),
                     labels = c("Turnover component of\nJaccard's dissimilarity", 
                                "Species richness")) +
  scale_linetype_manual(values = c('Jtu_new_norm' = 1,
                                   'S_pois_new' = 2),
                        labels = c("Turnover component of\nJaccard's dissimilarity", 
                                   "Species richness")) +
  labs(x = 'Study-level slope (RCT model)',
       y = 'Study-level slope (BT model)',
       tag = 'D') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
    legend.position = c(0,1), 
    legend.background = element_blank(),
    legend.justification = c(0,1),
    legend.title = element_blank()) +
  guides(shape = guide_legend(override.aes = list(colour = c('#d8b365','#5ab4ac'))))

cowplot::plot_grid(fig_a, fig_b,
                   fig_c, fig_d,
                   nrow = 2)

# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/figSx_low_level_compare.png',
#        width = 220, height = 220, units = 'mm')


##----high level: taxa-level of the bt model is most appropriate to compare to the other models 
# (as taxa is in the highest level grouping covariate of both rrt and rct)
# biome vs biome for the marine realm: rrt vs bt
bt_taxa_coef <- BT_taxa_estimate %>% 
  filter(model=='S_pois_new' | model=='Jtu_new_norm') %>% 
  mutate(bt_slope = Estimate.cYEAR,
         bt_upper = upper_slope,
         bt_lower = lower_slope) %>% 
  separate(level, c('region', 'taxa'), sep = '_(?!.*_)',
           remove = F) %>% 
  select(region, taxa, bt_slope, bt_upper, bt_lower, model)

rrt_taxa_coef <- rrt_rrt_estimate %>% 
  filter(model!='Jne_rlm_reg_txa') %>% 
  # separate on the last (rightmost) underscore
  separate(rrt, c('realm_region', 'taxa'), sep = '_(?!.*_)',
           remove = F) %>% 
  separate(realm_region, c('realm' , 'region'), sep = '(^[^_]*_)',
           remove = F) %>% 
  separate(realm_region, c('realm', 'nuu'), sep = '_', remove = F) %>% 
  mutate(rrt_slope = Estimate.cYEAR,
         rrt_lower = lower_slope,
         rrt_upper = upper_slope,
         model = ifelse(model=='Jtu_rlm_reg_txa', 'Jtu_new_norm', 'S_pois_new')) %>% 
  select(-nuu) %>% 
  select(realm, region, taxa, rrt_slope, rrt_lower, rrt_upper, model) 

marine_taxa_compare <- inner_join(bt_taxa_coef,
                                  rrt_taxa_coef %>% 
                                    filter(realm=='Marine'),
                                  by = c('region', 'taxa', 'model'))

# need to put region information in from the metadata for the non-marine biomes
terr_regions <- rarefied_medians_region %>% 
  filter(REALM!='Marine') %>% 
  distinct(Biome, region) #


other_taxa_compare <- inner_join(bt_taxa_coef %>% 
                                   mutate(Biome=region) %>% 
                                   select(-region) %>% 
                                   left_join(terr_regions,
                                             by = 'Biome') %>% 
                                   # get rid of the marine guys.
                                   # notice that we have a single estimate from the BT model
                                   # for multiple regions (for which we will have estimates from RRT)
                                   filter(!is.na(region)) ,
                                  rrt_taxa_coef %>% 
                                    filter(realm!='Marine'),
                                  by = c('region', 'taxa', 'model'))


col_realm = c('Marine' = 'blue', 'Terrestrial' = 'orange', 'Freshwater' = 'dark green')

bind_rows(marine_taxa_compare,
          other_taxa_compare) %>% 
  # filter(model!='Jtu_new_norm') %>%
  ggplot() +
  # facet_grid(region~taxa) +
  # facet_wrap(~model, scales = 'free') +
  geom_linerange(aes(x = rrt_slope, ymin = bt_lower, ymax = bt_upper, 
                     colour = realm,
                     linetype = model),
                 size = .8,
                 alpha = 0.25) +
  geom_errorbarh(aes(xmin = rrt_lower, xmax = rrt_upper, 
                     colour = realm,
                     linetype = model,
                     y = bt_slope),
                 height = 0, 
                 size = .8,
                 alpha = 0.25) +
  geom_point(aes(x = rrt_slope, y = bt_slope, colour = realm,
                 shape = model),
             alpha = 0.75,
             size = 4) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  # overall turnover estimates
  geom_point(data = global_trends,
             aes(x = rrt_jtu, y = bt_jtu), 
             size = 2) +
  geom_linerange(data = global_trends,
                 aes(x = rrt_jtu, ymin = bt_jtu_lower, ymax = bt_jtu_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_jtu, xmin = rrt_jtu_lower, xmax = rrt_jtu_upper),
                 height = 0) +
  # overall richness estimates
  geom_point(data = global_trends,
             aes(x = rrt_s, y = bt_s), 
             size = 2, shape = 17) +
  geom_linerange(data = global_trends,
                 aes(x = rrt_s, ymin = bt_s_lower, ymax = bt_s_upper)) +
  geom_errorbarh(data = global_trends,
                 aes(y = bt_s, xmin = rrt_s_lower, xmax = rrt_s_upper),
                 height = 0) +
  scale_shape_manual(values = c('Jtu_new_norm' = 19,
                                'S_pois_new' = 17),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")) +
  scale_linetype_manual(values = c('Jtu_new_norm' = 1,
                                'S_pois_new' = 2),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")) +
  scale_color_manual(values = col_realm) +
  labs(x = 'Taxon-level slope (RRT model: realm-region-taxon estimates)',
       y = 'Taxon-level slope (BT model: taxon within biome estimates)') +
  theme_bw() +
  theme(#panel.grid = element_blank(),
    legend.position = c(0,1), 
    legend.background = element_blank(),
    legend.justification = c(0,1),
    legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/FigS4.pdf',
#        width = 200, height = 200, units = 'mm')


other_taxa_compare %>% 
  mutate(Biome = gsub(pattern = '_', ' ', Biome)) %>% 
  filter(model=='Jtu_new_norm') %>%
  ggplot() +
  facet_wrap(taxa~region, nrow = 5, labeller = labeller(Biome = label_wrap_gen())) +
  # facet_wrap(~model, scales = 'free') +
  geom_linerange(aes(x = rrt_slope, ymin = bt_lower, ymax = bt_upper, 
                     colour = Biome,
                     linetype = model),
                 size = .8,
                 alpha = 0.25) +
  geom_errorbarh(aes(xmin = rrt_lower, xmax = rrt_upper, 
                     colour = Biome,
                     linetype = model,
                     y = bt_slope),
                 height = 0, 
                 size = .8,
                 alpha = 0.25) +
  geom_point(aes(x = rrt_slope, y = bt_slope, colour = Biome,
                 shape = model),
             alpha = 0.75,
             size = 4) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  # # overall turnover estimates
  # geom_point(data = global_trends,
  #            aes(x = rrt_jtu, y = bt_jtu), 
  #            size = 2) +
  # geom_linerange(data = global_trends,
  #                aes(x = rrt_jtu, ymin = bt_jtu_lower, ymax = bt_jtu_upper)) +
  # geom_errorbarh(data = global_trends,
  #                aes(y = bt_jtu, xmin = rrt_jtu_lower, xmax = rrt_jtu_upper),
  #                height = 0) +
  # # overall richness estimates
  # geom_point(data = global_trends,
  #            aes(x = rrt_s, y = bt_s), 
  #            size = 2, shape = 17) +
  # geom_linerange(data = global_trends,
  #                aes(x = rrt_s, ymin = bt_s_lower, ymax = bt_s_upper)) +
  # geom_errorbarh(data = global_trends,
  #                aes(y = bt_s, xmin = rrt_s_lower, xmax = rrt_s_upper),
  #                height = 0) +
  scale_shape_manual(guide = F,
                     values = c('Jtu_new_norm' = 19,
                                'S_pois_new' = 17),
                     labels = c("Turnover component of\nJaccard's dissimilarity",
                                "Species richness")) +
  scale_linetype_manual(guide = F,
                        values = c('Jtu_new_norm' = 1,
                                   'S_pois_new' = 2),
                        labels = c("Turnover component of\nJaccard's dissimilarity",
                                   "Species richness")) +
  scale_color_brewer(type='qual', palette = 'Dark2') +
  scale_x_continuous(breaks = c(0, 0.01, 0.02, 0.03, 0.04),
                     labels = c(0, '', 0.02, '', 0.04)) +
  labs(x = 'Taxon-level slope (RRT model: realm-region-taxon estimates)',
       y = 'Taxon-level slope (BT model: taxon within biome estimates)') +
  theme_bw() +
  coord_flip()+
  theme(#panel.grid = element_blank(),
    legend.position = c(1,0), 
    legend.background = element_blank(),
    legend.justification = c(1,0),
    legend.direction = 'horizontal',
    legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 1))

# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/FigS5.png',
#        width = 300, height = 300, units = 'mm')


bind_rows(marine_taxa_compare,
          other_taxa_compare) %>% 
  filter(model=='Jtu_new_norm') %>%
  mutate(rho_estimate_jtu = cor.test(bt_slope, rrt_slope, method = 'spearman')$estimate %>% signif(digits = 2),
         p_value_jtu = cor.test(bt_slope, rrt_slope, method = 'spearman')$p.value %>% signif(digits = 2))


rarefied_medians_region <- rarefied_medians_region %>% 
  unite(rlm_region_taxa, REALM, region, taxa_mod, remove=FALSE)

##	count cells within realm/climate/taxa hierarchy
cell_count <- rarefied_medians_region %>%
  group_by(rlm_region_taxa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians_region <- left_join(cell_count, rarefied_medians_region, by='rlm_region_taxa')  

##	filter to ecoregions with >3 cells
rarefied_medians_region <- rarefied_medians_region %>%
  filter(n_cells > 3 & BROAD_TYPE=='count')

rarefied_medians_region %>% 
  filter(REALM!='Marine') %>% 
  group_by(rlm_region_taxa) %>% 
  summarise(n_biome_taxa = n_distinct(Biome, taxa_mod),
            n_biome = n_distinct(Biome))
