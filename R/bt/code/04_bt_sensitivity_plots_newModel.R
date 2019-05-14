##------------sensitivity plots for the BT models---------------------
# update to plot results of new models (include studyID in hierarchy)
rm(list=ls())

library(brms)
library(tidyverse)

load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/model_coefs_ranefs/BTRfyID_coef_ranef_inclNewModel.Rdata')

##----the data models were fit too
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')

##	create new covariate that is concatenation of realm_climate_taxa
##	count cells within realm/climate/taxa hierarchy
cell_count <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, 
                              by=c('Biome', 'taxa_mod'))

##	filter to  count data with >3 cells
rarefied_medians <- rarefied_medians %>%
  filter(n_cells > 3 & BROAD_TYPE=='count')

## get duration, num_years and other metadata for each rarefyID
cell_meta <- rarefied_medians %>%
  distinct(REALM, climate_mod, taxa_mod, STUDY_ID, rarefyID, 
           rarefyID_x, rarefyID_y, num_years, duration, 
           SamplePool, SampleN, startYear, endYear)

initialS <- rarefied_medians %>% 
  distinct(rarefyID, .keep_all=TRUE) %>%
  filter(YEAR == startYear) %>%
  mutate(initialS = S) %>%
  select(rarefyID, initialS)

cell_meta <- inner_join(initialS, 
                        cell_meta,
                        by = 'rarefyID')

# clean up the rarefyID estimates
bt_rarefyID_coef <- BTRfyID_rarefyID_coef %>%
  filter(model=='Jtu_new_norm' | model=='Jne_new_norm' | model=='S_pois_new') %>% 
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

# add the meta data
bt_rarefyID_coef <- right_join(cell_meta, 
                              bt_rarefyID_coef, 
                              by = 'rarefyID')

bt_rarefyID_coef$model_clean <- factor(bt_rarefyID_coef$model, levels = c('S_pois_new', 'Jne_new_norm', 'Jtu_new_norm'),
                                       labels = c('Species richness', 'Nestedness component', 'Turnover component'))

##  colour, shape and linetype settings
#cols = c('down' = 'red', 'neutral' = 'grey', 'up' = 'blue')
cols = c('Tropical' = 'blue', 'Polar' = 'snow3', 'Temperate' = 'orange')
col_realm = c('Marine' = 'blue', 'Terrestrial' = 'orange', 'Freshwater' = 'dark green')
col_change = c('up' = 'blue', 'down' = 'red', 'neutral' = 'grey')
shapes = c('Marine' = 19, 'Freshwater' = 17, 'Terrestrial' = 15)
shape_txa = c('All' = 6, 'Amphibians' = 8, 'Benthos' = 2, 'Birds' = 15, 'Fish' = 1, 'Invertebrates' = 0, 'Mammals' = 17, 'Marine invertebrates/plants' = 7, 'Plant' = 5)
linetypes = c('Marine' = 1, 'Freshwater' = 1, 'Terrestrial' = 1)


##---------------------rarefyID slope estimates as a function of duration etc-------------------------
# cell slope estimates as a function of the number of samples
bt_numYears <- bt_rarefyID_coef %>% filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
  geom_point(aes(num_years, Estimate.cYEAR, colour=change, size = duration), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(name='Cell-level\nchange', values = col_change) +
  scale_size_area(name='Duration\n(years)', breaks = c(2,4,8,16,32,64,128)) +
  # stat_smooth(se=T, aes(num_years, Estimate.cYEAR), colour = 'black',
  #             method = 'lm') +
  stat_smooth(se=F, aes(num_years, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 3)) +
  labs(x = 'Number of years sampled',
       y = 'Slope estimate (cell-level)',
       subtitle = 'A.   Cell-level slopes as function of the number of years in time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))
# ggsave('BT_cell-level_slope~num_years1.png', width = 220, height = 200, units = 'mm')

bt_duration <- bt_rarefyID_coef %>% 
  filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
  geom_point(aes(duration, Estimate.cYEAR, colour=change, size = duration), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = F,
                     name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(2,4,8,16,32,64)) +
  # stat_smooth(se=T, aes(duration, Estimate.cYEAR), colour = 'black',
  #             method = 'lm') +
  stat_smooth(se=F, aes(duration, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Duration of sampling (years)',
       y = 'Slope estimate (cell-level)',
       subtitle = 'B.   Cell-level slopes as function of the duration of the time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) 

bt_startYear <- bt_rarefyID_coef %>% 
  filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
  geom_point(aes(startYear, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, 
                     name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(2,4,8,16,32,64)) +
  stat_smooth(se=T, aes(startYear, Estimate.cYEAR), colour = 'black',
              method = 'lm') +
  # stat_smooth(se=F, aes(startYear, Estimate.cYEAR), colour = 'black',
  #             method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Start year',
       y = 'Slope estimate (cell-level)',
       subtitle = 'C.   Cell-level slopes as function of start year of time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) 


bt_initalS <- bt_rarefyID_coef %>% 
  filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
  geom_point(aes(initialS, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, 
                     name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(2,4,8,16,32,64)) +
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8,16,32,64,128,256,512,1024), 
                     labels = c(1,2,4,8,16,32,64, '',256, '',1024)) +
  stat_smooth(se=T, aes(initialS, Estimate.cYEAR), colour = 'black',
              method = 'lm') +
  # stat_smooth(se=F, aes(initialS, Estimate.cYEAR), colour = 'black',
  #             method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Initial species richness',
       y = 'Slope estimate (cell-level)',
       subtitle = 'D.   Cell-level slopes as a function of initial species richness') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

# put 'em together
cowplot::plot_grid(bt_numYears, bt_duration,
                   bt_startYear, bt_initalS, nrow = 4, align = 'hv')
# ggsave('FigSx_bt_cell-level_slope_sensitivity_lm.png', width = 400, height = 400, units = 'mm')


# plot the correlogram for cell-level slopes
library(pgirmess)
coords <- cbind(bt_rarefyID_coef %>% 
                  filter(model=='S_pois_new') %>% 
                  .$rarefyID_x,
                bt_rarefyID_coef %>% 
                  filter(model=='S_pois_new') %>% 
                  .$rarefyID_y)
names(coords) <- c('x', 'y')
library(ncf)
S_correlog <- ncf::correlog(coords$x, coords$y, 
                            bt_rarefyID_coef %>% 
                              filter(model=='S_pois_new') %>% 
                              .$Estimate.cYEAR,
                    increment=2, resamp=1)
S_correlog <- correlog(coords=, 
                 z=bt_rarefyID_coef %>% 
                   filter(model=='S_pois_new') %>% 
                   .$Estimate.cYEAR,
                 method="Moran")
