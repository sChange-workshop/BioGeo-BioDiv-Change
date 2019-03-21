# code modified 120219: new models
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(tidyverse)

load('~/Desktop/revision_models/rlm_clm_txa_modelCoefs.Rdata')

##---------add the number of time series used to estimate each slope-------------
##----the data models were fit too
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')

##	create new covariate that is concatenation of realm_climate_taxa
rarefied_medians <- rarefied_medians %>%
  unite(rlm_clm_txa, REALM, climate_mod, taxa_mod, remove=FALSE)

## get duration, num_years and other metadata for each rarefyID
cell_meta <- rarefied_medians %>%
  distinct(REALM, climate_mod, taxa_mod, rarefyID, rarefyID_x, rarefyID_y, num_years, duration, SamplePool, SampleN, startYear, endYear)

initialS <- rarefied_medians %>% distinct(rarefyID, .keep_all=TRUE) %>%
  filter(YEAR == startYear) %>%
  mutate(initialS = S) %>%
  select(rarefyID, initialS)

cell_meta <- inner_join(cell_meta, initialS, by='rarefyID')


## need to split level into rct and rarefyID
rarefyID_estimate <- bind_rows(
  # need to do this differently for the new model
  rlm_clm_txa_rarefyID_estimate %>%
  filter(model=='Jtu_new_norm' | model=='Jne_new_norm' | model=='S_pois_new') %>% 
  separate(level, into = c('Realm', 'Climate', 'Taxa', 'STUDY_ID', 'b5' , 'b6'), sep = '_', remove = FALSE) %>%
  unite(rarefyID, b5, b6))

# check if we have metadata for all the cells in the model
rarefyID_estimate %>% 
  filter(!rarefyID %in% cell_meta$rarefyID)

## combine with rarefyID estimates for plotting
rarefyID_estimate <- right_join(cell_meta, rarefyID_estimate, by = 'rarefyID') %>%
  mutate(change = ifelse(lower_slope > 0, 'up',
                         ifelse(upper_slope < 0, 'down', 'neutral'))) %>%
  unite(RealmClimate, c(Realm,Climate), remove = FALSE) 

rarefyID_estimate$model_name <- factor(rarefyID_estimate$model, levels = c('S_pois_new', 'Jne_new_norm', 'Jtu_new_norm'),
                                  labels = c('Species richness', 'Nestedness component', 'Turnover component'))


##  set working directory to save plots
# setwd('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/rct/figs/sensitivity/')
##  colour, shape and linetype settings
#cols = c('down' = 'red', 'neutral' = 'grey', 'up' = 'blue')
cols = c('Tropical' = 'blue', 'Polar' = 'snow3', 'Temperate' = 'orange')
col_realm = c('Marine' = 'blue', 'Terrestrial' = 'orange', 'Freshwater' = 'dark green')
col_change = c('up' = 'blue', 'down' = 'red', 'neutral' = 'grey')
shapes = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 2)
shape_txa = c('All' = 6, 'Amphibians' = 8, 'Benthos' = 2, 'Birds' = 15, 'Fish' = 1, 'Invertebrates' = 0, 'Mammals' = 17, 'Marine invertebrates/plants' = 7, 'Plant' = 5)
linetypes = c('Marine' = 1, 'Freshwater' = 1, 'Terrestrial' = 1)


##---------------------rarefyID slope estimates as a function of duration etc-------------------------
# cell slope estimates as a function of the number of samples
rct_numYears <- rarefyID_estimate %>% filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(num_years, Estimate.cYEAR, colour=change, size = duration), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(name='Cell-level\nchange', values = col_change) +
  scale_size_area(name='Duration\n(years)', breaks = c(2,4,8,16,32,64,128)) +
  # stat_smooth(se=T, aes(num_years, Estimate.cYEAR), colour = 'black',
  #             method = 'lm') +
  stat_smooth(se=F, aes(num_years, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4), lty = 1) +
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

# duration
rct_duration <-
  rarefyID_estimate %>% filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(duration, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = F,
                     name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(2,4,8,16,32,64)) +
  # stat_smooth(se=T, aes(duration, Estimate.cYEAR), colour = 'black',
  #             method = 'lm') +
  stat_smooth(se=F, aes(duration, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4), lty = 1) +
  labs(x = 'Duration of sampling (years)',
       y = 'Slope estimate (cell-level)',
       subtitle = 'B.   Cell-level slopes as function of the duration of the time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) #+
  # guides(colour = guide_legend(override.aes = list(alpha=1)))


rct_startYear <- rarefyID_estimate %>% 
  filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(startYear, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(2,4,8,16,32,64)) +
  # stat_smooth(se=T, aes(startYear, Estimate.cYEAR), colour = 'black',
  #             method = 'lm') +
  stat_smooth(se=F, aes(startYear, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4), lty = 1) +
  labs(x = 'Start year',
       y = 'Slope estimate (cell-level)',
       subtitle = 'C.   Cell-level slopes as function of time series start year') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) #+
  

rct_initialS <- rarefyID_estimate %>% 
  filter(time_period=='ALL' & model == 'S_pois_new' | model=='Jne_new_norm' | model=='Jtu_new_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(initialS, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, name='Cell-level\nchange', values = col_change) +
    scale_size_area(name = 'Number of\nyears sampled', breaks = c(2,4,8,16,32,64)) +
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8,16,32,64,128,256,512,1024),
                     labels = c(1,2,4,8,16,32,64,'',256,'',1024)) +
  # stat_smooth(se=T, aes(initialS, Estimate.cYEAR), colour = 'black',
  #             method = 'lm') +
  stat_smooth(se=F, aes(initialS, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4), lty = 1) +
  labs(x = 'Initial species richness',
       y = 'Slope estimate (cell-level)',
       subtitle = 'D.   Cell-level slopes as a function of the initial species richness') +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size= 16),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) #+
  # guides(colour = guide_legend(override.aes = list(alpha=1)))

cowplot::plot_grid(rct_numYears, rct_duration,
                   rct_startYear, rct_initialS, nrow = 4, align = 'hv')
# ggsave('FigSx_rct_cell-level_slope_sensitivity.png', width = 400, height = 400, units = 'mm')

#------------histograms of cell level slopes--------------
rarefyID_estimate <- rarefyID_estimate %>%
  mutate(change = ifelse(lower_slope > 0, 'up',
                         ifelse(upper_slope < 0, 'down', 'neutral'))) 

rarefyID_estimate$model_name <- factor(rarefyID_estimate$model, levels = c('S_pois', 'Jbeta_norm', 'Jtu_norm', 'Jne_norm', 'N_lognorm', 'ENSPIE_pois'),
                                       labels = c('Species richness', 'Total dissimilarity', 'Turnover component', 'Nestedness component', 'Abundance (N)', 'ENSPIE'))
rlm_clm_txa_global_estimates$model_name <- factor(rlm_clm_txa_global_estimates$model, levels = c('Jbeta_norm', 'Jtu_norm', 'Jne_norm', 'N_lognorm', 'ENSPIE_pois', 'S_pois'),
                                                  labels = c('Total dissimilarity', 'Turnover component', 'Nestedness component', 'Abundance (N)', 'ENSPIE', 'Species richness'))

col_change = c('up' = 'blue', 'down' = 'red', 'neutral' = 'grey')

rct_cell_slope_hist <- ggplot() +
  facet_wrap(~model_name, scales = 'free', nrow = 1) +
  geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'Jne_norm'),
                 aes(x = Estimate.cYEAR, fill = change), binwidth = 0.001) +#
  geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'Jtu_norm'),
                 aes(x = Estimate.cYEAR, fill = change), binwidth = 0.0005) +#
  # geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'N_lognorm'),
  #                aes(x = Estimate.cYEAR, fill = change), binwidth = 0.005) +#
  # geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'ENSPIE_pois'),
  #                aes(x = Estimate.cYEAR, fill = change), binwidth = 0.001) +#
  geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'S_pois'),
                 aes(x = Estimate.cYEAR, fill = change), binwidth = 0.005) +#
  geom_vline(xintercept = 0, lty=2) +
  geom_vline(data = rlm_clm_txa_global_estimates %>% filter(time_period=='ALL' & (model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm') & term=='slope'),
             aes(xintercept = Estimate, group = model), lwd=1.5) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, time_period=='ALL' & (model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm') & term=='slope'), 
            aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf), alpha=0.4) +
  scale_fill_manual(name = 'Cell-level change', values = col_change) +
  labs(y = 'Number of cells',
       x = 'Cell-level slope estimate', 
       subtitle = 'b.   Frequency distributions of cell-level slopes for realm-climate-taxa models') +
  theme_bw() +
  theme(legend.position = c(0.9, 0.8),
        legend.background = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')) 


# ggsave('rct_cell_slopes_histogram.png', width = 300, height = 100, units = 'mm')

cowplot::plot_grid(bt_cell_slope_hist, rct_cell_slope_hist, nrow = 2, align = 'hv')
# ggsave('bt_rct__cell_slopes_histogram.png', width = 300, height = 200, units = 'mm')


# study-level sensitivity
study_meta <- rarefied_medians %>%
  group_by(STUDY_ID) %>% 
  summarise(REALM = unique(REALM), 
            # climate_mod = unique(climate_mod), 
            taxa_mod = unique(taxa_mod), 
            n_cells = n_distinct(rarefyID), 
            mu_years = mean(num_years), 
            mu_duration = mean(duration), 
            mu_SamplePool = mean(SamplePool),
            mu_SampleN = mean(SampleN), 
            mu_startYear = mean(startYear))

rlm_clm_txa_studyID_estimate %>% 
  separate(level, into = c('Realm', 'Climate', 'Taxa', 'STUDY_ID'), sep = '_', remove = FALSE) %>% 
  filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_new_norm' ) %>%
  inner_join(study_meta %>% mutate(STUDY_ID = as.character(STUDY_ID)), by = 'STUDY_ID') %>% 
  ggplot() +
  facet_wrap(~model, scales='free', nrow = 1) +
  geom_point(aes(mu_years, Estimate.cYEAR, colour=change, size = mu_duration), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(name='Cell-level\nchange', values = col_change) +
  scale_size_area(name='Duration\n(years)', breaks = c(2,4,8,16,32,64,128)) +
  stat_smooth(se=F, aes(mu_years, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Number of years sampled',
       y = 'Slope estimate (study-level)') +
       # subtitle = 'a.   Cell-level slopes as function of the number of years in time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))
