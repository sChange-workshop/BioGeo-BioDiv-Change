##------------sensitivity plots for the rct models---------------------
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(tidyverse)

load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/rct/RCTRfyID_rarefyID_coef.Rdata')

##---------add the number of time series used to estimate each slope-------------
##----the data models were fit too
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')

##	create new covariate that is concatenation of realm_climate_taxa
rarefied_medians <- rarefied_medians %>%
  unite(rlm_clm_txa, REALM, climate_mod, taxa_mod, remove=FALSE)

##	count cells within realm/climate/taxa hierarchy
cell_count <- rarefied_medians %>%
  group_by(rlm_clm_txa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by='rlm_clm_txa')

##	filter to ecoregions with >3 cells
rarefied_medians <- rarefied_medians %>%
  filter(n_cells > 3)

## get duration, num_years and other metadata for each rarefyID
cell_meta <- rarefied_medians %>%
  distinct(REALM, climate_mod, taxa_mod, rarefyID, rarefyID_x, rarefyID_y, num_years, duration, SamplePool, SampleN, startYear, endYear)

initialS <- rarefied_medians %>% distinct(rarefyID, .keep_all=TRUE) %>%
  filter(YEAR == startYear) %>%
  mutate(initialS = S) %>%
  select(rarefyID, initialS)

cell_meta <- inner_join(cell_meta, initialS, by='rarefyID')

## need to split level into rct and rarefyID
rarefyID_estimate <- rlm_clm_txa_rarefyID_estimate %>%
  separate(level, into = c('Realm', 'Climate', 'Taxa', 'b4', 'b5'), sep = '_', remove = FALSE) %>%
  unite(rarefyID, b4, b5)

## combine with rarefyID estimates for plotting
rarefyID_estimate <- inner_join(cell_meta, rarefyID_estimate, by = 'rarefyID') %>%
  mutate(change = ifelse(lower_slope > 0, 'up',
                         ifelse(upper_slope < 0, 'down', 'neutral'))) %>%
  unite(RealmClimate, c(Realm,Climate), remove = FALSE)

rarefyID_estimate$model_name <- factor(rarefyID_estimate$model, levels = c('S_pois', 'Jbeta_norm', 'Jne_norm', 'Jtu_norm', 'N_lognorm', 'ENSPIE_pois'),
                                  labels = c('Species richness', 'Total dissimilarity', 'Nestedness component', 'Turnover component', 'Abundance (N)', 'ENSPIE'))


# meta data for rct-level estimates (these will have to be means - multiple start years, durations etc)
rct_meta <- rarefied_medians %>%
  group_by(rlm_clm_txa) %>%
  summarise(
    N_samples = mean(num_years),
    duration_bar = mean(duration),
    start_bar = mean(startYear),
    end_bar = mean(endYear),
    Spool_bar = mean(SamplePool)) %>%
  mutate(rct = rlm_clm_txa)

rct_estimate <- inner_join(rct_meta, rlm_clm_txa_estimate, by = 'rct')

rct_estimate$model_name <- factor(rct_estimate$model, levels = c('S_pois', 'Jbeta_norm', 'Jne_norm', 'Jtu_norm', 'N_lognorm', 'ENSPIE_pois'),
                                  labels = c('Species richness', 'Total dissimilarity', 'Nestedness component', 'Turnover component', 'Abundance (N)', 'ENSPIE'))

##  set working directory to save plots
setwd('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/rct/figs/sensitivity/')
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
rct_numYears <- rarefyID_estimate %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(num_years, Estimate.cYEAR, colour=change, size = duration), alpha = 0.2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(name='Cell-level\nchange', values = col_change) +
  scale_size_area(name='Duration\n(years)', breaks = c(2,4,8,16,32,64,128)) +
  stat_smooth(se=F, aes(num_years, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Number of years sampled',
       y = 'Slope estimate (cell-level)',
       subtitle = 'a.   Cell-level slopes as function of the number of years in time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

# ggsave('cell-level_slope~num_years1.png', width = 220, height = 200, units = 'mm')
# work towards a plot showing the distribution of cell-slopes as a function of StudyMethod


rct_startYear <- rarefyID_estimate %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(startYear, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(25, 50, 75)) +
  stat_smooth(se=F, aes(startYear, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Start year',
       y = 'Slope estimate (cell-level)',
       subtitle = 'b.   Cell-level slopes as function of time series start year') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')) #+
  # guides(colour = guide_legend(override.aes = list(alpha=1)))

# ggsave('cell-level_slope~startYear1.png', width = 230, height = 200, units = 'mm')

rct_initialS <- rarefyID_estimate %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_wrap(~model_name, scales='free', nrow = 1) +
  geom_point(aes(initialS, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(25, 50, 75)) +
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8,16,32,64,128,256,512,1024),
                     labels = c(1,2,4,8,16,32,64,'',256,'',1024)) +
  stat_smooth(se=F, aes(initialS, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Initial species richness',
       y = 'Slope estimate (cell-level)',
       subtitle = 'c.   Cell-level slopes as a function of the initial species richness') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')) #+
  # guides(colour = guide_legend(override.aes = list(alpha=1)))

cowplot::plot_grid(rct_numYears, rct_startYear, rct_initialS, nrow = 3, align = 'hv')
# ggsave('rct_cell-level_slope_sensitivity_9panel.png', width = 350, height = 350, units = 'mm')

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