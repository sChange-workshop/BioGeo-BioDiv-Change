##------------sensitivity plots for the BT models---------------------
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(tidyverse)

source('~/Dropbox/1current/BioTime/local_code/hierarchical/2aBiomeTaxa/BTRfyID_coef_wrangle2.R')

##  set working directory to save plots

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
bt_numYears <- BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
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

# ggsave('BT_cell-level_slope~num_years1.png', width = 220, height = 200, units = 'mm')

bt_startYear <- BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
  geom_point(aes(startYear, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(25, 50, 75)) +
  stat_smooth(se=F, aes(startYear, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Start year',
       y = 'Slope estimate (cell-level)',
       subtitle = 'b.   Cell-level slopes as function of start year of time series') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold'))
# ggsave('BT_cell-level_slope~startYear1.png', width = 230, height = 200, units = 'mm')

initialS <- rarefied_medians %>% distinct(rarefyID, .keep_all=TRUE) %>%
  filter(YEAR == startYear) %>%
  mutate(initialS = S) %>%
  select(rarefyID, initialS)

BTRfyID_rarefyID_coef <- inner_join(initialS, BTRfyID_rarefyID_coef, by = 'rarefyID')
  
bt_initalS <- BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_wrap(~model_clean, scales='free', nrow = 1) +
  geom_point(aes(initialS, Estimate.cYEAR, colour=change, size = num_years), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(guide = FALSE, name='Cell-level\nchange', values = col_change) +
  scale_size_area(name = 'Number of\nyears sampled', breaks = c(25, 50, 75)) +
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8,16,32,64,128,256,512,1024), 
                     labels = c(1,2,4,8,16,32,64, '',256, '',1024)) +
  stat_smooth(se=F, aes(initialS, Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = 'Initial species richness',
       y = 'Slope estimate (cell-level)',
       subtitle = 'c.   Cell-level slopes as a function of initial species richness') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold'))

# put 'em together
cowplot::plot_grid(bt_numYears, bt_startYear, bt_initalS, nrow = 3, align = 'hv')
# ggsave('BT_cell-level_slope_sensitivity_9panel.png', width = 350, height = 350, units = 'mm')


## ---histograms of cell-level slopes
bt_cell_slope_hist <- ggplot() +
  facet_wrap(~model_clean, scales = 'free', nrow = 1) +
  geom_histogram(data = BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model=='Jne_norm' ),
                 aes(x = Estimate.cYEAR, fill = change), binwidth = 0.001) +#
  geom_histogram(data = BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model=='Jtu_norm' ),
                 aes(x = Estimate.cYEAR, fill = change), binwidth = 0.0005) +#
  # geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'N_lognorm'),
  #                aes(x = Estimate.cYEAR, fill = change), binwidth = 0.005) +#
  # geom_histogram(data = rarefyID_estimate %>% filter(time_period=='ALL' & model == 'ENSPIE_pois'),
  #                aes(x = Estimate.cYEAR, fill = change), binwidth = 0.001) +#
  geom_histogram(data = BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model == 'S_pois'),
                 aes(x = Estimate.cYEAR, fill = change), binwidth = 0.005) +#
  geom_vline(xintercept = 0, lty=2) +
  geom_vline(data = BT_global_estimates %>% filter(time_period=='ALL' & (model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm') & term=='Slope'),
             aes(xintercept = Estimate, group = model), lwd=1.5) +
  geom_rect(data = filter(BT_global_estimates, time_period=='ALL' & (model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm') & term=='Slope'), 
            aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf), alpha=0.4) +
  scale_fill_manual(name = 'Cell-level change', values = col_change) +
  labs(y = 'Number of cells',
       # x = 'Cell-level slope estimate',
  subtitle = 'a.   Frequency distributions of cell-level slopes for biome-taxa models') +
  theme_bw() +
  theme(legend.position = c(0.9, 0.8),
        legend.background = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')) 


# ggsave('bt_cell_slopes_histogram.png', width = 300, height = 100, units = 'mm')
xy <- rarefied_medians %>% distinct(rarefyID, rarefyID_x, rarefyID_y)
BTRfyID_rarefyID_coef <- inner_join(xy, BTRfyID_rarefyID_coef, by = 'rarefyID')

BTRfyID_rarefyID_coef %>% filter(time_period=='ALL' & model == 'S_pois' | model=='Jne_norm' | model=='Jtu_norm' ) %>%
  ggplot() +
  facet_grid(Realm2 ~ model_clean) +
  geom_point(aes((rarefyID_y), Estimate.cYEAR, colour=change, size = duration), alpha = 0.2) +
  scale_color_manual(name='Cell-level\nchange', values = col_change) +
  scale_size_area(name='Duration\n(years)', breaks = c(2,4,8,16,32,64,128)) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(-60, -23.5, 0, 23.5, 60)) +
  stat_smooth(se=F, aes((rarefyID_y), Estimate.cYEAR), colour = 'black',
              method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4)) +
  labs(x = '|Latitude|',
       y = 'Slope estimate (cell-level)') +
  theme_bw() +
  coord_flip() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size= 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold'),
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))
