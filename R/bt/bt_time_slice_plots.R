##------results of bt models-----------------------
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(tidyverse)

# get cleaned coef estimates
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/2bt_coef_wrangle.R')

BT_taxa_estimate$taxa_mod2 <- factor(BT_taxa_estimate$taxa_mod, 
       levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                  "All", "Marine invertebrates/plants", "Mammals"),
       labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                  "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

##----------------get ready to plot coefficients----------------------------
##  load the estimates (if you've got 'em)
##  set working directory to save plots
setwd('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/figs/time_slice/')
##  colour, shape and linetype settings
#cols = c('down' = 'red', 'neutral' = 'grey', 'up' = 'blue')
taxa_col = c('Multiple taxa' = '#e6f598',
             'Amphibians' = '#fee08b',
             'Benthos' = '#5e4fa2',
             'Birds' = '#f46d43',
             'Fish' = '#3288bd',
             'Invertebrates' = '#abdda4',
             'Mammals' = '#9e0142',
             'Marine invertebrates/plants' = '#66c2a5',
             'Plant' = '#fdae61')

shapes = c('Marine' = 19, 'Terrestrial' = 17, 'Freshwater' = 8)

linetypes = c('Marine' = 1, 'Freshwater' = 3, 'Terrestrial' = 1)

##---------turnover component-------------
ggplot() +
  facet_grid(~time_period) +#, scales = 'free'
  geom_point(data = filter(BT_taxa_estimate, model=='Jtu_norm'), 
             aes(x = taxa_mod2, y = Estimate.cYEAR, group=interaction(Biome, taxa_mod2), shape=REALM, colour = taxa_mod2), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(BT_taxa_estimate, model=='Jtu_norm'), 
                 aes(x = taxa_mod2, ymin = lower_slope, group=interaction(Biome, taxa_mod2), ymax = upper_slope, 
                     linetype=REALM, colour = taxa_mod2), position = position_dodge(width=0.7)) +
  # geom_text(data = filter(rlm_clm_txa_estimate, model=='Jtu_norm'),
  #           aes(x=Taxa, y=upper_slope+0.03, label=n_cells, group=interaction(Climate, REALM)),
  #           position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(name= 'Taxa', values=taxa_col) +
  scale_shape_manual(name = 'Realm', values = shapes) +
  scale_linetype_manual(name = 'Realm', values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(BT_global_estimates, model=='Jtu_norm' & term=='Slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(BT_global_estimates, model=='Jtu_norm' & term=='Slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope') +
  xlab('Taxa') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Turnover') +
  coord_flip() #+

# ggsave('bt_jtu_time_slice_sensitivity.pdf', width = 290, height = 200, units = 'mm')

# richness
ggplot() +
  facet_grid(~time_period) +#, scales = 'free'
  geom_point(data = filter(BT_taxa_estimate, model=='S_pois'), 
             aes(x = taxa_mod2, y = Estimate.cYEAR, group=interaction(Biome, taxa_mod2), shape=REALM, colour = taxa_mod2), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(BT_taxa_estimate, model=='S_pois'), 
                 aes(x = taxa_mod2, ymin = lower_slope, group=interaction(Biome, taxa_mod2), ymax = upper_slope, 
                     linetype=REALM, colour = taxa_mod2), position = position_dodge(width=0.7)) +
  # geom_text(data = filter(BT_taxa_estimate, model=='S_pois'),
  #           aes(x=taxa_mod2, y=upper_slope+0.1, label=n_cells, group=interaction(Biome, taxa_mod2)),
  #           position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(name= 'Taxa', values=taxa_col) +
  scale_shape_manual(name = 'Realm', values = shapes) +
  scale_linetype_manual(name = 'Realm', values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(BT_global_estimates, model=='S_pois' & term=='Slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(BT_global_estimates, model=='S_pois' & term=='Slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope') +
  xlab('Taxa') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Species richness') +
  coord_flip() #+

ggsave('bt_S_time_slice_sensitivity.pdf', width = 290, height = 200, units = 'mm')
