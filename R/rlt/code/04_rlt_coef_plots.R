##------results of realm/climate/taxa hierarchical NEW MODELS-----------------------
# code modified 210219
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(tidyverse)

load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/rlt/model_fits_output/rlm_clm_txa_modelCoefs.Rdata')

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
  filter(n_cells > 3 & BROAD_TYPE=='count')

##  redo cell counts (this will be for the whole data set combined)
cell_count3_ALL <- rarefied_medians %>%
  group_by(rlm_clm_txa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  mutate(time_period='ALL')

cell_count3_STUDY <- rarefied_medians %>%
  group_by(rlm_clm_txa, STUDY_ID) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  mutate(time_period='ALL')

##  create the time sliced data
# load('/gpfs1/data/sChange/BioTime/data/rarefied_medians.Rdata')
# rarefied_medians <- rarefied_medians %>%
#   unite(rlm_clm_txa, REALM, climate_mod, taxa_mod, remove=FALSE)
ts1 <- filter(rarefied_medians, YEAR >= 1951 & YEAR <= 1970)
ts2 <- filter(rarefied_medians, YEAR >= 1971 & YEAR <= 1990)
ts3 <- filter(rarefied_medians, YEAR >= 1991 & YEAR <= 2010)

##  determine which rarefyID's have only one time point (this is a potential problem)
##  duration==1, misses when duration==0!!
oneYear_ts1 <- ts1 %>% 
  group_by(rarefyID) %>% 
  dplyr::summarise(duration = max(YEAR) - min(YEAR)) %>%
  filter(duration == 1)

oneYear_ts2 <- ts2 %>% 
  group_by(rarefyID) %>% 
  dplyr::summarise(duration = max(YEAR) - min(YEAR)) %>%
  filter(duration == 1)

oneYear_ts3 <- ts3 %>% 
  group_by(rarefyID) %>% 
  dplyr::summarise(duration = max(YEAR) - min(YEAR)) %>%
  filter(duration == 1)

ts1 <- ts1 %>% filter(!(rarefyID %in% oneYear_ts1$rarefyID))
ts2 <- ts2 %>% filter(!(rarefyID %in% oneYear_ts2$rarefyID))
ts3 <- ts3 %>% filter(!(rarefyID %in% oneYear_ts3$rarefyID))

##  redo cell counts for each time slice & reduce to rlm_clm_txa with > 3 time series
cell_count_ts1 <- ts1 %>%
  filter(BROAD_TYPE=='count') %>%
  group_by(rlm_clm_txa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  filter(n_cells > 3) %>%
  mutate(time_period = '1951-1970')

cell_count_ts2 <- ts2 %>%
  filter(BROAD_TYPE=='count') %>%
  group_by(rlm_clm_txa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  mutate(time_period = '1971-1990') 

cell_count_ts3 <- ts3 %>%
  filter(BROAD_TYPE=='count') %>%
  group_by(rlm_clm_txa) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  mutate(time_period = '1991-2010') 

cell_counts <- bind_rows(cell_count3_ALL, cell_count_ts1, cell_count_ts2, cell_count_ts3) %>%
  mutate(rct = rlm_clm_txa) %>%
  select(-rlm_clm_txa)

##  need cell counts for studies too...
cell_count_ts1S <- ts1 %>%
  filter(rlm_clm_txa %in% cell_count_ts1$rlm_clm_txa) %>% 
  group_by(rlm_clm_txa, STUDY_ID) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>% 
  mutate(time_period = '1951-1970')

cell_count_ts2S <- ts2 %>%
  filter(rlm_clm_txa %in% cell_count_ts2$rlm_clm_txa) %>% 
  group_by(rlm_clm_txa, STUDY_ID) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  mutate(time_period = '1971-1990') 

cell_count_ts3S <- ts3 %>%
  filter(rlm_clm_txa %in% cell_count_ts3$rlm_clm_txa) %>% 
  group_by(rlm_clm_txa, STUDY_ID) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  mutate(time_period = '1991-2010') 

cell_counts_study <- bind_rows(cell_count3_STUDY, cell_count_ts1S, cell_count_ts2S, cell_count_ts3S) %>%
  unite(level, c(rlm_clm_txa, STUDY_ID))

##  combine with RCT estimates for plotting
rlm_clm_txa_estimate <- inner_join(cell_counts, rlm_clm_txa_estimate, by=c('rct', 'time_period'))

##  split rct into realm climate taxa
rlm_clm_txa_estimate <- rlm_clm_txa_estimate %>% 
  separate(rct, c('Realm', 'Climate', 'Taxa'), sep='_', remove=FALSE) 

# northern hemisphere window
rlm_clm_txa_estimate$Climate <- factor(rlm_clm_txa_estimate$Climate, levels = c('Tropical', 'Temperate', 'Polar'))

# add cell counts and split grouping variables
rlm_clm_txa_studyID_estimate <- right_join(cell_counts_study, rlm_clm_txa_studyID_estimate, by=c('level', 'time_period'))
rlm_clm_txa_studyID_estimate <- rlm_clm_txa_studyID_estimate %>% 
  separate(level, c('Realm', 'Climate', 'Taxa', 'StudyID'), sep='_')

##  fix the levels of rarefyID slope estimates so as they read rarefyIDs
rarefyID_estimate <- rlm_clm_txa_rarefyID_estimate %>% 
  separate(level, c('Realm', 'Climate', 'Taxa', 'StudyID', 'cell'), sep='_') %>%
  unite(rarefyID, c(StudyID, cell)) %>%
  unite(rlm_clm_txa, c(Realm, Climate, Taxa), remove=FALSE)


#cols = c('down' = 'red', 'neutral' = 'grey', 'up' = 'blue')
cols = c('Tropical' = 'blue', 'Polar' = 'snow3', 'Temperate' = 'orange')
col_realm = c('Marine' = 'blue', 'Terrestrial' = 'orange', 'Freshwater' = 'dark green')
shapes = c('Marine' = 19, 'Freshwater' = 17, 'Terrestrial' = 15)
shape_txa = c('All' = 6, 'Amphibians' = 8, 'Benthos' = 2, 'Birds' = 15, 'Fish' = 1, 'Invertebrates' = 0, 'Mammals' = 17, 'Marine invertebrates/plants' = 7, 'Plant' = 5)
linetypes = c('Marine' = 1, 'Freshwater' = 1, 'Terrestrial' = 1)

##-------plot nestedness component of total dissimilarity coefficients-------
ggplot() +
  facet_grid(~time_period, scales = 'free') +
  geom_point(data = filter(rlm_clm_txa_estimate, model=='Jne_new_norm'), 
             aes(x = Taxa, y = Estimate.cYEAR, group=interaction(Climate, Realm), colour = Climate, shape=Realm), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(rlm_clm_txa_estimate, model=='Jne_new_norm'),
                 aes(x = Taxa, ymin = lower_slope, group=interaction(Climate, Realm), ymax = upper_slope, 
                     colour = Climate, linetype=Realm), position = position_dodge(width=0.7)) +
  geom_text(data = filter(rlm_clm_txa_estimate, model=='Jne_new_norm'),
            aes(x=Taxa, y=upper_slope+0.03, label=n_cells, group=interaction(Climate, Realm)),
            position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(values=cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(rlm_clm_txa_global_estimates, model=='Jne_norm' & term=='slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, model=='Jne_norm' & term=='slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope') +
  xlab('Taxa') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Realm-latitude-taxa nestedness component [% dissimilarity/yr]') +
  coord_flip() #+
  # ggsave('rlm_clm_txa_Jne_norm_timeSlice_free.png', width=290, height = 200, units='mm')

# plot with climate on the yaxis
ggplot() +
  facet_grid(~time_period, scales = 'free') +#
  geom_point(data = filter(rlm_clm_txa_estimate, model=='Jne_new_norm'), 
             aes(x = Climate, y = Estimate.cYEAR, group=interaction(Taxa, Realm), colour = Realm, shape=Taxa), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(rlm_clm_txa_estimate, model=='Jne_new_norm'),
                 aes(x = Climate, ymin = lower_slope, group=interaction(Taxa, Realm), ymax = upper_slope, 
                     colour = Realm), position = position_dodge(width=0.7)) +
  geom_text(data = filter(rlm_clm_txa_estimate, model=='Jne_new_norm'),
            aes(x=Climate, y=upper_slope+0.03, label=n_cells, group=interaction(Taxa, Realm)),
            position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(values=col_realm) +
  scale_shape_manual(values = shape_txa) +
  # scale_linetype_manual(values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(rlm_clm_txa_global_estimates, model=='Jne_new_norm' & term=='slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, model=='Jne_new_norm' & term=='slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope') +
  xlab('Latitude') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Realm-latitude-taxa nestedness component [% dissimilarity/yr]') +
  coord_flip() #+
  # ggsave('rct_Jne_norm_timeSlice_free.png', width=290, height = 200, units='mm')

##---------turnover component-------------
ggplot() +
  facet_grid(~time_period) +#, scales = 'free'
  geom_point(data = filter(rlm_clm_txa_estimate, model=='Jtu_new_norm'), 
             aes(x = Taxa, y = Estimate.cYEAR, group=interaction(Climate, Realm), colour = Climate, shape=Realm), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(rlm_clm_txa_estimate, model=='Jtu_new_norm'),
                 aes(x = Taxa, ymin = lower_slope, group=interaction(Climate, Realm), ymax = upper_slope, 
                     colour = Climate, linetype=Realm), position = position_dodge(width=0.7)) +
  geom_text(data = filter(rlm_clm_txa_estimate, model=='Jtu_new_norm'),
            aes(x=Taxa, y=upper_slope+0.03, label=n_cells, group=interaction(Climate, Realm)),
            position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(values=cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(rlm_clm_txa_global_estimates, model=='Jtu_new_norm' & term=='slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, model=='Jtu_new_norm' & term=='slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope') +
  xlab('Taxon groups') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Estimates of change in turnover component') +
  coord_flip() #+
  # ggsave('rlm_clm_txa_Jtu_norm_timeSlice_fixed.png', width=290, height = 200, units='mm')

# plot with climate on the yaxis
ggplot() +
  facet_grid(~time_period, scales = 'free') +#, scales = 'free'
  geom_point(data = filter(rlm_clm_txa_estimate, model=='Jtu_new_norm'), 
             aes(x = Climate, y = Estimate.cYEAR, group=interaction(Taxa, Realm), colour = Realm, shape=Taxa), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(rlm_clm_txa_estimate, model=='Jtu_new_norm'),
                 aes(x = Climate, ymin = lower_slope, group=interaction(Taxa, Realm), ymax = upper_slope, 
                     colour = Realm), position = position_dodge(width=0.7)) +
  geom_text(data = filter(rlm_clm_txa_estimate, model=='Jtu_new_norm'),
            aes(x=Climate, y=upper_slope+0.03, label=n_cells, group=interaction(Taxa, Realm)),
            position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(values=col_realm) +
  scale_shape_manual(values = shape_txa) +
  # scale_linetype_manual(values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(rlm_clm_txa_global_estimates, model=='Jtu_new_norm' & term=='slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, model=='Jtu_new_norm' & term=='slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope [% dissimilarity/yr]') +
  xlab('Latitude') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Estimates of change in turnover component of dissimilarity') +
  coord_flip()#+
  # ggsave('rct_Jtu_norm_timeSlice_free.png', width=290, height = 200, units='mm')
# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science rev3/figures/FigS18.png',
#        width = 290, height = 200, units = 'mm')
##---------S-------------
ggplot() +
  facet_grid(~time_period) +#
  geom_point(data = filter(rlm_clm_txa_estimate, model=='S_pois_new_new'), 
             aes(x = Taxa, y = Estimate.cYEAR, group=interaction(Climate, Realm), colour = Climate, shape=Realm), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(rlm_clm_txa_estimate, model=='S_pois_new_new'),
                 aes(x = Taxa, ymin = lower_slope, group=interaction(Climate, Realm), ymax = upper_slope, 
                     colour = Climate, linetype=Realm), position = position_dodge(width=0.7)) +
  geom_text(data = filter(rlm_clm_txa_estimate, model=='S_pois_new'),
            aes(x=Taxa, y=upper_slope+0.03, label=n_cells, group=interaction(Climate, Realm)),
            position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(values=cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(rlm_clm_txa_global_estimates, model=='S_pois_new' & term=='slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, model=='S_pois_new' & term=='slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope') +
  xlab('Taxa') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Estimates of change in species richness') +
  coord_flip() 

# plot with climate on the y-axis

ggplot() +
  facet_grid(~time_period) +#, scales = 'free'
  geom_point(data = filter(rlm_clm_txa_estimate, model=='S_pois_new'), 
             aes(x = Climate, y = Estimate.cYEAR, group=interaction(Taxa, Realm), colour = Realm, shape=Taxa), 
             size=2, position = position_dodge(width=0.7)) +
  geom_linerange(data = filter(rlm_clm_txa_estimate, model=='S_pois_new'),
                 aes(x = Climate, ymin = lower_slope, group=interaction(Taxa, Realm), ymax = upper_slope, 
                     colour = Realm), position = position_dodge(width=0.7)) +
  geom_text(data = filter(rlm_clm_txa_estimate, model=='S_pois_new'),
            aes(x=Climate, y=upper_slope+0.03, label=n_cells, group=interaction(Taxa, Realm)),
            position = position_dodge(width=0.7), size=3) +
  scale_colour_manual(values=col_realm) +
  scale_shape_manual(values = shape_txa) +
  # scale_linetype_manual(values = linetypes) +
  geom_hline(yintercept = 0, lty=2, alpha = 0.8, lwd = 0.1) +
  geom_hline(data = filter(rlm_clm_txa_global_estimates, model=='S_pois_new' & term=='slope'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(rlm_clm_txa_global_estimates, model=='S_pois_new' & term=='slope'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  ylab('Estimated slope [log(S)/yr]') +
  xlab('Latitude') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.7), axis.text.y = element_text(size =10)) +
  ggtitle('Estimates of change in species richness') +
  coord_flip() 

# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science rev3/figures/FigS17.png',
#        width = 290, height = 200, units = 'mm')
