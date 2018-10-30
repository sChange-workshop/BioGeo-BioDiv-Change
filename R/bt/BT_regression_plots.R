# code to plot the biome-taxa level regression estiamtes
# code to examine relationships between alpha and beta-diversity change
rm(list=ls())

#------run script to clean coefficients and load a couple of extra libraries for plotting------------
source('~/Dropbox/1current/BioTime/local_code/hierarchical/2aBiomeTaxa/BTRfyID_coef_wrangle2.R')
##---------------set working directory, colours etc for plotting----------------------
##  set working directory to save plots
setwd('~/Dropbox/1current/BioTime/iDiv2017/figs/')
setwd('~/Dropbox/BiogeoBioTIME/Figures/')
setwd('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/figs/')
##  colour, shape and linetype settings
#cols = c('down' = 'red', 'neutral' = 'grey', 'up' = 'blue')
cols_change = c('up' = 'blue', 'neutral' = 'dark green', 'down' = 'red')
cols_change_grey = c('up' = 'blue', 'neutral' = 'grey', 'down' = 'red')
shapes_realm = c('Marine' = 19, 'Freshwater' = 17, 'Terrestrial' = 15)
linetypes_realm = c('Marine' = 1, 'Freshwater' = 2, 'Terrestrial' = 3)

cols = c('Marine' = 'blue', 'Freshwater' = 'dark green', 'Terrestrial' = 'orange')
shapes = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
           'Plant' = 5, 'All' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)

cols_climate = c('Temperate' = 'dark green', 'Polar' = 'light blue', 'Tropical' = 'red')
cols_climate2 = c('temperate/polar' = 'dark green', 'tropical' = 'red')
cols_taxa = c('Invertebrates' = '#a6cee3', 'Fish' = '#1f78b4', 'Benthos' = '#b2df8a', 'Birds' = '#33a02c',
              'Mammals' = '#fb9a99', 'Plant' = '#e31a1c', 'All' = '#fdbf6f', 'Marine invertebrates/plants' = '#ff7f00',
              'Amphibians' = '#cab2d6')
col_Biome_ranef = c('up' = 'blue', 'down' = 'red')

# wrangle biome names for cleaner plots
# rarefied_medians <- rarefied_medians %>%
#   mutate(Biome = gsub('_', ' ', Biome),
#          # fix some other random ones up
#          Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
#          Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
#          Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
#          Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
#          Biome = ifelse(Biome=='large lakes', 'Large lakes', Biome),
#          Biome = ifelse(Biome=='polar freshwaters', 'Polar freshwaters', Biome),
#          Biome = ifelse(Biome=='temperate upland rivers', 'Temperate upland rivers', Biome),
#          Biome = ifelse(Biome=='temperate coastal rivers', 'Temperate coastal rivers', Biome),
#          Biome = ifelse(Biome=='temperate floodplain rivers and wetlands', 'Temperate floodplain rivers and wetlands', Biome))
# 
# BT_taxa_estimate <- BT_taxa_estimate %>%
#   mutate(Biome = gsub('_', ' ', Biome),
#          # fix some other random ones up
#          Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
#          Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
#          Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
#          Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
#          Biome = ifelse(Biome=='large lakes', 'Large lakes', Biome),
#          Biome = ifelse(Biome=='polar freshwaters', 'Polar freshwaters', Biome),
#          Biome = ifelse(Biome=='temperate upland rivers', 'Temperate upland rivers', Biome),
#          Biome = ifelse(Biome=='temperate coastal rivers', 'Temperate coastal rivers', Biome),
#          Biome = ifelse(Biome=='temperate floodplain rivers and wetlands', 'Temperate floodplain rivers and wetlands', Biome),
#          Realm2 = ifelse(REALM=='Marine', 'Marine', 'Terrestrial/Freshwater'))
# 
# BT_taxa_estimate$taxa_mod2 <- factor(BT_taxa_estimate$taxa_mod, 
#                                      levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
#                                                 "All", "Marine invertebrates/plants", "Mammals"),
#                                      labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
#                                                 "Multiple taxa", "Marine invertebrates/plants", "Mammals"))
# 
# BT_biome_estimate <- BT_biome_estimate %>%
#   mutate(# fix some other random ones up
#     Biome_clean = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
#     Biome_clean = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
#     Biome_clean = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
#     Biome_clean = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
#     Biome_clean = ifelse(Biome=='large_lakes', 'Large lakes', Biome),
#     Biome_clean = ifelse(Biome=='polar_freshwaters', 'Polar freshwaters', Biome),
#     Biome_clean = ifelse(Biome=='temperate_upland_rives', 'Temperate upland rivers', Biome),
#     Biome_clean = gsub('_', ' ', Biome))
# 
#--------regression relationships with data: global and taxa-level estimates (coloured by realm)---------------
Jbeta_dataSlopes <- ggplot(filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3)) +
  #facet_wrap(~model_name, scales='free') +
  geom_point(aes(YEAR, y= Jbeta_base), colour='grey', alpha=0.4, size=1) +
  geom_segment(data = filter(BT_taxa_estimate, model=='Jbeta_norm'),
               aes(x=start2, y=(Estimate.Intercept+Estimate.cYEAR*start), 
                   xend=stop2, yend=(Estimate.Intercept+Estimate.cYEAR*stop), colour=REALM), lwd=0.5) +
  geom_segment(data = filter(BT_global_estimates, model=='Jbeta_norm' & time_period=='ALL'),
               aes(x=start2, y=(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
                   yend=(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_colour_manual(values=cols) +
  ylab('Total dissimilarity') +
  xlab('') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position=c(0.1, 0.9), panel.grid = element_blank(), legend.direction = 'vertical', legend.text = element_text(size=12), 
        legend.title = element_blank(), legend.background = element_blank()) 

Jne_dataSlopes <- ggplot(filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3)) +
  #facet_wrap(~model_name, scales='free') +
  geom_point(aes(YEAR, y= Jne_base), colour='grey', alpha=0.4, size=1) +
  # geom_segment(data = filter(BTRfyID_rarefyID_coef, model=='Jne_norm'),
  #              aes(x=start2, y=(Estimate.Intercept+Estimate.cYEAR*start), 
  #                  xend=stop2, yend=(Estimate.Intercept+Estimate.cYEAR*stop), colour=REALM), alpha=0.2, lwd=0.1) +
  geom_segment(data = filter(BT_taxa_estimate, model=='Jne_norm'),
               aes(x=start2, y=(Estimate.Intercept+Estimate.cYEAR*start), 
                   xend=stop2, yend=(Estimate.Intercept+Estimate.cYEAR*stop), colour=REALM), lwd=0.5) +
  geom_segment(data = filter(BT_global_estimates, model=='Jne_norm' & time_period=='ALL'),
               aes(x=start2, y=(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
                   yend=(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_colour_manual(values=cols) +
  ylab('Nestedness component of dissimilarity') +
  xlab('') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position=c(0.1, 0.9), panel.grid = element_blank(), legend.direction = 'vertical', legend.text = element_text(size=12), 
        legend.title = element_blank(), legend.background = element_blank()) 

Jtu_dataSlopes <- ggplot(filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3)) +
  #facet_wrap(~model_name, scales='free') +
  geom_point(aes(YEAR, y= Jtu_base), colour='grey', alpha=0.4, size=1) +
  geom_segment(data = filter(BT_taxa_estimate, model=='Jtu_norm'),
               aes(x=start2, y=(Estimate.Intercept+Estimate.cYEAR*start), 
                   xend=stop2, yend=(Estimate.Intercept+Estimate.cYEAR*stop), colour=REALM), lwd=0.5) +
  geom_segment(data = filter(BT_global_estimates, model=='Jtu_norm' & time_period=='ALL'),
               aes(x=start2, y=(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
                   yend=(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_colour_manual(values=cols) +
  ylab('Turnover component of dissimilarity') +
  xlab('') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = 'none') 

BT_taxa_estimate_clm <- inner_join(rarefied_medians %>% distinct(Biome, taxa_mod, climate_mod),
                                   BT_taxa_estimate, by = c('Biome', 'taxa_mod'))
BT_taxa_estimate_clm %>% distinct(Biome, taxa_mod)

bt_data <- rarefied_medians

# S_dataSlopes <- 
  ggplot() +
  facet_wrap(Biome~taxa_mod, scales='free') +
  # rarefyID estimates
  # geom_segment(data = filter(BTRfyID_rarefyID_coef, model=='S_pois'),
  #              aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
  #                  xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop)), colour='dark grey', lwd=1, alpha=0.2) +
  geom_point(data = rarefied_medians %>% filter(climate_mod=='Tropical'),#filter(BROAD_TYPE=='count' & n_cells>3  & climate_mod=='Tropical'),
             aes(YEAR, y= S, colour = taxa_mod), alpha=0.5, size=0.75) +#, colour='grey'
  # rct estimates
  #   geom_segment(data = filter(rlm_clm_txa_estimate, model=='S_pois' & time_period=='ALL' & Climate=='Tropical') ,
  #                aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
  #                    xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop), colour = Taxa), lwd=1.2, lty=1) +
  # # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate_clm, model=='S_pois' & climate_mod=='Tropical' & time_period=='ALL'),
               aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
                   xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop), colour=taxa_mod, linetype = REALM), lwd=0.5) +
  # biome estimates
  # geom_segment(data = filter(BT_biome_estimate, model=='S_pois') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
  #              aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
  #                  xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop), colour=REALM), lwd=0.5) +
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(trans = 'log', breaks = c(1,10,100,1000)) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Species richness') +
  xlab('Year') +
  theme_bw() +
  coord_cartesian(ylim=c(1,1000)) +
  theme(legend.position='none', panel.grid = element_blank(), legend.direction = 'vertical', legend.title = element_blank(), legend.text = element_text(size=14), 
        legend.background = element_blank(), axis.text = element_text(size = 14), axis.title = element_text(size = 18)) 


cowplot::plot_grid(S_dataSlopes, Jtu_dataSlopes, ncol = 2)

#--------regression faceted on biomes to show sometimes contrasting taxaonomic trends-----------
# round axis labels to whole numbers
#Our transformation function
scaleFUN <- function(x) sprintf("%.1f", x)

Jtu_dataSlopes_terr_biomes <- ggplot() +
  facet_wrap(~Biome_clean, scales='free', labeller = label_wrap_gen(width = 20)) +
  geom_point(data = filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3 & REALM!='Marine'),
             aes(YEAR, y=Jtu_base, colour = taxa_mod), alpha=0.5, size=0.75) +
  # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate, model=='Jtu_norm' & REALM!='Marine'),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop, colour = taxa_mod), 
               lwd=1) +
  # biome estimates
  geom_segment(data = filter(BT_biome_estimate, model=='Jtu_norm' & REALM!='Marine') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop), lwd=1) +
  # global estimate
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(labels = scaleFUN) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Turnover component of total Jaccard dissimilarity') +
  xlab('Year') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position=c(0.85, 0.1), panel.grid = element_blank(), 
        legend.direction = 'vertical', legend.title = element_blank(), 
        legend.background = element_blank()) 

# pdf('Jtu_terr_biome_taxa_regression.pdf', width = 11.7, height = 8.3)
# Jtu_dataSlopes_terr_biomes
# dev.off()

Jtu_dataSlopes_marine_biomes <- ggplot() +
  facet_wrap(~Biome_clean, scales='free', labeller = label_wrap_gen(width = 20)) +
  geom_point(data = filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3 & REALM=='Marine'),
             aes(YEAR, y=Jtu_base, colour = taxa_mod), alpha=0.5, size=0.75) +
  # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate, model=='Jtu_norm' & REALM=='Marine'),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop, colour = taxa_mod), 
               lwd=1) +
  # biome estimates
  geom_segment(data = filter(BT_biome_estimate, model=='Jtu_norm' & REALM=='Marine') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop), lwd=1) +
  # global estimate
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(labels = scaleFUN) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Turnover component of total Jaccard dissimilarity') +
  xlab('Year') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position=c(0.85, 0.075), panel.grid = element_blank(), 
        legend.direction = 'vertical', legend.title = element_blank(), 
        legend.background = element_blank())  +
  guides(colour = guide_legend(nrow = 4))

# pdf('Jtu_marine_biome_taxa_regression.pdf', width = 11.7, height = 8.3)
# Jtu_dataSlopes_marine_biomes
# dev.off()

Jne_dataSlopes_terr_biomes <- ggplot() +
  facet_wrap(~Biome_clean, scales='free', labeller = label_wrap_gen(width = 20)) +
  geom_point(data = filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3 & REALM!='Marine'),
             aes(YEAR, y=Jne_base, colour = taxa_mod), alpha=0.5, size=0.75) +
  # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate, model=='Jne_norm' & REALM!='Marine'),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop, colour = taxa_mod), 
               lwd=1) +
  # biome estimates
  geom_segment(data = filter(BT_biome_estimate, model=='Jne_norm' & REALM!='Marine') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop), lwd=1) +
  # global estimate
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(labels = scaleFUN) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Turnover component of total Jaccard dissimilarity') +
  xlab('Year') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position=c(0.85, 0.1), panel.grid = element_blank(), 
        legend.direction = 'vertical', legend.title = element_blank(), 
        legend.background = element_blank()) 

# pdf('Jne_terr_biome_taxa_regression.pdf', width = 11.7, height = 8.3)
# Jne_dataSlopes_terr_biomes
# dev.off()

Jne_dataSlopes_marine_biomes <- ggplot() +
  facet_wrap(~Biome_clean, scales='free', labeller = label_wrap_gen(width = 20)) +
  geom_point(data = filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3 & REALM=='Marine'),
             aes(YEAR, y=Jne_base, colour = taxa_mod), alpha=0.5, size=0.75) +
  # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate, model=='Jne_norm' & REALM=='Marine'),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop, colour = taxa_mod), 
               lwd=1) +
  # biome estimates
  geom_segment(data = filter(BT_biome_estimate, model=='Jne_norm' & REALM=='Marine') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
               aes(x=start2, y=Estimate.Intercept+Estimate.cYEAR*start,
                   xend=stop2, yend=Estimate.Intercept+Estimate.cYEAR*stop), lwd=1) +
  # global estimate
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(labels = scaleFUN) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Turnover component of total Jaccard dissimilarity') +
  xlab('Year') +
  theme_bw() +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position=c(0.85, 0.075), panel.grid = element_blank(), 
        legend.direction = 'vertical', legend.title = element_blank(), 
        legend.background = element_blank())  +
  guides(colour = guide_legend(nrow = 4))

# pdf('Jne_marine_biome_taxa_regression.pdf', width = 11.7, height = 8.3)
# Jne_dataSlopes_marine_biomes
# dev.off()


S_dataSlopes_terr_biomes <- ggplot() +
  facet_wrap(~Biome_clean, scales='free') +
  geom_point(data = filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3 & REALM!='Marine'),
             aes(YEAR, y=S, colour = taxa_mod), alpha=0.5, size=0.75) +
  # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate, model=='S_pois' & REALM!='Marine'),
               aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
                   xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop), colour = taxa_mod), 
               lwd=1) +
  # biome estimates
  geom_segment(data = filter(BT_biome_estimate, model=='S_pois' & REALM!='Marine') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
               aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
                   xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop)), lwd=1) +
  # global estimate
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(trans = 'log', labels = scaleFUN) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Species richness') +
  xlab('Year') +
  theme_bw() +
  # coord_cartesian(ylim=c(1,1000)) +
  theme(legend.position=c(0.85, 0.1), panel.grid = element_blank(), 
        legend.direction = 'vertical', legend.title = element_blank(), 
        legend.background = element_blank()) 

# pdf('terr_biome_taxa_regression.pdf', width = 11.7, height = 8.3)
# S_dataSlopes_terr_biomes
# dev.off()

S_dataSlopes_marine_biomes <- ggplot() +
  facet_wrap(~Biome_clean, scales='free', labeller = label_wrap_gen(width = 20)) +
  geom_point(data = filter(rarefied_medians, BROAD_TYPE=='count' & n_cells>3 & REALM=='Marine'), 
             aes(YEAR, y=S, colour = taxa_mod), alpha=0.5, size=0.75) +
  # taxa estimates
  geom_segment(data = filter(BT_taxa_estimate, model=='S_pois' & REALM=='Marine'),
               aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
                   xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop), colour = taxa_mod), 
               lwd=1) +
  # biome estimates
  geom_segment(data = filter(BT_biome_estimate, model=='S_pois' & REALM=='Marine') %>% distinct(Estimate.cYEAR, .keep_all=TRUE),
               aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start),
                   xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop)), lwd=1) +
  # global estimate
  # geom_segment(data = filter(BT_global_estimates, model=='S_pois' & time_period=='ALL'),
  #              aes(x=start2, y=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*start), xend=stop2, 
  #                  yend=exp(Estimate[term=='Intercept']+Estimate[term=='Slope']*stop)), colour='black', lwd=1.5) +
  scale_y_continuous(trans = 'log', labels = scaleFUN) +
  scale_colour_manual(values=cols_taxa) +
  ylab('Species richness') +
  xlab('Year') +
  theme_bw() +
  # coord_cartesian(ylim=c(1,1000)) +
  theme(legend.position=c(0.7, 0.075), panel.grid = element_blank(), 
        legend.direction = 'horizontal', legend.title = element_blank(), 
        legend.background = element_blank()) +
  guides(colour = guide_legend(nrow = 4))

# pdf('marine_biome_taxa_regression.pdf', width = 11.7, height = 8.3)
# S_dataSlopes_marine_biomes
# dev.off()


#------some maps & distributions of the to help understand the dataset level estimates for suppMat-----
world <- map_data('world')

cell_map_realm <- ggplot() +
  # geom_sf(data = filter(mar_shp2, Biome %in% mar_biomes$Biome),
  #         aes(Biome, fill = Slope_ranef*100, linetype = NA)) +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#f0f0f0', size=0) +
  geom_point(data = rarefied_medians %>% distinct(rarefyID, .keep_all=T),
             aes(x = rarefyID_x, y = rarefyID_y, colour = REALM), size = 0.5, alpha = 0.75) +
  scale_color_manual(values = cols) +
  xlab('') +
  ylab('') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        plot.margin = margin(0,0,0,0, 'mm'),
        legend.position = c(0.2, 0.95), legend.direction = 'horizontal',
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

cell_map_realm
# ggsave('cell_map_realm.pdf', width = 290, height = 200, units = 'mm')
# ggsave('cell_map_realm.png', width = 290, height = 200, units = 'mm')

cell_count_biome <- ggplot() +
  facet_wrap( ~ REALM, scales = 'free_x') + 
  geom_bar(data = rarefied_medians %>% distinct(rarefyID, .keep_all=TRUE),
           aes(x = Biome_clean),
           stat = 'count') +
  labs(x = 'Biome',
       y = 'Number of time series (cells)') +
  scale_y_log10(breaks = c(1,10,100,1000,10000)) +
  theme_bw() +
  theme() +
  coord_flip()

cell_count_biome
# ggsave('cell_count_biome.pdf', width = 290, height = 200, units = 'mm')
# ggsave('cell_count_biome.png', width = 290, height = 200, units = 'mm')

rarefied_medians$taxa_mod2 <- factor(rarefied_medians$taxa_mod, 
                                     levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "All", "Marine invertebrates/plants", "Mammals"),
                                     labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "Multiple taxa", "Marine invertebrates/plants", "Mammals"))


taxa_col = c('Multiple taxa' = '#e6f598',
             'Amphibians' = '#fee08b',
             'Benthos' = '#5e4fa2',
             'Birds' = '#f46d43',
             'Fish' = '#3288bd',
             'Invertebrates' = '#abdda4',
             'Mammals' = '#9e0142',
             'Marine invertebrates/plants' = '#66c2a5',
             'Plant' = '#fdae61')

biome_taxa_breaks <- with(rarefied_medians,
                          paste(Biome, taxa_mod2, sep = '.')) %>% unique()

biome_taxa_labs <- data.frame(breaks = biome_taxa_breaks) %>%
  separate(col = breaks,  into = c('biome_lab', 'taxa_lab'), sep = '[.]', remove = F)


marine_biome_taxa_cell_count <- ggplot() +
  facet_wrap( ~ Realm2, scales = 'free_x') + 
  geom_col(data = BT_taxa_estimate %>% filter(model=='S_pois' & Realm2=='Marine'),
           aes(x = interaction(Biome, taxa_mod2), y = n_cells, fill = taxa_mod2)) +
  labs(x = 'Biome',
       y = 'Number of time series (cells)') +
  scale_y_log10(breaks = c(1,10,100,1000,10000)) +
  scale_x_discrete(breaks = biome_taxa_breaks, labels = biome_taxa_labs$biome_lab) +
  scale_fill_manual(values = taxa_col) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) +
  coord_flip()


legend_taxa_1row <- ggplot() +
  geom_col(data = BT_taxa_estimate %>% filter(model=='S_pois' & Realm2!='Marine'),
           aes(x = interaction(Biome, taxa_mod2), y = n_cells, fill = taxa_mod2)) +
  labs(x = 'Biome',
       y = 'Number of time series (cells)') +
  scale_y_log10(breaks = c(1,10,100,1000,10000)) +
  scale_x_discrete(breaks = biome_taxa_breaks, labels = biome_taxa_labs$biome_lab) +
  scale_fill_manual(name = 'Taxa', values = taxa_col, drop = FALSE) +
  theme_bw() +
  theme(legend.position = 'top', legend.background = element_rect(fill='transparent'),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 24)) +
  guides(fill = guide_legend(nrow = 1)) +
  coord_flip() 

source('~/Dropbox/1current/R_random/functions/gg_legend.R')        
taxa_legend <- gg_legend(legend_taxa_1row)

terr_biome_taxa_cell_count <- ggplot() +
  facet_wrap( ~ Realm2, scales = 'free_x') + 
  geom_col(data = BT_taxa_estimate %>% filter(model=='S_pois' & Realm2!='Marine'),
           aes(x = interaction(Biome, taxa_mod2), y = n_cells, fill = taxa_mod2)) +
  labs(x = 'Biome',
       y = 'Number of time series (cells)') +
  scale_y_log10(breaks = c(1,10,100,1000,10000)) +
  scale_x_discrete(name = '',
                   breaks = biome_taxa_breaks, labels = biome_taxa_labs$biome_lab) +
  scale_fill_manual(name = 'Taxa', values = taxa_col, drop = FALSE) +
  theme_bw() +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) +
  coord_flip()

cell_count_row <- cowplot::plot_grid(marine_biome_taxa_cell_count, terr_biome_taxa_cell_count, ncol = 2, rel_widths = c(0.75, 1),
                                     labels = c('b', 'c'), label_fontface = 'bold')

cowplot::plot_grid(cell_map_realm, taxa_legend, cell_count_row, nrow = 3, rel_heights = c(0.7,0.05,1), labels = c('a', '', ''), label_fontface = 'bold')
# ggsave('Supplement_map_cell_count.png', width = 300, height = 400, units = 'mm')
