# code to plot coefficients of new model: this time with realm coded into groups: realm-region-taxa

library(tidyverse)

load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/rrt/model_coef_ranef/rlm_reg_txa_coef_ranef.Rdata')

# first need to wrangle to levels for plots
rrt_rrt_estimate <- rrt_rrt_estimate %>% 
  # separate on the last (rightmost) underscore
  separate(rrt, c('realm_region', 'taxa'), sep = '_(?!.*_)',
           remove = F)

rrt_rrt_estimate <- rrt_rrt_estimate %>% 
  # separate realm and region
  separate(realm_region, c('realm' , 'region'), sep = '(^[^_]*_)',
           remove = F) %>% 
  separate(realm_region, c('realm', 'nuu'), sep = '_', remove = F) %>% 
  select(-nuu)

# add realm metadata
load('~/Dropbox/BiogeoBioTIME/rarefied_medians_continents.Rdata')

rarefied_medians <- rarefied_medians %>% 
  unite(rrt, REALM, region, taxa_mod, remove = F)

rrt <- rarefied_medians %>% 
  select(REALM, region, taxa_mod, rrt, Biome) %>% 
  distinct(region, taxa_mod, rrt, .keep_all = T) %>% 
  rename(taxa = taxa_mod)


rrt_rrt_estimate <- rrt_rrt_estimate %>%
  left_join(rrt,
            by = c('rrt', 'region', 'taxa')) 

# rename group of taxa labelled 'All' to 'Multiple'
rrt_rrt_estimate$taxa2 <- factor(rrt_rrt_estimate$taxa, 
                                      levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                 "All", "Marine invertebrates/plants", "Mammals"),
                                      labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                 "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

# colours for each taxa
taxa_col = c('Multiple taxa' = '#e6f598',
             'Amphibians' = '#fee08b',
             'Benthos' = '#5e4fa2',
             'Birds' = '#f46d43',
             'Fish' = '#3288bd',
             'Invertebrates' = '#abdda4',
             'Mammals' = '#9e0142',
             'Marine invertebrates/plants' = '#66c2a5',
             'Plant' = '#fdae61')

# set shapes for plotting groups of taxa
taxa_shape = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
               'Plant' = 5, 'Multiple taxa' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)


# combine terrestrial and freshwater realms
rrt_rrt_estimate <- rrt_rrt_estimate %>% 
  mutate(realm2 = ifelse(REALM=='Marine', REALM, 'Terrestrial and freshwater'))

# tidy region names for plotting
rrt_rrt_estimate <- rrt_rrt_estimate %>% 
  mutate(region = gsub('_', ' ', region))

# first, get legend for building plot
taxa_col_shape_leg <- ggplot() +
  facet_wrap(~realm2, ncol = 2) +
  geom_point(data = rrt_rrt_estimate %>% filter(model=='Jtu_rlm_reg_txa'),
             aes(x = region, y = Estimate.cYEAR,
                 group = taxa2,
                 colour = taxa2,
                 shape = taxa2),
             position = position_dodge(width = 0.5),
             size = 2) +
  geom_linerange(data = rrt_rrt_estimate %>% filter(model=='Jtu_rlm_reg_txa'),
                 aes(x = region, ymin = lower_slope, ymax = upper_slope,
                     group = taxa2,
                     colour = taxa2),
                 position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = rrt_global_estimates %>% 
               filter(model=='Jtu_rlm_reg_txa' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_rect(data = rrt_global_estimates %>% 
              filter(model=='Jtu_rlm_reg_txa' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.2) +
  scale_colour_manual(name = 'Taxa', values=taxa_col) +
  scale_shape_manual(name = 'Taxa', values = taxa_shape) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal') #+
# guides(shape = guide_legend(ncol = 1),
#        colour = guide_legend(ncol = 1))

source('~/Dropbox/1current/R_random/functions/gg_legend.R')
legend1 <- gg_legend(taxa_col_shape_leg)

# rotate line for legend: see https://stackoverflow.com/questions/35703983/how-to-change-angle-of-line-in-customized-legend-in-ggplot2
library(grid)
GeomLinerange$draw_key <- function(data, params, size) 
{
  segmentsGrob(0, 0.5, 1, 0.5, gp = gpar(col = alpha(data$colour, 
                                                     data$alpha), lwd = data$size * .pt, lty = data$linetype, 
                                         lineend = "butt"))
}
# separate realms to declutter
marine_region_taxa_coef <- ggplot() +
  # facet_wrap(.~climate_mod, scales = 'free') +
  geom_linerange(data = rrt_rrt_estimate %>% filter(model=='Jtu_rlm_reg_txa' & realm2=='Marine'),
                 aes(x = reorder(region, Estimate.cYEAR), ymin = lower_slope, ymax = upper_slope,
                     group = taxa2,
                     colour = taxa2),
                 position = position_dodge(width = 0.7)) +
  geom_point(data = rrt_rrt_estimate %>% filter(model=='Jtu_rlm_reg_txa' & realm2=='Marine'),
             aes(x = reorder(region, Estimate.cYEAR), y = Estimate.cYEAR,
                 group = taxa2,
                 colour = taxa2,
                 shape = taxa2),
             position = position_dodge(width = 0.7),
             size = 2, 
             stroke = 1.2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = rrt_global_estimates %>% 
               filter(model=='Jtu_rlm_reg_txa' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_rect(data = rrt_global_estimates %>% 
              filter(model=='Jtu_rlm_reg_txa' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.2) +
  labs(x = '',
       y = '',
       subtitle = 'Marine region - taxon combinations',
       tag = 'A') +
  scale_x_discrete(labels = scales::wrap_format(20), 
                   # expand = c(0.1,0.1)
  ) +
  scale_colour_manual(values=taxa_col) +
  scale_shape_manual(values = taxa_shape) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 8, lineheight = .8))

terr_region_taxa_coef <- ggplot() +
  # facet_wrap(~climate_mod, scales = 'free') +
  geom_linerange(data = rrt_rrt_estimate %>% filter(model=='Jtu_rlm_reg_txa' & realm2!='Marine'),
                 aes(x = reorder(region, Estimate.cYEAR), ymin = lower_slope, ymax = upper_slope,
                     group = interaction(realm,taxa2),
                     colour = taxa2, linetype = realm),
                 position = position_dodge(width = 0.5)) +
  geom_point(data = rrt_rrt_estimate %>% filter(model=='Jtu_rlm_reg_txa' & realm2!='Marine'),
             aes(x = reorder(region, Estimate.cYEAR), y = Estimate.cYEAR,
                 group = interaction(realm,taxa2),
                 colour = taxa2,
                 shape = taxa2),
             position = position_dodge(width = 0.5),
             size = 2, 
             stroke = 1.2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = rrt_global_estimates %>% 
               filter(model=='Jtu_rlm_reg_txa' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_rect(data = rrt_global_estimates %>% 
              filter(model=='Jtu_rlm_reg_txa' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.2) +
  labs(x = '',
       y = '',
       subtitle = 'Terrestrial and freshwater region - taxon combinations',
       tag = 'B') +
  scale_x_discrete(labels = scales::wrap_format(12)) +
  scale_colour_manual(guide = F, 
                      values=taxa_col) +
  scale_shape_manual(guide = F, 
                     values = taxa_shape) +
  scale_linetype_manual(name = 'Realm', 
                        values = c('Terrestrial' = 1, 'Freshwater' = 2)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_blank(),
        
        axis.text.y = element_text(size = 10, lineheight = .8))


top =  cowplot::plot_grid(NULL, legend1, NULL, ncol = 3)
bottom = cowplot::plot_grid(marine_region_taxa_coef,
                            terr_region_taxa_coef,
                            ncol = 2, align = 'hv') +
  cowplot::draw_label('Turnover [proportion of species/year]', y = 0.012) +
  cowplot::draw_label('Region', x = 0.012, angle = 90) 

cowplot::plot_grid(top, bottom,
                   ncol = 1,
                   rel_heights = c(0.05, 1))

# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/FigSx_rlm_region_taxa_turnover.png',
#        width = 290, height = 250, units = 'mm')


# separate realms to declutter
marine_region_taxa_coef <- ggplot() +
  # facet_wrap(.~climate_mod, scales = 'free') +
  geom_linerange(data = rrt_rrt_estimate %>% filter(model=='S_rlm_reg_txa' & realm2=='Marine'),
                 aes(x = reorder(region, Estimate.cYEAR), ymin = lower_slope, ymax = upper_slope,
                     group = taxa2,
                     colour = taxa2),
                 position = position_dodge(width = 0.7)) +
  geom_point(data = rrt_rrt_estimate %>% filter(model=='S_rlm_reg_txa' & realm2=='Marine'),
             aes(x = reorder(region, Estimate.cYEAR), y = Estimate.cYEAR,
                 group = taxa2,
                 colour = taxa2,
                 shape = taxa2),
             position = position_dodge(width = 0.7),
             size = 2, 
             stroke = 1.2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = rrt_global_estimates %>% 
               filter(model=='S_rlm_reg_txa' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_rect(data = rrt_global_estimates %>% 
              filter(model=='S_rlm_reg_txa' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.2) +
  labs(x = '',
       y = '',
       subtitle = 'Marine region - taxon combinations',
       tag = 'A') +
  scale_x_discrete(labels = scales::wrap_format(20), 
                   # expand = c(0.1,0.1)
  ) +
  scale_colour_manual(values=taxa_col) +
  scale_shape_manual(values = taxa_shape) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 8, lineheight = .8))


terr_region_taxa_coef <- ggplot() +
  # facet_wrap(~climate_mod, scales = 'free') +
  geom_linerange(data = rrt_rrt_estimate %>% filter(model=='S_rlm_reg_txa' & realm2!='Marine'),
                 aes(x = reorder(region, Estimate.cYEAR), ymin = lower_slope, ymax = upper_slope,
                     group = interaction(realm,taxa2),
                     colour = taxa2, linetype = realm),
                 position = position_dodge(width = 0.5)) +
  geom_point(data = rrt_rrt_estimate %>% filter(model=='S_rlm_reg_txa' & realm2!='Marine'),
             aes(x = reorder(region, Estimate.cYEAR), y = Estimate.cYEAR,
                 group = interaction(realm,taxa2),
                 colour = taxa2,
                 shape = taxa2),
             position = position_dodge(width = 0.5),
             size = 2, 
             stroke = 1.2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = rrt_global_estimates %>% 
               filter(model=='S_rlm_reg_txa' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_rect(data = rrt_global_estimates %>% 
              filter(model=='S_rlm_reg_txa' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.2) +
  labs(x = '',
       y = '',
       subtitle = 'Terrestrial and freshwater region - taxon combinations',
       tag = 'B') +
  scale_x_discrete(labels = scales::wrap_format(12)) +
  scale_colour_manual(guide = F, 
                      values=taxa_col) +
  scale_shape_manual(guide = F, 
                     values = taxa_shape) +
  scale_linetype_manual(name = 'Realm', 
                        values = c('Terrestrial' = 1, 'Freshwater' = 2)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_blank(),
        
        axis.text.y = element_text(size = 10, lineheight = .8))


top =  cowplot::plot_grid(NULL, legend1, NULL, ncol = 3)
bottom = cowplot::plot_grid(marine_region_taxa_coef,
                            terr_region_taxa_coef,
                            ncol = 2, align = 'hv') +
  cowplot::draw_label('Change in species richness [log(S)/yr]', y = 0.012) +
  # cowplot::draw_label('Turnover [proportion of species/yr]', y = 0.012) +
  cowplot::draw_label('Region', x = 0.012, angle = 90) 

cowplot::plot_grid(top, bottom,
                   ncol = 1,
                   rel_heights = c(0.05, 1))

ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/FigS6.png',
       width = 290, height = 250, units = 'mm')
