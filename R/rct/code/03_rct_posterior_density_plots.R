# plot posterior densities of the rct study level estimates (at the realm-climate level)
rm(list=ls())
# setwd('~/Desktop/revision_code/')
# rm(list=ls())
library(brms) 
library(tidyverse)
library(ggridges)
#------posterior samples from biome and biome-taxa level----------
load('~/Desktop/revision_models/S_pois_RCTSRfyID_count-4950831.Rdata')
load('~/Desktop/revision_models/Jtu_norm_RCTSRfyID_count.Rdata')

rct_levels <- S_pois_RCTSRfyID$data %>% 
  as_tibble() %>% 
  distinct(rlm_clm_txa) %>% 
  mutate(level = rlm_clm_txa) %>% 
  nest(level)

study_levels <- S_pois_RCTSRfyID$data %>% 
  as_tibble() %>% 
  distinct(`rlm_clm_txa:STUDY_ID`) %>% 
  mutate(level = `rlm_clm_txa:STUDY_ID`) %>% 
  nest(level)

rct_sample_posterior <- rct_levels %>%
  mutate(S_posteriorSamp = purrr::map(data, ~posterior_samples(S_pois_RCTSRfyID, 
                                                               pars = paste('r_rlm_clm_txa[', as.character(.x$level), ',cYEAR]', sep=''),
                                                               exact = TRUE,
                                                               subset = floor(runif(n = 1000,
                                                                                    min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         Jtu_posterior = purrr::map(data, ~posterior_samples(Jtu_norm_RCTSRfyID,
                                                             pars = paste('r_rlm_clm_txa[', as.character(.x$level), ',cYEAR]', sep =''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))

S_rct_fixed <- fixef(S_pois_RCTSRfyID, robust = T, probs = c(0.05, 0.95))
Jtu_rct_fixed <- fixef(Jtu_norm_RCTSRfyID, robust = T, probs = c(0.05, 0.95))

rct_sample_posterior <- rct_sample_posterior %>% 
  select(-data) %>% 
  unnest() %>% 
  separate(rlm_clm_txa, into = c('Realm', 'Latitude', 'Taxa'), remove = F) %>% 
  mutate(S_global_slope = S_rct_fixed['cYEAR','Estimate'],
         S_upper_slope = S_rct_fixed['cYEAR','Q95'],
         S_lower_slope = S_rct_fixed['cYEAR','Q5'],
         Jtu_global_slope = Jtu_rct_fixed['cYEAR','Estimate'],
         Jtu_upper_slope = Jtu_rct_fixed['cYEAR','Q95'],
         Jtu_lower_slope = Jtu_rct_fixed['cYEAR','Q5'])

# posterior distributions of the studies
study_sample_posterior <- study_levels %>%
  mutate(S_posteriorSamp = purrr::map(data, ~posterior_samples(S_pois_RCTSRfyID, 
                                                               pars = paste('r_rlm_clm_txa:STUDY_ID[', as.character(.x$level), ',cYEAR]', sep=''),
                                                               exact = TRUE,
                                                               subset = floor(runif(n = 1000,
                                                                                    min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         Jtu_posterior = purrr::map(data, ~posterior_samples(Jtu_norm_RCTSRfyID,
                                                             pars = paste('r_rlm_clm_txa:STUDY_ID[', as.character(.x$level), ',cYEAR]', sep =''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))


study_sample_posterior <- study_sample_posterior %>% 
  select(-data) %>% 
  unnest() %>% 
  separate(`rlm_clm_txa:STUDY_ID`, into = c('Realm', 'Latitude', 'Taxa', 'STUDY_ID'), remove = F) %>% 
  mutate(S_global_slope = S_rct_fixed['cYEAR','Estimate'],
         S_upper_slope = S_rct_fixed['cYEAR','Q95'],
         S_lower_slope = S_rct_fixed['cYEAR','Q5'],
         Jtu_global_slope = Jtu_rct_fixed['cYEAR','Estimate'],
         Jtu_upper_slope = Jtu_rct_fixed['cYEAR','Q95'],
         Jtu_lower_slope = Jtu_rct_fixed['cYEAR','Q5']) %>% 
  unite(lat_realm, c(Latitude, Realm), remove = F, sep = ' ')

study_sample_posterior$lat_realm <- factor(study_sample_posterior$lat_realm, 
                                         levels = c('Tropical Marine', 'Tropical Terrestrial', 'Temperate Freshwater',
                                                    'Temperate Marine', 'Temperate Terrestrial', 'Polar Freshwater',
                                                    'Polar Marine', 'Polar Terrestrial'),
                                         labels = c('Tropical (marine)', 'Tropical (terrestrial)', 'Temperate (freshwater)',
                                                    'Temperate (marine)', 'Temperate (terrestrial)', 'Polar (freshwater)',
                                                    'Polar (marine)', 'Polar (terrestrial)'))

# make a new lat_taxa label your plotting 
study_sample_posterior <- study_sample_posterior %>%
  unite(lat_taxa, c(Latitude, Taxa), remove = F, sep = ' ')
# 
study_sample_posterior$lat_taxa <- factor(study_sample_posterior$lat_taxa,
                                          levels = c('Tropical Birds', 'Tropical Fish',
                                                     'Tropical Invertebrates', 'Tropical Plant',
                                                     'Temperate All', 'Temperate Amphibians',
                                                     'Temperate Benthos', 'Temperate Birds',
                                                     'Temperate Fish', 'Temperate Invertebrates',
                                                     'Temperate Mammals', 'Temperate Plant',
                                                     'Polar Benthos', 'Polar Birds',
                                                     'Polar Fish', 'Polar Invertebrates',
                                                     'Polar Mammals', 'Polar Plant'),
                                           labels = c('Tropical Birds', 'Tropical Fish',
                                                      'Tropical Invertebrates', 'Tropical Plants',
                                                      'Temperate multiple taxa', 'Temperate Amphibians',
                                                      'Temperate Benthos', 'Temperate Birds',
                                                      'Temperate Fish', 'Temperate Invertebrates',
                                                      'Temperate Mammals', 'Temperate Plants',
                                                      'Polar Benthos', 'Polar Birds',
                                                      'Polar Fish', 'Polar Invertebrates',
                                                      'Polar Mammals', 'Polar Plants'))

study_sample_posterior$Realm <- factor(study_sample_posterior$Realm,
                                     levels = c('Freshwater', 'Terrestrial', 'Marine'))



# new latitude realm concatenation and ordering for plotting
rct_sample_posterior <- rct_sample_posterior %>% 
  unite(lat_realm, c(Latitude, Realm), remove = F, sep = ' ')

rct_sample_posterior$lat_realm <- factor(rct_sample_posterior$lat_realm, 
                                         levels = c('Tropical Marine', 'Tropical Terrestrial', 'Temperate Freshwater',
                                                    'Temperate Marine', 'Temperate Terrestrial', 'Polar Freshwater',
                                                    'Polar Marine', 'Polar Terrestrial'),
                                         labels = c('Tropical (marine)', 'Tropical (terrestrial)',
                                                    'Temperate (freshwater)', 'Temperate (marine)',
                                                    'Temperate (terrestrial)', 'Polar (freshwater)',
                                                    'Polar (marine)', 'Polar (terrestrial)'))

rct_sample_posterior$Realm <- factor(rct_sample_posterior$Realm,
                                     levels = c('Freshwater', 'Terrestrial', 'Marine'))
# tidy up taxa labels
rct_sample_posterior$taxa2 <- factor(rct_sample_posterior$Taxa,
                                         levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "All", "Marine invertebrates/plants", "Mammals"),
                                         labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plants", "Amphibians",
                                                    "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

study_sample_posterior$taxa2 <- factor(study_sample_posterior$Taxa,
                                     levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "All", "Marine invertebrates/plants", "Mammals"),
                                     labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plants", "Amphibians",
                                                "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

# colour the taxa
taxa_col = c('Multiple taxa' = '#e6f598', 'Amphibians' = '#fee08b',
             'Benthos' = '#5e4fa2', 'Birds' = '#f46d43',
             'Fish' = '#3288bd', 'Invertebrates' = '#abdda4',
             'Mammals' = '#9e0142', 'Marine invertebrates/plants' = '#66c2a5',
             'Plants' = '#fdae61')


rm(S_pois_RCTSRfyID, Jtu_norm_RCTSRfyID)

# Want to know how many studies in each group 
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
rarefied_medians <- left_join(cell_count, rarefied_medians, by='rlm_clm_txa')  # cell_count_alt[,c(1,5)]

##	filter to ecoregions with >3 cells
rarefied_medians <- rarefied_medians %>%
  filter(n_cells > 3 & BROAD_TYPE=='count')

# count studies for figure 2b and 4b
study_count <- rarefied_medians %>% 
  rename(Realm = REALM,
         Latitude = climate_mod) %>% 
  group_by(Realm, Latitude) %>% 
  summarise(n.study = n_distinct(STUDY_ID)) %>% 
  bind_rows(
    tibble(Realm = 'Freshwater',
           Latitude = 'Tropical',
           n.study = 0)
  )
##----plot preliminary-----
# i want a separate legend
source('~/Dropbox/1current/R_random/functions/gg_legend.R')

taxa_fill_horiz_legend <-
ggplot() +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = lat_realm,
                          fill = taxa2),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  # scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 3)) +
  scale_fill_manual(name = 'Taxa', values = taxa_col) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Latitudinal band and realm',
       x = 'Taxon-level species richness change [log(S)/yr]',
       tag = 'A') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0)) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15)) +
  guides(fill = guide_legend(nrow = 2, keywidth = 1, keyheight = 1, 
                             # override.aes = list(alpha = 1)
                             ), 
         linetype = guide_legend(override.aes = list(fill = 'White')))

taxa_legend <- gg_legend(taxa_fill_horiz_legend)
#--------plot-------------------
# species richness, rct model: panel 1 rct level fill for taxa, panel 2 study level, fill for realm
s_rct_p1 <-
ggplot() +
  facet_wrap(~Latitude) +
  geom_rect(data = rct_sample_posterior %>% distinct(S_lower_slope, S_upper_slope),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = Realm,
                          fill = taxa2
                          ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  # scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 3)) +
  scale_fill_manual(name = 'Taxa', values = taxa_col) +
  scale_x_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03),
                     labels = c(-0.02, '', 0, '', 0.02, '')) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Realm',
       x = 'Taxon-level species richness change [log(S)/yr]',
       tag = 'A') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0)) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none',
        # legend.direction = 'horizontal',
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),#
        axis.text.x = element_text(size = 13),# vjust = 0.8, angle = 30),
        axis.text.y = element_text(size = 13, vjust = 0),
        axis.title = element_text(size = 15)) #+
  # guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5, override.aes = list(alpha = 1)), 
  #        linetype = guide_legend(override.aes = list(fill = 'White')))


# make legend for insetting
three_grey_legend <- ggplot() +
  facet_wrap(~Latitude) +
  geom_rect(data = study_sample_posterior %>% distinct(S_lower_slope, S_upper_slope, Latitude),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges_gradient(data = study_sample_posterior,
                               aes(x = S_posteriorSamp + unique(S_global_slope), y = Realm, 
                                   fill = stat(quantile)
                               ),
                               linetype = 0,
                               scale = 0.95, alpha = 0.6,
                               calc_ecdf = T,
                               quantiles = c(0.75, 0.95)) +
  scale_fill_manual(name = 'Posterior\nprobability',
                    values = c('#cccccc', '#969696', '#636363'),
                    labels = c('< 10%', '10-40%', '50%')) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = c(1,0), legend.justification = c(1,0),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text( size = 14),#
        # strip.text = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15)
  ) +
  guides(fill = guide_legend(keywidth = 0.7, keyheight = 0.7,
                             # override.aes = list(alpha = 1),
                             ncol = 1))

inset_legend <- gg_legend(three_grey_legend)

s_rct_study_p2 <- ggplot() +
  facet_wrap(~Latitude) +
  geom_rect(data = study_sample_posterior %>% distinct(S_lower_slope, S_upper_slope, Latitude),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges_gradient(data = study_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = Realm, 
                          fill = stat(quantile)
                          ),
                      linetype = 0,
                      scale = 0.95, alpha = 0.6,
                      calc_ecdf = T,
                      quantiles = c(0.05, 0.25, 0.75, 0.95)) +
  scale_fill_manual(name = 'Posterior\nprobability',
                    values = c('#cccccc', '#969696', '#636363',
                               '#969696', '#cccccc'),
                    labels = c('< 5%', '5-25%', '50%',
                               '5-25%', '< 5%')) +
  # scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  # scale_fill_manual(name = 'Taxa', values = taxa_col) +
  # scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 0, 'Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_text(data = study_count,
            aes(x=-0.22, y=Realm, 
                label=paste('n[study] == ', n.study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme_bw() +
  labs(y = 'Realm',
       x = 'Study-level species richness change [log(S)/yr]',
       tag = 'B') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0)) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none',
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text( size = 14),#
        # strip.text = element_blank(),
        axis.text = element_text(size = 13),
        axis.text.y = element_text(size = 13, vjust = 0),
        axis.title = element_text(size = 15)
        ) +
  guides(fill = guide_legend(keywidth = 0.75, keyheight = 0.75,
                             # override.aes = list(alpha = 1),
                             ncol = 1))

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

top <- cowplot::plot_grid(NULL, taxa_legend, NULL, nrow = 1)
bottom_inset <- cowplot::plot_grid(s_rct_study_p2 + 
                                     annotation_custom2(inset_legend,
                                                        data = data.frame(Realm = 'Freshwater',
                                                                          Latitude = 'Tropical',
                                                                          x = 0, y = 3), 
                                                       xmin = 0.1, xmax = Inf, ymin = -Inf, ymax = 2))

bottom <- cowplot::plot_grid(s_rct_p1, bottom_inset, align = 'hv', nrow = 2)
cowplot::plot_grid(top, bottom, nrow = 2, rel_heights = c(0.1,1))
ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/Fig2_alt1.png', width = 230, height = 230, units = 'mm')

# Jtu_rct_density <- 
jtu_rct_p1 <-
  ggplot() +
  facet_wrap(~Latitude) +
  geom_rect(data = rct_sample_posterior %>% distinct(Jtu_lower_slope, Jtu_upper_slope),
            aes(xmin = Jtu_lower_slope, xmax = Jtu_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = Jtu_posterior + unique(Jtu_global_slope), y = Realm, 
                          fill = taxa2
                          ),
                      linetype = 0,
                      scale = 1, alpha = 0.6) +
  # scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  scale_fill_manual(name = 'Taxa', values = taxa_col) +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02, 0.04, 0.06, 0.08),
                     labels = c(-0.02, 0, 0.02, '', 0.06, ''),
                     ) +
  # scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 0, 'Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = Jtu_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05, 0, 0.12, 0)) +
  theme_bw() +
  labs(y = 'Realm',
       x = 'Taxon-level turnover [proportion of species/yr]',
       tag = 'A') +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none', #c(0.92, 0.9),
        strip.background = element_blank(),
        strip.text = element_text( size = 14),#hjust = 0,
        axis.text.x = element_text(size = 13),# vjust = 0.8, angle = 30),
        axis.text.y = element_text(size = 13, vjust = 0),
        axis.title = element_text(size = 15))


# ggsave('FigSx_turnover_density_taxaLevel.pdf', width = 200, height = 200, units = 'mm')
jtu_study_p2 <- ggplot() +
  facet_wrap(~Latitude) +
  geom_rect(data = study_sample_posterior %>% distinct(Jtu_lower_slope, Jtu_upper_slope),
            aes(xmin = Jtu_lower_slope, xmax = Jtu_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges_gradient(data = study_sample_posterior,
                               aes(x = Jtu_posterior + unique(Jtu_global_slope), y = Realm, 
                                   fill = stat(quantile)
                               ),
                               linetype = 0,
                               scale = 0.95, alpha = 0.6,
                               calc_ecdf = T,
                               quantiles = c(0.05, 0.25, 0.75, 0.95)) +
  scale_fill_manual(name = 'Posterior\nprobability',
                    values = c('#cccccc', '#969696', '#636363',
                               '#969696', '#cccccc'),
                    labels = c('< 5%', '5-25%', '50%',
                               '5-25%', '< 5%')) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = Jtu_global_slope)) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  geom_text(data = study_count,
            aes(x=-0.04, y=Realm, 
                label=paste('n[study] == ', n.study)),
            size=3.5,
            nudge_y = 0.3, parse = T) +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05, 0, 0.12, 0)) +
  theme_bw() +
  labs(y = 'Realm',
       x = 'Study-level turnover [proportion of species/yr]',
       tag = 'B') +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none', #c(1,0), legend.justification = c(1,0),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),#hjust = 0, 
        axis.text.x = element_text(size = 13),# vjust = 0.8, angle = 30),
        axis.text.y = element_text(size = 13, vjust = 0),
        axis.title = element_text(size = 15)) +
  guides(fill = guide_legend(keywidth = 0.6, keyheight = 0.6,
                             # override.aes = list(alpha = 1),
                             ncol = 1))


bottom_inset2 <- cowplot::plot_grid(jtu_study_p2 + 
                                     annotation_custom2(inset_legend,
                                                        data = data.frame(Realm = 'Freshwater',
                                                                          Latitude = 'Tropical',
                                                                          x = 0, y = 3), 
                                                        xmin = 0.1, xmax = Inf, ymin = -Inf, ymax = 2))

bottom <- cowplot::plot_grid(jtu_rct_p1, bottom_inset2, align = 'hv', nrow = 2)
cowplot::plot_grid(top, bottom, nrow = 2, rel_heights = c(0.1,1))
ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/Fig4_alt1.png', 
       width = 230, height = 230, units = 'mm')

s_study_rct_taxa_lat <- 
  ggplot() +
  geom_rect(data = study_sample_posterior %>% distinct(S_lower_slope, S_upper_slope, Latitude),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = study_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = lat_taxa, 
                          fill = Realm
                      ),
                      linetype = 0,
                      scale = 0.95, alpha = 0.6) +
  scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  # scale_fill_manual(name = 'Taxa', values = taxa_col) +
  # scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 0, 'Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = '',
       x = 'Study-level species richness change [log(S)/yr]',
       tag = 'A') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0),
                   # limits = rev(levels(study_sample_posterior$lat_taxa))
                   ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none', #c(0.925, 0.9),
        # legend.direction = 'horizontal',
        # legend.text = element_text(size = 14),
        # legend.title = element_text(size = 15),
        axis.text.y = element_blank()#,
        # axis.text.x = element_text(size = 14),
        # axis.title = element_text(size = 15)
  ) #+

jtu_study_rct_taxa_lat <- ggplot() +
  geom_rect(data = study_sample_posterior %>% distinct(Jtu_lower_slope, Jtu_upper_slope),
            aes(xmin = Jtu_lower_slope, xmax = Jtu_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = study_sample_posterior,
                      aes(x = Jtu_posterior + unique(Jtu_global_slope), y = lat_taxa, 
                          fill = Realm
                      ),
                      linetype = 0,
                      scale = 0.95, alpha = 0.6) +
  scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  # scale_fill_manual(name = 'Taxa', values = taxa_col) +
  # scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 0, 'Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = Jtu_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = '',
       x = 'Study-level turnover [proportion of species/yr]',
       tag = 'B') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0),
                   # limits = rev(levels(study_sample_posterior$lat_taxa))
                   ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none', #c(0.925, 0.9),
        # legend.direction = 'horizontal',
        # legend.text = element_text(size = 14),
        # legend.title = element_text(size = 15),
        axis.text.y = element_blank()#,
        # axis.text.x = element_text(size = 14),
        # axis.title = element_text(size = 15)
  ) #+



cowplot::plot_grid(s_study_rct_taxa_lat,
                   jtu_study_rct_taxa_lat, 
                   nrow = 1, align = 'hv'
                   # rel_widths = c(1,0.9)
                   )
