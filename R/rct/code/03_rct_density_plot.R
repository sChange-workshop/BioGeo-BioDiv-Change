# code to plot posterior density plots for the rct model
# needs to run 04_bt_sensitivity_plots.R if you want to produce two panel figure

# rm(list=ls())
library(brms) 
library(tidyverse)
library(ggridges)

#------posterior samples from biome and biome-taxa level----------
load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/rct/S_pois_RCTRfyID_count-4436001.Rdata')
load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/rct/Jtu_norm_RCTRfyID_count-1487091.Rdata')


#------wrangle for plotting-----------------------
rct_levels <- S_pois_RCTRfyID_countData$data %>% 
  as_tibble() %>% 
  distinct(rlm_clm_txa) %>% 
  mutate(level = rlm_clm_txa) %>% 
  nest(level)

rct_sample_posterior <- rct_levels %>%
  mutate(S_posteriorSamp = purrr::map(data, ~posterior_samples(S_pois_RCTRfyID_countData, 
                                                               pars = paste('r_rlm_clm_txa[', as.character(.x$level), ',cYEAR]', sep=''),
                                                               exact = TRUE,
                                                               subset = floor(runif(n = 1000,
                                                                                    min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         Jtu_posterior = purrr::map(data, ~posterior_samples(Jtu_norm_RCTRfyID_countData,
                                                            pars = paste('r_rlm_clm_txa[', as.character(.x$level), ',cYEAR]', sep =''),
                                                            exact = TRUE,
                                                            subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))

S_rct_fixed <- fixef(S_pois_RCTRfyID_countData, robust = T, probs = c(0.05, 0.95))
Jtu_rct_fixed <- fixef(Jtu_norm_RCTRfyID_countData, robust = T, probs = c(0.05, 0.95))

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
  
rm(S_pois_RCTRfyID_countData, Jtu_norm_RCTRfyID_countData)

# new latitude realm concatenation and ordering for plotting
rct_sample_posterior <- rct_sample_posterior %>% 
  unite(lat_realm, c(Latitude, Realm), remove = F, sep = ' ')

rct_sample_posterior$lat_realm <- factor(rct_sample_posterior$lat_realm, 
                                         levels = c('Polar Freshwater',
                                                    'Polar Marine',
                                                    'Polar Terrestrial',
                                                    'Temperate Freshwater',
                                                    'Temperate Marine',
                                                    'Temperate Terrestrial',
                                                    'Tropical Marine',
                                                    'Tropical Terrestrial'),
                                         labels = c('Polar (freshwater)',
                                                    'Polar (marine)',
                                                    'Polar (terrestrial)',
                                                    'Temperate (freshwater)',
                                                    'Temperate (marine)',
                                                    'Temperate (terrestrial)',
                                                    'Tropical (marine)',
                                                    'Tropical (terrestrial)'))


rct_sample_posterior$Realm <- factor(rct_sample_posterior$Realm,
                                     levels = c('Terrestrial', 'Marine', 'Freshwater'))


library(viridis)
codes <- viridis(3, end = 0.8)

order <- with(rct_sample_posterior, lat_realm)
levels(order)

lat_lab_colour <- rct_sample_posterior %>% 
  distinct(Latitude, Realm, lat_realm) %>% 
  mutate(colour = ifelse(Latitude=='Polar', codes[1],
                         ifelse(Latitude=='Temperate', codes[2], codes[3])))

check <- lat_lab_colour$lat_realm[match(levels(order), lat_lab_colour$lat_realm)]

sum(check!=levels(order))
lat_colour <- lat_lab_colour$colour[match(levels(order), lat_lab_colour$lat_realm)]



#--------plot-------------------
S_rct_density <- ggplot() +
  geom_rect(data = rct_sample_posterior %>% distinct(S_lower_slope, S_upper_slope, Latitude),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.25) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = lat_realm, fill = Realm, linetype = Realm),
                      scale = 0.8, alpha = 0.6) +
  scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 0, 'Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  theme_bw() +
  ylab('Latitudinal band') +
  xlab('Taxa-level species richness change estimate') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0)) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.7, 0.6),
        legend.direction = 'vertical',
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(size = 14, colour = lat_colour),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 15)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

cowplot::plot_grid(S_biome_taxa_density, S_rct_density, ncol = 2, rel_widths = c(1, 0.7), labels = 'auto')
# ggsave('test.pdf', width = 400, height = 300, units = 'mm')

Jtu_rct_density <- ggplot() +
  geom_rect(data = rct_sample_posterior %>% distinct(Jtu_lower_slope, Jtu_upper_slope),
            aes(xmin = Jtu_lower_slope, xmax = Jtu_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.25) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = Jtu_posterior + unique(Jtu_global_slope), y = lat_realm, fill = Realm, linetype = Realm),
                      scale = 0.8, alpha = 0.6) +
  scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 0, 'Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = Jtu_global_slope)) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05, 0, 0.12, 0)) +
  theme_bw() +
  ylab('Latitudinal band') +
  xlab('Taxa-level turnover') +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.7, 0.7),
        legend.direction = 'vertical',
        axis.text.y = element_text(colour = lat_colour)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

cowplot::plot_grid(Jtu_biome_taxa_density, Jtu_rct_density, ncol = 2, rel_widths = c(1, 0.7), labels = 'auto')
# ggsave('Jtu_taxa_density_2models.pdf', width = 400, height = 300, units = 'mm')

taxa_col = c('All' = '#e6f598',
             'Amphibians' = '#fee08b',
             'Benthos' = '#5e4fa2',
             'Birds' = '#f46d43',
             'Fish' = '#3288bd',
             'Invertebrates' = '#abdda4',
             'Mammals' = '#9e0142',
             'Marine invertebrates/plants' = '#66c2a5',
             'Plant' = '#fdae61')

ggplot() +
  geom_rect(data = rct_sample_posterior %>% distinct(S_lower_slope, S_upper_slope),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.9) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = Latitude, 
                          linetype = Realm, fill = Taxa),
                      scale = 1, alpha = 0.6) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 3)) +
  scale_fill_manual(name = 'Taxa', values = taxa_col) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  theme_bw() +
  ylab('Latitudinal band') +
  xlab('Taxa-level species richness change') +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9, 0.7),
        legend.direction = 'vertical',
        legend.background = element_blank()) +
  guides(fill = guide_legend(ncol = 1, override.aes = list(alpha = 1)), 
         linetype = guide_legend(override.aes = list(fill = 'White')))

# ggsave('S_rct_taxa_density_latitude_2.png', width = 200, height = 200, units = 'mm')

ggplot() +
  geom_rect(data = rct_sample_posterior %>% distinct(Jtu_lower_slope, Jtu_upper_slope),
            aes(xmin = Jtu_lower_slope, xmax = Jtu_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.5) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = Jtu_posterior + unique(Jtu_global_slope), y = Latitude, 
                          linetype = Realm, fill = Taxa),
                      scale = 1, alpha = 0.6) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 3)) +
  scale_fill_manual(name = 'Taxa', values = taxa_col) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = Jtu_global_slope)) +
  theme_bw() +
  ylab('Latitudinal band') +
  xlab('Taxa-level turnover') +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9, 0.7),
        legend.direction = 'vertical',
        legend.background = element_blank()) +
  guides(fill = guide_legend(ncol = 1, override.aes = list(alpha = 1)), 
         linetype = guide_legend(override.aes = list(fill = 'White')))

# ggsave('Jtu_rct_taxa_density_latitude_2.png', width = 200, height = 200, units = 'mm')


#---combine freshwater/terrestrial for consistency with initial maps-------
rct_sample_posterior <- rct_sample_posterior %>% 
  mutate(Realm2 = ifelse(Realm=='Marine', 'Marine', 'Terrestrial/Freshwater'))

rct_sample_posterior <- rct_sample_posterior %>% 
  unite(lat_realm2, c(Latitude, Realm2), remove = F, sep = ' ')

rct_sample_posterior$lat_realm2 <- factor(rct_sample_posterior$lat_realm2, 
                                          levels = c('Polar Terrestrial/Freshwater',
                                                     'Polar Marine',
                                                     'Temperate Terrestrial/Freshwater',
                                                     'Temperate Marine',
                                                     'Tropical Marine',
                                                     'Tropical Terrestrial/Freshwater'),
                                          labels = c('Polar (terrestrial/freshwater)',
                                                     'Polar (marine)',
                                                     'Temperate (terrestrial/freshwater)',
                                                     'Temperate (marine)',
                                                     'Tropical (marine)',
                                                     'Tropical (terrestrial/freshwater)'))

codes <- viridis(3, end = 0.8)

order <- with(rct_sample_posterior, lat_realm)
levels(order)

lat_lab_colour <- rct_sample_posterior %>% 
  distinct(Latitude, Realm, lat_realm) %>% 
  mutate(colour = ifelse(Latitude=='Polar', codes[1],
                         ifelse(Latitude=='Temperate', codes[2], codes[3])))

check <- lat_lab_colour$lat_realm[match(levels(order), lat_lab_colour$lat_realm)]

sum(check!=levels(order))
lat_colour <- lat_lab_colour$colour[match(levels(order), lat_lab_colour$lat_realm)]


ggplot() +
  geom_rect(data = rct_sample_posterior %>% distinct(S_lower_slope, S_upper_slope, Latitude),
            aes(xmin = S_lower_slope, xmax = S_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.25) +
  geom_density_ridges(data = rct_sample_posterior,
                      aes(x = S_posteriorSamp + unique(S_global_slope), y = lat_realm2, fill = Realm2, linetype = Realm2),
                      scale = 0.8, alpha = 0.6) +
  scale_fill_manual(name = 'Realm', values = c('Marine' = '#0072B2', 'Terrestrial/Freshwater' = '#D55E00', 'Freshwater' = '#009E73')) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial/Freshwater' = 0)) +
  geom_vline(data = rct_sample_posterior,
             aes(xintercept = S_global_slope)) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  theme_bw() +
  ylab('Latitudinal band') +
  xlab('Taxa-level species richness change estimate') +
  scale_y_discrete(labels = scales::wrap_format(12), expand = c(0.05,0,0.1,0)) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.7, 0.6),
        legend.direction = 'vertical',
        axis.text.y = element_text(colour = lat_colour)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))
