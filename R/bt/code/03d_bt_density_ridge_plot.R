# plot posterior densities of the biome-taxa level estimates
setwd('~/Dropbox/BiogeoBioTIME/Figures/')
# rm(list=ls())
library(brms) 
library(tidyverse)
library(ggridges)
#--------rarefied_medians.Rdata for meta data not in model object
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')

rarefied_medians <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  mutate(n_cells = n_distinct(rarefyID)) %>%
  ungroup() %>%
  filter(n_cells > 3)

realm_climate_biome <- rarefied_medians %>%
  distinct(REALM, climate_mod, Biome) %>%
  ungroup()

#------posterior samples from biome and biome-taxa level----------
load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/model_coefs_ranefs/S_pois_BTRfyID_count_stdT_autoScale-4591549.Rdata')
load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/model_coefs_ranefs/Jtu_norm_BTRfyID_count-4464540.Rdata')

# get the biome levels in the data the model was fit to
biome_levels_model <- S_pois_BTRfyID_countData$data %>%
  distinct(Biome)
  
# need to figure out what to do with Biomes that overlap latitudinal divisions...
biomes_2_see <- rarefied_medians %>%
  distinct(climate_mod, Biome) %>%
  group_by(Biome) %>%
  filter(n() > 1)

#  check some biome-taxa combos cross our (arbitrary) latitudinal bands
# rarefied_medians %>% 
#   filter(Biome %in% biomes_2_see$Biome) %>%
#   ggplot() +
#   facet_wrap( ~ Biome, scales = 'free') +
#   geom_histogram(aes(rarefyID_y))

realm_climate_biome <- realm_climate_biome %>%
  filter(Biome %in% biome_levels_model$Biome) %>%
  mutate(climate_mod = ifelse(Biome=='Arctic', 'Polar', 
                              ifelse(Biome=='Boreal_Forests_Taiga', 'Temperate',
                                     ifelse(Biome=='Cold_Temperate_Northeast_Pacific', 'Temperate',
                                            ifelse(Biome=='Continental_High_Antarctic', 'Polar',
                                                   ifelse(Biome=='Hawaii', 'Tropical', 
                                                          ifelse(Biome=='Northern_European_Seas', 'Temperate',
                                                                 ifelse(Biome=='Subantarctic_Islands', 'Polar',
                                                                        ifelse(Biome=='Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands', 'Temperate',
                                                                               ifelse(Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests', 'Tropical', 
                                                                                      ifelse(Biome=='Tropical_Northwestern_Atlantic', 'Tropical', 
                                                                                             ifelse(Biome=='Northeast_Australian_Shelf', 'Tropical', climate_mod))))))))))))

realm_climate_biome %>%
  distinct(Biome, climate_mod) %>%
  group_by(Biome) %>%
  filter(n() > 1) %>% data.frame()


  
biome_levels <- inner_join(biome_levels_model, realm_climate_biome, by = 'Biome') %>%
  distinct(Biome, .keep_all = T) %>%
  mutate(level = Biome) %>%
  nest(level)

biome_sample_posterior <- biome_levels %>%
  mutate(S_posteriorSamp = purrr::map(data, ~posterior_samples(S_pois_BTRfyID_countData, 
                                                               pars = paste('r_Biome[', as.character(.x$level), ',cYEAR]', sep=''),
                                                               exact = TRUE,
                                                               subset = floor(runif(n = 1000,
                                                                                    min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         Jtu_posteriorSamp = purrr::map(data, ~posterior_samples(Jtu_norm_BTRfyID_countData, 
                                                                 pars = paste('r_Biome[', as.character(.x$level), ',cYEAR]', sep=''),
                                                                 exact = TRUE,
                                                                 subset = floor(runif(n = 1000,
                                                                                      min = 1, max = 2000))) %>% unlist() %>% as.numeric()))

biome_sample_posterior <- biome_sample_posterior %>%
  select(-data) %>%
  unnest()

bt_levels <- S_pois_BTRfyID_countData$data %>% 
  distinct(Biome, taxa_mod, `Biome:taxa_mod`) %>%
  mutate(level = `Biome:taxa_mod`) %>%
  select(-`Biome:taxa_mod`) %>%
  group_by(Biome, taxa_mod) %>%
  nest(level) %>%
  ungroup()



global_posterior_sample_a <- tibble(
  model = c('S_pois'),
  model_name = c('S_pois_BTRfyID_countData')
) %>%
  nest(model_name) %>%
  mutate(cYEAR = purrr::map(data, ~posterior_samples(S_pois_BTRfyID_countData,
                                                     pars = 'b_cYEAR',
                                                     exact = TRUE,
                                                     subset = floor(runif(n = 1000,
                                                                          min = 1, max = 2000))) %>% unlist() %>% as.numeric))
global_posterior_sample_b <- tibble(
  model = c('Jtu_norm'),
  model_name = c('Jtu_norm_BTRfyID_countData')) %>%
  nest(model_name) %>%
  mutate(cYEAR = purrr::map(data, ~posterior_samples(Jtu_norm_BTRfyID_countData,
                                                     pars = 'b_cYEAR',
                                                     exact = TRUE,
                                                     subset = floor(runif(n = 1000,
                                                                          min = 1, max = 2000))) %>% unlist() %>% as.numeric))


global_posterior_sample <- bind_rows(global_posterior_sample_a %>% select(-data),
                                     global_posterior_sample_b %>% select(-data))

bt_sample_posterior <- bt_levels %>%
  mutate(S_posteriorSamp = purrr::map(data, ~posterior_samples(S_pois_BTRfyID_countData, 
                                                               pars = paste('r_Biome:taxa_mod[', as.character(.x$level), ',cYEAR]', sep=''),
                                                               exact = TRUE,
                                                               subset = floor(runif(n = 1000,
                                                                                    min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         Jtu_posteriorSamp = purrr::map(data, ~posterior_samples(Jtu_norm_BTRfyID_countData, 
                                                                 pars = paste('r_Biome:taxa_mod[', as.character(.x$level), ',cYEAR]', sep=''),
                                                                 exact = TRUE,
                                                                 subset = floor(runif(n = 1000,
                                                                                      min = 1, max = 2000))) %>% unlist() %>% as.numeric()))

bt_sample_posterior <- bt_sample_posterior %>%
  select(-data) %>%
  unnest() %>% 
  # join with the climate and realm meta data
  inner_join(biome_levels, by = 'Biome') %>%
  select(-data)

bt_sample_posterior <- bt_sample_posterior %>% 
  group_by(REALM, Biome, taxa_mod) %>%
  mutate(index = 1:n()) %>%
  ungroup()


biome_sample_posterior <- biome_sample_posterior %>%
  mutate(S_global = global_posterior_sample_a %>% unnest(cYEAR) %>% .$cYEAR %>% median,
         S_global_upper = global_posterior_sample_a %>% unnest(cYEAR) %>% .$cYEAR %>% quantile(probs = 0.95),
         S_global_lower = global_posterior_sample_a %>% unnest(cYEAR) %>% .$cYEAR %>% quantile(probs = 0.05),
         S_biome_coef = S_posteriorSamp + S_global) 


Jtu_bt_sample_posterior <- bt_sample_posterior %>%
  mutate(Jtu_global = global_posterior_sample_b %>% unnest(cYEAR) %>% .$cYEAR %>% median,
         Jtu_global_upper = global_posterior_sample_b %>% unnest(cYEAR) %>% .$cYEAR %>% quantile(probs = 0.95),
         Jtu_global_lower = global_posterior_sample_b %>% unnest(cYEAR) %>% .$cYEAR %>% quantile(probs = 0.05),
         Jtu_taxa_coef = Jtu_posteriorSamp + Jtu_global)


Jtu_bt_sample_posterior <- Jtu_bt_sample_posterior %>%
  mutate(Biome = gsub('_', ' ', Biome),
         # fix some other random ones up
         Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='large lakes', 'Large lakes', Biome),
         Biome = ifelse(Biome=='polar freshwaters', 'Polar freshwaters', Biome),
         Biome = ifelse(Biome=='temperate upland rivers', 'Temperate upland rivers', Biome),
         Biome = ifelse(Biome=='temperate coastal rivers', 'Temperate coastal rivers', Biome),
         Biome = ifelse(Biome=='temperate floodplain rivers and wetlands', 'Temperate floodplain rivers and wetlands', Biome))


S_bt_sample_posterior <- bt_sample_posterior %>%
  mutate(S_global = global_posterior_sample_a %>% unnest(cYEAR) %>% .$cYEAR %>% median,
         S_taxa_coef = S_posteriorSamp + S_global)

S_bt_pooled_CI <- S_bt_sample_posterior %>%
  group_by(Biome) %>%
  summarise(lower90 = quantile(S_taxa_coef, probs = 0.05),
            upper90 = quantile(S_taxa_coef, probs = 0.95)) %>%
  ungroup()

S_bt_sample_posterior <- inner_join(S_bt_sample_posterior, S_bt_pooled_CI, by = c('Biome'))

S_bt_sample_posterior <- S_bt_sample_posterior %>%
  mutate(CI = ifelse((S_taxa_coef<lower90 | S_taxa_coef>upper90), 'outside', 'inside'),
         alpha = ifelse(CI=='inside', 0.8, 0.2),
         Biome = gsub('_', ' ', Biome),
         # fix some other random ones up
         Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='large lakes', 'Large lakes', Biome),
         Biome = ifelse(Biome=='polar freshwaters', 'Polar freshwaters', Biome),
         Biome = ifelse(Biome=='temperate upland rivers', 'Temperate upland rivers', Biome),
         Biome = ifelse(Biome=='temperate coastal rivers', 'Temperate coastal rivers', Biome),
         Biome = ifelse(Biome=='temperate floodplain rivers and wetlands', 'Temperate floodplain rivers and wetlands', Biome))

# tidy up 
rm(S_pois_BTRfyID_countData, Jtu_norm_BTRfyID_countData)


# show the taxa...
# colours for each taxa
taxa_col2 = c('Multiple taxa' = '#e6f598',
              'Amphibians' = '#fee08b',
              'Benthos' = '#5e4fa2',
              'Birds' = '#f46d43',
              'Fish' = '#3288bd',
              'Invertebrates' = '#abdda4',
              'Mammals' = '#9e0142',
              'Marine invertebrates/plants' = '#66c2a5',
              'Plant' = '#fdae61')

# vector of colours for biome labels based on latitude
library(viridis)
codes <- viridis(3, end = 0.8)
order <- with(S_bt_sample_posterior, reorder(Biome, S_taxa_coef))
levels(order)

biome_lab_colour <- S_bt_sample_posterior %>% 
  distinct(Biome, climate_mod) %>% 
  mutate(colour = ifelse(climate_mod=='Polar', codes[1],
                           ifelse(climate_mod=='Temperate', codes[2], codes[3])))

check <- biome_lab_colour$Biome[match(levels(order), biome_lab_colour$Biome)]
sum(check!=levels(order))
biome_colour <- biome_lab_colour$colour[match(levels(order), biome_lab_colour$Biome)]

# tidy taxa names
S_bt_sample_posterior$taxa_mod2 <- factor(S_bt_sample_posterior$taxa_mod, 
                                     levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "All", "Marine invertebrates/plants", "Mammals"),
                                     labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

Jtu_bt_sample_posterior$taxa_mod2 <- factor(Jtu_bt_sample_posterior$taxa_mod, 
                                          levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                     "All", "Marine invertebrates/plants", "Mammals"),
                                          labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                     "Multiple taxa", "Marine invertebrates/plants", "Mammals"))


S_biome_taxa_density <- ggplot() +
  geom_rect(data = biome_sample_posterior %>% distinct(S_global_lower, S_global_upper),
            aes(xmin = S_global_lower, xmax = S_global_upper), ymin = -Inf, ymax = Inf,
            alpha = 0.25) +
  geom_density_ridges(data = S_bt_sample_posterior,
                      aes(x = S_taxa_coef, y = reorder(Biome, S_taxa_coef),
                          fill = taxa_mod2, linetype = REALM),
                      lwd = 0.2, scale = 1, alpha = 0.6) +
  scale_fill_manual(name = 'Taxa', values = taxa_col2,
                    guide = guide_legend(title.position = 'top')) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 3),
                        guide = guide_legend(title.position = 'top', override.aes = list(fill = 'White'),
                                             nrow = 2)) +
  scale_y_discrete(labels = scales::wrap_format(30), expand = c(0.01, 0, 0.02, 0)) +
  geom_vline(data = biome_sample_posterior,
             aes(xintercept = unique(S_global))) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  theme_bw() +
  ylab('Biome') + 
  xlab('Taxa-level species richness change estimate') +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, lineheight = .7, colour = biome_colour),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.key = element_blank(),
        legend.direction = 'horizontal',
        legend.position = 'top',
        # legend.justification = 'left',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11)) 


S_biomJtu_order <- with(Jtu_bt_sample_posterior, reorder(Biome, Jtu_taxa_coef))

Jtu_biome_lab_colour <- Jtu_bt_sample_posterior %>% 
  distinct(Biome, climate_mod) %>% 
  mutate(colour = ifelse(climate_mod=='Polar', codes[1],
                         ifelse(climate_mod=='Temperate', codes[2], codes[3])))

check <- Jtu_biome_lab_colour$Biome[match(levels(Jtu_order), Jtu_biome_lab_colour$Biome)]
sum(check!=levels(Jtu_order))
Jtu_colour <- Jtu_biome_lab_colour$colour[match(levels(Jtu_order), Jtu_biome_lab_colour$Biome)]


Jtu_biome_taxa_density <- ggplot() +
  geom_rect(data = Jtu_bt_sample_posterior %>% distinct(Jtu_global_lower, Jtu_global_upper),
            aes(xmin = Jtu_global_lower, xmax = Jtu_global_upper), ymin = -Inf, ymax = Inf,
            alpha = 0.25) +
  geom_density_ridges(data = Jtu_bt_sample_posterior,
                      aes(x = Jtu_taxa_coef, y = reorder(Biome, Jtu_taxa_coef), linetype = REALM, fill = taxa_mod2),
                      scale = 1, lwd = 0.2, alpha = 0.6) +
  geom_vline(data = Jtu_bt_sample_posterior,
             aes(xintercept = unique(Jtu_global))) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  scale_fill_manual(name = 'Taxa', values = taxa_col2,
                    guide = guide_legend(title.position = 'top')) +
  scale_linetype_manual(name = 'Realm', values = c('Marine' = 0, 'Terrestrial' = 1, 'Freshwater' = 3)) +
  scale_y_discrete(labels = scales::wrap_format(30), expand = c(0.01, 0, 0.02, 0)) +
  theme_bw() +
  ylab('Biome') +
  xlab('Taxa-level turnover') +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 10, lineheight = .7, colour = Jtu_colour),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.key = element_blank(),
        legend.direction = 'horizontal',
        legend.position = 'top',
        legend.justification = 'left',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11)) +
  guides(linetype = guide_legend(title.position = 'top', override.aes = list(fill = 'White'),
                                 nrow = 2))


# ggsave('Jtu_taxa_density_colourNames.png', width = 200, height = 200, units = 'mm')
# cowplot::plot_grid(jtu_taxa_into_biomes_density, jtu_biome_taxa_density, ncol = 2, align = 'hv')
# ggsave('Fig4_Jtu_taxa_density_2panel.png', width = 350, height = 200, units = 'mm')


