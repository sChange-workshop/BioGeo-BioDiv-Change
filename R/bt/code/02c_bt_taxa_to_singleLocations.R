# code to get geography for plotting study estimates: get one or more points for biome-taxa-study combinations
source('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/code/02_newModel_studyLevel_wrangle.R')
world <- map_data('world')

# function to find the convex hull of a polygon
find_hull <- function(df) df[chull(df$rarefyID_x, df$rarefyID_y), ]
# function to find centre of hull
hull_centre <- function(rarefyID_x, rarefyID_y){
  hc_x = geosphere::geomean(cbind(x = rarefyID_x, y = rarefyID_y))[1]
  hc_y = geosphere::geomean(cbind(x = rarefyID_x, y = rarefyID_y))[2]
  return(cbind.data.frame(hc_x, hc_y))
}  

##  get unique coords for each biome, taxa combination
studies_ <- BT_study_estimate %>% 
  distinct(STUDY_ID)

coords <- rarefied_medians %>% 
  distinct(REALM, Biome, taxa_mod, STUDY_ID, rarefyID, rarefyID_x, rarefyID_y) %>% 
  # filter to the studies for which we have study-level slope estimates
  filter(STUDY_ID %in% studies_$STUDY_ID)


##--------marine realm wrangle------------------
marine_hulls <- plyr::ddply(filter(coords, REALM=='Marine'),
                     plyr::.(REALM, Biome, taxa_mod, STUDY_ID), find_hull) %>%
  as_tibble()


marine_nest <- marine_hulls %>%
  group_by(REALM, Biome, taxa_mod, STUDY_ID) %>%
  filter(n_distinct(rarefyID) > 1) %>%
  nest(rarefyID_x, rarefyID_y) %>% 
  mutate(hull_centre = purrr::map(data, ~ hull_centre(.x$rarefyID_x, .x$rarefyID_y)))
  

mar_hull_cent <- marine_nest %>% unnest(hull_centre)

# we want a point for each unique combination of biome/taxa/study: ok
mar_hull_cent %>% 
  group_by(Biome, taxa_mod) %>% 
  summarise(n.study = n_distinct(STUDY_ID), # 
            points = n_distinct(hc_x, hc_y)) %>% 
  filter(points!=n.study)

# and we need points for the studies that only had one location 
marine_singleLoc <- marine_hulls %>% 
  group_by(REALM, Biome, taxa_mod, STUDY_ID) %>% 
  filter(n_distinct(rarefyID)==1) %>% 
  mutate(hc_x = rarefyID_x,
         hc_y = rarefyID_y) %>% 
  select(REALM, Biome, taxa_mod, STUDY_ID, hc_x, hc_y) %>% 
  ungroup()

# rejoin with other marine location data
BTS_marine_locations <- bind_rows(mar_hull_cent %>% select(-data),
                           marine_singleLoc)
  
##----------terrestrial/freshwater-------------
# coords %>%
#   filter(REALM == 'Terrestrial' & Biome=='Temperate_Conifer_Forests') %>%
#   ggplot() +
#   facet_wrap(~Biome, scales ='free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5) +
#   theme(legend.position = 'top', legend.direction = 'horizontal') 

terr_hulls <- plyr::ddply(filter(coords, REALM!='Marine'),
                          plyr::.(REALM, Biome, taxa_mod, STUDY_ID), find_hull) %>%
  as_tibble()

terr_nest <- terr_hulls %>%
  group_by(REALM, Biome, taxa_mod, STUDY_ID) %>%
  filter(n_distinct(rarefyID) > 1) %>%
  nest(rarefyID_x, rarefyID_y) %>% 
  mutate(hull_centre = purrr::map(data, ~ hull_centre(.x$rarefyID_x, .x$rarefyID_y)))


terr_hull_cent <- terr_nest %>% unnest(hull_centre)

# we want a point for each unique combination of biome/taxa/study: ok
terr_hull_cent %>% 
  group_by(Biome, taxa_mod) %>% 
  summarise(n.study = n_distinct(STUDY_ID), # 
            points = n_distinct(hc_x, hc_y)) %>% 
  filter(points!=n.study)

# and we need points for the studies that only had one location 
terr_singleLoc <- terr_hulls %>% 
  group_by(REALM, Biome, taxa_mod, STUDY_ID) %>% 
  filter(n_distinct(rarefyID)==1) %>% 
  mutate(hc_x = rarefyID_x,
         hc_y = rarefyID_y) %>% 
  select(REALM, Biome, taxa_mod, STUDY_ID, hc_x, hc_y) %>% 
  ungroup()

# rejoin with other marine location data
BTS_terr_locations <- bind_rows(terr_hull_cent %>% select(-data),
                                  terr_singleLoc)

# fix biome names for cleaner plot
coords <- coords %>%
  mutate(Biome2 = gsub("_", " ", Biome))


##------combined marine and terrestrial map--------------

# want to combine the biome/taxa correlation data with these geographic data
# the difference between these two dataframes is that _allPoints show taxa-level estimates in all 
# parts of the biomes in which they were observed (rather than reducing to a single point, as for the other dataframe)
all_points <- bind_rows(BTS_marine_locations,
                        BTS_terr_locations)

all_points <- all_points %>%
  mutate(Biome2 = gsub("_", " ", Biome)) %>% 
  unite(bts, c(Biome, taxa_mod, STUDY_ID), remove = F)

# create a filter to get the combinations for biome-taxa-study we want
bts_filter <- study_corr %>% 
  unite(bts, c(Biome, taxa_mod, STUDY_ID)) %>% 
  distinct(bts)

# we are missing some locations for bts combinations
study_corr %>% 
  mutate(Biome = ifelse(Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests_',
                        'Tropical_and_Subtropical_Moist_Broadleaf_Forests',
                        Biome)) %>% 
  unite(bts, c(Biome, taxa_mod, STUDY_ID), remove = F) %>% 
  filter(bts %in% all_points$bts) %>% 
  distinct(Biome, taxa_mod, STUDY_ID)

ab_concept_taxa_plot_allPoints <- left_join(study_corr %>%
                                               select(Biome, taxa_mod, STUDY_ID,
                                                  deltaJne, deltaJne_lower, deltaJne_upper, deltaJtu, deltaJtu_lower,deltaJtu_upper,
                                                  deltaS, deltaS_lower,deltaS_upper,
                                                  quad, alphaS, alphaJtu, alphaJne) %>% 
                                              mutate(Biome = ifelse(Biome=='Tropical_and_Subtropical_Moist_Broadleaf_Forests_',
                                                                    'Tropical_and_Subtropical_Moist_Broadleaf_Forests',
                                                                    Biome)) %>% 
                                              unite(bts, c(Biome, taxa_mod, STUDY_ID), remove = F) %>% 
                                              filter(bts %in% all_points$bts) %>% 
                                              select(-bts),
                                     all_points %>% 
                                       mutate(STUDY_ID = as.character(STUDY_ID)) %>% 
                                       select(-bts),
                                     by = c('Biome', 'taxa_mod', 'STUDY_ID'))

# add indicator to change point stroke as a function of significant trends
study_corr <- study_corr %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 1.1, 1),
         sizeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 2.5, .5),
         sizeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 2.5, .5))

ab_concept_taxa_plot_allPoints <- ab_concept_taxa_plot_allPoints %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 1.1, 1),
         sizeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 2.5, .25),
         sizeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 2.5, .25))


# combine terrestrial and freshwater for plotting
ab_concept_taxa_plot_allPoints <- ab_concept_taxa_plot_allPoints %>%
  mutate(realm2 = ifelse(REALM=='Marine', 'Marine', 'Terrestrial and freshwater'))

# tidy taxa names: All to Multiple
ab_concept_taxa_plot_allPoints$taxa_mod2 <- factor(ab_concept_taxa_plot_allPoints$taxa_mod, 
                                         levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "All", "Marine invertebrates/plants", "Mammals"),
                                         labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

# study_corr$taxa_mod2 <- factor(study_corr$taxa_mod, 
#                                                    levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
#                                                               "All", "Marine invertebrates/plants", "Mammals"),
#                                                    labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plants", "Amphibians",
#                                                               "Multiple taxa", "Marine invertebrates/plants", "Mammals"))
# join all the unique rarefyIDs with 
coords_quad <- inner_join(coords, study_corr %>% select(Biome, taxa_mod, quad, alphaS))

# blue green (colour blind friendly - from colorbrewer2.org)
# quad_col = c('c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')
# quad_sig_fill = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
#                   'c1' = 'white', 'c2' = 'white', 'c3' = 'white', 'c4' = 'white')
# quad_sig_col = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
#                  'c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')

# pink - purple
quad_col = c('c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')
quad_sig_fill = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
                  'c1' = 'white', 'c2' = 'white', 'c3' = 'white', 'c4' = 'white')
quad_sig_col = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
                 'c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')

# maybe no longer use shapes for taxa? 
# shapes = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
#            'Plant' = 5, 'All' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)
# 
# shapes_mod2 = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
#                 'Plant' = 5, 'Multiple taxa' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)

# # put the alpha-beta diversity concept on a map 
# alpha_beta_map <-
# ggplot() +
#   # facet_wrap(~Biome) +
#   geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#cccccc', size=0) +
#   # geom_polygon(data = filter(terr_hulls, Biome=='Boreal_Forests_Taiga'), alpha = 0.5,
#   #              aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   # geom_point(data = filter(coords),
#   #            aes(x = rarefyID_x, y = rarefyID_y, shape = taxa_mod), size = 0.2, alpha = 0.1) +
#   geom_point(data = filter(ab_concept_taxa_plot_allPoints, quad != 'c3' & quad != 'c4'),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod2, colour = quad, stroke = strokeBLUE, size = sizeBLUE),#
#              position = position_jitter(width = 0.25, height = 0.25)) +
#   geom_point(data = filter(ab_concept_taxa_plot_allPoints, quad == 'c3' | quad == 'c4'),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod2, colour = quad, stroke = strokeBLUE, size = sizeBLUE),
#              position = position_jitter(width = 0.25, height = 0.25)) +
#   # geom_hline(yintercept = 23.5, lty = 2) +
#   # geom_hline(yintercept = -23.5, lty = 2) +
#   scale_shape_manual(name = 'Taxa', values = shapes_mod2) +
#   scale_color_manual(guide = FALSE, values = quad_col) +
#   scale_size_area(guide = FALSE) +
#   xlab('') +
#   ylab('') +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
#   scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
#   coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
#   labs(subtitle = 'd') +
#   # coord_map('mollweide', xlim = c(-180, 180)) +
#   theme(axis.text = element_text(size = 14), panel.grid.minor = element_blank(),
#         legend.position = c(0.2, 0.3), legend.direction = 'horizontal', 
#         legend.title = element_text(size = 14), legend.text = element_text(size = 12),
#         legend.background = element_rect(fill = 'transparent'),
#         plot.subtitle = element_text(size = 14, face = 'bold')) +
#   guides(shape = guide_legend(title.position = 'top', ncol = 2, override.aes = list(size = 2)))


# ggsave('bt_study_alpha_beta_map.png', width = 290, height = 200, units = 'mm')
# shapes for realm
shapes_realm <- c('Marine' = 15, 'Terrestrial' = 16, 'Freshwater' = 17)

ab_concept_taxa_plot_allPoints$taxa_mod2 <-  factor(ab_concept_taxa_plot_allPoints$taxa_mod, 
                                                    levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                               "All", "Marine invertebrates/plants", "Mammals"),
                                                    labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plants", "Amphibians",
                                                               "Multiple taxa", "Marine invertebrates/plants", "Mammals"))
taxa_col = c('Multiple taxa' = '#e6f598',
             'Amphibians' = '#fee08b',
             'Benthos' = '#5e4fa2',
             'Birds' = '#f46d43',
             'Fish' = '#3288bd',
             'Invertebrates' = '#abdda4',
             'Mammals' = '#9e0142',
             'Marine invertebrates/plants' = '#66c2a5',
             'Plants' = '#fdae61')


# separate horizontal legend for taxa and realm
realm_taxa_horiz_legend <- ab_concept_taxa_plot_allPoints %>% 
  ggplot() +
  geom_linerange(aes(x = hc_y, ymin = deltaS_lower, colour = taxa_mod2, 
                     ymax = deltaS_upper),
                 alpha = 0.3) +
  geom_hline(data = BT_global_estimates %>% filter(model=='S_pois_new' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_hline(yintercept = 0, lty = 2, lwd = 0.5) +
  geom_rect(data = BT_global_estimates %>% filter(model=='S_pois_new' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.3) +
  geom_point(aes(x = hc_y, y = deltaS, colour = taxa_mod2, shape = REALM),
             size = 3) +
  # scale_color_manual(values = c('Marine' = 'Dark blue', 'Terrestrial' = 'Dark orange', 'Freshwater' = 'Dark green'), guide = FALSE) +
  scale_shape_manual(name = 'Realm', 
                     values = shapes_realm) +
  scale_color_manual(name = 'Taxa', 
                     values = taxa_col) +
  scale_x_continuous(breaks = c(-60, -23.5, 0, 23.5, 60)) +
  labs(x = 'Latitude',
       y = 'Change in species richness [log(S)/yr]'#,
       #subtitle = 'Study-level estimates'
       ) +
  coord_flip()  +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top', 
        legend.direction = 'horizontal')  

source('~/Dropbox/1current/R_random/functions/gg_legend.R') 
realm_taxa_legend <- gg_legend(realm_taxa_horiz_legend)

bt_study_s_lat <- ab_concept_taxa_plot_allPoints %>% 
  ggplot() +
  geom_linerange(aes(x = hc_y, ymin = deltaS_lower, colour = taxa_mod2, 
                     ymax = deltaS_upper),
                 alpha = 0.3) +
  geom_hline(data = BT_global_estimates %>% filter(model=='S_pois_new' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_hline(yintercept = 0, lty = 2, lwd = 0.5) +
  geom_rect(data = BT_global_estimates %>% filter(model=='S_pois_new' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.3) +
  geom_point(aes(x = hc_y, y = deltaS, colour = taxa_mod2, shape = REALM),
             size = 3, alpha = 0.4) +
  # scale_color_manual(values = c('Marine' = 'Dark blue', 'Terrestrial' = 'Dark orange', 'Freshwater' = 'Dark green'), guide = FALSE) +
  scale_color_manual(values = taxa_col) +
  scale_shape_manual(values = shapes_realm) +
  scale_x_continuous(breaks = c(-60, -23.5, 0, 23.5, 60)) +
  labs(x = 'Latitude',
       y = 'Change in species richness [log(S)/yr]',
       subtitle = 'Study-level estimates', 
       tag = 'A') +
  coord_flip()  +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none')

bt_study_jtu_lat <- ab_concept_taxa_plot_allPoints %>%
  ggplot() +
  geom_linerange(aes(x = hc_y, ymin = deltaJtu_lower, colour = taxa_mod2, 
                     ymax = deltaJtu_upper),
                 alpha = 0.3) +
  geom_hline(data = BT_global_estimates %>% filter(model=='Jtu_new_norm' & term=='Slope'),
             aes(yintercept = Estimate)) +
  geom_hline(yintercept = 0, lty = 2, lwd = 0.5) +
  geom_rect(data = BT_global_estimates %>% filter(model=='Jtu_new_norm' & term=='Slope'),
            aes(xmin = -Inf, xmax = Inf, ymin = lower, ymax = upper),
            alpha = 0.3) +
  geom_point(data = ab_concept_taxa_plot_allPoints,
    aes(x = hc_y, y = deltaJtu, colour = taxa_mod2, shape = REALM),
             size = 3, alpha = 0.4) +
  scale_color_manual(values = taxa_col) +
  # scale_color_manual(name = 'Realm',
  #                    values = c('Marine' = 'Dark blue', 'Terrestrial' = 'Dark orange', 'Freshwater' = 'Dark green')) +
  scale_shape_manual(name = 'Realm',
                     values = shapes_realm) +
  scale_x_continuous(breaks = c(-60, -23.5, 0, 23.5, 60)) +
  labs(x = '',
       y = 'Turnover [proportion of species/yr]',
       subtitle = '',
       tag = 'B') +
  coord_flip()  +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none') 

top <- cowplot::plot_grid(NULL, realm_taxa_legend, NULL, nrow = 1)
bottom <- cowplot::plot_grid(bt_study_s_lat, bt_study_jtu_lat, nrow = 1)
cowplot::plot_grid(top, bottom, nrow = 2, rel_heights = c(0.05,1))
ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Revision/figures/FigS11.pdf', width = 290, height = 200, units = 'mm')
