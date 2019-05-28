# code to examine relationships between alpha and beta-diversity change at the study level (new model)

#--Taxa-level plots------
study_corr <- study_corr %>%
  mutate(realm2 = ifelse(REALM=='Marine', REALM, 'Terrestrial and freshwater'),
         realm_label = ifelse(REALM=='A.   Marine', REALM, 'B.   Terrestrial and freshwater'))

# rename group of taxa labelled 'All' to 'Multiple'
study_corr$taxa_mod2 <- factor(study_corr$taxa_mod, 
                              levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                         "All", "Marine invertebrates/plants", "Mammals"),
                              labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                         "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

# set shapes for plotting groups of taxa
shapes_mod2 = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
                'Plant' = 5, 'Multiple taxa' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)

# shapes for realm
shapes_realm <- c('Marine' = 15, 'Terrestrial' = 16, 'Freshwater' = 17)

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

# colours for conceptual model relating changes in alpha & beta diversity
# blue green (colour blind friendly - from colorbrewer2.org) l.blue, d.blue, l.green, d.green
# quad_col = c('c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')
# quad_sig_fill = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
#                   'c1' = 'white', 'c2' = 'white', 'c3' = 'white', 'c4' = 'white')
# quad_sig_col = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
#                  'c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')

# pink purple
quad_col = c('c1' = '#beaed4', 'c2' = '#7570b3', 'c3' = '#fb9a99', 'c4' = '#e7298a')
quad_sig_fill = c('c1_sig' = '#beaed4', 'c2_sig' = '#7570b3', 'c3_sig' = '#fb9a99', 'c4_sig' = '#e7298a', 
                  'c1' = 'white', 'c2' = 'white', 'c3' = 'white', 'c4' = 'white')
quad_sig_col = c('c1_sig' = '#beaed4', 'c2_sig' = '#7570b3', 'c3_sig' = '#fb9a99', 'c4_sig' = '#e7298a', 
                 'c1' = '#beaed4', 'c2' = '#7570b3', 'c3' = '#fb9a99', 'c4' = '#e7298a')

#------taxa-level relationships----------------
# ad-hoc transformation for spreading values around zero.
# sign_sqrt <- scales::trans_new('sign_sqrt',
#                                transform = function(x) {sign(x) * sqrt(abs(x))},
#                                inverse = function(x){sign(x) * abs(x)^2})

# plot change dissmilarity coefficient (either Jtu or Jne) as a function of the richness coefficient
# errorbars omitted for clarity;
alpha_beta_scatter <-
  ggplot() +
  # facet_wrap(~taxa_mod, scales = 'free') +
  # Jtu > Jne
  # geom_errorbarh(data = study_corr %>% filter(Jtu_grt_Jne=='Jtu > Jne'),
  #                aes(x = deltaS, y = deltaJtu, xmin = deltaS_lower, xmax = deltaS_upper, colour = quad, height = 0, stroke = strokeBLUE,
  #                    alpha = ab_sig)) +
  # geom_linerange(data = study_corr %>% filter(Jtu_grt_Jne=='Jtu > Jne'),
  #                aes(x = deltaS,  ymin = deltaJtu_lower, ymax = deltaJtu_upper, colour = quad, stroke = strokeBLUE,
  #                    alpha = ab_sig)) +
  geom_point(data = study_corr %>% filter(Jtu_grt_Jne=='Jtu > Jne'), 
             aes(x = (deltaS), (deltaJtu), colour = quad, shape = REALM, size = sizeBLUE),
             alpha = 0.5) +#
  # Jne > Jtu
  # geom_errorbarh(data = study_corr %>% filter(Jtu_grt_Jne=='Jne > Jtu'),
  #                aes(x = deltaS, y = deltaJne, xmin = deltaS_lower, xmax = deltaS_upper, colour = quad, height = 0, stroke = strokeGREEN,
  #                    alpha = ab_sig)) +
  # geom_linerange(data = study_corr %>% filter(Jtu_grt_Jne=='Jne > Jtu'),
  #                aes(x = deltaS,  ymin = deltaJne_lower, ymax = deltaJne_upper, colour = quad, stroke = strokeGREEN,
  #                    alpha = ab_sig)) +
  geom_point(data = study_corr %>% filter(Jtu_grt_Jne=='Jne > Jtu'),
             aes(x = deltaS, deltaJne, colour = quad, shape = REALM, size = sizeGREEN),
             alpha = 0.5) +
  # stat_smooth(data = study_corr %>% filter(Jtu_grt_Jne=='Jtu > Jne'),
  #             aes(x = deltaS, deltaJtu),
  #             colour = '#7570b3',
  #             method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4), lty = 1) +
  # stat_smooth(data = study_corr %>% filter(Jtu_grt_Jne=='Jne > Jtu'),
  #             aes(x = deltaS, deltaJne),
  #             colour = '#e7298a',
  #             method = 'gam', formula = y ~ s(x, bs = 'cs', k = 4), lty = 2) +
  geom_hline(yintercept = 0, lty = 1) +
  geom_vline(xintercept = 0, lty = 1) +
  labs(y = (expression(paste(Delta, ' dissimilarity component [proportion of species/yr]', sep = ' '))),
       x = (expression(paste(Delta, ' S [log(S)/yr]', sep = ' '))),
       subtitle = 'B') +
  scale_color_manual(values = quad_col, guide = FALSE) +
  # scale_color_manual(values = c('Marine' = 'Dark blue', 'Terrestrial' = 'Dark orange', 'Freshwater' = 'Dark green'), guide = FALSE) +
  # scale_alpha(range = c(0.7, 1), guide = FALSE) +
  scale_size_area(guide = FALSE) +
  scale_shape_manual(values = shapes_realm) +
  # scale_x_continuous(trans = sign_sqrt, breaks = c(-0.2, -0.1, -0.01, 0, 0.01, 0.1, 0.2), labels = c(-0.2, -0.1, -0.01, 0, 0.01, 0.1, 0.2)) +
  # scale_y_continuous(trans = sign_sqrt, breaks = c(0, 0.01, 0.02, 0.04, 0.08, 0.12)) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.9), legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), #, angle = 70, vjust = 0.7
        axis.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')#,
        # plot.margin = unit(c(0,0,0,0), 'cm')
        ) +
  guides(shape = guide_legend(title = '', override.aes = list(size = 3, alpha = 1)))
#       legend.position = c(0.1, 0.9), legend.text = element_text(size = 22), legend.title = element_text(size = 26)) +
# guides(shape = guide_legend(override.aes = list(size = 4)))

# ggsave('Fig5_gamVis.png', width = 100, height = 100, units = 'mm')

# summary of alpha_beta conceptual model
a_b_summary_plot <-
  ggplot() +
  # facet_wrap(~realm2, ncol = 1) +
  geom_histogram(data = study_corr,
                 aes(x = quad, fill = quad_sig, colour = quad_sig), 
                 stat = 'count') +
  scale_fill_manual(values = quad_sig_fill, guide = FALSE) +
  scale_colour_manual(values = quad_sig_col, guide = FALSE) +
  scale_x_discrete(breaks = c('c1', 'c2', 'c3', 'c4'),
                   labels = c(expression(paste(symbol('\255'), 'Jtu', symbol('\257'), 'S')),
                              expression(paste(symbol('\255'), 'Jtu', symbol('\255'), 'S')),
                              expression(paste(symbol('\255'), 'Jne', symbol('\257'), 'S')),
                              expression(paste(symbol('\255'), 'Jne', symbol('\255'), 'S')))) +
  labs(y = 'Number of biome-taxa-study combinations',
       x = '',
       subtitle = 'C') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14, face = 'bold')#,
        # plot.margin = unit(c(0,0,0,0), 'cm')
        )

top1 <- cowplot::ggdraw() + cowplot::draw_image('~/Dropbox/BiogeoBioTIME/Figures/Conceptual_figure_pink_purple.png',
                                                clip = 'on')
top1a <- cowplot::plot_grid(NULL, top1, NULL, rel_widths = c(0,1,0))
top2 <- cowplot::plot_grid(alpha_beta_scatter, a_b_summary_plot, nrow = 1, align = 'hv')
# 1 row
# top <- cowplot::plot_grid(top1, alpha_beta_scatter, a_b_summary_plot, nrow = 1, axis = 't',
#                           rel_widths = c(0.9,1,1))

# 2 rows
cowplot::plot_grid(top1, top2, nrow = 2, align = 'hv',
                   scale = c(1.,1))
# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Revision/figures/Fig5_2row.pdf', width = 220, height = 220, units = 'mm')

# save without map
top
# ggsave('Fig5_filledSymbols.png', width = 390, height = 150, units = 'mm')
# run code to generate alpha_beta_map

cowplot::plot_grid(top, alpha_beta_map, ncol = 1, rel_heights = c(0.5,0.9))
# cowplot::plot_grid(top2, alpha_beta_map, ncol = 1)

setwd('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/figs/')
# ggsave('alpha_beta_taxaAllPoints_Concept_trans.pdf', width = 350, height = 350, units = 'mm')
# ggsave('Fig5_newModel_withMap.png', width = 350, height = 350, units = 'mm')

#---regression plots for focal results---------------
rarefied_medians$taxa_mod2 <- factor(rarefied_medians$taxa_mod, 
                                     levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "All", "Marine invertebrates/plants", "Mammals"),
                                     labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

BT_taxa_estimate$taxa_mod2 <- factor(BT_taxa_estimate$taxa_mod, 
                                     levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "All", "Marine invertebrates/plants", "Mammals"),
                                     labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

# put the Jtu_grt_Jne covariate (& other plotting metadata) into rarefied_medians for colour coding the points
quad_model <- study_corr %>%
  distinct(Biome, taxa_mod, quad, Jtu_grt_Jne, sizeBLUE, sizeGREEN, ab_sig)
rarefied_medians1 <- inner_join(quad_model, rarefied_medians, by = c('Biome', 'taxa_mod'))
BT_taxa_estimate <- inner_join(quad_model, BT_taxa_estimate, by = c('Biome', 'taxa_mod'))



# wrangle biome names for cleaner plots
rarefied_medians1 <- rarefied_medians1 %>%
  mutate(Biome_clean = gsub('_', ' ', Biome),
         # fix some other random ones up
         Biome_clean = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome_clean = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome_clean = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome_clean = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
         Biome_clean = ifelse(Biome=='large lakes', 'Large lakes', Biome),
         Biome_clean = ifelse(Biome=='polar freshwaters', 'Polar freshwaters', Biome),
         Biome_clean = ifelse(Biome=='temperate upland rivers', 'Temperate upland rivers', Biome),
         Biome_clean = ifelse(Biome=='temperate coastal rivers', 'Temperate coastal rivers', Biome),
         Biome_clean = ifelse(Biome=='temperate floodplain rivers and wetlands', 'Temperate floodplain rivers and wetlands', Biome))

BT_taxa_estimate <- BT_taxa_estimate %>%
  mutate(Biome_clean = gsub('_', ' ', Biome),
         # fix some other random ones up
         Biome_clean = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome_clean = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome_clean = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome_clean = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 'Temperate Grasslands, Savannas and Shrublands', Biome),
         Biome_clean = ifelse(Biome=='large lakes', 'Large lakes', Biome),
         Biome_clean = ifelse(Biome=='polar freshwaters', 'Polar freshwaters', Biome),
         Biome_clean = ifelse(Biome=='temperate upland rivers', 'Temperate upland rivers', Biome),
         Biome_clean = ifelse(Biome=='temperate coastal rivers', 'Temperate coastal rivers', Biome),
         Biome_clean = ifelse(Biome=='temperate floodplain rivers and wetlands', 'Temperate floodplain rivers and wetlands', Biome),
         Realm2 = ifelse(REALM=='Marine', 'Marine', 'Terrestrial/Freshwater'))



# create a filter for the biome-taxa combinations with Jne > Jtu
focal_bt <- study_corr %>%
  filter(Jtu_grt_Jne=='Jne > Jtu' & quad!=quad_sig)

# linetype for change different from zero (solid) or not (dashed)
change_linetype = c('neutral' = 2, 'up' = 1, 'down' = 1)

jne_grt_jtu_jne_scatter <- ggplot() +
  facet_wrap(~Biome, scales='free') +
  geom_point(data = rarefied_medians1 %>% filter(n_cells > 3 & Biome %in% focal_bt$Biome & Jtu_grt_Jne=='Jtu > Jne') %>%
               distinct(Biome, taxa_mod2, YEAR, Jtu_base, quad, ab_sig, sizeBLUE),
             aes(YEAR, y= Jtu_base, colour=quad, shape = taxa_mod2), alpha= 0.5) +#, alpha = ab_sig, size = sizeBLUE
  geom_point(data = rarefied_medians1 %>% filter(n_cells > 3 & Biome %in% focal_bt$Biome & Jtu_grt_Jne=='Jne > Jtu') %>%
               distinct(Biome, taxa_mod2, YEAR, Jne_base, quad, ab_sig, sizeGREEN),
             aes(YEAR, y= Jne_base, colour=quad, shape = taxa_mod2), alpha = 0.5) +#alpha = ab_sig, size = sizeGREEN
  geom_segment(data = BT_taxa_estimate %>% filter(model=='Jne_norm' & n_cells > 3 & Biome %in% focal_bt$Biome & Jtu_grt_Jne=='Jne > Jtu'), 
               aes(x=start2, y=(Estimate.Intercept+Estimate.cYEAR*start), 
                   xend=stop2, yend=(Estimate.Intercept+Estimate.cYEAR*stop), colour=quad, linetype = change), lwd=2) +
  geom_segment(data = BT_taxa_estimate %>% filter(model=='Jtu_norm' & n_cells > 3 & Biome %in% focal_bt$Biome & Jtu_grt_Jne=='Jtu > Jne'), 
               aes(x=start2, y=(Estimate.Intercept+Estimate.cYEAR*start), 
                   xend=stop2, yend=(Estimate.Intercept+Estimate.cYEAR*stop), colour=quad, linetype = change), lwd=2) +
  scale_colour_manual(values=quad_col) +
  scale_shape_manual(values = shapes_mod2) +
  scale_linetype_manual(values = change_linetype) +
  scale_alpha(range = c(0.7, 1), guide = FALSE) +
  scale_size_area(guide = FALSE) +
  ylab('Turnover (blue) or nestedness (green)') +
  xlab('') +
  theme_bw() +
  scale_y_continuous(breaks = c(0,1), labels = c(0,1)) +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = 'none', panel.grid = element_blank())

jne_grt_jtu_S_scatter <- ggplot() +
  facet_wrap(~Biome, scales='free') +
  geom_point(data = rarefied_medians1 %>% filter(n_cells > 3 & Biome %in% focal_bt$Biome & Jtu_grt_Jne=='Jtu > Jne') %>%
               distinct(Biome, taxa_mod2, YEAR, S, quad, ab_sig, sizeBLUE),
             aes(YEAR, y= S, colour=quad, shape = taxa_mod2), alpha = 0.5) +#, alpha = ab_sig, size = sizeBLUE
  geom_point(data = rarefied_medians1 %>% filter(n_cells > 3 & Biome %in% focal_bt$Biome & Jtu_grt_Jne=='Jne > Jtu') %>%
               distinct(Biome, taxa_mod2, YEAR, S, quad, ab_sig, sizeGREEN),
             aes(YEAR, y= S, colour=quad, shape = taxa_mod2), alpha = 0.5) +#, alpha = ab_sig, size = sizeGREEN
  geom_segment(data = BT_taxa_estimate %>% filter(model=='S_pois' & n_cells > 3 & Biome %in% focal_bt$Biome), 
               aes(x=start2, y=exp(Estimate.Intercept+Estimate.cYEAR*start), 
                   xend=stop2, yend=exp(Estimate.Intercept+Estimate.cYEAR*stop), colour=quad, linetype = change), lwd=2) +
  # stat_smooth(method = 'lm', se = F,
  #             data = rarefied_medians %>% filter(n_cells > 3 & Biome=='Cold_Temperate_Northwest_Atlantic' & taxa_mod2=='Fish'),
  #             aes(x = YEAR, y = S), colour = 'black') +
  # stat_smooth(method = 'lm', se = F,
  #             data = rarefied_medians %>% filter(n_cells > 3 & Biome=='Cold_Temperate_Northwest_Atlantic' & taxa_mod2=='Invertebrates'),
  #             aes(x = YEAR, y = S), colour = 'black') +
  scale_y_continuous(trans = 'log', breaks = c(1,2,16, 32, 128, 256, 512), expand = c(0,1)) +
  scale_colour_manual(guide = FALSE, values=quad_col) +
  scale_shape_manual(name = 'Taxa', values = shapes_mod2) +
  scale_linetype_manual(guide = FALSE, values = change_linetype) +
  scale_alpha(range = c(0.5, 1), guide = FALSE) +
  scale_size_area(guide = FALSE) +
  ylab('Species richness') +
  xlab('Year') +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        # legend.direction = 'horizontal') +#,
         legend.position = 'none')
  # guides(shape = guide_legend(nrow = 1, override.aes = list(alpha = 1, size = 1.5)))


# source('~/Dropbox/1current/R_random/functions/gg_legend.R')        
# taxa_legend <- gg_legend(jne_grt_jtu_S_scatter)

cowplot::plot_grid(taxa_legend, jne_grt_jtu_jne_scatter, jne_grt_jtu_S_scatter, nrow = 3,
                   rel_heights = c(0.05,1,1), labels = c('', 'a', 'b'))
# ggsave('biome_case_study_regression_all.png', width = 290, height = 180, units = 'mm')
