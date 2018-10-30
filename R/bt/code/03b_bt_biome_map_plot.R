# code to plot biome maps, 

# run code to wrangle biome data in preparation
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/02d_bt_biome_map_wrangle.R')

# run code to plot posterior distribution of global estimates for inset
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/03a_bt_plot_global_estimates.R')

#----------plot Jtu biome random effects and taxa estimates------------------
# label for the legend
Jtu_leg_label <- c(Jtu_re_quantile[1:3], 0, Jtu_re_quantile[4:6]) %>% signif(2) %>% as.numeric() %>% format(scientific = T)
# alt legend label with the numbers representing biome estimates (not random effects)
Jtu_estimate_label <- as.numeric(signif(c(Jtu_re_quantile[1:3], 0, Jtu_re_quantile[4:6]) + 
                                          BT_global_estimates %>% filter(model=='Jtu_norm' & time_period=='ALL') %>% 
                                          filter(term=='Slope') %>% .$Estimate, 4) )

combined_label <- paste0(paste(Jtu_leg_label, Jtu_estimate_label, sep = ' ('),')') 

mar_biome_plot_Jtu <-  ggplot() +
  geom_polygon(data = filter(alldf, Realm2 == 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.85) +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_point(data = filter(coords2, REALM == 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = Jtu_re), size = 0.5, stroke = 1.2) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                          '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                                '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(y = '',
       x = '',
       subtitle = 'a    Marine') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.21, 0.4), legend.direction = 'horizontal',
        # legend.position = 'none',
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 0))

terr_biome_plot_Jtu <- ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_point(data = coords2 %>% dplyr::filter(REALM!='Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = Jtu_re), size = 0.5, stroke = 1.2) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==1),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 1) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==2),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.9) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==4),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.8) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==5),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 1) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                          '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                                '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  labs(x = '',
       y = '',
       subtitle = 'b    Terrestrial/Freshwater') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.21, 0.4), legend.direction = 'horizontal',
        # legend.position = 'none',
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 0))

# two-panel figure with maps only
top <- cowplot::plot_grid(mar_biome_plot_Jtu + annotation_custom(ggplotGrob(global_jtu_inset), 
                                                                 xmin = -170, xmax = -100, ymin = -75, ymax = 0))

bottom <- cowplot::plot_grid(terr_biome_plot_Jtu + annotation_custom(ggplotGrob(global_jtu_inset), 
                                                                     xmin = -170, xmax = -100, ymin = -75, ymax = 0))


# pdf(width = 11, height = 12, file = 'Jtu_mapOnly_inset2.pdf')
# cowplot::plot_grid(top, bottom, nrow = 2)
# dev.off()


#----------plot species richness biome random effects and taxa estimates------------------
# label for the legend
leg_label <- c(S_re_quantile[1:3], 0, S_re_quantile[4:6]) %>% signif(2) %>% as.numeric() %>% format(scientific = T)
# alt legend label with the numbers representing biome estimates (not random effects)
S_estimate_label <- as.numeric(signif(c(S_re_quantile[1:3], 0, S_re_quantile[4:6]) + s_summary$median, 3))

S_combined_label <- paste0(paste(leg_label, S_estimate_label, sep = ' ('),')') 

mar_biome_plot_S<- ggplot() +
  # geom_sf(data = filter(mar_shp2, Biome %in% mar_biomes$Biome),
  #         aes(Biome, fill = Slope_ranef*100, linetype = NA)) +
  geom_polygon(data = filter(alldf, Realm2 == 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.85) +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_point(data = filter(coords2, REALM == 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = S_re), size = 0.5, stroke = 1.2) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                          '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(S_combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                                '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(y = '',
       x = '',
       subtitle = 'a    Marine') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.2, 0.4), legend.direction = 'horizontal', 
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 1))


terr_biome_plot_S <- ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_polygon(data = filter(alldf, Realm2 != 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.85) +
  geom_point(data = filter(coords2, REALM != 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = S_re), size = 0.5, stroke = 1.2) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                          '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(S_combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                                '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(y = '',
       x = '',
       subtitle = 'b    Terrestrial/Freshwater') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.2, 0.4), legend.direction = 'horizontal', 
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 1))

# without taxa estimates
# pdf(width = 11, height = 12, file = 'S_map_noTaxa_inset2.pdf')
cowplot::plot_grid(mar_biome_plot_S + annotation_custom(ggplotGrob(global_s_inset), xmin = -169, xmax = -99, ymin = -75, ymax = 0),
                   terr_biome_plot_S + annotation_custom(ggplotGrob(global_s_inset), xmin = -169, xmax = -99, ymin = -75, ymax = 0), nrow = 2)
# dev.off()


#----------plot Jne biome random effects and taxa estimates------------------
# label for the legend
Jne_leg_label <- c(Jne_re_quantile[1:3], 0, Jne_re_quantile[4:6]) %>% signif(2) %>% as.numeric() %>% format(scientific = T)

Jne_estimate_label <- as.numeric(signif(c(Jne_re_quantile[1:3], 0, Jne_re_quantile[4:6]) + 
                                          BT_global_estimates %>% filter(model=='Jne_norm' & time_period=='ALL') %>% 
                                          filter(term=='Slope') %>% .$Estimate, 4) )

Jne_combined_label <- paste0(paste(Jne_leg_label, Jne_estimate_label, sep = ' ('),')') 

mar_biome_plot_Jne <- ggplot() +
  geom_polygon(data = filter(alldf, Realm2 == 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.85) +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_point(data = filter(coords2, REALM == 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = Jne_re), size = 0.5, stroke = 1.2) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                          '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(Jne_combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                                '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(y = '',
       x = '',
       subtitle = 'a    Marine biomes') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.2, 0.42), legend.direction = 'horizontal', 
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=0.5)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure\n(estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, 
                             title.vjust = 5, label.vjust = 0.8))

ab_concept_taxa_plot_allPoints <- ab_concept_taxa_plot_allPoints %>%
  mutate(realm_label = ifelse(REALM=='Marine', 'b   Marine', 'd   Terrestrial/Freshwater'))

terr_biome_plot_Jne <- ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_point(data = filter(coords2, REALM != 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = Jne_re), size = 0.5, stroke = 1.2) +
  geom_polygon(data = filter(alldf, Realm2 != 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.85) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                          '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(Jne_combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#ef8a62', '3' = '#fddbc7',
                                                '4' = '#d1e5f0', '5' = '#67a9cf', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(x = '',
       y = '',
       subtitle = 'c    Terrestrial/Freshwater biomes') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.2, 0.42), legend.direction = 'horizontal', 
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=0.5)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure\n(estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, 
                             title.vjust = 5, label.vjust = 0.8))


# two-panel figure with maps only
# top <- cowplot::plot_grid(mar_biome_plot_Jne + annotation_custom(ggplotGrob(global_jne_inset), 
                                                                 # xmin = -158, xmax = -88, ymin = -73, ymax = 0))
# 
# bottom <- cowplot::plot_grid(terr_biome_plot_Jne + annotation_custom(ggplotGrob(global_jne_inset), 
#                                                                      xmin = -170, xmax = -100, ymin = -73, ymax = 0))


# pdf(width = 11, height = 12, file = 'Jne_mapOnly.pdf')
# cowplot::plot_grid(top, bottom, nrow = 2)
# dev.off()

# for four panel figure with taxa-level estimates
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

Jne_marine <- ggplot() +
  # facet_wrap(~realm_label, ncol = 1) +
  geom_hline(yintercept = 0, lty=2, alpha = 1, lwd = 0.5) +
  geom_hline(data = filter(BT_global_estimates, model=='Jne_norm' & term=='Slope' & time_period=='ALL'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(BT_global_estimates, model=='Jne_norm' & term=='Slope' & time_period=='ALL'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  geom_linerange(data = filter(ab_concept_taxa_plot_allPoints, realm2=='Marine'),
                 aes(x = hc_y, ymin = deltaJne_lower, ymax = deltaJne_upper, 
                     group=interaction(REALM, Biome, taxa_mod), colour = taxa_mod2), alpha = 0.6, lwd = 0.6) +
  geom_point(data = filter(ab_concept_taxa_plot_allPoints, realm2=='Marine'), 
             aes(x = hc_y, y = deltaJne, group= interaction(REALM, Biome, taxa_mod2), colour = taxa_mod2, shape=taxa_mod2), 
             size=2) +
  scale_shape_manual(name = 'Taxa', values = shapes_mod2) +
  scale_colour_manual(name = 'Taxa', values = taxa_col2) + 
  labs(y = 'Nestedness change per year',
       x = 'Latitude',
       subtitle = 'b   Marine') +
  scale_x_continuous(breaks = c(-60, -23.5, 0, 23.5, 60)) +
  # scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1), labels = c(0, '', '0.04', '', '0.08', '')) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12), 
        legend.position = 'top', legend.background = element_rect(fill='transparent'),
        legend.justification = 'left',
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = 'bold', hjust = 0),
        strip.background = element_blank(),
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(shape = guide_legend(nrow = 2), colour = guide_legend(nrow = 2)) +
  coord_flip()


Jne_terr <- ggplot() +
  # facet_wrap(~realm_label, ncol = 1) +
  geom_hline(yintercept = 0, lty=2, alpha = 1, lwd = 0.5) +
  geom_hline(data = filter(BT_global_estimates, model=='Jne_norm' & term=='Slope' & time_period=='ALL'), aes(yintercept = Estimate)) +
  geom_rect(data = filter(BT_global_estimates, model=='Jne_norm' & term=='Slope' & time_period=='ALL'), 
            aes(ymin = lower, ymax = upper, xmin = -Inf, xmax = Inf), alpha=0.4) +
  geom_linerange(data = filter(ab_concept_taxa_plot_allPoints, realm2!='Marine'),
                 aes(x = hc_y, ymin = deltaJne_lower, ymax = deltaJne_upper, 
                     group=interaction(REALM, Biome, taxa_mod), colour = taxa_mod2), alpha = 0.6, lwd = 0.6) +
  geom_point(data = filter(ab_concept_taxa_plot_allPoints, realm2!='Marine'), 
             aes(x = hc_y, y = deltaJne, group= interaction(REALM, Biome, taxa_mod2), colour = taxa_mod2, shape=taxa_mod2), 
             size=2) +
  scale_shape_manual(name = 'Taxa', values = shapes_mod2) +
  scale_colour_manual(name = 'Taxa', values = taxa_col2) + 
  labs(y = 'Nestedness change per year',
       x = 'Latitude',
       subtitle = 'd   Terrestrial/Freshwater') +
  scale_x_continuous(breaks = c(-60, -23.5, 0, 23.5, 60)) +
  # scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1), labels = c(0, '', '0.04', '', '0.08', '')) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12), 
        legend.position = 'top', legend.background = element_rect(fill='transparent'),
        legend.justification = 'left',
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = 'bold', hjust = 0),
        strip.background = element_blank(),
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(shape = guide_legend(nrow = 2), colour = guide_legend(nrow = 2)) +
  coord_flip()


# four panel figure maps + taxa coefficients
top <- cowplot::plot_grid(mar_biome_plot_Jne + annotation_custom(ggplotGrob(global_jne_inset), 
                                                                 xmin = -158, xmax = -88, ymin = -73, ymax = 0),
                          Jne_marine, nrow = 1, rel_widths = c(1, 0.6))
bottom <- cowplot::plot_grid(terr_biome_plot_Jne + annotation_custom(ggplotGrob(global_jne_inset), 
                                                                     xmin = -158, xmax = -88, ymin = -73, ymax = 0),
                             Jne_terr, nrow = 1, rel_widths = c(1, 0.6))

# with taxa estimates
# pdf(width = 15, height = 13, file = 'Jne_map2_withTaxa.pdf')
# cowplot::plot_grid(top, bottom, nrow = 2)
# dev.off()

# png(width = 15, height = 13, file = 'Jne_map2_withTaxa.png', units = 'in', res = 150)
# cowplot::plot_grid(top, bottom, nrow = 2)
# dev.off()

# setwd('~/Desktop/')