# code to plot biome maps

# run code to plot posterior distribution of global estimates for inset
source('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/code/03a_bt_plot_global_estimates.R')

#----------plot Jtu biome random effects and taxa estimates------------------
# label for the legend
Jtu_leg_label <- c(Jtu_re_quantile[1:4], 0, Jtu_re_quantile[5:6]) %>% signif(2) %>% as.numeric() %>% format(scientific = T)
# alt legend label with the numbers representing biome estimates (not random effects)
Jtu_estimate_label <- as.numeric(signif(c(Jtu_re_quantile[1:4], 0, Jtu_re_quantile[5:6]) + 
                                          BT_global_estimates %>% filter(model=='Jtu_new_norm' & time_period=='ALL') %>% 
                                          filter(term=='Slope') %>% .$Estimate, 4) )

combined_label <- paste0(paste(Jtu_leg_label, Jtu_estimate_label, sep = ' ('),')') 

mar_biome_plot_Jtu <-  ggplot() +
  geom_polygon(data = filter(alldf, Realm2 == 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), 
               alpha = 0.85) +#colour = 'black', 
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  geom_point(data = filter(coords2, REALM == 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = Jtu_re), size = 0.5, stroke = 1.2) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                          '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                                '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  # scale_linetype_manual(guide = F, values = c('yes' = 1, 'no'= 0)) +
  scale_size_area() +
  labs(y = '',
       x = '',
       subtitle = 'A    Marine') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.21, 0.425), legend.direction = 'horizontal',
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
  # geom_polygon(data = filter(alldf, Realm2 != 'Marine'),
  #              aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), 
  #              alpha = 0.85) +#colour = 'black', 
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==1),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==2),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==3),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==5),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jtu_re==6),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jtu_re), alpha = 0.75) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                          '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'),
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                                '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'),
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  # scale_linetype_manual(guide = F, values = c('yes' = 1, 'no'= 0)) +
  labs(x = '',
       y = '',
       subtitle = 'B    Terrestrial and freshwater') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.21, 0.425), legend.direction = 'horizontal',
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
                                               xmin = -160, xmax = -90, ymin = -75, ymax = 0))

bottom <- cowplot::plot_grid(terr_biome_plot_Jtu + annotation_custom(ggplotGrob(global_jtu_inset), 
                                                                    xmin = -160, xmax = -90, ymin = -75, ymax = 0))


# pdf(width = 11, height = 12, file = 'Fig3_changeLabel.pdf')
# png(width = 11, height = 12, file = 'Fig3_changeLabel.png', units = 'in', res = 150)
# cowplot::plot_grid(top, bottom, nrow = 2)
# dev.off()


#----------plot species richness biome random effects and taxa estimates------------------
# label for the legend
leg_label <- c(S_re_quantile[1:3], 0, S_re_quantile[4:6]) %>% signif(2) %>% as.numeric() %>% format(scientific = T)
# alt legend label with the numbers representing biome estimates (not random effects)
S_estimate_label <- as.numeric(signif(c(S_re_quantile[1:3], 0, S_re_quantile[4:6]) + 
                                        BT_global_estimates %>% filter(model=='S_pois_new' & time_period=='ALL') %>% 
                                        filter(term=='Slope') %>% .$Estimate, 4) )

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
       subtitle = 'A    Marine') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.2, 0.425), legend.direction = 'horizontal', 
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
  geom_point(data = filter(coords2, REALM != 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = S_re), size = 0.5, stroke = 1.2) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & S_re==1),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & S_re==2),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & S_re==3),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & S_re==5),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & S_re==6),
               aes(x = long, y = lat, group = group, linetype = NA, fill = S_re), alpha = 0.75) +
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
       subtitle = 'B    Terrestrial and freshwater') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.2, 0.425), legend.direction = 'horizontal', 
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 1))

# without taxa estimates
# pdf(width = 11, height = 12, file = 'Fig1_labelChange.pdf')
# png(width = 11, height = 12, file = 'Fig1.png', units = 'in', res = 150)
cowplot::plot_grid(mar_biome_plot_S + annotation_custom(ggplotGrob(global_s_inset), xmin = -176, xmax = -106, ymin = -75, ymax = 0),
                   terr_biome_plot_S + annotation_custom(ggplotGrob(global_s_inset), xmin = -176, xmax = -106, ymin = -75, ymax = 0), 
                   nrow = 2)
# dev.off()


#----------plot Jne biome random effects and taxa estimates------------------
# label for the legend
Jne_leg_label <- c(Jne_re_quantile[1:4], 0, Jne_re_quantile[5:6]) %>% signif(2) %>% as.numeric() %>% format(scientific = T)

Jne_estimate_label <- as.numeric(signif(c(Jne_re_quantile[1:4], 0, Jne_re_quantile[5:6]) + 
                                          BT_global_estimates %>% filter(model=='Jne_norm' & time_period=='ALL') %>% 
                                          filter(term=='Slope') %>% .$Estimate, 4) )

Jne_combined_label <- paste0(paste(Jne_leg_label, Jne_estimate_label, sep = ' ('),')') 

mar_biome_plot_Jne <- ggplot() +
  geom_polygon(data = filter(alldf, Realm2 == 'Marine'),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.85) +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  # geom_point(data = filter(coords2, REALM == 'Marine'),
  #            aes(x = rarefyID_x, y = rarefyID_y, colour = Jne_re), size = 0.5, stroke = 1.2) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                          '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(Jne_combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                                '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(y = '',
       x = '',
       subtitle = 'A    Marine biomes') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.21, 0.425), legend.direction = 'horizontal',
        # legend.position = 'none',
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 0))


terr_biome_plot_Jne <- ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#7f7f7f', size=0) +
  # geom_point(data = filter(coords2, REALM != 'Marine'),
  #            aes(x = rarefyID_x, y = rarefyID_y, colour = Jne_re), size = 0.5, stroke = 1.2) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jne_re==1),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jne_re==2),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jne_re==3),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jne_re==4),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.75) +
  geom_polygon(data = filter(alldf, realm != 'Marine' & Jne_re==5),
               aes(x = long, y = lat, group = group, linetype = NA, fill = Jne_re), alpha = 0.75) +
  scale_fill_manual(name = '', values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                          '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'), 
                    breaks = c('1', '2', '3', '4', '5', '6'),
                    labels = c(Jne_combined_label[-1]), drop = FALSE) +
  scale_colour_manual(guide = FALSE, values = c('1' = '#b2182b', '2' = '#d6604d', '3' = '#f4a582',
                                                '4' = '#fddbc7', '5' = '#4393c3', '6' = '#2166ac'), 
                      breaks = c('1', '2', '3', '4', '5', '6')) +
  scale_size_area() +
  labs(x = '',
       y = '',
       subtitle = 'B    Terrestrial and freshwater biomes') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        legend.position = c(0.21, 0.425), legend.direction = 'horizontal',
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
top <- cowplot::plot_grid(mar_biome_plot_Jne + annotation_custom(ggplotGrob(global_jne_inset),
                                                                 xmin = -160, xmax = -90, ymin = -73, ymax = 0))

bottom <- cowplot::plot_grid(terr_biome_plot_Jne + annotation_custom(ggplotGrob(global_jne_inset),
                                                                     xmin = -160, xmax = -90, ymin = -73, ymax = 0))


# pdf(width = 11, height = 12, file = 'FigS8_changeLabel.pdf')
# png(width = 11, height = 12, units = 'in', res = 150, file = 'FigS7.png')
# cowplot::plot_grid(top, bottom, nrow = 2)
# dev.off()

# alternate legend/global trend panel
source('~/Dropbox/1current/R_random/functions/gg_legend.R')
Jtu_departure_legend <- ggplot() +
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
       subtitle = 'A    Marine') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),#)
        #legend.position = c(0.2, 0.425), 
        legend.direction = 'horizontal', 
        legend.key.size = unit(5, 'mm'), legend.spacing.x=unit(0, 'mm'),
        legend.text = element_text(angle = 90, margin = margin(t=1)),
        plot.margin = margin(0,0,0,0, 'mm'), legend.background = element_rect(fill = 'transparent'),
        legend.text.align = 0.9,
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(fill = guide_legend(title = 'Biome departure (estimate)', 
                             title.position = 'top',  label.position = 'top', nrow = 1, label.vjust = 0,
                             title.hjust = 1))

jtu_legend <- gg_legend(Jtu_departure_legend)
jtu_guide = cowplot::plot_grid(jtu_legend, global_jtu_inset, ncol = 1, align = 'h', axis = 't')

jtu_top <- cowplot::plot_grid(NULL, jtu_guide, NULL, nrow = 1)
# two-panel figure with maps only
jtu_marine <- cowplot::plot_grid(mar_biome_plot_Jtu)
jtu_terr <- cowplot::plot_grid(terr_biome_plot_Jtu)


# pdf(width = 10, height = 12, file = 'Fig3_changeLabel3.pdf')
# png(width = 11, height = 12, file = 'Fig3_changeLabel.png', units = 'in', res = 150)
cowplot::plot_grid(jtu_top, jtu_marine, jtu_terr, nrow = 3, rel_heights = c(0.5,1,1),
                   rel_widths = c(0.5,1,1))
# dev.off()
