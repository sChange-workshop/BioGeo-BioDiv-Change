# update code for new models
library(brms)
library(tidyverse)

setwd('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/figs/')

# load model fits to extract posterior samples
load('~/Desktop/revision_models/Jtu_norm_BTSRfyID_count.Rdata')
load('~/Desktop/revision_models/S_pois_BTSRfyID_expPrior.Rdata')
load('~/Desktop/revision_models/Jne_norm_BTSRfyID_count.Rdata')

# want coefficient plot of the posterior distribution of the global slope
jne_post <- posterior_samples(Jne_norm_BTSRfyID, pars = 'b_cYEAR', subset = 2000:3000)

jne_summary <- jne_post %>%
  summarise(
    median = median(b_cYEAR),
    lower50 = quantile(b_cYEAR, probs = 0.25),
    upper50 = quantile(b_cYEAR, probs = 0.75),
    lower90 = quantile(b_cYEAR, probs = 0.05),
    upper90 = quantile(b_cYEAR, probs = 0.95))

jtu_post <- posterior_samples(Jtu_norm_BTSRfyID, pars = 'b_cYEAR', subset = 2000:3000)

jtu_summary <- jtu_post %>%
  summarise(
    median = median(b_cYEAR),
    lower50 = quantile(b_cYEAR, probs = 0.25),
    upper50 = quantile(b_cYEAR, probs = 0.75),
    lower90 = quantile(b_cYEAR, probs = 0.05),
    upper90 = quantile(b_cYEAR, probs = 0.95))

s_post <- posterior_samples(S_pois_BTSRfyID_countData, pars = 'b_cYEAR', subset = 2000:3000)

s_summary <- s_post %>%
  summarise(
    median = median(b_cYEAR),
    lower50 = quantile(b_cYEAR, probs = 0.25),
    upper50 = quantile(b_cYEAR, probs = 0.75),
    lower90 = quantile(b_cYEAR, probs = 0.05),
    upper90 = quantile(b_cYEAR, probs = 0.95),
    lower95 = quantile(b_cYEAR, probs = 0.025),
    upper95 = quantile(b_cYEAR, probs = 0.975))

# tidy up (the model objects are big)
rm(Jne_norm_BTSRfyID, Jtu_norm_BTSRfyID, S_pois_BTSRfyID_countData)

global_jne_inset  = ggplot() +
  # geom_density(data = jtu_post,
  #              aes(x = b_cYEAR, linetype = NA),  fill = 'grey') +
  geom_point(data = jne_summary,
             aes(x = median, y = 0.1), size = 6) +
  geom_segment(data = jne_summary,
               aes(x = lower50, y = 0.1, xend = upper50, yend= 0.1), size = 3) +
  geom_segment(data = jne_summary,
               aes(x = lower90, y = 0.1, xend = upper90, yend= 0.1), size = 1.5) +
  scale_x_continuous(position = 'bottom', breaks = c(0.005, 0.0055, 0.006, 0.0065, 0.007), labels = c(0.005, '', 0.006, '', 0.007)) +
  scale_y_continuous(expand = c(0.02,0)) +
  coord_cartesian(ylim = c(0,1)) +
  ylab('') +
  xlab('Change in nestedness [proportion of species/yr]') +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), 
        plot.background = element_rect(fill = 'transparent', colour = NA),
        # axis.text.x = element_text(size = 28),  axis.title = element_text(size = 28),
        axis.line.x = element_line(size = 1),
        # axis.ticks.x = element_line(size = 2), axis.ticks.length = unit(4, 'mm'),
        axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(),
        legend.background = element_rect(fill = alpha('white', alpha = 0.4)))

global_jtu_inset <- ggplot() +
  # geom_density(data = jtu_post,
  #              aes(x = b_cYEAR, linetype = NA),  fill = 'grey') +
  geom_point(data = jtu_summary,
             aes(x = median, y = 0.1), size = 6) +
  geom_segment(data = jtu_summary,
               aes(x = lower50, y = 0.1, xend = upper50, yend= 0.1), size = 3) +
  geom_segment(data = jtu_summary,
               aes(x = lower90, y = 0.1, xend = upper90, yend= 0.1), size = 1.5) +
  scale_x_continuous(position = 'bottom', breaks = c(0.025, 0.0275, 0.03, 0.0325), labels = c(0.025, '', 0.03, '')) +
  scale_y_continuous(expand = c(0.02,0)) +
  coord_cartesian(ylim = c(0,1)) +
  ylab('') +
  xlab('Turnover [proportion of species/yr]') +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), 
        plot.background = element_rect(fill = 'transparent', colour = NA),
        # axis.text.x = element_text(size = 28),  axis.title = element_text(size = 28),
        axis.line.x = element_line(size = 1),
        # axis.ticks.x = element_line(size = 2), axis.ticks.length = unit(4, 'mm'),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.background = element_rect(fill = alpha('white', alpha = 0.5)),
        plot.margin = margin(0,0,0,0, 'mm'))

# ggsave('Jtu_overall_interval.pdf', width = 100, height = 40, units = 'mm')

global_s_inset <- ggplot() +
  # geom_density(data = jtu_post,
  #              aes(x = b_cYEAR, linetype = NA),  fill = 'grey') +
  geom_point(data = s_summary,
             aes(x = median, y = 0.1), size = 6) +
  geom_segment(data = s_summary,
               aes(x = lower50, y = 0.1, xend = upper50, yend= 0.1), size = 3) +
  geom_segment(data = s_summary,
               aes(x = lower90, y = 0.1, xend = upper90, yend= 0.1), size = 1.5) +
  scale_x_continuous(position = 'bottom', breaks = c(0, 0.005, 0.01)) +
  scale_y_continuous(expand = c(0.02,0)) +
  coord_cartesian(ylim = c(0,1)) +
  ylab('') +
  xlab('Species richness change [log(S)/yr]') +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), 
        plot.background = element_rect(fill = 'transparent', colour = NA),
        # axis.text.x = element_text(size = 28),  axis.title = element_text(size = 28),
        axis.line.x = element_line(size = 1),
        # axis.ticks.x = element_line(size = 2), axis.ticks.length = unit(4, 'mm'),
        axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(),
        legend.background = element_rect(fill = alpha('white', alpha = 0.5)))

# ggsave('S_overall_interval.pdf', width = 100, height = 40, units = 'mm')
