## compare the coefficients of jtu_norm and jtu_beta
rm(list=ls())
library(tidyverse)
library(ggridges)
load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/model_coefs_ranefs/BTRfyID_coef_ranef_inclNewModel.Rdata')

# get the z1i Jtu model too...
load('~/Desktop/revision_models/Jtu_z1i_realm_BTSRfyID_count-4946085.Rdata')

# posterior distributions of the coi parameter for supplement
coi_post <- posterior_samples(Jtu_z1i_realm_BTSRfyID_countData,
                              pars = c('^b_coi_', '^b_zoi_')) %>% 
  as_tibble() 

coi_post2 <- tibble(
  Realm = rep(c('Freshwater', 'Marine', 'Terrestrial'), each = 4000),
  coi = c(plogis(coi_post$b_coi_Intercept), 
          plogis(coi_post$b_coi_Intercept + coi_post$b_coi_REALMMarine),
          plogis(coi_post$b_coi_Intercept + coi_post$b_coi_REALMTerrestrial)))

ggplot() +
  geom_density_ridges(data = coi_post2,
                      aes(x = coi, 
                          y = Realm),
                      linetype = 0,
                      scale = 0.95, alpha = 0.6)

jtu_z1i_fixef <- fixef(Jtu_z1i_realm_BTSRfyID_countData, robust = T, probs = c(0.05, 0.95)) %>% 
  as_tibble() %>% 
  mutate(parname = rownames(fixef(Jtu_z1i_realm_BTSRfyID_countData)))

coi <- tibble(Realm = c('Freshwater', 'Marine', 'Terrestrial'),
              # pr one = zoi * coi
              pr_one = c((jtu_z1i_fixef %>% filter(parname=='coi_Intercept') %>% .$Estimate *
                jtu_z1i_fixef %>% filter(parname=='zoi_Intercept') %>% .$Estimate) %>% plogis,
                ((jtu_z1i_fixef %>% filter(parname=='coi_Intercept' |parname=='coi_REALMMarine') %>% .$Estimate %>% sum) *
                   (jtu_z1i_fixef %>% filter(parname=='zoi_Intercept' |parname=='zoi_REALMMarine') %>% .$Estimate %>% sum)) %>% plogis,
              ((jtu_z1i_fixef %>% filter(parname=='coi_Intercept' |parname=='coi_REALMTerrestrial') %>% .$Estimate %>% sum) *
                 (jtu_z1i_fixef %>% filter(parname=='zoi_Intercept' |parname=='zoi_REALMTerrestrial') %>% .$Estimate %>% sum)) %>% plogis),
              
              coi = c(jtu_z1i_fixef %>% filter(parname=='coi_Intercept') %>% .$Estimate %>% plogis,
                      jtu_z1i_fixef %>% filter(parname=='coi_Intercept' | parname=='coi_REALMMarine') %>% 
                        .$Estimate %>% sum %>%  plogis,
                      jtu_z1i_fixef %>% filter(parname=='coi_Intercept' | parname=='coi_REALMTerrestrial') %>% 
                        .$Estimate %>% sum %>%  plogis),
              coi_upper = c(jtu_z1i_fixef %>% filter(parname=='coi_Intercept') %>% .$Q95 %>% plogis,
                            jtu_z1i_fixef %>% filter(parname=='coi_Intercept' | parname=='coi_REALMMarine') %>% 
                              .$Q95 %>% sum %>%  plogis,
                            jtu_z1i_fixef %>% filter(parname=='coi_Intercept' | parname=='coi_REALMTerrestrial') %>% 
                              .$Q95 %>% sum %>%  plogis),
              coi_lower = c(jtu_z1i_fixef %>% filter(parname=='coi_Intercept') %>% .$Q5 %>% plogis,
                            jtu_z1i_fixef %>% filter(parname=='coi_Intercept' | parname=='coi_REALMMarine') %>% 
                              .$Q5 %>% sum %>%  plogis,
                            jtu_z1i_fixef %>% filter(parname=='coi_Intercept' | parname=='coi_REALMTerrestrial') %>% 
                              .$Q5 %>% sum %>%  plogis))

ggplot() +
  geom_point(data = coi,
             aes(x = pr_one, y = Realm, colour = Realm), 
             size = 2) +
  geom_errorbarh(data = coi,
                 aes(xmin = coi_lower, xmax = coi_upper, y = Realm, colour = Realm),
                 height = 0) +
  scale_colour_manual(guide = F,
                      values = c('Marine' = '#0072B2', 'Terrestrial' = '#D55E00', 'Freshwater' = '#009E73')) +
  labs(x = 'Probability of complete turnover',
       y = 'Realm') +
  theme_bw() 

# ggsave('FigSx_Jtu_z1i_PrJtu1.png', width = 100, height = 100, units = 'mm')

# compare  parameter estimates
# rarefyID
bt_rid_coefs <- bind_cols(
  # new turnover
  BTRfyID_rarefyID_coef %>% 
    filter(model=='Jtu_new_norm' & time_period=='ALL') %>% 
    mutate(jtu_norm = Estimate.cYEAR,
           jtu_norm_upper = upper_slope, 
           jtu_norm_lower = lower_slope) %>% 
    select(level, jtu_norm, jtu_norm_upper, jtu_norm_lower),
  BTRfyID_rarefyID_coef %>% 
    filter(model=='Jtu_beta' & time_period=='ALL') %>% 
    mutate(jtu_beta = Estimate.cYEAR,
           jtu_beta_upper = upper_slope, 
           jtu_beta_lower = lower_slope) %>% 
    select(jtu_beta, jtu_beta_upper, jtu_beta_lower)
  ) %>% 
  mutate(rho_estimate = cor.test(jtu_norm, jtu_beta, method = 'spearman')$estimate %>% signif(digits = 2),
         p_value = cor.test(jtu_norm, jtu_beta, method = 'spearman')$p.value %>% signif(digits = 2))


# study level
bt_study_coefs <- bind_cols(
  # new turnover
  BT_studyID_estimate %>% 
    filter(model=='Jtu_new_norm' & time_period=='ALL') %>% 
    mutate(jtu_norm = Estimate.cYEAR,
           jtu_norm_upper = upper_slope, 
           jtu_norm_lower = lower_slope) %>% 
    select(level, jtu_norm, jtu_norm_upper, jtu_norm_lower),
  BT_studyID_estimate %>% 
    filter(model=='Jtu_beta' & time_period=='ALL') %>% 
    mutate(jtu_beta = Estimate.cYEAR,
           jtu_beta_upper = upper_slope, 
           jtu_beta_lower = lower_slope) %>% 
    select(jtu_beta, jtu_beta_upper, jtu_beta_lower)
) %>% 
  mutate(rho_estimate = cor.test(jtu_norm, jtu_beta, method = 'spearman')$estimate %>% signif(digits = 2),
         p_value = cor.test(jtu_norm, jtu_beta, method = 'spearman')$p.value %>% signif(digits = 2))

bt_coefs <- bind_cols(
  # new turnover
  BT_taxa_estimate %>% 
    filter(model=='Jtu_new_norm' & time_period=='ALL') %>% 
    mutate(jtu_norm = Estimate.cYEAR,
           jtu_norm_upper = upper_slope, 
           jtu_norm_lower = lower_slope) %>% 
    select(level, jtu_norm, jtu_norm_upper, jtu_norm_lower),
  BT_taxa_estimate %>% 
    filter(model=='Jtu_beta' & time_period=='ALL') %>% 
    mutate(jtu_beta = Estimate.cYEAR,
           jtu_beta_upper = upper_slope, 
           jtu_beta_lower = lower_slope) %>% 
    select(jtu_beta, jtu_beta_upper, jtu_beta_lower)
) %>% 
  mutate(rho_estimate = cor.test(jtu_norm, jtu_beta, method = 'spearman')$estimate %>% signif(digits = 2),
         p_value = cor.test(jtu_norm, jtu_beta, method = 'spearman')$p.value %>% signif(digits = 2))

biome_coefs <- bind_cols(
  # new turnover
  BT_biome_estimate %>% 
    filter(model=='Jtu_new_norm' & time_period=='ALL') %>% 
    mutate(jtu_norm = Estimate.cYEAR,
           jtu_norm_upper = upper_slope, 
           jtu_norm_lower = lower_slope) %>% 
    select(Biome, jtu_norm, jtu_norm_upper, jtu_norm_lower),
  BT_biome_estimate %>% 
    filter(model=='Jtu_beta' & time_period=='ALL') %>% 
    mutate(jtu_beta = Estimate.cYEAR,
           jtu_beta_upper = upper_slope, 
           jtu_beta_lower = lower_slope) %>% 
    select(jtu_beta, jtu_beta_upper, jtu_beta_lower)
) %>% 
  mutate(rho_estimate = cor.test(jtu_norm, jtu_beta, method = 'spearman')$estimate %>% signif(digits = 2),
         p_value = cor.test(jtu_norm, jtu_beta, method = 'spearman')$p.value %>% signif(digits = 2))

global_coefs <- bind_cols(
  # new turnover
  BT_global_estimates %>% 
    filter(model=='Jtu_new_norm' & time_period=='ALL' & term=='Slope') %>% 
    mutate(jtu_norm = Estimate,
           jtu_norm_upper = upper, 
           jtu_norm_lower = lower) %>% 
    select(jtu_norm, jtu_norm_upper, jtu_norm_lower),
  BT_global_estimates %>% 
    filter(model=='Jtu_beta' & time_period=='ALL' & term=='Slope') %>% 
    mutate(jtu_beta = Estimate,
           jtu_beta_upper = upper, 
           jtu_beta_lower = lower) %>% 
    select(jtu_beta, jtu_beta_upper, jtu_beta_lower)
)

##------rarefyID
beta_vs_norm_cellLevel <- ggplot() +
  geom_point(data = bt_rid_coefs,
             aes(x = jtu_beta, y = jtu_norm),
             alpha = 0.2) +
  # geom_linerange(data = rid_coefs,
  #                aes(x = jtu_old, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
  #                size = 0.5) +
  # geom_errorbarh(data = rid_coefs,
  #                aes(y = jtu_norm, xmin = jtu_old_lower, xmax = jtu_old_upper),
  #                height = 0, size = 0.5) +
  # add the global estimates
  geom_point(data = global_coefs,
             aes(x = jtu_beta, y = jtu_norm), 
             size = 2,
             col = 'red') +
  geom_linerange(data = global_coefs,
                 aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
                 size = 0.5,
                 col = 'red') +
  geom_errorbarh(data = global_coefs,
                 aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
                 height = 0, 
                 size = 0.5,
                 col = 'red') +
  annotate("text", x = -Inf, y = Inf, hjust = -1, vjust = 2, 
           label = paste("paste(italic(rho) == " , unique(bt_rid_coefs$rho_estimate), ")"),
           parse = TRUE) +
  labs(x = 'Turnover (Beta error)',
       y = 'Turnover (Gaussian error)',
       tag = 'A',
       subtitle = expression(paste('Cell-level ', italic(beta)[Year], " coefficients"))) +
  # geom_abline(intercept = 0, slope = 1, lty =2) +
  # coord_equal() +
  theme_bw()

# biome-taxa-study level
beta_vs_norm_studyLevel <- ggplot() +
  geom_point(data = bt_study_coefs,
             aes(x = jtu_beta, y = jtu_norm),
             alpha = 0.2) +
  # geom_linerange(data = bt_study_coefs,
  #                aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
  #                size = 0.5,
  #                alpha = 0.1) +
  # geom_errorbarh(data = bt_study_coefs,
  #                aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
  #                height = 0, size = 0.5,
  #                alpha = 0.1) +
  # add the global estimates
  geom_point(data = global_coefs,
             aes(x = jtu_beta, y = jtu_norm), 
             size = 2,
             col = 'red') +
  geom_linerange(data = global_coefs,
                 aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
                 size = 0.5,
                 col = 'red') +
  geom_errorbarh(data = global_coefs,
                 aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
                 height = 0, 
                 size = 0.5,
                 col = 'red') +
  annotate("text", x = -Inf, y = Inf, hjust = -1, vjust = 2, 
           label = paste("paste(italic(rho) == " , unique(bt_study_coefs$rho_estimate), ")"),
           parse = TRUE) +
  # geom_text(data = bt_study_coefs %>% distinct(rho_estimate, .keep_all = T),
  #           aes(x = 0.08, y = 0.035, label = paste("paste(italic(rho) == " , rho_estimate, ")")),
  #           parse = TRUE) +
  labs(x = 'Turnover (Beta error)',
       y = 'Turnover (Gaussian error)',
       tag = 'B',
       subtitle = expression(paste('Study-level ', paste(italic(beta)[Year], ' coefficients')))) +
  # coord_equal() +
  theme_bw()

# biome-taxa level
beta_vs_norm_btLevel <- ggplot() +
  geom_point(data = bt_coefs,
             aes(x = jtu_beta, y = jtu_norm),
             alpha = 0.2) +
  # geom_linerange(data = bt_coefs,
  #                aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
  #                size = 0.5,
  #                alpha = 0.1) +
  # geom_errorbarh(data = bt_coefs,
  #                aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
  #                height = 0, size = 0.5,
  #                alpha = 0.1) +
  # add the global estimates
  geom_point(data = global_coefs,
             aes(x = jtu_beta, y = jtu_norm), 
             size = 2,
             col = 'red') +
  geom_linerange(data = global_coefs,
                 aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
                 size = 0.5,
                 col = 'red') +
  geom_errorbarh(data = global_coefs,
                 aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
                 height = 0, 
                 size = 0.5,
                 col = 'red') +
  annotate("text", x = -Inf, y = Inf, hjust = -1, vjust = 2, 
           label = paste("paste(italic(rho) == " , unique(bt_coefs$rho_estimate), ")"),
           parse = TRUE) +
  labs(x = 'Turnover (Beta error)',
       y = 'Turnover (Gaussian error)',
       tag = 'C',
       subtitle = expression(paste(italic(beta)[Year], ' coefficients for taxa within biomes'))) +
  # geom_abline(intercept = 0, slope = 1, lty =2) +
  # coord_fixed() +
  theme_bw()

beta_vs_norm_biomeLevel <- ggplot() +
  geom_point(data = biome_coefs,
             aes(x = jtu_beta, y = jtu_norm),
             alpha = 0.2) +
  # geom_linerange(data = biome_coefs,
  #                aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
  #                size = 0.5,
  #                alpha = 0.1) +
  # geom_errorbarh(data = biome_coefs,
  #                aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
  #                height = 0, size = 0.5,
  #                alpha = 0.1) +
  # add the global estimates
  geom_point(data = global_coefs,
             aes(x = jtu_beta, y = jtu_norm), 
             size = 2,
             col = 'red') +
  geom_linerange(data = global_coefs,
                 aes(x = jtu_beta, ymin = jtu_norm_lower, ymax = jtu_norm_upper),
                 size = 0.5,
                 col = 'red') +
  geom_errorbarh(data = global_coefs,
                 aes(y = jtu_norm, xmin = jtu_beta_lower, xmax = jtu_beta_upper),
                 height = 0, 
                 size = 0.5,
                 col = 'red') +
  annotate("text", x = -Inf, y = Inf, hjust = -1, vjust = 2, 
           label = paste("paste(italic(rho) == " , unique(biome_coefs$rho_estimate), ")"),
           parse = TRUE) +
  labs(x = 'Turnover (Beta error)',
       y = 'Turnover (Gaussian error)',
       subtitle = expression(paste('Biome-level ', italic(beta)[Year], ' coefficients')),
       tag = 'D') +
  # coord_fixed() +
  theme_bw()

cowplot::plot_grid(beta_vs_norm_cellLevel, beta_vs_norm_studyLevel,
                   beta_vs_norm_btLevel, beta_vs_norm_biomeLevel,
                   nrow = 2)

# ggsave('FigSx_gaussian_vs_beta_error.png', width = 250, height = 250, units = 'mm')
# ggsave('FigSx_gaussian_vs_beta_error.pdf', width = 250, height = 250, units = 'mm')
bt_rid_coefs %>% 
  filter(jtu_norm < 0.015)
