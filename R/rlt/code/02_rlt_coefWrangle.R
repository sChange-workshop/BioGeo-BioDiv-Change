# compare coefs of old rct model with new one...richness and turnover

rm(list=ls())

library(brms)
library(tidyverse)

load('~/Desktop/revision_models/rlm_clm_txa_modelCoefs.Rdata')

load('~/Desktop/revision_models/Jtu_norm_RCTSRfyID_count_21div-4950828.Rdata')
load('~/Desktop/revision_models/S_pois_RCTSRfyID_count-4950831.Rdata')
load('~/Desktop/revision_models/Jne_norm_RCTSRfyID-4950830.Rdata')

Jne_new_norm_ALL_global <- fixef(Jne_norm_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')

names(Jne_new_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jne_new_norm_ALL_global <- Jne_new_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral')),
    term = c('intercept', 'slope')) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
Jne_new_norm_ALL_groupCoefs <- coef(Jne_norm_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_new_norm_ALL_rct <- as_tibble(Jne_new_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_new_norm_ALL_groupCoefs[[1]]))
names(Jne_new_norm_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rct <- Jne_new_norm_ALL_rct %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_new_norm_ALL_studyID <- as_tibble(Jne_new_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[2]]))
names(Jne_new_norm_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_studyID <- Jne_new_norm_ALL_studyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_new_norm_ALL_rarefyID <- as_tibble(Jne_new_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[3]]))
names(Jne_new_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rarefyID <- Jne_new_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

# repeat for turnover
Jtu_new_norm_ALL_global <- fixef(Jtu_norm_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

names(Jtu_new_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jtu_new_norm_ALL_global <- Jtu_new_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral')),
    term = c('intercept', 'slope')) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
Jtu_new_norm_ALL_groupCoefs <- coef(Jtu_norm_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_new_norm_ALL_rct <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_new_norm_ALL_groupCoefs[[1]]))
names(Jtu_new_norm_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_rct <- Jtu_new_norm_ALL_rct %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_new_norm_ALL_studyID <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[2]]))
names(Jtu_new_norm_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_studyID <- Jtu_new_norm_ALL_studyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_new_norm_ALL_rarefyID <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[3]]))
names(Jtu_new_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_rarefyID <- Jtu_new_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

## get the coefficients of the richness model
S_pois_new_ALL_global <- fixef(S_pois_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL')

names(S_pois_new_ALL_global)[3:4] <- c('lower', 'upper')

S_pois_new_ALL_global <- S_pois_new_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral')),
    term = c('intercept', 'slope')) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
S_pois_new_ALL_groupCoefs <- coef(S_pois_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_new_ALL_rct <- as_tibble(S_pois_new_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_new_ALL_groupCoefs[[1]]))
names(S_pois_new_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rct <- S_pois_new_ALL_rct %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_ALL_studyID <- as_tibble(S_pois_new_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[2]]))
names(S_pois_new_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_studyID <- S_pois_new_ALL_studyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_ALL_rarefyID <- as_tibble(S_pois_new_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[3]]))
names(S_pois_new_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rarefyID <- S_pois_new_ALL_rarefyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 



## check the 21 divergent transitions 
jtu_post <- as.array(Jtu_norm_RCTSRfyID$fit)
jtu_nuts <- nuts_params(Jtu_norm_RCTSRfyID)

# these types of plots can help diagnose whether divergent transitions are problematic
library(bayesplot)
color_scheme_set("darkgray")

mcmc_parcoord(jtu_post, pars = c("b_cYEAR", "sd_rlm_clm_txa__cYEAR",
                               "sd_rlm_clm_txa:STUDY_ID__cYEAR",
                               "sd_rlm_clm_txa:STUDY_ID:rarefyID__cYEAR"),
              np = jtu_nuts)

bayesplot::mcmc_pairs(jtu_post,
                      pars = c('b_cYEAR', 'sd_rlm_clm_txa__cYEAR', 'sd_rlm_clm_txa:STUDY_ID__cYEAR',
                               'sd_rlm_clm_txa:STUDY_ID:rarefyID__cYEAR'),
                      # transformations = list('sd_Biome__cYEAR' = 'log',
                      #                        'sd_Biome:taxa_mod__cYEAR' = 'log',
                      #                        'sd_Biome:taxa_mod:STUDY_ID__cYEAR' = 'log'),
                      off_diag_args = list('alpha' = 0.5),
                      condition = pairs_condition(nuts = 'energy__'),
                      lp = log_posterior(Jtu_norm_RCTSRfyID$fit),
                      np = jtu_nuts,
                      np_style = pairs_style_np(div_color = 'firebrick',
                                                td_color = 'blue',
                                                td_size = 2))



# add the new jtu coefs to the existing dataframe
load('~/Desktop/revision_models/rlm_clm_txa_modelCoefs.Rdata')
rlm_clm_txa_global_estimates <- bind_rows(rlm_clm_txa_global_estimates,
                                          Jne_new_norm_ALL_global)

rlm_clm_txa_estimate <- bind_rows(rlm_clm_txa_estimate,
                                   Jne_new_norm_ALL_rct)

rlm_clm_txa_rarefyID_estimate <- bind_rows(rlm_clm_txa_rarefyID_estimate,
                                           Jne_new_norm_ALL_rarefyID)

rlm_clm_txa_studyID_estimate <- bind_rows(rlm_clm_txa_studyID_estimate,
                                          Jne_new_norm_ALL_studyID)

save(rlm_clm_txa_global_estimates, rlm_clm_txa_estimate,
     rlm_clm_txa_studyID_estimate, rlm_clm_txa_rarefyID_estimate,
  file = '~/Desktop/revision_models/rlm_clm_txa_modelCoefs.Rdata')
