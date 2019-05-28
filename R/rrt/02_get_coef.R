## modify code for new region model that included realm: realm-region-taxa
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(dplyr)
#library(broom)
library(tidyr)
library(tibble)

#----load data
##----count data only, whole time period--------
load('~/Desktop/revision_models/rrt_models/S_pois_rlm_reg_txa-5211694.Rdata')
##--species richness coefficients and random effects---------
S_pois_new_ALL_global <- fixef(S_pois_RTSRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL')

names(S_pois_new_ALL_global)[3:4] <- c('lower', 'upper')

S_pois_new_ALL_global <- S_pois_new_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 



S_pois_new_ALL_groupCoefs <- coef(S_pois_RTSRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 

##  break into rrt and rarefyID estimates
S_pois_ALL_rrt <- as_tibble(S_pois_new_ALL_groupCoefs[[1]]) %>%
  mutate(rrt = rownames(S_pois_new_ALL_groupCoefs[[1]]))
names(S_pois_ALL_rrt)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_rrt <- S_pois_ALL_rrt %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_ALL_studyID <- as_tibble(S_pois_new_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[2]]))
names(S_pois_new_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_studyID <- S_pois_new_ALL_studyID %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

S_pois_new_ALL_rarefyID <- as_tibble(S_pois_new_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[3]]))
names(S_pois_new_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rarefyID <- S_pois_new_ALL_rarefyID %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#-----random effects for map---------
S_pois_new_ALL_ranef <- ranef(S_pois_RTSRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
# separate rrt and taxa slope random effects
S_pois_new_ALL_rrt_ranef <- as_tibble(S_pois_new_ALL_ranef$rlm_region_taxa[,,'cYEAR']) %>%
  mutate(rrt = rownames(S_pois_new_ALL_ranef$rlm_region_taxa))
names(S_pois_new_ALL_rrt_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rrt_ranef <- S_pois_new_ALL_rrt_ranef %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL')

S_pois_new_ALL_studyID_ranef <- as_tibble(S_pois_new_ALL_ranef$`rlm_region_taxa:STUDY_ID`[,,'cYEAR']) %>%
  mutate(level = rownames(S_pois_new_ALL_ranef$`rlm_region_taxa:STUDY_ID`))
names(S_pois_new_ALL_studyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_studyID_ranef <- S_pois_new_ALL_studyID_ranef %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL')

S_pois_new_ALL_rarefyID_ranef <- as_tibble(S_pois_new_ALL_ranef$`rlm_region_taxa:STUDY_ID:rarefyID`[,,'cYEAR']) %>%
  mutate(level = rownames(S_pois_new_ALL_ranef$`rlm_region_taxa:STUDY_ID:rarefyID`))
names(S_pois_new_ALL_rarefyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rarefyID_ranef <- S_pois_new_ALL_rarefyID_ranef %>%
  mutate(
    model = 'S_rlm_reg_txa',
    time_period = 'ALL')

# make some space, and load the turnover model
rm(S_pois_RTSRfyID_countData)
load('~/Desktop/revision_models/rrt_models/jtu_norm_rlm_reg_txa-5211695.Rdata')
##--turnover coefficients and random effect-------------
Jtu_new_norm_ALL_global <- fixef(Jtu_norm_CTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL')

names(Jtu_new_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jtu_new_norm_ALL_global <- Jtu_new_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

Jtu_new_norm_ALL_groupCoefs <- coef(Jtu_norm_CTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 

##  break into rrt and rarefyID estimates
Jtu_norm_new_rrt <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rrt = rownames(Jtu_new_norm_ALL_groupCoefs[[1]]))
names(Jtu_norm_new_rrt)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_new_rrt <- Jtu_norm_new_rrt %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


Jtu_new_norm_ALL_studyID <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[2]]))
names(Jtu_new_norm_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_studyID <- Jtu_new_norm_ALL_studyID %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


Jtu_new_norm_ALL_rarefyID <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[3]]))
names(Jtu_new_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_rarefyID <- Jtu_new_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#-----random effects for map---------
Jtu_new_norm_ALL_ranef <- ranef(Jtu_norm_CTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
# separate rrt and taxa slope random effects
Jtu_norm_new_rrt_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$rlm_region_taxa[,,'cYEAR']) %>%
  mutate(rrt = rownames(Jtu_new_norm_ALL_ranef$rlm_region_taxa))
names(Jtu_norm_new_rrt_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_norm_new_rrt_ranef <- Jtu_norm_new_rrt_ranef %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL')

Jtu_new_norm_ALL_studyID_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID`[,,'cYEAR']) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID`))
names(Jtu_new_norm_ALL_studyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_studyID_ranef <- Jtu_new_norm_ALL_studyID_ranef %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL')

Jtu_new_norm_ALL_rarefyID_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID:rarefyID`[,,'cYEAR']) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID:rarefyID`))
names(Jtu_new_norm_ALL_rarefyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_rarefyID_ranef <- Jtu_new_norm_ALL_rarefyID_ranef %>%
  mutate(
    model = 'Jtu_rlm_reg_txa',
    time_period = 'ALL')

rm(Jtu_norm_CTSRfyID)
load('~/Desktop/revision_models/rrt_models/jne_norm_rlm_reg_txa-5211696.Rdata')
# repeat for nestedness
Jne_new_norm_ALL_global <- fixef(Jne_norm_CTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL')

names(Jne_new_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jne_new_norm_ALL_global <- Jne_new_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

Jne_new_norm_ALL_groupCoefs <- coef(Jne_norm_CTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 

##  break into rrt and rarefyID estimates
Jne_norm_new_rrt <- as_tibble(Jne_new_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rrt = rownames(Jne_new_norm_ALL_groupCoefs[[1]]))
names(Jne_norm_new_rrt)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_new_rrt <- Jne_norm_new_rrt %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


Jne_new_norm_ALL_studyID <- as_tibble(Jne_new_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[2]]))
names(Jne_new_norm_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_studyID <- Jne_new_norm_ALL_studyID %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


Jne_new_norm_ALL_rarefyID <- as_tibble(Jne_new_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[3]]))
names(Jne_new_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rarefyID <- Jne_new_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#-----random effects for map---------
Jne_new_norm_ALL_ranef <- ranef(Jne_norm_CTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
# separate rrt and taxa slope random effects
Jne_new_norm_ALL_rrt_ranef <- as_tibble(Jne_new_norm_ALL_ranef$rlm_region_taxa[,,'cYEAR']) %>%
  mutate(rrt = rownames(Jne_new_norm_ALL_ranef$rlm_region_taxa))
names(Jne_new_norm_ALL_rrt_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rrt_ranef <- Jne_new_norm_ALL_rrt_ranef %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL')

Jne_new_norm_ALL_studyID_ranef <- as_tibble(Jne_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID`[,,'cYEAR']) %>%
  mutate(level = rownames(Jne_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID`))
names(Jne_new_norm_ALL_studyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_studyID_ranef <- Jne_new_norm_ALL_studyID_ranef %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL')

Jne_new_norm_ALL_rarefyID_ranef <- as_tibble(Jne_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID:rarefyID`[,,'cYEAR']) %>%
  mutate(level = rownames(Jne_new_norm_ALL_ranef$`rlm_region_taxa:STUDY_ID:rarefyID`))
names(Jne_new_norm_ALL_rarefyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rarefyID_ranef <- Jne_new_norm_ALL_rarefyID_ranef %>%
  mutate(
    model = 'Jne_rlm_reg_txa',
    time_period = 'ALL')


rrt_global_estimates <- bind_rows(S_pois_new_ALL_global,
                                 Jtu_new_norm_ALL_global,
                                 Jne_new_norm_ALL_global) %>% 
  mutate(term = rep(c('Intercept', 'Slope'), times = 3))

rrt_rrt_estimate <- bind_rows(S_pois_ALL_rrt,
                                   Jtu_norm_new_rrt,
                                   Jne_norm_new_rrt)

rrt_studyID_estimate <- bind_rows(S_pois_new_ALL_studyID,
                                 Jtu_new_norm_ALL_studyID,
                                 Jne_new_norm_ALL_studyID)

rrt_rarefyID_coef <- bind_rows(S_pois_new_ALL_rarefyID,
                              Jtu_new_norm_ALL_rarefyID,
                              Jne_new_norm_ALL_rarefyID)

rrt_rrt_ranef <- bind_rows(S_pois_new_ALL_rrt_ranef,
                                Jtu_norm_new_rrt_ranef,
                                Jne_new_norm_ALL_rrt_ranef)

rrt_studyID_ranef <- bind_rows(S_pois_new_ALL_studyID_ranef,
                              Jtu_new_norm_ALL_studyID_ranef,
                              Jne_new_norm_ALL_studyID_ranef)

rrt_rarefyID_ranef <- bind_rows(S_pois_new_ALL_rarefyID_ranef,
                               Jtu_new_norm_ALL_rarefyID_ranef,
                               Jne_new_norm_ALL_rarefyID_ranef)

save(rrt_global_estimates, 
     rrt_rrt_estimate, rrt_studyID_estimate, rrt_rarefyID_coef,
     rrt_rrt_ranef, rrt_studyID_ranef, rrt_rarefyID_ranef,
     file = '~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/rrt/model_coef_ranef/rlm_reg_txa_coef_ranef.Rdata')

