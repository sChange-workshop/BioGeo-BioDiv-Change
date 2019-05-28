## get model coefs new nestedness and turnover model fit to count data
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(dplyr)
#library(broom)
library(tidyr)
library(tibble)

#----load data
##----count data only, whole time period--------
load('~/Desktop/revision_models/Jtu_norm_BTSRfyID_count.Rdata')
load('~/Desktop/revision_models/Jne_norm_BTSRfyID_count-4950633.Rdata')
load('~/Desktop/revision_models/S_pois_BTSRfyID_count-5050747.Rdata')


##--species richness coefficients and random effects---------
S_pois_new_ALL_global <- fixef(S_pois_BTSRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL')

names(S_pois_new_ALL_global)[3:4] <- c('lower', 'upper')

S_pois_new_ALL_global <- S_pois_new_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 



S_pois_new_ALL_groupCoefs <- coef(S_pois_BTSRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 

##  break into biome and rarefyID estimates
S_pois_ALL_biome <- as_tibble(S_pois_new_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(S_pois_new_ALL_groupCoefs[[1]]))
names(S_pois_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_newALL_biome <- S_pois_ALL_biome %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_ALL_taxa <- as_tibble(S_pois_new_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[2]]))
names(S_pois_new_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_taxa <- S_pois_new_ALL_taxa %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_ALL_studyID <- as_tibble(S_pois_new_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[3]]))
names(S_pois_new_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_studyID <- S_pois_new_ALL_studyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

S_pois_new_ALL_rarefyID <- as_tibble(S_pois_new_ALL_groupCoefs[[4]]) %>%
  mutate(level = rownames(S_pois_new_ALL_groupCoefs[[4]]))
names(S_pois_new_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rarefyID <- S_pois_new_ALL_rarefyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#-----random effects for map---------
S_pois_new_ALL_ranef <- ranef(S_pois_BTSRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
S_pois_new_ALL_biome_ranef <- as_tibble(S_pois_new_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(S_pois_new_ALL_ranef$Biome))
names(S_pois_new_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_biome_ranef <- S_pois_new_ALL_biome_ranef %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL')

S_pois_new_ALL_Taxa_ranef <- as_tibble(S_pois_new_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(S_pois_new_ALL_ranef$`Biome:taxa_mod`))
names(S_pois_new_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_Taxa_ranef <- S_pois_new_ALL_Taxa_ranef %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL')

S_pois_new_ALL_studyID_ranef <- as_tibble(S_pois_new_ALL_ranef$`Biome:taxa_mod:STUDY_ID`[,,'cYEAR']) %>%
  mutate(rarefyID = rownames(S_pois_new_ALL_ranef$`Biome:taxa_mod:STUDY_ID`))
names(S_pois_new_ALL_studyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_studyID_ranef <- S_pois_new_ALL_studyID_ranef %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL')

S_pois_new_ALL_rarefyID_ranef <- as_tibble(S_pois_new_ALL_ranef$`Biome:taxa_mod:STUDY_ID:rarefyID`[,,'cYEAR']) %>%
  mutate(rarefyID = rownames(S_pois_new_ALL_ranef$`Biome:taxa_mod:STUDY_ID:rarefyID`))
names(S_pois_new_ALL_rarefyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_new_ALL_rarefyID_ranef <- S_pois_new_ALL_rarefyID_ranef %>%
  mutate(
    model = 'S_pois_new',
    time_period = 'ALL')

##--turnover coefficients and random effect-------------
Jtu_new_norm_ALL_global <- fixef(Jtu_norm_BTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

names(Jtu_new_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jtu_new_norm_ALL_global <- Jtu_new_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

Jtu_new_norm_ALL_groupCoefs <- coef(Jtu_norm_BTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 

##  break into biome and rarefyID estimates
Jtu_norm_new_biome <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jtu_new_norm_ALL_groupCoefs[[1]]))
names(Jtu_norm_new_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_new_biome <- Jtu_norm_new_biome %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


Jtu_new_norm_ALL_taxa <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[2]]))
names(Jtu_new_norm_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_taxa <- Jtu_new_norm_ALL_taxa %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


Jtu_new_norm_ALL_studyID <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[3]]))
names(Jtu_new_norm_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_studyID <- Jtu_new_norm_ALL_studyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


Jtu_new_norm_ALL_rarefyID <- as_tibble(Jtu_new_norm_ALL_groupCoefs[[4]]) %>%
  mutate(level = rownames(Jtu_new_norm_ALL_groupCoefs[[4]]))
names(Jtu_new_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_rarefyID <- Jtu_new_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#-----random effects for map---------
Jtu_new_norm_ALL_ranef <- ranef(Jtu_norm_BTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
Jtu_new_norm_ALL_biome_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(Jtu_new_norm_ALL_ranef$Biome))
names(Jtu_new_norm_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_biome_ranef <- Jtu_new_norm_ALL_biome_ranef %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

Jtu_new_norm_ALL_Taxa_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(Jtu_new_norm_ALL_ranef$`Biome:taxa_mod`))
names(Jtu_new_norm_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_Taxa_ranef <- Jtu_new_norm_ALL_Taxa_ranef %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

Jtu_new_norm_ALL_studyID_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID`[,,'cYEAR']) %>%
  mutate(rarefyID = rownames(Jtu_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID`))
names(Jtu_new_norm_ALL_studyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_studyID_ranef <- Jtu_new_norm_ALL_studyID_ranef %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

Jtu_new_norm_ALL_rarefyID_ranef <- as_tibble(Jtu_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID:rarefyID`[,,'cYEAR']) %>%
  mutate(rarefyID = rownames(Jtu_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID:rarefyID`))
names(Jtu_new_norm_ALL_rarefyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_new_norm_ALL_rarefyID_ranef <- Jtu_new_norm_ALL_rarefyID_ranef %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

# repeat for nestedness
Jne_new_norm_ALL_global <- fixef(Jne_norm_BTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')

names(Jne_new_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jne_new_norm_ALL_global <- Jne_new_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

Jne_new_norm_ALL_groupCoefs <- coef(Jne_norm_BTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 

##  break into biome and rarefyID estimates
Jne_norm_new_biome <- as_tibble(Jne_new_norm_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jne_new_norm_ALL_groupCoefs[[1]]))
names(Jne_norm_new_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_new_biome <- Jne_norm_new_biome %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


Jne_new_norm_ALL_taxa <- as_tibble(Jne_new_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[2]]))
names(Jne_new_norm_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_taxa <- Jne_new_norm_ALL_taxa %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


Jne_new_norm_ALL_studyID <- as_tibble(Jne_new_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[3]]))
names(Jne_new_norm_ALL_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_studyID <- Jne_new_norm_ALL_studyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


Jne_new_norm_ALL_rarefyID <- as_tibble(Jne_new_norm_ALL_groupCoefs[[4]]) %>%
  mutate(level = rownames(Jne_new_norm_ALL_groupCoefs[[4]]))
names(Jne_new_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rarefyID <- Jne_new_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#-----random effects for map---------
Jne_new_norm_ALL_ranef <- ranef(Jne_norm_BTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
Jne_new_norm_ALL_biome_ranef <- as_tibble(Jne_new_norm_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(Jne_new_norm_ALL_ranef$Biome))
names(Jne_new_norm_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_biome_ranef <- Jne_new_norm_ALL_biome_ranef %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')

Jne_new_norm_ALL_Taxa_ranef <- as_tibble(Jne_new_norm_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(Jne_new_norm_ALL_ranef$`Biome:taxa_mod`))
names(Jne_new_norm_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_Taxa_ranef <- Jne_new_norm_ALL_Taxa_ranef %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')

Jne_new_norm_ALL_studyID_ranef <- as_tibble(Jne_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID`[,,'cYEAR']) %>%
  mutate(rarefyID = rownames(Jne_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID`))
names(Jne_new_norm_ALL_studyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_studyID_ranef <- Jne_new_norm_ALL_studyID_ranef %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')

Jne_new_norm_ALL_rarefyID_ranef <- as_tibble(Jne_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID:rarefyID`[,,'cYEAR']) %>%
  mutate(rarefyID = rownames(Jne_new_norm_ALL_ranef$`Biome:taxa_mod:STUDY_ID:rarefyID`))
names(Jne_new_norm_ALL_rarefyID_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_new_norm_ALL_rarefyID_ranef <- Jne_new_norm_ALL_rarefyID_ranef %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')


# put 'em together and save
load('~/Dropbox/1current/BioTime/local_code/hierarchical/6results/bt/model_coefs_ranefs/BTRfyID_coef_ranef_inclNewModel.Rdata')

BT_global_estimates <- bind_rows(S_pois_new_ALL_global,
                                 Jtu_new_norm_ALL_global,
                                 Jne_new_norm_ALL_global)

BT_biome_estimate <- bind_rows(S_pois_ALL_biome,
                               Jtu_norm_new_biome,
                               Jne_norm_new_biome)

BT_biome_ranef <- bind_rows(S_pois_new_ALL_biome_ranef,
                            Jtu_new_norm_ALL_biome_ranef,
                            Jne_new_norm_ALL_biome_ranef)

BT_taxa_estimate <- bind_rows(S_pois_new_ALL_taxa, 
                              Jtu_new_norm_ALL_taxa,
                              Jne_new_norm_ALL_taxa)

BT_Taxa_ranef <- bind_rows(S_pois_new_ALL_Taxa_ranef,
                           Jtu_new_norm_ALL_Taxa_ranef,
                           Jne_new_norm_ALL_Taxa_ranef)

BT_studyID_estimate <- bind_rows(S_pois_new_ALL_studyID,
                                 Jtu_new_norm_ALL_studyID,
                                 Jne_new_norm_ALL_studyID)

BT_STUDYID_ranef <- bind_rows(S_pois_new_ALL_studyID_ranef,
                              Jtu_new_norm_ALL_studyID_ranef,
                              Jne_new_norm_ALL_studyID_ranef)

BTRFYID_rarefyID_coef <- bind_rows(S_pois_new_ALL_rarefyID,
                                   Jtu_new_norm_ALL_rarefyID,
                                   Jne_new_norm_ALL_rarefyID)

BT_rarefyID_ranef <- bind_rows(S_pois_new_ALL_rarefyID_ranef,
                               Jtu_new_norm_ALL_rarefyID_ranef,
                               Jne_new_norm_ALL_rarefyID_ranef)

save(BT_global_estimates, 
     BT_biome_estimate, BT_biome_ranef,
     BT_taxa_estimate, BT_Taxa_ranef,
     BT_studyID_estimate, BT_STUDYID_ranef,
     BTRfyID_rarefyID_coef, BT_rarefyID_ranef,
     file = '~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BT_model_coef_ranef.Rdata')

