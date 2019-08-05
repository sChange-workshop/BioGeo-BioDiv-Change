##------get coefficients for rlm_clm_txa hierarchical models-----------------------
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(tidyverse)

##----load model fits data-------
# load('~/Desktop/revision_models/Jne_norm_RCTSRfyID-4950830.Rdata')
load('~/Desktop/revision_models/Jne_norm_RCTSRfyID_slice1_28div.Rdata')
load('~/Desktop/revision_models/Jne_norm_RCTSR_slice2_29div-4950838.Rdata')
load('~/Desktop/revision_models/Jne_norm_RCTSRfyID_slice3_34div-4950839.Rdata')

# load('~/Desktop/revision_models/Jtu_norm_RCTSRfyID_count_21div-4950828.Rdata')
load('~/Desktop/revision_models/Jtu_norm_RCTSRfyID_slice1-4951060.Rdata')
load('~/Desktop/revision_models/Jtu_norm_RCTSRfyID_slice2_1rhat.Rdata')
load('~/Desktop/revision_models/Jtu_norm_RCTSRfyID_slice3_1rhat.Rdata')

# load('~/Desktop/revision_models/S_pois_RCTSRfyID_count-4950831.Rdata')
load('~/Desktop/revision_models/S_pois_RCTSR_slice1_7div.Rdata')
load('~/Desktop/revision_models/S_pois_RCTSRfyID_slice2-4950841.Rdata')
load('~/Desktop/revision_models/S_pois_RCTSRfyID_slice3-4950842.Rdata')



##----------Jne_norm---------------------
Jne_norm_ALL_global <- fixef(Jne_norm_RCTSRfyID, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = 'ALL')

names(Jne_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_global <- Jne_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice1
Jne_norm_ALL_slice1 <- fixef(Jne_norm_RCTSRfyID_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1951-1970')

names(Jne_norm_ALL_slice1)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_slice1 <- Jne_norm_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice2
Jne_norm_ALL_slice2 <- fixef(Jne_norm_RCTSRfyID_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1971-1990')

names(Jne_norm_ALL_slice2)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_slice2 <- Jne_norm_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice3
Jne_norm_ALL_slice3 <- fixef(Jne_norm_RCTSRfyID_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1991-2010')

names(Jne_norm_ALL_slice3)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_slice3 <- Jne_norm_ALL_slice3 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
Jne_norm_ALL_groupCoefs <- coef(Jne_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_ALL_rct <- as_tibble(Jne_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_ALL_groupCoefs[[1]]))
names(Jne_norm_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_rct <- Jne_norm_ALL_rct %>%
  mutate(
    model = 'Jne_norm',
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
##  slice1
Jne_norm_slice1_groupCoefs <- coef(Jne_norm_RCTSRfyID_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_slice1_rct <- as_tibble(Jne_norm_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_slice1_groupCoefs[[1]]))
names(Jne_norm_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_rct <- Jne_norm_slice1_rct %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice1_studyID <- as_tibble(Jne_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice1_groupCoefs[[2]]))
names(Jne_norm_slice1_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_studyID <- Jne_norm_slice1_studyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice1_rarefyID <- as_tibble(Jne_norm_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_slice1_groupCoefs[[3]]))
names(Jne_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_rarefyID <- Jne_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 
##slice2
Jne_norm_slice2_groupCoefs <- coef(Jne_norm_RCTSRfyID_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_slice2_rct <- as_tibble(Jne_norm_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_slice2_groupCoefs[[1]]))
names(Jne_norm_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_rct <- Jne_norm_slice2_rct %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice2_studyID <- as_tibble(Jne_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice2_groupCoefs[[2]]))
names(Jne_norm_slice2_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_studyID <- Jne_norm_slice2_studyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice2_rarefyID <- as_tibble(Jne_norm_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_slice2_groupCoefs[[3]]))
names(Jne_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_rarefyID <- Jne_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice3
Jne_norm_slice3_groupCoefs <- coef(Jne_norm_RCTSRfyID_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_slice3_rct <- as_tibble(Jne_norm_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_slice3_groupCoefs[[1]]))
names(Jne_norm_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_rct <- Jne_norm_slice3_rct %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice3_studyID <- as_tibble(Jne_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice3_groupCoefs[[2]]))
names(Jne_norm_slice3_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_studyID <- Jne_norm_slice3_studyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice3_rarefyID <- as_tibble(Jne_norm_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_slice3_groupCoefs[[3]]))
names(Jne_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_rarefyID <- Jne_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jne_new_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 



##----------Jtu_norm---------------------
Jtu_norm_ALL_global <- fixef(Jtu_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = 'ALL')

names(Jtu_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_global <- Jtu_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice1
Jtu_norm_ALL_slice1 <- fixef(Jtu_norm_RCTSRfyID_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1951-1970')

names(Jtu_norm_ALL_slice1)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_slice1 <- Jtu_norm_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice2
Jtu_norm_ALL_slice2 <- fixef(Jtu_norm_RCTSRfyID_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1971-1990')

names(Jtu_norm_ALL_slice2)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_slice2 <- Jtu_norm_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice3
Jtu_norm_ALL_slice3 <- fixef(Jtu_norm_RCTSRfyID_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1991-2010')

names(Jtu_norm_ALL_slice3)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_slice3 <- Jtu_norm_ALL_slice3 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
Jtu_norm_ALL_groupCoefs <- coef(Jtu_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_ALL_rct <- as_tibble(Jtu_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_ALL_groupCoefs[[1]]))
names(Jtu_norm_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_rct <- Jtu_norm_ALL_rct %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_ALL_rarefyID <- as_tibble(Jtu_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_ALL_groupCoefs[[2]]))
names(Jtu_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_rarefyID <- Jtu_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice1
Jtu_norm_slice1_groupCoefs <- coef(Jtu_norm_RCTSRfyID_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_slice1_rct <- as_tibble(Jtu_norm_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_slice1_groupCoefs[[1]]))
names(Jtu_norm_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_rct <- Jtu_norm_slice1_rct %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice1_studyID <- as_tibble(Jtu_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice1_groupCoefs[[2]]))
names(Jtu_norm_slice1_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_studyID <- Jtu_norm_slice1_studyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice1_rarefyID <- as_tibble(Jtu_norm_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_slice1_groupCoefs[[3]]))
names(Jtu_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_rarefyID <- Jtu_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice2
Jtu_norm_slice2_groupCoefs <- coef(Jtu_norm_RCTSRfyID_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_slice2_rct <- as_tibble(Jtu_norm_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_slice2_groupCoefs[[1]]))
names(Jtu_norm_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_rct <- Jtu_norm_slice2_rct %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice2_studyID <- as_tibble(Jtu_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice2_groupCoefs[[2]]))
names(Jtu_norm_slice2_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_studyID <- Jtu_norm_slice2_studyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice2_rarefyID <- as_tibble(Jtu_norm_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_slice2_groupCoefs[[3]]))
names(Jtu_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_rarefyID <- Jtu_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice3
Jtu_norm_slice3_groupCoefs <- coef(Jtu_norm_RCTSRfyID_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_slice3_rct <- as_tibble(Jtu_norm_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_slice3_groupCoefs[[1]]))
names(Jtu_norm_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_rct <- Jtu_norm_slice3_rct %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice3_studyID <- as_tibble(Jtu_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice3_groupCoefs[[2]]))
names(Jtu_norm_slice3_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_studyID <- Jtu_norm_slice3_studyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice3_rarefyID <- as_tibble(Jtu_norm_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_slice3_groupCoefs[[3]]))
names(Jtu_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_rarefyID <- Jtu_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jtu_new_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


##-species richness---------
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

##  Jne_norm slice1
S_pois_ALL_slice1 <- fixef(S_pois_RCTSRfyID_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1951-1970')

names(S_pois_ALL_slice1)[3:4] <- c('lower', 'upper')

S_pois_ALL_slice1 <- S_pois_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice2
S_pois_ALL_slice2 <- fixef(S_pois_RCTSRfyID_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1971-1990')

names(S_pois_ALL_slice2)[3:4] <- c('lower', 'upper')

S_pois_ALL_slice2 <- S_pois_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice3
S_pois_ALL_slice3 <- fixef(S_pois_RCTSRfyID_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1991-2010')

names(S_pois_ALL_slice3)[3:4] <- c('lower', 'upper')

S_pois_ALL_slice3 <- S_pois_ALL_slice3 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 


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

# slice1
S_pois_new_slice1_groupCoefs <- coef(S_pois_RCTSRfyID_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_new_slice1_rct <- as_tibble(S_pois_new_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_new_slice1_groupCoefs[[1]]))
names(S_pois_new_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice1_rct <- S_pois_new_slice1_rct %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_slice1_studyID <- as_tibble(S_pois_new_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_new_slice1_groupCoefs[[2]]))
names(S_pois_new_slice1_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice1_studyID <- S_pois_new_slice1_studyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_slice1_rarefyID <- as_tibble(S_pois_new_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_new_slice1_groupCoefs[[3]]))
names(S_pois_new_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice1_rarefyID <- S_pois_new_slice1_rarefyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

# slice2
S_pois_new_slice2_groupCoefs <- coef(S_pois_RCTSRfyID_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_new_slice2_rct <- as_tibble(S_pois_new_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_new_slice2_groupCoefs[[1]]))
names(S_pois_new_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice2_rct <- S_pois_new_slice2_rct %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_slice2_studyID <- as_tibble(S_pois_new_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_new_slice2_groupCoefs[[2]]))
names(S_pois_new_slice2_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice2_studyID <- S_pois_new_slice2_studyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_slice2_rarefyID <- as_tibble(S_pois_new_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_new_slice2_groupCoefs[[3]]))
names(S_pois_new_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice2_rarefyID <- S_pois_new_slice2_rarefyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

# slice3
S_pois_new_slice3_groupCoefs <- coef(S_pois_RCTSRfyID_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_new_slice3_rct <- as_tibble(S_pois_new_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_new_slice3_groupCoefs[[1]]))
names(S_pois_new_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice3_rct <- S_pois_new_slice3_rct %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_slice3_studyID <- as_tibble(S_pois_new_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_new_slice3_groupCoefs[[2]]))
names(S_pois_new_slice3_studyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice3_studyID <- S_pois_new_slice3_studyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_new_slice3_rarefyID <- as_tibble(S_pois_new_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_new_slice3_groupCoefs[[3]]))
names(S_pois_new_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_new_slice3_rarefyID <- S_pois_new_slice3_rarefyID %>%
  mutate(
    model = 'S_pois_new',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

# put this time slices into the dataframe
load('~/Desktop/revision_models/rlm_clm_txa_modelCoefs.Rdata')
rlm_clm_txa_global_estimates <- bind_rows(rlm_clm_txa_global_estimates,
                                          Jne_norm_ALL_slice1,
                                          Jne_norm_ALL_slice2,
                                          Jne_norm_ALL_slice3,
                                          Jtu_norm_ALL_slice1,
                                          Jtu_norm_ALL_slice2,
                                          Jtu_norm_ALL_slice3,
                                          S_pois_ALL_slice1,
                                          S_pois_ALL_slice2,
                                          S_pois_ALL_slice3)

rlm_clm_txa_estimate <- bind_rows(rlm_clm_txa_estimate,
                                  Jne_norm_slice1_rct,
                                  Jne_norm_slice2_rct,
                                  Jne_norm_slice3_rct,
                                  Jtu_norm_slice1_rct,
                                  Jtu_norm_slice2_rct,
                                  Jtu_norm_slice3_rct,
                                  S_pois_new_slice1_rct,
                                  S_pois_new_slice2_rct,
                                  S_pois_new_slice3_rct)

rlm_clm_txa_rarefyID_estimate <- bind_rows(rlm_clm_txa_rarefyID_estimate,
                                           Jne_norm_slice1_rarefyID,
                                           Jne_norm_slice2_rarefyID,
                                           Jne_norm_slice3_rarefyID,
                                           Jtu_norm_slice1_rarefyID,
                                           Jtu_norm_slice2_rarefyID,
                                           Jtu_norm_slice3_rarefyID,
                                           S_pois_new_slice1_rarefyID,
                                           S_pois_new_slice2_rarefyID,
                                           S_pois_new_slice3_rarefyID)

rlm_clm_txa_studyID_estimate <- bind_rows(rlm_clm_txa_studyID_estimate,
                                          Jne_norm_slice1_studyID,
                                          Jne_norm_slice2_studyID,
                                          Jne_norm_slice3_studyID,
                                          Jtu_norm_slice1_studyID,
                                          Jtu_norm_slice2_studyID,
                                          Jtu_norm_slice3_studyID,
                                          S_pois_new_slice1_studyID,
                                          S_pois_new_slice2_studyID,
                                          S_pois_new_slice3_studyID)

save(rlm_clm_txa_global_estimates, rlm_clm_txa_estimate,
     rlm_clm_txa_studyID_estimate, rlm_clm_txa_rarefyID_estimate,
     file = '~/Desktop/revision_models/rlm_clm_txa_modelCoefs.Rdata')
