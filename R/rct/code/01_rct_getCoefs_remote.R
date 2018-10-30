##------get coefficients for rlm_clm_txa hierarchical models-----------------------
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(dplyr)
# library(broom)
library(tidyr)
library(tibble)

##----load model fits data-------
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jbeta_norm_RCTRfyID_count-1486688.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jne_norm_RCTRfyID_count-1486948.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jtu_norm_RCTRfyID_count-1487091.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/S_pois_RCTRfyID_count-4436001.Rdata')

##---time slices--------
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice1-1490016.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice2-4432581.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice3-4422158.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice1-1492370.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice2-3989799.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice3-4422159.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice1-2315138.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice2-3989800.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice3-4422160.Rdata')


load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice1-3989803.Rdata')
# file misnamed in script; correct
S_pois_RCTRfyID_countData_slice1 <- S_pois_RCTRfyID_countData_slice2;rm(S_pois_RCTRfyID_countData_slice2)
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice2-4487709.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice3-4487703.Rdata')


##----------Jbeta_norm---------------------
Jbeta_norm_ALL_global <- fixef(Jbeta_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL')

names(Jbeta_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jbeta_norm_ALL_global <- Jbeta_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jbeta_norm slice1
Jbeta_norm_ALL_slice1 <- fixef(Jbeta_norm_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970')

names(Jbeta_norm_ALL_slice1)[3:4] <- c('lower', 'upper')

Jbeta_norm_ALL_slice1 <- Jbeta_norm_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jbeta_norm slice2
Jbeta_norm_ALL_slice2 <- fixef(Jbeta_norm_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990')

names(Jbeta_norm_ALL_slice2)[3:4] <- c('lower', 'upper')

Jbeta_norm_ALL_slice2 <- Jbeta_norm_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jbeta_norm slice3
Jbeta_norm_ALL_slice3 <- fixef(Jbeta_norm_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010')

names(Jbeta_norm_ALL_slice3)[3:4] <- c('lower', 'upper')

Jbeta_norm_ALL_slice3 <- Jbeta_norm_ALL_slice3 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
Jbeta_norm_ALL_groupCoefs <- coef(Jbeta_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jbeta_norm_ALL_rct <- as_tibble(Jbeta_norm_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jbeta_norm_ALL_groupCoefs[[1]]))
names(Jbeta_norm_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_rct <- Jbeta_norm_ALL_rct %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_ALL_rarefyID <- as_tibble(Jbeta_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_ALL_groupCoefs[[2]]))
names(Jbeta_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_rarefyID <- Jbeta_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice1
Jbeta_norm_slice1_groupCoefs <- coef(Jbeta_norm_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jbeta_norm_slice1_rct <- as_tibble(Jbeta_norm_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jbeta_norm_slice1_groupCoefs[[1]]))
names(Jbeta_norm_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice1_rct <- Jbeta_norm_slice1_rct %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_slice1_rarefyID <- as_tibble(Jbeta_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_slice1_groupCoefs[[2]]))
names(Jbeta_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice1_rarefyID <- Jbeta_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##slice2
Jbeta_norm_slice2_groupCoefs <- coef(Jbeta_norm_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jbeta_norm_slice2_rct <- as_tibble(Jbeta_norm_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jbeta_norm_slice2_groupCoefs[[1]]))
names(Jbeta_norm_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice2_rct <- Jbeta_norm_slice2_rct %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_slice2_rarefyID <- as_tibble(Jbeta_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_slice2_groupCoefs[[2]]))
names(Jbeta_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice2_rarefyID <- Jbeta_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice3
Jbeta_norm_slice3_groupCoefs <- coef(Jbeta_norm_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jbeta_norm_slice3_rct <- as_tibble(Jbeta_norm_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jbeta_norm_slice3_groupCoefs[[1]]))
names(Jbeta_norm_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice3_rct <- Jbeta_norm_slice3_rct %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_slice3_rarefyID <- as_tibble(Jbeta_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_slice3_groupCoefs[[2]]))
names(Jbeta_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice3_rarefyID <- Jbeta_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 



##----------Jne_norm---------------------
Jne_norm_ALL_global <- fixef(Jne_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL')

names(Jne_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_global <- Jne_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice1
Jne_norm_ALL_slice1 <- fixef(Jne_norm_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970')

names(Jne_norm_ALL_slice1)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_slice1 <- Jne_norm_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice2
Jne_norm_ALL_slice2 <- fixef(Jne_norm_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990')

names(Jne_norm_ALL_slice2)[3:4] <- c('lower', 'upper')

Jne_norm_ALL_slice2 <- Jne_norm_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice3
Jne_norm_ALL_slice3 <- fixef(Jne_norm_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
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

Jne_norm_ALL_rarefyID <- as_tibble(Jne_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_ALL_groupCoefs[[2]]))
names(Jne_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_rarefyID <- Jne_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice1
Jne_norm_slice1_groupCoefs <- coef(Jne_norm_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_slice1_rct <- as_tibble(Jne_norm_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_slice1_groupCoefs[[1]]))
names(Jne_norm_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_rct <- Jne_norm_slice1_rct %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice1_rarefyID <- as_tibble(Jne_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice1_groupCoefs[[2]]))
names(Jne_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_rarefyID <- Jne_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##slice2
Jne_norm_slice2_groupCoefs <- coef(Jne_norm_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_slice2_rct <- as_tibble(Jne_norm_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_slice2_groupCoefs[[1]]))
names(Jne_norm_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_rct <- Jne_norm_slice2_rct %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice2_rarefyID <- as_tibble(Jne_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice2_groupCoefs[[2]]))
names(Jne_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_rarefyID <- Jne_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice3
Jne_norm_slice3_groupCoefs <- coef(Jne_norm_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jne_norm_slice3_rct <- as_tibble(Jne_norm_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jne_norm_slice3_groupCoefs[[1]]))
names(Jne_norm_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_rct <- Jne_norm_slice3_rct %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice3_rarefyID <- as_tibble(Jne_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice3_groupCoefs[[2]]))
names(Jne_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_rarefyID <- Jne_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


##----------Jtu_norm---------------------
Jtu_norm_ALL_global <- fixef(Jtu_norm_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL')

names(Jtu_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_global <- Jtu_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice1
Jtu_norm_ALL_slice1 <- fixef(Jtu_norm_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970')

names(Jtu_norm_ALL_slice1)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_slice1 <- Jtu_norm_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice2
Jtu_norm_ALL_slice2 <- fixef(Jtu_norm_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990')

names(Jtu_norm_ALL_slice2)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_slice2 <- Jtu_norm_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice3
Jtu_norm_ALL_slice3 <- fixef(Jtu_norm_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
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
Jtu_norm_slice1_groupCoefs <- coef(Jtu_norm_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_slice1_rct <- as_tibble(Jtu_norm_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_slice1_groupCoefs[[1]]))
names(Jtu_norm_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_rct <- Jtu_norm_slice1_rct %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice1_rarefyID <- as_tibble(Jtu_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice1_groupCoefs[[2]]))
names(Jtu_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_rarefyID <- Jtu_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##slice2
Jtu_norm_slice2_groupCoefs <- coef(Jtu_norm_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_slice2_rct <- as_tibble(Jtu_norm_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_slice2_groupCoefs[[1]]))
names(Jtu_norm_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_rct <- Jtu_norm_slice2_rct %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice2_rarefyID <- as_tibble(Jtu_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice2_groupCoefs[[2]]))
names(Jtu_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_rarefyID <- Jtu_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice3
Jtu_norm_slice3_groupCoefs <- coef(Jtu_norm_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
Jtu_norm_slice3_rct <- as_tibble(Jtu_norm_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(Jtu_norm_slice3_groupCoefs[[1]]))
names(Jtu_norm_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_rct <- Jtu_norm_slice3_rct %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice3_rarefyID <- as_tibble(Jtu_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice3_groupCoefs[[2]]))
names(Jtu_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_rarefyID <- Jtu_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 


##----------S_pois---------------------
S_pois_ALL_global <- fixef(S_pois_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL')

names(S_pois_ALL_global)[3:4] <- c('lower', 'upper')

S_pois_ALL_global <- S_pois_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice1
S_pois_ALL_slice1 <- fixef(S_pois_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970')

names(S_pois_ALL_slice1)[3:4] <- c('lower', 'upper')

S_pois_ALL_slice1 <- S_pois_ALL_slice1 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice2
S_pois_ALL_slice2 <- fixef(S_pois_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990')

names(S_pois_ALL_slice2)[3:4] <- c('lower', 'upper')

S_pois_ALL_slice2 <- S_pois_ALL_slice2 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice3
S_pois_ALL_slice3 <- fixef(S_pois_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010')

names(S_pois_ALL_slice3)[3:4] <- c('lower', 'upper')

S_pois_ALL_slice3 <- S_pois_ALL_slice3 %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##--------------rlm_clm_txa (rct) (and rarefyID) estimates
S_pois_ALL_groupCoefs <- coef(S_pois_RCTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_ALL_rct <- as_tibble(S_pois_ALL_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_ALL_groupCoefs[[1]]))
names(S_pois_ALL_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_rct <- S_pois_ALL_rct %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_ALL_rarefyID <- as_tibble(S_pois_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_ALL_groupCoefs[[2]]))
names(S_pois_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_rarefyID <- S_pois_ALL_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice1
S_pois_slice1_groupCoefs <- coef(S_pois_RCTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_slice1_rct <- as_tibble(S_pois_slice1_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_slice1_groupCoefs[[1]]))
names(S_pois_slice1_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice1_rct <- S_pois_slice1_rct %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_slice1_rarefyID <- as_tibble(S_pois_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_slice1_groupCoefs[[2]]))
names(S_pois_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice1_rarefyID <- S_pois_slice1_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##slice2
S_pois_slice2_groupCoefs <- coef(S_pois_RCTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_slice2_rct <- as_tibble(S_pois_slice2_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_slice2_groupCoefs[[1]]))
names(S_pois_slice2_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice2_rct <- S_pois_slice2_rct %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_slice2_rarefyID <- as_tibble(S_pois_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_slice2_groupCoefs[[2]]))
names(S_pois_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice2_rarefyID <- S_pois_slice2_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

##  slice3
S_pois_slice3_groupCoefs <- coef(S_pois_RCTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into rct and rarefyID estimates
S_pois_slice3_rct <- as_tibble(S_pois_slice3_groupCoefs[[1]]) %>%
  mutate(rct = rownames(S_pois_slice3_groupCoefs[[1]]))
names(S_pois_slice3_rct)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice3_rct <- S_pois_slice3_rct %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_slice3_rarefyID <- as_tibble(S_pois_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_slice3_groupCoefs[[2]]))
names(S_pois_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice3_rarefyID <- S_pois_slice3_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 



##---------------------------combine all together---------------------------
rlm_clm_txa_global_estimates <- bind_rows(
  Jbeta_norm_ALL_global, Jbeta_norm_ALL_slice1, Jbeta_norm_ALL_slice2, Jbeta_norm_ALL_slice3,  
  Jne_norm_ALL_global, Jne_norm_ALL_slice1, Jne_norm_ALL_slice2, Jne_norm_ALL_slice3,  
  Jtu_norm_ALL_global, Jtu_norm_ALL_slice1, Jtu_norm_ALL_slice2, Jtu_norm_ALL_slice3, 
  S_pois_ALL_global, S_pois_ALL_slice1, S_pois_ALL_slice2, S_pois_ALL_slice3
) %>%
  mutate(term = rep(c('intercept', 'slope'), times=4))

rlm_clm_txa_estimate <- bind_rows(
  Jbeta_norm_ALL_rct, Jbeta_norm_slice1_rct, Jbeta_norm_slice2_rct, Jbeta_norm_slice3_rct,
  Jne_norm_ALL_rct, Jne_norm_slice1_rct, Jne_norm_slice2_rct, Jne_norm_slice3_rct,
  Jtu_norm_ALL_rct, Jtu_norm_slice1_rct, Jtu_norm_slice2_rct, Jtu_norm_slice3_rct,
  S_pois_ALL_rct, S_pois_slice1_rct, S_pois_slice2_rct, S_pois_slice3_rct)

rlm_clm_txa_rarefyID_estimate <- bind_rows(
  Jbeta_norm_ALL_rarefyID, Jbeta_norm_slice1_rarefyID, Jbeta_norm_slice2_rarefyID, Jbeta_norm_slice3_rarefyID,
  Jne_norm_ALL_rarefyID, Jne_norm_slice1_rarefyID, Jne_norm_slice2_rarefyID, Jne_norm_slice3_rarefyID,
  Jtu_norm_ALL_rarefyID, Jtu_norm_slice1_rarefyID, Jtu_norm_slice2_rarefyID, Jtu_norm_slice3_rarefyID,
  S_pois_ALL_rarefyID, S_pois_slice1_rarefyID, S_pois_slice2_rarefyID, S_pois_slice3_rarefyID)




##----save-----------
setwd('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/')
save(rlm_clm_txa_global_estimates, rlm_clm_txa_estimate, rlm_clm_txa_rarefyID_estimate,
     file='RCTRfyID_rarefyID_coef.Rdata')
