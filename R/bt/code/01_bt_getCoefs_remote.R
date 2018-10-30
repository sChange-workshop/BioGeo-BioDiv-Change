##------diagnostics for biome/taxa/rarefyID hierarchical models-----------------------
##------data and libraries-----------------------
rm(list=ls())

library(brms)
library(dplyr)
#library(broom)
library(tidyr)
library(tibble)

#----load data
##----count data only, whole time period--------
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jbeta_norm_BTRfyID_count-4442234.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_norm_BTRfyID_count-4435033.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_norm_BTRfyID_count-4464540.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/S_pois_BTRfyID_count_stdT_autoScale-4591549.Rdata')


##---time slices--------
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice1-630993.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice2-4464543.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice3-4435037.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice1-1918907.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice2-4454341.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice3-4435038.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice1-2314679.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice2-4508925.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice3-4435039.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice1-414930.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice2-4508928.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice3_stdR-4569976.Rdata')


##------------------------------get coefficients for plotting----

##-----Jbeta:global estimates----------------------------------------------
Jbeta_norm_ALL_global <- fixef(Jbeta_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
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
Jbeta_norm_slice1_global <- fixef(Jbeta_norm_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970')

names(Jbeta_norm_slice1_global)[3:4] <- c('lower', 'upper')

Jbeta_norm_slice1_global <- Jbeta_norm_slice1_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jbeta_norm slice2
Jbeta_norm_slice2_global <- fixef(Jbeta_norm_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990')

names(Jbeta_norm_slice2_global)[3:4] <- c('lower', 'upper')

Jbeta_norm_slice2_global <- Jbeta_norm_slice2_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jbeta_norm slice3
Jbeta_norm_slice3_global <- fixef(Jbeta_norm_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010')

names(Jbeta_norm_slice3_global)[3:4] <- c('lower', 'upper')

Jbeta_norm_slice3_global <- Jbeta_norm_slice3_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 


##-----Jbeta_all:biome, taxa (and rarefyID) estimates------------------
Jbeta_norm_ALL_groupCoefs <- coef(Jbeta_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into biome, taxa and rarefyID estimates
Jbeta_norm_ALL_biome <- as_tibble(Jbeta_norm_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jbeta_norm_ALL_groupCoefs[[1]]))
names(Jbeta_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_biome <- Jbeta_norm_ALL_biome %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_ALL_taxa <- as_tibble(Jbeta_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_ALL_groupCoefs[[2]]))
names(Jbeta_norm_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_taxa <- Jbeta_norm_ALL_taxa %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jbeta_norm_ALL_rarefyID <- as_tibble(Jbeta_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jbeta_norm_ALL_groupCoefs[[3]]))
names(Jbeta_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_rarefyID <- Jbeta_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#------Jbeta_slice1: break into biome, taxa and rarefyID estimates--------
Jbeta_norm_slice1_groupCoefs <- coef(Jbeta_norm_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95))
Jbeta_norm_slice1_biome <- as_tibble(Jbeta_norm_slice1_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jbeta_norm_slice1_groupCoefs[[1]]))
names(Jbeta_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_biome <- Jbeta_norm_ALL_biome %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_slice1_taxa <- as_tibble(Jbeta_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_slice1_groupCoefs[[2]]))
names(Jbeta_norm_slice1_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice1_taxa <- Jbeta_norm_slice1_taxa %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jbeta_norm_slice1_rarefyID <- as_tibble(Jbeta_norm_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jbeta_norm_slice1_groupCoefs[[3]]))
names(Jbeta_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice1_rarefyID <- Jbeta_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------Jbeta_slice2: break into biome, taxa and rarefyID estimates--------
Jbeta_norm_slice2_groupCoefs <- coef(Jbeta_norm_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95))
Jbeta_norm_slice2_biome <- as_tibble(Jbeta_norm_slice2_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jbeta_norm_slice2_groupCoefs[[1]]))
names(Jbeta_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_biome <- Jbeta_norm_ALL_biome %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_slice2_taxa <- as_tibble(Jbeta_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_slice2_groupCoefs[[2]]))
names(Jbeta_norm_slice2_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice2_taxa <- Jbeta_norm_slice2_taxa %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jbeta_norm_slice2_rarefyID <- as_tibble(Jbeta_norm_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jbeta_norm_slice2_groupCoefs[[3]]))
names(Jbeta_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice2_rarefyID <- Jbeta_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------Jbeta_slice3: break into biome, taxa and rarefyID estimates--------
Jbeta_norm_slice3_groupCoefs <- coef(Jbeta_norm_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95))
Jbeta_norm_slice3_biome <- as_tibble(Jbeta_norm_slice3_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jbeta_norm_slice3_groupCoefs[[1]]))
names(Jbeta_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_biome <- Jbeta_norm_ALL_biome %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jbeta_norm_slice3_taxa <- as_tibble(Jbeta_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jbeta_norm_slice3_groupCoefs[[2]]))
names(Jbeta_norm_slice3_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice3_taxa <- Jbeta_norm_slice3_taxa %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jbeta_norm_slice3_rarefyID <- as_tibble(Jbeta_norm_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jbeta_norm_slice3_groupCoefs[[3]]))
names(Jbeta_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jbeta_norm_slice3_rarefyID <- Jbeta_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))




##-----Jne:global estimates---------------------------------------------------------
Jne_norm_ALL_global <- fixef(Jne_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
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
Jne_norm_slice1_global <- fixef(Jne_norm_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970')

names(Jne_norm_slice1_global)[3:4] <- c('lower', 'upper')

Jne_norm_slice1_global <- Jne_norm_slice1_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice2
Jne_norm_slice2_global <- fixef(Jne_norm_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990')

names(Jne_norm_slice2_global)[3:4] <- c('lower', 'upper')

Jne_norm_slice2_global <- Jne_norm_slice2_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jne_norm slice3
Jne_norm_slice3_global <- fixef(Jne_norm_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1991-2010')

names(Jne_norm_slice3_global)[3:4] <- c('lower', 'upper')

Jne_norm_slice3_global <- Jne_norm_slice3_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 


##-----Jne_all:biome, taxa (and rarefyID) estimates--------------
Jne_norm_ALL_groupCoefs <- coef(Jne_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into biome and rarefyID estimates
Jne_norm_ALL_biome <- as_tibble(Jne_norm_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jne_norm_ALL_groupCoefs[[1]]))
names(Jne_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_biome <- Jne_norm_ALL_biome %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_ALL_taxa <- as_tibble(Jne_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_ALL_groupCoefs[[2]]))
names(Jne_norm_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_taxa <- Jne_norm_ALL_taxa %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_ALL_rarefyID <- as_tibble(Jne_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_ALL_groupCoefs[[3]]))
names(Jne_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_rarefyID <- Jne_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#------Jne_slice1: break into biome, taxa and rarefyID estimates--------
Jne_norm_slice1_groupCoefs <- coef(Jne_norm_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95))
Jne_norm_slice1_biome <- as_tibble(Jne_norm_slice1_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jne_norm_slice1_groupCoefs[[1]]))
names(Jne_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_biome <- Jne_norm_ALL_biome %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice1_taxa <- as_tibble(Jne_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice1_groupCoefs[[2]]))
names(Jne_norm_slice1_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_taxa <- Jne_norm_slice1_taxa %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jne_norm_slice1_rarefyID <- as_tibble(Jne_norm_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_slice1_groupCoefs[[3]]))
names(Jne_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice1_rarefyID <- Jne_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------Jne_slice2: break into biome, taxa and rarefyID estimates--------
Jne_norm_slice2_groupCoefs <- coef(Jne_norm_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95))
Jne_norm_slice2_biome <- as_tibble(Jne_norm_slice2_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jne_norm_slice2_groupCoefs[[1]]))
names(Jne_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_biome <- Jne_norm_ALL_biome %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice2_taxa <- as_tibble(Jne_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice2_groupCoefs[[2]]))
names(Jne_norm_slice2_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_taxa <- Jne_norm_slice2_taxa %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jne_norm_slice2_rarefyID <- as_tibble(Jne_norm_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_slice2_groupCoefs[[3]]))
names(Jne_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice2_rarefyID <- Jne_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------Jne_slice3: break into biome, taxa and rarefyID estimates--------
Jne_norm_slice3_groupCoefs <- coef(Jne_norm_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95))
Jne_norm_slice3_biome <- as_tibble(Jne_norm_slice3_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jne_norm_slice3_groupCoefs[[1]]))
names(Jne_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_biome <- Jne_norm_ALL_biome %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jne_norm_slice3_taxa <- as_tibble(Jne_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jne_norm_slice3_groupCoefs[[2]]))
names(Jne_norm_slice3_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_taxa <- Jne_norm_slice3_taxa %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jne_norm_slice3_rarefyID <- as_tibble(Jne_norm_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jne_norm_slice3_groupCoefs[[3]]))
names(Jne_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jne_norm_slice3_rarefyID <- Jne_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jne_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))




##-----Jtu:global estimates-------------------------
Jtu_norm_ALL_global <- fixef(Jtu_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL')

names(Jtu_norm_ALL_global)[3:4] <- c('lower', 'upper')

Jtu_norm_ALL_global <- Jtu_norm_ALL_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jbeta_norm slice1
Jtu_norm_slice1_global <- fixef(Jtu_norm_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970')

names(Jtu_norm_slice1_global)[3:4] <- c('lower', 'upper')

Jtu_norm_slice1_global <- Jtu_norm_slice1_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice2
Jtu_norm_slice2_global <- fixef(Jtu_norm_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990')

names(Jtu_norm_slice2_global)[3:4] <- c('lower', 'upper')

Jtu_norm_slice2_global <- Jtu_norm_slice2_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  Jtu_norm slice3
Jtu_norm_slice3_global <- fixef(Jtu_norm_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1991-2010')

names(Jtu_norm_slice3_global)[3:4] <- c('lower', 'upper')

Jtu_norm_slice3_global <- Jtu_norm_slice3_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 


##-----Jtu_ALL:biome, taxa (and rarefyID) estimates-------------------------
Jtu_norm_ALL_groupCoefs <- coef(Jtu_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into biome and rarefyID estimates
Jtu_norm_ALL_biome <- as_tibble(Jtu_norm_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jtu_norm_ALL_groupCoefs[[1]]))
names(Jtu_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_biome <- Jtu_norm_ALL_biome %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_ALL_taxa <- as_tibble(Jtu_norm_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_ALL_groupCoefs[[2]]))
names(Jtu_norm_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_taxa <- Jtu_norm_ALL_taxa %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jtu_norm_ALL_rarefyID <- as_tibble(Jtu_norm_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_ALL_groupCoefs[[3]]))
names(Jtu_norm_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_rarefyID <- Jtu_norm_ALL_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#------Jtu_slice1: break into biome, taxa and rarefyID estimates--------
Jtu_norm_slice1_groupCoefs <- coef(Jtu_norm_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95))
Jtu_norm_slice1_biome <- as_tibble(Jtu_norm_slice1_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jtu_norm_slice1_groupCoefs[[1]]))
names(Jtu_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_biome <- Jtu_norm_ALL_biome %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice1_taxa <- as_tibble(Jtu_norm_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice1_groupCoefs[[2]]))
names(Jtu_norm_slice1_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_taxa <- Jtu_norm_slice1_taxa %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jtu_norm_slice1_rarefyID <- as_tibble(Jtu_norm_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_slice1_groupCoefs[[3]]))
names(Jtu_norm_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice1_rarefyID <- Jtu_norm_slice1_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------Jtu_slice2: break into biome, taxa and rarefyID estimates--------
Jtu_norm_slice2_groupCoefs <- coef(Jtu_norm_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95))
Jtu_norm_slice2_biome <- as_tibble(Jtu_norm_slice2_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jtu_norm_slice2_groupCoefs[[1]]))
names(Jtu_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_biome <- Jtu_norm_ALL_biome %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice2_taxa <- as_tibble(Jtu_norm_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice2_groupCoefs[[2]]))
names(Jtu_norm_slice2_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_taxa <- Jtu_norm_slice2_taxa %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jtu_norm_slice2_rarefyID <- as_tibble(Jtu_norm_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_slice2_groupCoefs[[3]]))
names(Jtu_norm_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice2_rarefyID <- Jtu_norm_slice2_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------Jtu_slice3: break into biome, taxa and rarefyID estimates--------
Jtu_norm_slice3_groupCoefs <- coef(Jtu_norm_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95))
Jtu_norm_slice3_biome <- as_tibble(Jtu_norm_slice3_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(Jtu_norm_slice3_groupCoefs[[1]]))
names(Jtu_norm_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_biome <- Jtu_norm_ALL_biome %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

Jtu_norm_slice3_taxa <- as_tibble(Jtu_norm_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(Jtu_norm_slice3_groupCoefs[[2]]))
names(Jtu_norm_slice3_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_taxa <- Jtu_norm_slice3_taxa %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

Jtu_norm_slice3_rarefyID <- as_tibble(Jtu_norm_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(Jtu_norm_slice3_groupCoefs[[3]]))
names(Jtu_norm_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
Jtu_norm_slice3_rarefyID <- Jtu_norm_slice3_rarefyID %>%
  mutate(
    model = 'Jtu_norm',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))



##-----S:global estimates----------------------------
S_pois_ALL_global <- fixef(S_pois_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) %>%
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
S_pois_slice1_global <- fixef(S_pois_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970')

names(S_pois_slice1_global)[3:4] <- c('lower', 'upper')

S_pois_slice1_global <- S_pois_slice1_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice2
S_pois_slice2_global <- fixef(S_pois_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990')

names(S_pois_slice2_global)[3:4] <- c('lower', 'upper')

S_pois_slice2_global <- S_pois_slice2_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##  S_pois slice3
S_pois_slice3_global <- fixef(S_pois_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95)) %>%
  as_tibble() %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010')

names(S_pois_slice3_global)[3:4] <- c('lower', 'upper')

S_pois_slice3_global <- S_pois_slice3_global %>%
  mutate(
    change = ifelse(lower > 0, 'up',
                    ifelse(upper < 0, 'down', 'neutral'))) 

##-----S_all:biome, taxa (and rarefyID) estimates------
S_pois_ALL_groupCoefs <- coef(S_pois_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
##  break into biome and rarefyID estimates
S_pois_ALL_biome <- as_tibble(S_pois_ALL_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(S_pois_ALL_groupCoefs[[1]]))
names(S_pois_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_biome <- S_pois_ALL_biome %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_ALL_taxa <- as_tibble(S_pois_ALL_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_ALL_groupCoefs[[2]]))
names(S_pois_ALL_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_taxa <- S_pois_ALL_taxa %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_ALL_rarefyID <- as_tibble(S_pois_ALL_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_ALL_groupCoefs[[3]]))
names(S_pois_ALL_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_rarefyID <- S_pois_ALL_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

#------S_pois_slice1: break into biome, taxa and rarefyID estimates--------
S_pois_slice1_groupCoefs <- coef(S_pois_BTRfyID_countData_slice1, robust = TRUE, probs = c(0.05, 0.95))
S_pois_slice1_biome <- as_tibble(S_pois_slice1_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(S_pois_slice1_groupCoefs[[1]]))
names(S_pois_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_biome <- S_pois_ALL_biome %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_slice1_taxa <- as_tibble(S_pois_slice1_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_slice1_groupCoefs[[2]]))
names(S_pois_slice1_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice1_taxa <- S_pois_slice1_taxa %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

S_pois_slice1_rarefyID <- as_tibble(S_pois_slice1_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_slice1_groupCoefs[[3]]))
names(S_pois_slice1_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice1_rarefyID <- S_pois_slice1_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = '1951-1970',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------S_pois_slice2: break into biome, taxa and rarefyID estimates--------
S_pois_slice2_groupCoefs <- coef(S_pois_BTRfyID_countData_slice2, robust = TRUE, probs = c(0.05, 0.95))
S_pois_slice2_biome <- as_tibble(S_pois_slice2_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(S_pois_slice2_groupCoefs[[1]]))
names(S_pois_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_biome <- S_pois_ALL_biome %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_slice2_taxa <- as_tibble(S_pois_slice2_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_slice2_groupCoefs[[2]]))
names(S_pois_slice2_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice2_taxa <- S_pois_slice2_taxa %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

S_pois_slice2_rarefyID <- as_tibble(S_pois_slice2_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_slice2_groupCoefs[[3]]))
names(S_pois_slice2_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice2_rarefyID <- S_pois_slice2_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = '1971-1990',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))


#------S_pois_slice3: break into biome, taxa and rarefyID estimates--------
S_pois_slice3_groupCoefs <- coef(S_pois_BTRfyID_countData_slice3, robust = TRUE, probs = c(0.05, 0.95))
S_pois_slice3_biome <- as_tibble(S_pois_slice3_groupCoefs[[1]]) %>%
  mutate(Biome = rownames(S_pois_slice3_groupCoefs[[1]]))
names(S_pois_ALL_biome)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_ALL_biome <- S_pois_ALL_biome %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral'))) 

S_pois_slice3_taxa <- as_tibble(S_pois_slice3_groupCoefs[[2]]) %>%
  mutate(level = rownames(S_pois_slice3_groupCoefs[[2]]))
names(S_pois_slice3_taxa)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice3_taxa <- S_pois_slice3_taxa %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))

S_pois_slice3_rarefyID <- as_tibble(S_pois_slice3_groupCoefs[[3]]) %>%
  mutate(level = rownames(S_pois_slice3_groupCoefs[[3]]))
names(S_pois_slice3_rarefyID)[c(3:4, 7:8)] <- c('lower_intercept', 'upper_intercept', 'lower_slope', 'upper_slope') 
S_pois_slice3_rarefyID <- S_pois_slice3_rarefyID %>%
  mutate(
    model = 'S_pois',
    time_period = '1991-2010',
    change = ifelse(lower_slope > 0, 'up',
                    ifelse(upper_slope < 0, 'down', 'neutral')))




##-----Jbeta: ranef only-----------------------
Jbeta_norm_ALL_ranef <- ranef(Jbeta_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
Jbeta_norm_ALL_biome_ranef <- as_tibble(Jbeta_norm_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(Jbeta_norm_ALL_ranef$Biome))
names(Jbeta_norm_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_biome_ranef <- Jbeta_norm_ALL_biome_ranef %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL')

Jbeta_norm_ALL_Taxa_ranef <- as_tibble(Jbeta_norm_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(Jbeta_norm_ALL_ranef$`Biome:taxa_mod`))
names(Jbeta_norm_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jbeta_norm_ALL_Taxa_ranef <- Jbeta_norm_ALL_Taxa_ranef %>%
  mutate(
    model = 'Jbeta_norm',
    time_period = 'ALL')

#------Jne:-ranef only----------------
Jne_norm_ALL_ranef <- ranef(Jne_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
Jne_norm_ALL_biome_ranef <- as_tibble(Jne_norm_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(Jne_norm_ALL_ranef$Biome))
names(Jne_norm_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_biome_ranef <- Jne_norm_ALL_biome_ranef %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL')

Jne_norm_ALL_Taxa_ranef <- as_tibble(Jne_norm_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(Jne_norm_ALL_ranef$`Biome:taxa_mod`))
names(Jne_norm_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jne_norm_ALL_Taxa_ranef <- Jne_norm_ALL_Taxa_ranef %>%
  mutate(
    model = 'Jne_norm',
    time_period = 'ALL')


#------Jtu:ranef only----------------
Jtu_norm_ALL_ranef <- ranef(Jtu_norm_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
Jtu_norm_ALL_biome_ranef <- as_tibble(Jtu_norm_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(Jtu_norm_ALL_ranef$Biome))
names(Jtu_norm_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_biome_ranef <- Jtu_norm_ALL_biome_ranef %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL')

Jtu_norm_ALL_Taxa_ranef <- as_tibble(Jtu_norm_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(Jtu_norm_ALL_ranef$`Biome:taxa_mod`))
names(Jtu_norm_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
Jtu_norm_ALL_Taxa_ranef <- Jtu_norm_ALL_Taxa_ranef %>%
  mutate(
    model = 'Jtu_norm',
    time_period = 'ALL')



#------ S-ranef only----------------
S_pois_ALL_ranef <- ranef(S_pois_BTRfyID_countData, robust = TRUE, probs = c(0.05, 0.95)) 
# separate biome and taxa slope random effects
S_pois_ALL_biome_ranef <- as_tibble(S_pois_ALL_ranef$Biome[,,'cYEAR']) %>%
  mutate(Biome = rownames(S_pois_ALL_ranef$Biome))
names(S_pois_ALL_biome_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_ALL_biome_ranef <- S_pois_ALL_biome_ranef %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL')

S_pois_ALL_Taxa_ranef <- as_tibble(S_pois_ALL_ranef$`Biome:taxa_mod`[,,'cYEAR']) %>%
  mutate(Taxa = rownames(S_pois_ALL_ranef$`Biome:taxa_mod`))
names(S_pois_ALL_Taxa_ranef)[c(1:4)] <- c('Slope_ranef', 'Slope_error', 'lower_slope', 'upper_slope') 
S_pois_ALL_Taxa_ranef <- S_pois_ALL_Taxa_ranef %>%
  mutate(
    model = 'S_pois',
    time_period = 'ALL')



##---------------------------combine all together---------------------------
BT_global_estimates <- bind_rows(
  Jbeta_norm_ALL_global, Jbeta_norm_slice1_global, Jbeta_norm_slice2_global, Jbeta_norm_slice3_global, # 
  Jne_norm_ALL_global, Jne_norm_slice1_global, Jne_norm_slice2_global, Jne_norm_slice3_global, #
  Jtu_norm_ALL_global, Jtu_norm_slice1_global, Jtu_norm_slice2_global, Jtu_norm_slice3_global,#, #
  S_pois_ALL_global, S_pois_slice1_global, S_pois_slice2_global, S_pois_slice3_global#, 
) %>%
  mutate(term = rep(c('Intercept', 'Slope'), times = 4))

BT_biome_estimate <- bind_rows(
  Jbeta_norm_ALL_biome, Jbeta_norm_slice1_biome, Jbeta_norm_slice2_biome, Jbeta_norm_slice3_biome,# 
  Jne_norm_ALL_biome, Jne_norm_slice1_biome, Jne_norm_slice2_biome, Jne_norm_slice3_biome, #
  Jtu_norm_ALL_biome, Jtu_norm_slice1_biome, Jtu_norm_slice2_biome, Jtu_norm_slice3_biome,#,# 
  S_pois_ALL_biome, S_pois_slice1_biome, S_pois_slice2_biome, S_pois_slice3_biome#, 
)

BT_taxa_estimate <- bind_rows(
  Jbeta_norm_ALL_taxa, Jbeta_norm_slice1_taxa, Jbeta_norm_slice2_taxa, Jbeta_norm_slice3_taxa,# 
  Jne_norm_ALL_taxa, Jne_norm_slice1_taxa, Jne_norm_slice2_taxa, Jne_norm_slice3_taxa, #
  Jtu_norm_ALL_taxa, Jtu_norm_slice1_taxa, Jtu_norm_slice2_taxa, Jtu_norm_slice3_taxa,#,# 
  S_pois_ALL_taxa, S_pois_slice1_taxa, S_pois_slice2_taxa, S_pois_slice3_taxa#, 
) 

BTRfyID_rarefyID_coef <- bind_rows(
  Jbeta_norm_ALL_rarefyID,
  Jne_norm_ALL_rarefyID,
  Jtu_norm_ALL_rarefyID,
  S_pois_ALL_rarefyID
)

BT_biome_ranef <- bind_rows(
  Jbeta_norm_ALL_biome_ranef,
  Jne_norm_ALL_biome_ranef,
  Jtu_norm_ALL_biome_ranef,
  S_pois_ALL_biome_ranef
)

BT_Taxa_ranef <- bind_rows(
  Jbeta_norm_ALL_Taxa_ranef,
  Jne_norm_ALL_Taxa_ranef,
  Jtu_norm_ALL_Taxa_ranef,
  S_pois_ALL_Taxa_ranef
)



#-----save--------
setwd('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/')
save(BT_global_estimates, BT_biome_estimate, BT_taxa_estimate, BTRfyID_rarefyID_coef, 
     BT_biome_ranef, BT_Taxa_ranef, file='BTRfyID_coef&ranef.Rdata')
