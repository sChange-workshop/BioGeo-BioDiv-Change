# code to check nuts (no u-turn sampler) diagnostics for all models
# code to run remotely and save results to file
rm(list=ls())

library(brms)
library(rstan)
library(dplyr)
library(tibble)

# source nuts diagnostics file (from Michael Betancourt)
source('/gpfs1/data/sChange/BioTime/hierarchical/final_results/stan_utility.R')

# #------------load rlm_clm_txa models------------
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jne_norm_RCTSRfyID-4950830.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTSRfyID_slice1.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTSR_slice2.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTSRfyID_slice3.Rdata')
# 
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jtu_norm_RCTSRfyID_count-4950828.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTSRfyID_slice1-4951060.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTSRfyID_slice2.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTSRfyID_slice3.Rdata')
#  
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/S_pois_RCTRfyID_count-4436001.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTSR_slice1.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTSRfyID_slice2-4950841.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTSRfyID_slice3-4950842.Rdata')

#-------load Biome/Taxa models------------------
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_norm_BTSRfyID_count-4950633.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_zi_BTSRfyID_count-4934401.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_norm_BTSRfyID_count.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_z1i_realm_BTSRfyID_count-4946085.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/S_pois_BTSRfyID_expPrior.Rdata')

#--------create lists of names and models ----------------
# create a character vector all of the model names
# rct_model_names <- c('Jne_norm_RCTRfyID_countData',
#                 'Jne_norm_RCTRfyID_countData_slice1', 'Jne_norm_RCTRfyID_countData_slice2', 'Jne_norm_RCTRfyID_countData_slice3',
#                 'Jtu_norm_RCTRfyID_countData',
#                 'Jtu_norm_RCTRfyID_countData_slice1', 'Jtu_norm_RCTRfyID_countData_slice2', 'Jtu_norm_RCTRfyID_countData_slice3',
#                 'S_pois_RCTRfyID_countData',
#                 'S_pois_RCTRfyID_countData_slice1', 'S_pois_RCTRfyID_countData_slice2', 'S_pois_RCTRfyID_countData_slice3'
#                 )
# rct_model_list <- list(Jne_norm_RCTRfyID_countData,
#                        Jne_norm_RCTRfyID_countData_slice1, Jne_norm_RCTRfyID_countData_slice2, Jne_norm_RCTRfyID_countData_slice3,
#                        Jtu_norm_RCTRfyID_countData,
#                        Jtu_norm_RCTRfyID_countData_slice1, Jtu_norm_RCTRfyID_countData_slice2, Jtu_norm_RCTRfyID_countData_slice3,
#                        S_pois_RCTRfyID_countData,
#                        S_pois_RCTRfyID_countData_slice1, S_pois_RCTRfyID_countData_slice2, S_pois_RCTRfyID_countData_slice3
# )
# 
# #--------check divergence-------------
# rct_nuts <- tibble()
# 
# for(i in 1:length(rct_model_list)){
#     div = check_div_energy(model = rct_model_names[i], fit = rct_model_list[[i]]$fit)
#     rct_nuts = bind_rows(rct_nuts, div)
# }

# ##	save
# setwd('/gpfs1/data/sChange/BioTime/hierarchical/final_results/')
# save(rct_nuts, file='rct_nuts_diagnostics.Rdata')

#--------create lists of names and models ----------------
# create a character vector all of the model names
bt_model_names <- c('Jne_norm_BTRfyID_countData', 'Jne_zi_BTRfyID_countData',
                     'Jtu_norm_BTRfyID_countData', 'Jtu_z1i_BTRfyID_countData',
                     'S_pois_BTRfyID_countData'
)
bt_model_list <- list(Jne_norm_BTRfyID_countData, Jne_zi_BTRfyID_countData,
                       Jne_norm_BTRfyID_countData_slice1, Jne_norm_BTRfyID_countData_slice2, Jne_norm_BTRfyID_countData_slice3,
                       Jtu_norm_BTRfyID_countData, Jtu_z1i_BTRfyID_countData,
                       Jtu_norm_BTRfyID_countData_slice1, Jtu_norm_BTRfyID_countData_slice2, Jtu_norm_BTRfyID_countData_slice3,
                       S_pois_BTRfyID_countData, 
                       S_pois_BTRfyID_countData_slice1, S_pois_BTRfyID_countData_slice2, S_pois_BTRfyID_countData_slice3
)

#--------check divergence-------------
bt_nuts <- tibble()

for(i in 1:length(bt_model_list)){
  print(paste('model', i, 'in', length(bt_model_names), sep = ' '))
  div = check_div_energy(model = bt_model_names[i], fit = bt_model_list[[i]]$fit)
  bt_nuts = bind_rows(bt_nuts, div)
}

##	save
# setwd('/gpfs1/data/sChange/BioTime/hierarchical/final_results/')
# save(bt_nuts, file='bt_nuts_diagnostics.Rdata')
