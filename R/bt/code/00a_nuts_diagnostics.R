# code to check nuts (no u-turn sampler) diagnostics for all models
# code to run remotely and save results to file
rm(list=ls())

library(brms)
library(rstan)
library(dplyr)
library(tibble)

# source nuts diagnostics file (from Michael Betancourt)
source('/gpfs1/data/sChange/BioTime/hierarchical/final_results/stan_utility.R')

#------------load rlm_clm_txa models------------
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jbeta_norm_RCTRfyID_count-1486688.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice1-1490016.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice2-4432581.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice3-4422158.Rdata')
# 
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jne_norm_RCTRfyID_count-1486948.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice1-1492370.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice2-3989799.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice3-4422159.Rdata')
# 
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jtu_norm_RCTRfyID_count-1487091.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice1-2315138.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice2-3989800.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice3-4422160.Rdata')
# 
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/N_lognorm_RCTRfyID_count-1487273.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/N_lognorm_RCTRfyID_count_slice1-1491388.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/N_lognorm_RCTRfyID_count_slice2-3989801.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/N_lognorm_RCTRfyID_count_slice3-4422224.Rdata')
# 
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/ENSPIE_pois_RCTRfyID_count-1488265.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/ENSPIE_pois_RCTRfyID_count_slice1-1491691.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/ENSPIE_pois_RCTRfyID_count_slice2-4432188.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/ENSPIE_pois_RCTRfyID_count_slice3-4422229.Rdata')
# 
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/S_pois_RCTRfyID_count-4436001.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice1-3989803.Rdata')
# S_pois_RCTRfyID_countData_slice1 <- S_pois_RCTRfyID_countData_slice2; rm(S_pois_RCTRfyID_countData_slice2)
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice2-4487709.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice3-4487703.Rdata')
# # load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/')

#-------load Biome/Taxa models------------------
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jbeta_norm_BTRfyID_count-4442234.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jbeta_z1i_BTRfyID_count-4460955.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice1-630993.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice2-4464543.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice3-4435037.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_norm_BTRfyID_count-4435033.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_zi_BTRfyID_count-4524232.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice1-1918907.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice2-4454341.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice3-4435038.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_norm_BTRfyID_count-4464540.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_z1i_BTRfyID_count-4508929.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice1-2314679.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice2-4508925.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice3-4435039.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/N_lognorm_BTRfyID_count-4508927.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/N_lognorm_BTRfyID_count_slice1-414867.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/N_lognorm_BTRfyID_count_slice2-4454343.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/N_lognorm_BTRfyID_count_slice3-4435040.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/ENSPIE_pois_BTRfyID_count-4487705.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/ENSPIE_pois_BTRfyID_count_slice1-414868.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/ENSPIE_pois_BTRfyID_count_slice2-4454344.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/ENSPIE_pois_BTRfyID_count_slice3-4435041.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/S_pois_BTRfyID_count_stdT_autoScale-4591549.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice1-414930.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice2-4508928.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice3_stdT-4569976.Rdata')

#--------create lists of names and models ----------------
# create a character vector all of the model names
# rct_model_names <- c('Jbeta_norm_RCTRfyID_countData', 
#                 'Jbeta_norm_RCTRfyID_countData_slice1', 'Jbeta_norm_RCTRfyID_countData_slice2', 'Jbeta_norm_RCTRfyID_countData_slice3',
#                 'Jne_norm_RCTRfyID_countData', 
#                 'Jne_norm_RCTRfyID_countData_slice1', 'Jne_norm_RCTRfyID_countData_slice2', 'Jne_norm_RCTRfyID_countData_slice3',
#                 'Jtu_norm_RCTRfyID_countData', 
#                 'Jtu_norm_RCTRfyID_countData_slice1', 'Jtu_norm_RCTRfyID_countData_slice2', 'Jtu_norm_RCTRfyID_countData_slice3',
#                 'N_lognorm_RCTRfyID_countData', 
#                 'N_lognorm_RCTRfyID_countData_slice1', 'N_lognorm_RCTRfyID_countData_slice2', 'N_lognorm_RCTRfyID_countData_slice3',
#                 'ENSPIE_pois_RCTRfyID_countData', 
#                 'ENSPIE_pois_RCTRfyID_countData_slice1', 'ENSPIE_pois_RCTRfyID_countData_slice2', 'ENSPIE_pois_RCTRfyID_countData_slice3',
#                 'S_pois_RCTRfyID_countData', 
#                 'S_pois_RCTRfyID_countData_slice1', 'S_pois_RCTRfyID_countData_slice2', 'S_pois_RCTRfyID_countData_slice3'
#                 )
# rct_model_list <- list(Jbeta_norm_RCTRfyID_countData, 
#                        Jbeta_norm_RCTRfyID_countData_slice1, Jbeta_norm_RCTRfyID_countData_slice2, Jbeta_norm_RCTRfyID_countData_slice3,
#                        Jne_norm_RCTRfyID_countData, 
#                        Jne_norm_RCTRfyID_countData_slice1, Jne_norm_RCTRfyID_countData_slice2, Jne_norm_RCTRfyID_countData_slice3,
#                        Jtu_norm_RCTRfyID_countData, 
#                        Jtu_norm_RCTRfyID_countData_slice1, Jtu_norm_RCTRfyID_countData_slice2, Jtu_norm_RCTRfyID_countData_slice3,
#                        N_lognorm_RCTRfyID_countData, 
#                        N_lognorm_RCTRfyID_countData_slice1, N_lognorm_RCTRfyID_countData_slice2, N_lognorm_RCTRfyID_countData_slice3,
#                        ENSPIE_pois_RCTRfyID_countData, 
#                        ENSPIE_pois_RCTRfyID_countData_slice1, ENSPIE_pois_RCTRfyID_countData_slice2, ENSPIE_pois_RCTRfyID_countData_slice3,
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
#                                   
# ##	save
# setwd('/gpfs1/data/sChange/BioTime/hierarchical/final_results/')
# save(rct_nuts, file='rct_nuts_diagnostics.Rdata')

#--------create lists of names and models ----------------
# create a character vector all of the model names
bt_model_names <- c('Jbeta_norm_BTRfyID_countData', 'Jbeta_z1i_BTRfyID_countData',
                     'Jbeta_norm_BTRfyID_countData_slice1', 'Jbeta_norm_BTRfyID_countData_slice2', 'Jbeta_norm_BTRfyID_countData_slice3',
                     'Jne_norm_BTRfyID_countData', 'Jne_zi_BTRfyID_countData',
                     'Jne_norm_BTRfyID_countData_slice1', 'Jne_norm_BTRfyID_countData_slice2', 'Jne_norm_BTRfyID_countData_slice3',
                     'Jtu_norm_BTRfyID_countData', 'Jtu_z1i_BTRfyID_countData',
                     'Jtu_norm_BTRfyID_countData_slice1', 'Jtu_norm_BTRfyID_countData_slice2', 'Jtu_norm_BTRfyID_countData_slice3',
                     'N_lognorm_BTRfyID_countData', 
                     'N_lognorm_BTRfyID_countData_slice1', 'N_lognorm_BTRfyID_countData_slice2', 'N_lognorm_BTRfyID_countData_slice3',
                     'ENSPIE_pois_BTRfyID_countData', 
                     'ENSPIE_pois_BTRfyID_countData_slice1', 'ENSPIE_pois_BTRfyID_countData_slice2', 'ENSPIE_pois_BTRfyID_countData_slice3',
                     'S_pois_BTRfyID_countData', 
                     'S_pois_BTRfyID_countData_slice1', 'S_pois_BTRfyID_countData_slice2', 'S_pois_BTRfyID_countData_slice3'
)
bt_model_list <- list(Jbeta_norm_BTRfyID_countData, Jbeta_norm_BTRfyID_countData_z1i,  
                       Jbeta_norm_BTRfyID_countData_slice1, Jbeta_norm_BTRfyID_countData_slice2, Jbeta_norm_BTRfyID_countData_slice3,
                       Jne_norm_BTRfyID_countData, Jne_zi_BTRfyID_countData,
                       Jne_norm_BTRfyID_countData_slice1, Jne_norm_BTRfyID_countData_slice2, Jne_norm_BTRfyID_countData_slice3,
                       Jtu_norm_BTRfyID_countData, Jtu_z1i_BTRfyID_countData,
                       Jtu_norm_BTRfyID_countData_slice1, Jtu_norm_BTRfyID_countData_slice2, Jtu_norm_BTRfyID_countData_slice3,
                       N_lognorm_BTRfyID_countData, 
                       N_lognorm_BTRfyID_countData_slice1, N_lognorm_BTRfyID_countData_slice2, N_lognorm_BTRfyID_countData_slice3,
                       ENSPIE_pois_BTRfyID_countData, 
                       ENSPIE_pois_BTRfyID_countData_slice1, ENSPIE_pois_BTRfyID_countData_slice2, ENSPIE_pois_BTRfyID_countData_slice3,
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
setwd('/gpfs1/data/sChange/BioTime/hierarchical/final_results/')
save(bt_nuts, file='bt_nuts_diagnostics.Rdata')
