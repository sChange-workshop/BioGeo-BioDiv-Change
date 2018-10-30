##------some diagnostic plots for biome-taxa hierarchical models-----------------------
##------libraries-----------------------
rm(list=ls())
library(brms)
library(rstan)
library(dplyr)
library(tibble)
library(bayesplot)

#-------load Biome/Taxa models------------------
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jbeta_norm_BTRfyID_count-4442234.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jbeta_z1i_BTRfyID_count-4460955.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice1-630993.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice2-4464543.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jbeta_norm_BTRfyID_count_slice3-4435037.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_norm_BTRfyID_count-4435033.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jne_zi_BTRfyID_count-4524232.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice1-1918907.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice2-4454341.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jne_norm_BTRfyID_count_slice3-4435038.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_norm_BTRfyID_count-4464540.Rdata')
# load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/Jtu_z1i_BTRfyID_count-4508929.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice1-2314679.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice2-4508925.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/Jtu_norm_BTRfyID_count_slice3-4435039.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/S_pois_BTRfyID_count_stdT_autoScale-4591549.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice1-414930.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice2-4508928.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/S_pois_BTRfyID_count_slice3_stdT-4569976.Rdata')


##--------------------------CHAIN INSPECTION----------------------------------------------------
# save the diagnositics of fits to all count data first
setwd('/gpfs1/data/sChange/BioTime/hierarchical/BiomeTaxa/results/figs/diagnostics/trace/countData/')
# Jbeta_norm
mcmc_trace(Jbeta_norm_BTRfyID_countData$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jbeta_norm_BTRfyID_count') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jbeta_norm_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# Jbeta_z1i
mcmc_trace(Jbeta_norm_BTRfyID_countData_z1i$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jbeta_z1i_BTRfyID_count') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jbeta_z1i_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# Jne_norm
mcmc_trace(Jne_norm_BTRfyID_countData$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('Jne_norm_BTRfyID_count') +
  ggsave('Jne_norm_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# Jne_zi
mcmc_trace(Jne_zi_BTRfyID_countData$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('Jne_zi_BTRfyID_count') +
  ggsave('Jne_zi_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# Jtu_norm
mcmc_trace(Jtu_norm_BTRfyID_countData$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jtu_norm_BTRfyID_count') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jtu_norm_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# Jtu_z1i
mcmc_trace(Jtu_z1i_BTRfyID_countData$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jtu_z1i_BTRfyID_count') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jtu_z1i_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# N_lognorm
mcmc_trace(N_lognorm_BTRfyID_countData$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none') +
  ggtitle('N_lognorm_BTRfyID_count') +
  ggsave('N_lognorm_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# ENSPIE_pois
mcmc_trace(ENSPIE_pois_BTRfyID_countData$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('ENSPIE_pois_BTRfyID_count') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('ENSPIE_pois_BTRfyID_count.png', width = 200, height = 290, units = 'mm')

# S_pois
mcmc_trace(S_pois_BTRfyID_countData$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('S_pois_BTRfyID_count') +
  ggsave('S_pois_BTRfyID_count.png', width = 200, height = 290, units = 'mm')



##-----trace plots for the time slices---------------------
setwd('/data/sChange/BioTime/hierarchical/BiomeTaxa/timeSlice/results/figures/diagnostics/trace/')

# Jbeta_norm
mcmc_trace(Jbeta_norm_BTRfyID_countData_slice1$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jbeta_norm_BTRfyID_count: 1951-1970') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jbeta_norm_BTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm')

mcmc_trace(Jbeta_norm_BTRfyID_countData_slice2$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jbeta_norm_BTRfyID_count: 1971-1990') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jbeta_norm_BTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm')

mcmc_trace(Jbeta_norm_BTRfyID_countData_slice3$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jbeta_norm_BTRfyID_count: 1991-2010') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jbeta_norm_BTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm')

# Jne_norm
mcmc_trace(Jne_norm_BTRfyID_countData_slice1$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('Jne_norm_BTRfyID_count: 1951-1970') +
  ggsave('Jne_norm_BTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm')

mcmc_trace(Jne_norm_BTRfyID_countData_slice2$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('Jne_norm_BTRfyID_count: 1971-1990') +
  ggsave('Jne_norm_BTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm')

mcmc_trace(Jne_norm_BTRfyID_countData_slice3$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('Jne_norm_BTRfyID_count: 1991-2010') +
  ggsave('Jne_norm_BTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm')

# Jtu_norm
mcmc_trace(Jtu_norm_BTRfyID_countData_slice1$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jtu_norm_BTRfyID_count: 1951-1970') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jtu_norm_BTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm')

mcmc_trace(Jtu_norm_BTRfyID_countData_slice2$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jtu_norm_BTRfyID_count: 1971-1990') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jtu_norm_BTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm')

mcmc_trace(Jtu_norm_BTRfyID_countData_slice3$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('Jtu_norm_BTRfyID_count: 1991-2010') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('Jtu_norm_BTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm')

# N_lognorm
mcmc_trace(N_lognorm_BTRfyID_countData_slice1$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none') +
  ggtitle('N_lognorm_BTRfyID_count: 1951-1970') +
  ggsave('N_lognorm_BTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm')

mcmc_trace(N_lognorm_BTRfyID_countData_slice2$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none') +
  ggtitle('N_lognorm_BTRfyID_count: 1971-1990') +
  ggsave('N_lognorm_BTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm')

mcmc_trace(N_lognorm_BTRfyID_countData_slice3$fit %>% as.array(),
           pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none') +
  ggtitle('N_lognorm_BTRfyID_count: 1991-2010') +
  ggsave('N_lognorm_BTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm')

# ENSPIE_pois
mcmc_trace(ENSPIE_pois_BTRfyID_countData_slice1$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('ENSPIE_pois_BTRfyID_count: 1951-1970') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('ENSPIE_pois_BTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm')

mcmc_trace(ENSPIE_pois_BTRfyID_countData_slice2$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('ENSPIE_pois_BTRfyID_count: 1971-1990') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('ENSPIE_pois_BTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm')

mcmc_trace(ENSPIE_pois_BTRfyID_countData_slice3$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  ggtitle('ENSPIE_pois_BTRfyID_count: 1991-2010') +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggsave('ENSPIE_pois_BTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm')

# S_pois
mcmc_trace(S_pois_BTRfyID_countData_slice1$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('S_pois_BTRfyID_count: 1951-1970') +
  ggsave('S_pois_BTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm')

mcmc_trace(S_pois_BTRfyID_countData_slice2$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('S_pois_BTRfyID_count: 1971-1990') +
  ggsave('S_pois_BTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm')

mcmc_trace(S_pois_BTRfyID_countData_slice3$fit %>% as.array(),
           # pars = 'sigma',
           regex_pars = c('^b_', '^sd_')) +
  theme(legend.position = 'none', title = element_text(size = 12)) +
  ggtitle('S_pois_BTRfyID_count: 1991-2010') +
  ggsave('S_pois_BTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm')

##-----------------------------------pp_checks...patience required!--------------------------------
setwd('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/figs/diagnostics/pp_check')

##  beta error (these look good, but the model is not easy to interpret, and 0's and 1's are not in the slope estimate!)
##  keep these plots, but I don't think we can use these models to interpret change through time!
# p1 <- pp_check(Jbeta_z1i_BTRfyID_countData) + 
#   ggtitle('Jbeta_z1i density of observed values and 10 posterior samples - ALL data')
# p2 <- pp_check(Jbeta_z1i_BTRfyID_countData_slice1) + 
#   ggtitle('Jbeta_z1i density of observed values and 10 posterior samples - 1951-1970')
# p3 <- pp_check(Jbeta_z1i_BTRfyID_countData_slice2) + 
#   ggtitle('Jbeta_z1i density of observed values and 10 posterior samples - 1971-1990')
# p4 <- pp_check(Jbeta_z1i_BTRfyID_countData_slice3) + 
#   ggtitle('Jbeta_z1i density of observed values and 10 posterior samples - 1991-2010')
# plot_grid(p1, p2, p3, p4)
# ggsave('Jbeta_z1i_pp_density.png', width=290, height=200, units = 'mm')

# p1 <- pp_check(Jne_zi_BTRfyID_countData) + 
#   ggtitle('Jne_zi density of observed values and 10 posterior samples - ALL data')
# p2 <- pp_check(Jne_zi_BTRfyID_countData_slice1) + 
#   ggtitle('Jne_zi density of observed values and 10 posterior samples - 1951-1970')
# p3 <- pp_check(Jne_zi_BTRfyID_countData_slice2) + 
#   ggtitle('Jne_zi density of observed values and 10 posterior samples - 1971-1990')
# p4 <- pp_check(Jne_zi_BTRfyID_countData_slice3) + 
#   ggtitle('Jne_zi density of observed values and 10 posterior samples - 1991-2010')
# plot_grid(p1, p2, p3, p4)
# ggsave('Jne_zi_pp_density.png', width=290, height=200, units = 'mm')

# p1 <- pp_check(Jtu_z1i_BTRfyID_countData) + 
#   ggtitle('Jtu_z1i density of observed values and 10 posterior samples - ALL data')
# p2 <- pp_check(Jtu_z1i_BTRfyID_countData_slice1) + 
#   ggtitle('Jtu_z1i density of observed values and 10 posterior samples - 1951-1970')
# p3 <- pp_check(Jtu_z1i_BTRfyID_countData_slice2) + 
# ggtitle('Jtu_z1i density of observed values and 10 posterior samples - 1971-1990')
#p4 <- pp_check(Jtu_z1i_BTRfyID_countData_slice3) + 
#  ggtitle('Jtu_z1i density of observed values and 10 posterior samples - 1991-2010')
#plot_grid(p1, p2, p3, p4)
#ggsave('Jtu_z1i_pp_density.png', width=290, height=200, units = 'mm')

##  gaussian error for beta-diversity metrics
pp_check(Jbeta_norm_BTRfyID_countData, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples') +
  ggsave('Jbeta_norm_BTRfyID_ALL_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jbeta_norm_BTRfyID_countData_slice1, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('Jbeta_norm_BTRfyID_slice1_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jbeta_norm_BTRfyID_countData_slice2, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('Jbeta_norm_BTRfyID_slice2_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jbeta_norm_BTRfyID_countData_slice3, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('Jbeta_norm_BTRfyID_slice3_int_grp_year.png', width=290, height=200, units='mm')

##----nestedness component
pp_check(Jne_norm_BTRfyID_countData, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples') +
  ggsave('Jne_norm_BTRfyID_ALL_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jne_norm_BTRfyID_countData_slice1, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('Jne_norm_BTRfyID_slice1_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jne_norm_BTRfyID_countData_slice2, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('Jne_norm_BTRfyID_slice2_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jne_norm_BTRfyID_countData_slice3, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('Jne_norm_BTRfyID_slice3_int_grp_year.png', width=290, height=200, units='mm')

##----------turnover component
pp_check(Jtu_norm_BTRfyID_countData, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples') +
  ggsave('Jtu_norm_BTRfyID_ALL_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jtu_norm_BTRfyID_countData_slice1, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('Jtu_norm_BTRfyID_slice1_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jtu_norm_BTRfyID_countData_slice2, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('Jtu_norm_BTRfyID_slice2_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jtu_norm_BTRfyID_countData_slice3, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('Jtu_norm_BTRfyID_slice3_int_grp_year.png', width=290, height=200, units='mm')


##-----------richness change
pp_check(S_poiss_BTRfyID_countData) + scale_x_continuous(trans='log')
pp_check(S_pois_BTRfyID_countData, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - all data') +
  ggsave('S_pois_BTRfyID_ALL_int_grp_year.png', width=290, height = 200, units='mm') 
pp_check(S_pois_BTRfyID_countData_slice1, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('S_pois_BTRfyID_slice1_int_grp_year.png', width=290, height = 200, units='mm') 
pp_check(S_pois_BTRfyID_countData_slice2, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('S_pois_BTRfyID_slice2_int_grp_year.png', width=290, height = 200, units='mm') 
pp_check(S_pois_BTRfyID_countData_slice3, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('S_pois_BTRfyID_slice3_int_grp_year.png', width=290, height = 200, units='mm') 

