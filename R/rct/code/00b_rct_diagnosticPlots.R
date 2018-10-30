##------some diagnostic plots for realm/climate/taxa hierarchical models-----------------------
##------data and libraries-----------------------
rm(list=ls())
library(brms)
library(rstan)
library(dplyr)
library(tibble)
library(bayesplot)

#------------load rlm_clm_txa models------------
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jbeta_norm_RCTRfyID_count-1486688.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice1-1490016.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice2-4432581.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jbeta_norm_RCTRfyID_count_slice3-4422158.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jne_norm_RCTRfyID_count-1486948.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice1-1492370.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice2-3989799.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jne_norm_RCTRfyID_count_slice3-4422159.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/Jtu_norm_RCTRfyID_count-1487091.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice1-2315138.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice2-3989800.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/Jtu_norm_RCTRfyID_count_slice3-4422160.Rdata')

load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/S_pois_RCTRfyID_count-4436001.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice1-3989803.Rdata')
# fix naming error
S_pois_RCTRfyID_countData_slice1 <- S_pois_RCTRfyID_countData_slice2; rm(S_pois_RCTRfyID_countData_slice2)
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice2-4487709.Rdata')
load('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/timeSlice/results/S_pois_RCTRfyID_count_slice3-4487703.Rdata')

##--------------------------CHAIN INSPECTION----------------------------------------------------
setwd('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/figs/diagnostics/trace')
# Jbeta_norm
png(filename = 'Jbeta_norm_RCTRfyID_count.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jbeta_norm_RCTRfyID_countData, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jbeta_norm_RCTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jbeta_norm_RCTRfyID_countData_slice1, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jbeta_norm_RCTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jbeta_norm_RCTRfyID_countData_slice2, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jbeta_norm_RCTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jbeta_norm_RCTRfyID_countData_slice3, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

# Jne_norm
png(filename = 'Jne_norm_RCTRfyID_count.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jne_norm_RCTRfyID_countData, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jne_norm_RCTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jne_norm_RCTRfyID_countData_slice1, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jne_norm_RCTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jne_norm_RCTRfyID_countData_slice2, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jne_norm_RCTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jne_norm_RCTRfyID_countData_slice3, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

# Jtu_norm
png(filename = 'Jtu_norm_RCTRfyID_count.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jtu_norm_RCTRfyID_countData, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jtu_norm_RCTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jtu_norm_RCTRfyID_countData_slice1, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jtu_norm_RCTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jtu_norm_RCTRfyID_countData_slice2, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'Jtu_norm_RCTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm', res = 150)
plot(Jtu_norm_RCTRfyID_countData_slice3, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

# S_pois
png(filename = 'S_pois_RCTRfyID_count.png', width = 200, height = 290, units = 'mm', res = 150)
plot(S_pois_RCTRfyID_countData, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'S_pois_RCTRfyID_count_slice1.png', width = 200, height = 290, units = 'mm', res = 150)
plot(S_pois_RCTRfyID_countData_slice1, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'S_pois_RCTRfyID_count_slice2.png', width = 200, height = 290, units = 'mm', res = 150)
plot(S_pois_RCTRfyID_countData_slice2, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

png(filename = 'S_pois_RCTRfyID_count_slice3.png', width = 200, height = 290, units = 'mm', res = 150)
plot(S_pois_RCTRfyID_countData_slice3, N = 9,
     theme = theme(legend.position = 'none', strip.text = element_blank())) 
dev.off()

##-----------------------------------pp_checks...patience required!--------------------------------
setwd('/gpfs1/data/sChange/BioTime/hierarchical/rlm_clm_txa/results/figs/diagnostics/pp_check')


##  gaussian error for beta-diversity metrics
pp_check(Jbeta_norm_RCTRfyID_countData, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples') +
  ggsave('Jbeta_norm_RCTRfyID_ALL_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jbeta_norm_RCTRfyID_countData_slice1, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('Jbeta_norm_RCTRfyID_slice1_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jbeta_norm_RCTRfyID_countData_slice2, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('Jbeta_norm_RCTRfyID_slice2_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jbeta_norm_RCTRfyID_countData_slice3, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jbeta_norm observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('Jbeta_norm_RCTRfyID_slice3_int_grp_year.png', width=290, height=200, units='mm')

##----nestedness component
pp_check(Jne_norm_RCTRfyID_countData, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples') +
  ggsave('Jne_norm_RCTRfyID_ALL_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jne_norm_RCTRfyID_countData_slice1, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('Jne_norm_RCTRfyID_slice1_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jne_norm_RCTRfyID_countData_slice2, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('Jne_norm_RCTRfyID_slice2_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jne_norm_RCTRfyID_countData_slice3, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jne_norm observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('Jne_norm_RCTRfyID_slice3_int_grp_year.png', width=290, height=200, units='mm')

##----------turnover component
pp_check(Jtu_norm_RCTRfyID_countData, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples') +
  ggsave('Jtu_norm_RCTRfyID_ALL_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jtu_norm_RCTRfyID_countData_slice1, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('Jtu_norm_RCTRfyID_slice1_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jtu_norm_RCTRfyID_countData_slice2, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('Jtu_norm_RCTRfyID_slice2_int_grp_year.png', width=290, height=200, units='mm')

pp_check(Jtu_norm_RCTRfyID_countData_slice3, type='intervals_grouped', group='rlm_clm_txa', x='cYEAR', nsamples=100) +
  ggtitle('Jtu_norm observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('Jtu_norm_RCTRfyID_slice3_int_grp_year.png', width=290, height=200, units='mm')

##--------------richness
pp_check(S_pois_RCTRfyID_countData) + scale_x_continuous(trans='log')
pp_check(S_pois_RCTRfyID_countData, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - all data') +
  ggsave('S_pois_RCTRfyID_ALL_int_grp_year.png', width=290, height = 200, units='mm') 
pp_check(S_pois_RCTRfyID_countData_slice1, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - 1951-1970') +
  ggsave('S_pois_RCTRfyID_slice1_int_grp_year.png', width=290, height = 200, units='mm') 
pp_check(S_pois_RCTRfyID_countData_slice2, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - 1971-1990') +
  ggsave('S_pois_RCTRfyID_slice2_int_grp_year.png', width=290, height = 200, units='mm') 
pp_check(S_pois_RCTRfyID_countData_slice3, type='intervals_grouped', group = 'rlm_clm_txa', x = 'cYEAR', nsamples = 100) +
  scale_y_continuous(trans='log2') +
  ggtitle('S_pois observed values and 90% quantile & median 100 posterior samples - 1991-2010') +
  ggsave('S_pois_RCTRfyID_slice3_int_grp_year.png', width=290, height = 200, units='mm') 


