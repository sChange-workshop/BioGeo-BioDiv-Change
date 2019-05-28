# source code to do turnover calculations
rm(list=ls())

library(tidyverse)
source('~/Desktop/revision_code/rarefysamplesturnoverbnh.R')

set.seed(403)

# simulation parameters
n_sim <- 1000
# length of the time series for each simulation
time_series_length <- floor(runif(n_sim, min = 2, max = 50))
# regional species pool size for each simulation
s_pool <- floor(runif(n_sim, min = 200, max = 1000))
# number of samples for each simulation
sample_effort <- floor(runif(n_sim, min = 2, max = 50))

# initialise vectors to store slope estimates in
jtu_slopes <- c()
jne_slopes <- c()

for(sim in 1:n_sim){
  print(paste('simulation ', sim, 'in ', n_sim))
  # initialise empty vectors for this simulation  
  Year = c()
  SampleID = c()
  Species = c()
  Abundance = c()

  for(y in 1:time_series_length[sim]){
    # random sampling effort
    S = sample_effort[sim]
    # create year covariate for modelling 
    Year = c(Year,rep(y,S))
    # assign sampleID
    SampleID = c(SampleID,rep(1,S))
    # sample from regional pool
    Species = c(Species,sample(s_pool[sim],S))
    # presence only data: abundance==1
    Abundance = c(Abundance,rep(1,S))
  }
  
  # call function to calculate temporal turnover metrics 
  # (function also does sample-based rarefaction to standardise sampling effort, not required here)
  a <- rarefysamplesturnoverbnh(Year, SampleID, Species, Abundance, resamps = 1)
  
  # fit models to turnover and nestedness; extract the slope coefficient
  jtu_slopes = c(jtu_slopes, with(a[[1]], coef(lm(Jtu ~ Year))['Year']))
  jne_slopes = c(jne_slopes, with(a[[1]], coef(lm(Jne ~ Year))['Year']))
}

slopes <- tibble(metric = rep(c('Turnover', 'Nestedness'),
                              each = length(jtu_slopes)),
                 slope = c(jtu_slopes, jne_slopes)
                 )

# load model fits to add an empirical estimate to the plot
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BTRfyID_coef_ranef_inclNewModel.Rdata')

empirical <- BT_global_estimates %>% 
  filter((model=='Jtu_new_norm' | model=='Jne_new_norm') & term=='Slope') %>% 
  mutate(metric = ifelse(model=='Jtu_new_norm', 'Turnover', 'Nestedness'))

ggplot() +
  facet_wrap(~metric) +
  geom_histogram(data = slopes, 
                 aes(x = slope), binwidth = 0.005) +
  geom_rect(data = empirical,
            aes(ymin = -Inf, ymax = Inf, xmin = lower, xmax = upper),
            alpha = 0.5) +
geom_vline(data = empirical,
             aes(xintercept = Estimate)) +
  labs(x = 'Slope estimate',
       y = 'Number of simulations') +
  theme_bw()

# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/FigS9_simulation.png',
#        width = 150, height = 80, units = 'mm')

slopes %>% 
  group_by(metric) %>% 
  summarise(median = median(slope, na.rm = T),
            lower = quantile(slope, probs = 0.05, na.rm = T),
            upper = quantile(slope, probs = 0.95, na.rm = T))
