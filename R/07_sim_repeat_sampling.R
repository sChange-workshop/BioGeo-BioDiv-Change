# source code to do turnover calculations
rm(list=ls())

library(tidyverse)
source('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/06_rarefysamplesturnoverbnh.R')

set.seed(403)

# simulation parameters
n_sim <- 1000
# length of the time series for each simulation
# time_series_length <- floor(runif(n_sim, min = 2, max = 50))
# what is the empirical distribution of time series length in our analyses?
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')
cell_count <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('Biome', 'taxa_mod'))

# get parameters of poisson lognormal for duration
duration_poilog <- poilog::poilogMLE(rarefied_medians %>% 
                    group_by(rarefyID) %>%
                    # distinct(num_years) %>%
                    distinct(duration) %>% .$duration)

##	filter to count data and biome/taxa combinations with >3 cells
# these are the data that the model was fit to
rarefied_medians <- rarefied_medians %>%
  filter(BROAD_TYPE=='count' & n_cells > 3)
# duration
inset <- rarefied_medians %>% 
  group_by(rarefyID) %>%
  # distinct(num_years) %>%
  distinct(duration) %>%
  # summarise(mean(log(duration)),
            # sd(log(duration)))
 ggplot() +
  geom_density(aes(x = duration, fill = 'empirical')) +
  # squint (or assume something) and it could be lognormal 
  geom_density(data = data.frame(duration_sim = rlnorm(1000,2.3,0.65)),#poilog::rpoilog(n_distinct(rarefied_medians %>%
                                                                             # distinct(duration) %>% 
                                                                             # .$duration),
                                                                # mu = duration_poilog$par[1],
                                                                # sig = duration_poilog$par[2])),#sads::rsad(#S = 50000, 
                                                           # frac = 1,
                                                           # sad = 'ls', coef = list(N = 285909, alpha = 34848.4))),#rlnorm(10000, 2, 0.8)
               aes( x= duration_sim, fill = 'simulated'), alpha = 0.5) +
  scale_fill_manual(name = '',
                    values = c('empirical' = 'black',
                               'simulated' = 'light grey')) +
  theme_bw() +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  # scale_x_continuous(trans = 'log2')

time_series_length <- floor(rlnorm(n_sim, meanlog = 2.3, sdlog = 0.65))
time_series_length <- time_series_length[time_series_length>1]  
# mu_log_num_years = 2, sd_log_num_years = 0.8
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
# calculate quantiles of simulated slopes
sim_quantiles <- slopes %>% 
  group_by(metric) %>% 
  summarise(median = median(slope, na.rm = T),
            lower = quantile(slope, probs = 0.05, na.rm = T),
            upper = quantile(slope, probs = 0.95, na.rm = T))

# load model fits to add an empirical estimate to the plot
load('~/Dropbox/1current/BioTime/BioGeo-BioDiv-Change/R/bt/model_fits_output/BT_model_coef_ranef.Rdata')

empirical <- BT_global_estimates %>% 
  filter((model=='Jtu_new_norm' | model=='Jne_new_norm') & term=='Slope') %>% 
  mutate(metric = ifelse(model=='Jtu_new_norm', 'Turnover', 'Nestedness'))

sim_result <- ggplot() +
  facet_wrap(~metric) +
  geom_histogram(data = slopes,
                 aes(x = slope), binwidth = 0.005) +
  geom_rect(data = empirical,
            aes(ymin = -Inf, ymax = Inf, xmin = lower, xmax = upper),
            alpha = 0.5) +
  geom_vline(data = empirical,
             aes(xintercept = Estimate)) +
  geom_vline(data = sim_quantiles,
             aes(xintercept = upper),
             lty = 2) +
  geom_vline(data = sim_quantiles,
             aes(xintercept = lower),
             lty = 2) +
  labs(x = 'Slope estimate',
       y = 'Number of simulations') +
  theme_bw()

# function for custom inset
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

sim_result + annotation_custom2(ggplotGrob(inset),
                               data = data.frame(metric = 'Nestedness',
                                                 x = 0, y = 3),
                  xmin = -0.081, xmax = -0.005, ymin = 500, ymax = 1000)
# this is not playing nice...saved using rstudio from plot screen
# ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/FigS9.png',
#        width = 150, height = 80, units = 'mm')



slopes %>% 
  group_by(metric) %>% 
  summarise(median = median(slope, na.rm = T),
            lower = quantile(slope, probs = 0.05, na.rm = T),
            upper = quantile(slope, probs = 0.95, na.rm = T))
