## Code to combine the estimates of change in the different metrics (alpha and beta diversity)
## and to add some covariates to these new data for plotting

#------run script to clean coefficients and load libraries------------
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/02a_bt_coef_wrangle.R')

##-------want to look at relationships among change in S, Jtu and Jne 

#--Taxa-level wrangle------
Jbeta_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jbeta_norm' & time_period=='ALL') %>%
  mutate(deltaJbeta = Estimate.cYEAR,
         deltaJbeta_lower = lower_slope,
         deltaJbeta_upper = upper_slope)

Jne_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jne_norm' & time_period=='ALL') %>%
  mutate(deltaJne = Estimate.cYEAR,
         deltaJne_lower = lower_slope,
         deltaJne_upper = upper_slope) %>%
  select(deltaJne,
         deltaJne_lower,
         deltaJne_upper) 

Jne_zi_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jne_zi' & time_period=='ALL') %>%
  mutate(deltaJne_zi = Estimate.cYEAR,
         deltaJne_zi_lower = lower_slope,
         deltaJne_zi_upper = upper_slope) %>%
  select(deltaJne_zi,
         deltaJne_zi_lower,
         deltaJne_zi_upper) 

Jtu_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jtu_norm' & time_period=='ALL') %>%
  mutate(deltaJtu = Estimate.cYEAR,
         deltaJtu_lower = lower_slope,
         deltaJtu_upper = upper_slope) %>%
  select(deltaJtu,
         deltaJtu_lower,
         deltaJtu_upper) 

Jtu_z1i_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jtu_z1i' & time_period=='ALL') %>%
  mutate(deltaJtu_z1i = Estimate.cYEAR,
         deltaJtu_z1i_lower = lower_slope,
         deltaJtu_z1i_upper = upper_slope) %>%
  select(deltaJtu_z1i,
         deltaJtu_z1i_lower,
         deltaJtu_z1i_upper)

S_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'S_pois' & time_period=='ALL') %>%
  mutate(deltaS = Estimate.cYEAR,
         deltaS_lower = lower_slope,
         deltaS_upper = upper_slope) %>% 
  select(deltaS,
         deltaS_lower,
         deltaS_upper)

taxa_corr <- bind_cols(
  Jbeta_taxa,
  Jne_taxa,
  Jtu_taxa,
  Jne_zi_taxa,
  Jtu_z1i_taxa,
  S_taxa
) %>%
  separate(Biome, into = c('q', 'r', 's', 't', 'u'), sep = '_', remove = FALSE) %>%
  mutate(r = ifelse(is.na(r), '', r),
         s = ifelse(is.na(s), '', s),
         t = ifelse(is.na(t), '', t),
         u = ifelse(is.na(u), '', u)) %>% 
  unite(Biome2, c(q, r, s, t, u), sep = ' ')

# add indicator for whether deltaJtu > deltaJne 
taxa_corr <- taxa_corr %>%
  mutate(Jtu_grt_Jne = ifelse(deltaJtu > deltaJne, 'Jtu > Jne', 'Jne > Jtu'),
         Jtu_grt_Jne_beta = ifelse(deltaJtu_z1i > deltaJne_zi, 'Jtu > Jne', 'Jne > Jtu'))

# add another indicator for quadrant conceptual model (four colours: Jne>Jtu & S < 0, Jne>Jtu & S>0, etc)
taxa_corr <- taxa_corr %>%
  mutate(quad = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS < 0), 'c1', 
                       ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS > 0), 'c2',
                              ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS < 0), 'c3', 'c4'))),
         quad_sig = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS_upper < 0 & deltaJtu_lower > 0), 'c1_sig',
                           ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS_lower > 0 & deltaJtu_lower > 0), 'c2_sig',
                                  ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS_upper < 0 & deltaJne_lower > 0), 'c3_sig', 
                                         ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS_lower > 0 & deltaJne_lower > 0), 'c4_sig', quad)))))

# blue green (colour blind friendly - from colorbrewer2.org)
quad_col = c('c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')
quad_sig_fill = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
                 'c1' = 'white', 'c2' = 'white', 'c3' = 'white', 'c4' = 'white')
quad_sig_col = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
                  'c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')

# add some covariate for plotting 
taxa_corr <- taxa_corr %>%
  mutate(alphaS = ifelse((deltaS_lower < 0 & deltaS_upper > 0), 0.95, 1),
         alphaJtu = ifelse((deltaJtu_lower < 0 & deltaJtu_upper > 0), 0.95, 1),
         alphaJne = ifelse((deltaJne_lower < 0 & deltaJne_upper > 0), 0.95, 1),
         ab_sig = ifelse(((Jtu_grt_Jne=='Jtu > Jne' & alphaJtu==1 & alphaS==1) |
                           (Jtu_grt_Jne=='Jne > Jtu' & alphaJne==1 & alphaS==1)), 1, 0.95))

taxa_corr <- taxa_corr %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse((Jtu_grt_Jne=='Jtu > Jne' & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse((Jtu_grt_Jne=='Jne > Jtu' & alphaS==1 & alphaJne==1), 1.1, 1),
         sizeBLUE = ifelse((Jtu_grt_Jne=='Jtu > Jne' & alphaS==1 & alphaJtu==1), 2.5, .25),
         sizeGREEN = ifelse((Jtu_grt_Jne=='Jne > Jtu' & alphaS==1 & alphaJne==1), 2.5, .25))
