# code to examine relationships between alpha and beta-diversity change
# rm(list=ls())

#------run script to clean coefficients and load libraries------------
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/2bt_coef_wrangle.R')

##-------want to look at relationships between S, Jtu and Jne 

#--Biome-level first-----------
# check if any biomes have deltaS > 0 different from zero
BT_biome_estimate %>%
  distinct(model, Estimate.cYEAR, .keep_all=TRUE) %>%
  filter(model == 'S_pois' & lower_slope > 0)

biome_corr <- BT_biome_estimate %>%
  # reduce to unique estimates for each Biome/model combination (has repeat rows for each cell)
  distinct(model, Estimate.cYEAR, .keep_all=TRUE) %>%
  filter(time_period=='ALL') %>%
  select(-taxa_mod)

Jbeta_biome <- biome_corr %>%
  filter(model=='Jbeta_norm') %>%
  mutate(deltaJbeta = Estimate.cYEAR,
         deltaJbeta_lower = lower_slope,
         deltaJbeta_upper = upper_slope)

Jne_biome <- biome_corr %>%
  filter(model=='Jne_norm') %>%
  mutate(deltaJne = Estimate.cYEAR,
         deltaJne_lower = lower_slope,
         deltaJne_upper = upper_slope) %>%
  select(deltaJne,
         deltaJne_lower,
         deltaJne_upper)

Jtu_biome <- biome_corr %>%
  filter(model=='Jtu_norm') %>%
  mutate(deltaJtu = Estimate.cYEAR,
         deltaJtu_lower = lower_slope,
         deltaJtu_upper = upper_slope) %>%
  select(deltaJtu,
         deltaJtu_lower,
         deltaJtu_upper)

N_biome <- biome_corr %>%
  filter(model=='N_lognorm') %>%
  mutate(deltaN = Estimate.cYEAR,
         deltaN_lower = lower_slope,
         deltaN_upper = upper_slope) %>%
  select(deltaN,
         deltaN_lower,
         deltaN_upper)

ENSPIE_biome <- biome_corr %>%
  filter(model=='ENSPIE_pois') %>%
  mutate(deltaENSPIE = Estimate.cYEAR,
         deltaENSPIE_lower = lower_slope,
         deltaENSPIE_upper = upper_slope) %>%
  select(deltaENSPIE,
         deltaENSPIE_lower,
         deltaENSPIE_upper)

S_biome <- biome_corr %>%
  filter(model=='S_pois') %>%
  mutate(deltaS = Estimate.cYEAR,
         deltaS_lower = lower_slope,
         deltaS_upper = upper_slope) %>%
  select(deltaS,
         deltaS_lower,
         deltaS_upper)

# put 'em back together
biome_corr <- bind_cols(
  Jbeta_biome,
  Jne_biome,
  Jtu_biome,
  N_biome,
  ENSPIE_biome,
  S_biome
) %>%
  separate(Biome, into = c('q', 'r', 's', 't', 'u'), sep = '_', remove = FALSE) %>%
  mutate(r = ifelse(is.na(r), '', r),
         s = ifelse(is.na(s), '', s),
         t = ifelse(is.na(t), '', t),
         u = ifelse(is.na(u), '', u)) %>% 
  unite(Biome2, c(q, r, s, t, u), sep = ' ') 


# add indicator for quadrant conceptual model (Jtu > Jne or Jne > Jtu)
biome_corr <- biome_corr %>%
  mutate(Jtu_grt_Jne = ifelse(deltaJtu > deltaJne, 'Jtu > Jne', 'Jne > Jtu'))

# add another indicator for quadrant conceptual model (four colours: Jne>Jtu & S < 0, Jne>Jtu & S>0, etc)
biome_corr <- biome_corr %>%
  mutate(quad = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS < 0), 'c1', 
                       ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS > 0), 'c2',
                              ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS < 0), 'c3', 'c4'))),
         # conceptual model of change in alpha diversity: N
         N_quad = ifelse((deltaN < 0 & deltaS > 0), 'n1',
                         ifelse((deltaN > 0 & deltaS > 0), 'n2',
                                ifelse((deltaN > 0 & deltaS < 0), 'n3', 'n4'))),
         # conceptual model of change in alpha diversity: ENSPIE
         ENSPIE_quad = ifelse((deltaENSPIE < 0 & deltaS > 0), 'e1',
                              ifelse((deltaENSPIE > 0 & deltaS > 0), 'e2',
                                     ifelse((deltaENSPIE > 0 & deltaS < 0), 'e3', 'e4'))))

# want to combine the quadrants for N and ENSPIE
biome_corr <- biome_corr %>%
  unite(N_ENSPIE_quad, c(N_quad, ENSPIE_quad), sep = '_', remove = FALSE)

# calculate rank correlations (following Magurran et al 2018 PNAS)
# rho_biome <- biome_corr %>%
#   group_by(REALM) %>%
#   filter(n_distinct(deltaS) > 2) %>%
#   nest(deltaJbeta, deltaJne, deltaJtu, deltaS) %>%
#   mutate(
#     rho_Jb_S = map(data, ~cor.test(x = .x$deltaJbeta, y = .x$deltaS)$estimate %>% as.numeric()),
#     rho_Jne_S = map(data, ~cor.test(x = .x$deltaJne, y = .x$deltaS)$estimate %>% as.numeric()),
#     rho_Jtu_S = map(data, ~cor.test(x = .x$deltaJtu, y = .x$deltaS)$estimate %>% as.numeric())) %>%
#   unnest(rho_Jb_S, rho_Jne_S, rho_Jtu_S) %>%
#   mutate(
#     x = -Inf,
#     y = Inf,
#     hjust = -0.1,
#     yjust = 0.2)




#--Taxa-level wrangle------
Jbeta_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jbeta_norm' & time_period == 'ALL') %>%
  mutate(deltaJbeta = Estimate.cYEAR,
         deltaJbeta_lower = lower_slope,
         deltaJbeta_upper = upper_slope)

Jne_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jne_norm' & time_period == 'ALL') %>%
  mutate(deltaJne = Estimate.cYEAR,
         deltaJne_lower = lower_slope,
         deltaJne_upper = upper_slope) %>%
  select(deltaJne,
         deltaJne_lower,
         deltaJne_upper) 

Jtu_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jtu_norm' & time_period == 'ALL') %>%
  mutate(deltaJtu = Estimate.cYEAR,
         deltaJtu_lower = lower_slope,
         deltaJtu_upper = upper_slope) %>%
  select(deltaJtu,
         deltaJtu_lower,
         deltaJtu_upper) 

N_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'N_lognorm' & time_period == 'ALL') %>%
  mutate(deltaN = Estimate.cYEAR,
         deltaN_lower = lower_slope,
         deltaN_upper = upper_slope) %>%
  select(deltaN,
         deltaN_lower,
         deltaN_upper)

ENSPIE_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'ENSPIE_pois' & time_period == 'ALL') %>%
  mutate(deltaENSPIE = Estimate.cYEAR,
         deltaENSPIE_lower = lower_slope,
         deltaENSPIE_upper = upper_slope) %>%
  select(deltaENSPIE,
         deltaENSPIE_lower,
         deltaENSPIE_upper)

S_taxa <- BT_taxa_estimate %>%
  # remove duplicate entries created when adding meta data
  distinct(model, Biome, taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'S_pois' & time_period == 'ALL') %>%
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
  N_taxa,
  ENSPIE_taxa,
  S_taxa
) %>%
  separate(Biome, into = c('q', 'r', 's', 't', 'u'), sep = '_', remove = FALSE) %>%
  mutate(r = ifelse(is.na(r), '', r),
         s = ifelse(is.na(s), '', s),
         t = ifelse(is.na(t), '', t),
         u = ifelse(is.na(u), '', u)) %>% 
  unite(Biome2, c(q, r, s, t, u), sep = ' ')

# calculate spearman correlations (a la Magurran et al 2018 PNAS) for
# taxa within realms
# rho_realm_taxa <- taxa_corr %>%
#   group_by(REALM, taxa_mod) %>%
#   filter(n_distinct(deltaS) > 2) %>%
#   nest(deltaJbeta, deltaJne, deltaJtu, deltaS) %>%
#   mutate(
#     rho_Jb_S = map(data, ~cor.test(x = .x$deltaJbeta, y = .x$deltaS)$estimate %>% as.numeric()),
#     rho_Jne_S = map(data, ~cor.test(x = .x$deltaJne, y = .x$deltaS)$estimate %>% as.numeric()),
#     rho_Jtu_S = map(data, ~cor.test(x = .x$deltaJtu, y = .x$deltaS)$estimate %>% as.numeric())) %>%
#   unnest(rho_Jb_S, rho_Jne_S, rho_Jtu_S) %>%
#   mutate(
#     x = -Inf,
#     y = Inf,
#     hjust = -0.1,
#     yjust = 0.2)

# add indicator for whether deltaJtu > deltaJne 
taxa_corr <- taxa_corr %>%
  mutate(Jtu_grt_Jne = ifelse(deltaJtu > deltaJne, 'Jtu > Jne', 'Jne > Jtu'))

# add another indicator for quadrant conceptual model (four colours: Jne>Jtu & S < 0, Jne>Jtu & S>0, etc)
taxa_corr <- taxa_corr %>%
  mutate(quad = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS < 0), 'c1', 
                       ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS > 0), 'c2',
                              ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS < 0), 'c3', 'c4'))),
         quad_sig = ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS_upper < 0 & deltaJtu_lower > 0), 'c1_sig',
                           ifelse((Jtu_grt_Jne=='Jtu > Jne' & deltaS_lower > 0 & deltaJtu_lower > 0), 'c2_sig',
                                  ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS_upper < 0 & deltaJne_lower > 0), 'c3_sig', 
                                         ifelse((Jtu_grt_Jne=='Jne > Jtu' & deltaS_lower > 0 & deltaJne_lower > 0), 'c4_sig', quad)))),
         # conceptual model of change in alpha diversity: N
         N_quad = ifelse((deltaN < 0 & deltaS > 0), 'n1',
                         ifelse((deltaN > 0 & deltaS > 0), 'n2',
                                ifelse((deltaN > 0 & deltaS < 0), 'n3', 'n4'))),
         N_quad_sig = ifelse((N_quad=='n1' & deltaN_upper < 0 & deltaS_lower > 0), 'n1_sig',
                             ifelse((N_quad=='n2' & deltaN_lower > 0 & deltaS_lower > 0), 'n2_sig',
                                    ifelse((N_quad=='n3' & deltaN_lower > 0 & deltaS_upper < 0), 'n3_sig',
                                           ifelse((N_quad=='n4' & deltaN_upper < 0 & deltaS_upper < 0), 'n4_sig', N_quad)))),
         # conceptual model of change in alpha diversity: ENSPIE
         ENSPIE_quad = ifelse((deltaENSPIE < 0 & deltaS > 0), 'e1',
                              ifelse((deltaENSPIE > 0 & deltaS > 0), 'e2',
                                     ifelse((deltaENSPIE > 0 & deltaS < 0), 'e3', 'e4'))),
         ENSPIE_quad_sig = ifelse((ENSPIE_quad=='e1' & deltaENSPIE_upper < 0 & deltaS_lower > 0), 'e1_sig',
                             ifelse((ENSPIE_quad=='e2' & deltaENSPIE_lower > 0 & deltaS_lower > 0), 'e2_sig',
                                    ifelse((ENSPIE_quad=='e3' & deltaENSPIE_lower > 0 & deltaS_upper < 0), 'e3_sig',
                                           ifelse((ENSPIE_quad=='e4' & deltaENSPIE_upper < 0 & deltaS_upper < 0), 'e4_sig', ENSPIE_quad)))))

# set colours
# blue green (colour blind friendly)
quad_col = c('c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')
quad_sig_fill = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
                 'c1' = 'white', 'c2' = 'white', 'c3' = 'white', 'c4' = 'white')
quad_sig_col = c('c1_sig' = '#a6cee3', 'c2_sig' = '#1f78b4', 'c3_sig' = '#b2df8a', 'c4_sig' = '#33a02c', 
                  'c1' = '#a6cee3', 'c2' = '#1f78b4', 'c3' = '#b2df8a', 'c4' = '#33a02c')
# browns
# N_quad_col = c('n1' = '#bf812d', 'n2' = '#8c510a', 'n3' = '#dfc27d', 'n4' = '#f6e8c3')
# greens
# ENSPIE_quad_col = c('e1' = '#35978f', 'e2' = '#01665e', 'e3' = '#80cdc1', 'e4' = '#c7eae5')
# blue-green
N_quad_col = c('n1' = '#a6cee3', 'n2' = '#1f78b4', 'n3' = '#33a02c', 'n4' = '#b2df8a')
N_quad_sig_fill = c('n1_sig' = '#a6cee3', 'n2_sig' = '#1f78b4', 'n3_sig' = '#33a02c', 'n4_sig' = '#b2df8a',
                   'n1' = 'white', 'n2' = 'white', 'n3' = 'white', 'n4' = 'white')
N_quad_sig_col = c('n1_sig' = '#a6cee3', 'n2_sig' = '#1f78b4', 'n3_sig' = '#33a02c', 'n4_sig' = '#b2df8a',
                   'n1' = '#a6cee3', 'n2' = '#1f78b4', 'n3' = '#33a02c', 'n4' = '#b2df8a')
# red-orange
ENSPIE_quad_col = c('e1' = '#fb9a99', 'e2' = '#e31a1c', 'e3' = '#ff7f00', 'e4' = '#fdbf6f')
ENSPIE_quad_sig_fill = c('e1_sig' = '#fb9a99', 'e2_sig' = '#e31a1c', 'e3_sig' = '#ff7f00', 'e4_sig' = '#fdbf6f',
                    'e1' = 'white', 'e2' = 'white', 'e3' = 'white', 'e4' = 'white')
ENSPIE_quad_sig_col = c('e1_sig' = '#fb9a99', 'e2_sig' = '#e31a1c', 'e3_sig' = '#ff7f00', 'e4_sig' = '#fdbf6f',
                    'e1' = '#fb9a99', 'e2' = '#e31a1c', 'e3' = '#ff7f00', 'e4' = '#fdbf6f')
# add alpha covariate for plotting deltaS different from zero
taxa_corr <- taxa_corr %>%
  mutate(alphaS = ifelse((deltaS_lower < 0 & deltaS_upper > 0), 0.95, 1),
         alphaJtu = ifelse((deltaJtu_lower < 0 & deltaJtu_upper > 0), 0.95, 1),
         alphaJne = ifelse((deltaJne_lower < 0 & deltaJne_upper > 0), 0.95, 1),
         ab_sig = ifelse(((Jtu_grt_Jne=='Jtu > Jne' & alphaJtu==1 & alphaS==1) |
                           (Jtu_grt_Jne=='Jne > Jtu' & alphaJne==1 & alphaS==1)), 1, 0.95),
         alphaN = ifelse((deltaN_lower < 0 & deltaN_upper > 0), 0.95, 1),
         alphaENSPIE = ifelse((deltaENSPIE_lower < 0 & deltaENSPIE_upper > 0), 0.95, 1))

taxa_corr <- taxa_corr %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse((Jtu_grt_Jne=='Jtu > Jne' & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse((Jtu_grt_Jne=='Jne > Jtu' & alphaS==1 & alphaJne==1), 1.1, 1),
         sizeBLUE = ifelse((Jtu_grt_Jne=='Jtu > Jne' & alphaS==1 & alphaJtu==1), 2.5, .25),
         sizeGREEN = ifelse((Jtu_grt_Jne=='Jne > Jtu' & alphaS==1 & alphaJne==1), 2.5, .25))

# want to combine the quadrants for N and ENSPIE
taxa_corr <- taxa_corr %>%
  unite(N_ENSPIE_quad, c(N_quad, ENSPIE_quad), sep = '_', remove = FALSE) %>%
  # add add indicator if changes differ from zero
  mutate(N_ENSPIE_quad_sig = ifelse((N_ENSPIE_quad=='n1_e1' & N_quad_sig=='n1_sig' & ENSPIE_quad_sig=='e1_sig'), 'n1_e1_sig',
                                    ifelse((N_ENSPIE_quad=='n1_e2' & N_quad_sig=='n1_sig' & ENSPIE_quad_sig=='e2_sig'), 'n1_e2_sig',
                                    ifelse((N_ENSPIE_quad=='n2_e1' & N_quad_sig=='n2_sig' & ENSPIE_quad_sig=='e1_sig'), 'n2_e1_sig',
                                    ifelse((N_ENSPIE_quad=='n2_e2' & N_quad_sig=='n2_sig' & ENSPIE_quad_sig=='e2_sig'), 'n2_e2_sig',
                                    ifelse((N_ENSPIE_quad=='n3_e3' & N_quad_sig=='n3_sig' & ENSPIE_quad_sig=='e3_sig'), 'n3_e3_sig',
                                    ifelse((N_ENSPIE_quad=='n3_e4' & N_quad_sig=='n3_sig' & ENSPIE_quad_sig=='e4_sig'), 'n3_e4_sig',
                                    ifelse((N_ENSPIE_quad=='n4_e3' & N_quad_sig=='n4_sig' & ENSPIE_quad_sig=='e3_sig'), 'n4_e3_sig',
                                    ifelse((N_ENSPIE_quad=='n4_e4' & N_quad_sig=='n4_sig' & ENSPIE_quad_sig=='e4_sig'), 'n4_e4_sig', N_ENSPIE_quad)))))))))

taxa_corr <- taxa_corr %>%
  # simplify realms for plotting, and add a label (not needed now with the tag argument in ggplot2)
  mutate(realm2 = ifelse(REALM=='Marine', REALM, 'Terrestrial/Freshwater'),
         realm_label = ifelse(REALM=='a.   Marine', REALM, 'b.   Terrestrial/Freshwater'))
