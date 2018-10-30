# code to make table for biome and taxa level coefficients
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/2bt_coef_wrangle.R')

library(tidyverse)
  
biome_print_table <- BT_biome_estimate %>% 
  filter(model=='S_pois' & time_period=='ALL') %>%
  distinct(REALM, Biome, start2, stop2, n_cells, Estimate.cYEAR, lower_slope, upper_slope) %>%
  mutate(Richness_change = round(Estimate.cYEAR, 4),
         Richness_lower = round(lower_slope, 4),
         Richness_upper = round(upper_slope, 4)) %>%
  select(-Estimate.cYEAR, -lower_slope, -upper_slope) %>%
  unite(richness, c(Richness_change, Richness_lower), sep = ' (', remove = F) %>% 
  unite(richness, c(richness, Richness_upper), sep = '__') %>% 
  mutate(richness = paste0(richness, ')')) %>% 
  select(-Richness_change, -Richness_lower) %>% 
  # turnover
  inner_join(
  BT_biome_estimate %>% 
  filter(model=='Jtu_norm' & time_period=='ALL') %>% 
  distinct(REALM, Biome, start2, stop2, n_cells, Estimate.cYEAR, lower_slope, upper_slope) %>% 
    mutate(Turnover_change = round(Estimate.cYEAR, 4),
           Turnover_lower = round(lower_slope, 4),
           Turnover_upper = round(upper_slope, 4)) %>% 
    select(-Estimate.cYEAR, -lower_slope, -upper_slope) %>% 
    unite(turnover, c(Turnover_change, Turnover_lower), sep = ' (', remove = F) %>% 
    unite(turnover, c(turnover, Turnover_upper), sep = '__') %>% 
    mutate(turnover = paste0(turnover, ')')) %>% 
    select(-Turnover_change, -Turnover_lower),
  # join by
  by = c('REALM', 'Biome', 'start2', 'stop2', 'n_cells')) %>% 
  # nestedness
  inner_join(
    BT_biome_estimate %>% 
    filter(model=='Jne_norm' & time_period=='ALL') %>% 
    distinct(REALM, Biome, start2, stop2, n_cells, Estimate.cYEAR, lower_slope, upper_slope) %>% 
      mutate(Nestedness_change = round(Estimate.cYEAR, 4),
             Nestedness_lower = round(lower_slope, 4),
             Nestedness_upper = round(upper_slope, 4)) %>% 
      select(-Estimate.cYEAR, -lower_slope, -upper_slope) %>% 
      unite(nestedness, c(Nestedness_change, Nestedness_lower), sep = ' (', remove = F) %>% 
      unite(nestedness, c(nestedness, Nestedness_upper), sep = '__') %>% 
      mutate(nestedness = paste0(nestedness, ')')) %>% 
      select(-Nestedness_change, -Nestedness_lower),
  # join by
  by = c('REALM', 'Biome', 'start2', 'stop2', 'n_cells'))

setwd('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/')
write.table(biome_print_table, file = "biome_coefficients.csv", sep = ",", quote = FALSE, row.names = F)


BT_taxa_estimate$taxa_mod2 <- factor(BT_taxa_estimate$taxa_mod, 
                                          levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                     "All", "Marine invertebrates/plants", "Mammals"),
                                          labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                     "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

taxa_print_table <- BT_taxa_estimate %>% 
  filter(model=='S_pois' & time_period=='ALL') %>%
  distinct(REALM, Biome, taxa_mod2, start2, stop2, n_cells, Estimate.cYEAR, lower_slope, upper_slope) %>%
  mutate(Richness_change = round(Estimate.cYEAR, 4),
         Richness_lower = round(lower_slope, 4),
         Richness_upper = round(upper_slope, 4)) %>%
  select(-Estimate.cYEAR, -lower_slope, -upper_slope) %>%
  unite(richness, c(Richness_change, Richness_lower), sep = ' (', remove = F) %>% 
  unite(richness, c(richness, Richness_upper), sep = '__') %>% 
  mutate(richness = paste0(richness, ')')) %>% 
  select(-Richness_change, -Richness_lower) %>% 
  # turnover
  inner_join(
    BT_taxa_estimate %>% 
      filter(model=='Jtu_norm' & time_period=='ALL') %>% 
      distinct(REALM, Biome, taxa_mod2, start2, stop2, n_cells, Estimate.cYEAR, lower_slope, upper_slope) %>% 
      mutate(Turnover_change = round(Estimate.cYEAR, 4),
             Turnover_lower = round(lower_slope, 4),
             Turnover_upper = round(upper_slope, 4)) %>% 
      select(-Estimate.cYEAR, -lower_slope, -upper_slope) %>% 
      unite(turnover, c(Turnover_change, Turnover_lower), sep = ' (', remove = F) %>% 
      unite(turnover, c(turnover, Turnover_upper), sep = '__') %>% 
      mutate(turnover = paste0(turnover, ')')) %>% 
      select(-Turnover_change, -Turnover_lower),
    # join by
    by = c('REALM', 'Biome', 'taxa_mod2', 'start2', 'stop2', 'n_cells')) %>% 
  # nestedness
  inner_join(
    BT_taxa_estimate %>% 
      filter(model=='Jne_norm' & time_period=='ALL') %>% 
      distinct(REALM, Biome, taxa_mod2, start2, stop2, n_cells, Estimate.cYEAR, lower_slope, upper_slope) %>% 
      mutate(Nestedness_change = round(Estimate.cYEAR, 4),
             Nestedness_lower = round(lower_slope, 4),
             Nestedness_upper = round(upper_slope, 4)) %>% 
      select(-Estimate.cYEAR, -lower_slope, -upper_slope) %>% 
      unite(nestedness, c(Nestedness_change, Nestedness_lower), sep = ' (', remove = F) %>% 
      unite(nestedness, c(nestedness, Nestedness_upper), sep = '__') %>% 
      mutate(nestedness = paste0(nestedness, ')')) %>% 
      select(-Nestedness_change, -Nestedness_lower),
    # join by
    by = c('REALM', 'Biome', 'taxa_mod2', 'start2', 'stop2', 'n_cells')) 
  

write.table(taxa_print_table, file = "taxa_coefficients.csv", sep = ",", quote = FALSE, row.names = F)
