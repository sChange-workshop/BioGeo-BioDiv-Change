# read terrestrial shape file

#----wrangle coefficients & calculate correlations---------
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/02c_bt_taxa_to_singleLocations.R')


# load biomes---------------------
world <- map_data('world')


library(sf)
# library(tidyverse) 
 
mar_shp <- sf::st_read('~/Dropbox/BiogeoBioTIME/biomes/marineInUSe.shp', quiet = T) %>% select(-lat, -long)
terr_shp <- sf::st_read('~/Dropbox/BiogeoBioTIME/biomes/terrInUSe.shp', quiet = T)  %>% select(-lat, -long)
fresh_shp <- sf::st_read('~/Dropbox/BiogeoBioTIME/biomes/freshinUse.shp', quiet = T)  %>% select(-lat, -long)

# create spatialdataframes (sp package) for use with geom_polygon
marine_spdf <- as(mar_shp, 'Spatial')
terr_spdf <- as(terr_shp, 'Spatial')
fresh_spdf <- as(fresh_shp, 'Spatial')


#-------process marine shp file: add slope ranef and flatten for ggplot---------
mar_biomes <- rarefied_medians %>% 
  filter(REALM=='Marine') %>%
  distinct(Biome) %>%
  mutate(Biome = gsub("_", " ", Biome))

# filter spatialdataframe to these biomes only
marine_spdf <- marine_spdf[which(marine_spdf$Biome %in% mar_biomes$Biome),]
# put the slope ranef in after fixing names
marine_biome_re <- bind_cols(BT_biome_ranef %>% 
                               filter(model=='S_pois') %>% 
                               mutate(S_biome_re = Slope_ranef) %>%
                               select(S_biome_re, Biome),
                             BT_biome_ranef %>% 
                               filter(model=='Jtu_norm') %>% 
                               mutate(Jtu_biome_re = Slope_ranef) %>%
                               select(Jtu_biome_re),
                             BT_biome_ranef %>% 
                             filter(model=='Jne_norm') %>% 
                             mutate(Jne_biome_re = Slope_ranef) %>%
                             select(Jne_biome_re)) %>%
                    mutate(Biome = gsub("_", " ", Biome))

# marine_spdf <- tmaptools::append_data(shp = marine_spdf, data = marine_biome_re, key.shp = 'Biome', key.data = 'Biome')
# or if you can't install tmaptools (I can't on one of my machines!)
marine_spdf@data = data.frame(marine_spdf@data, 
                              marine_biome_re[match(as.character(marine_spdf@data[,'Biome']), as.character(unlist(marine_biome_re[,'Biome']))),])

names(marine_spdf)

marine_spdf@data$id = rownames(marine_spdf@data)
marine_spdf.points = fortify(marine_spdf, region="id")
marine_spdf2 = plyr::join(marine_spdf.points, marine_spdf@data, by="id")

marine_df <- marine_spdf2 %>%
  mutate(Realm2 = 'Marine') %>%
  as_tibble()


#-------process terr shp file: add slope ranef and flatten for ggplot---------

terr_biomes <- rarefied_medians %>% 
  filter(REALM=='Terrestrial') %>%
  distinct(Biome) %>%
  mutate(Biome = gsub("_", " ", Biome),
         # fix some other random ones up
         Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 
                        'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 
                        'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 
                        'Temperate Grasslands, Savannas and Shrublands', Biome))

# filter spatialdataframe to these biomes only
terr_spdf <- terr_spdf[which(terr_spdf$Biome %in% terr_biomes$Biome),]

# fix names in ranef dataframe
terr_biome_re <- bind_cols(BT_biome_ranef %>% 
                             filter(model=='S_pois') %>% 
                             mutate(S_biome_re = Slope_ranef) %>%
                             select(S_biome_re, Biome),
                           BT_biome_ranef %>% 
                             filter(model=='Jtu_norm') %>% 
                             mutate(Jtu_biome_re = Slope_ranef) %>%
                             select(Jtu_biome_re),
                           BT_biome_ranef %>% 
                             filter(model=='Jne_norm') %>% 
                             mutate(Jne_biome_re = Slope_ranef) %>%
                             select(Jne_biome_re)) %>%
  mutate(Biome = gsub("_", " ", Biome),
         # fix some other random ones up
         Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 
                        'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 
                        'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 
                        'Temperate Grasslands, Savannas and Shrublands', Biome))

terr_spdf <- tmaptools::append_data(shp = terr_spdf, data = terr_biome_re, key.shp = 'Biome', key.data = 'Biome')
# or if you can't install tmaptools (I can't on one of my machines!)
terr_spdf@data = data.frame(terr_spdf@data, 
                              terr_biome_re[match(as.character(terr_spdf@data[,'Biome']), as.character(unlist(terr_biome_re[,'Biome']))),])


terr_spdf$Biome %>% levels()

terr_spdf@data$id = rownames(terr_spdf@data)
terr_spdf.points = fortify(terr_spdf, region="id")
terr_spdf2 = plyr::join(terr_spdf.points, terr_spdf@data, by="id")
terr_spdf2 <- terr_spdf2 %>% as_tibble()


#-------process freshwater shp file: add slope ranef and flatten for ggplot---------
fresh_biomes <- rarefied_medians %>% 
  filter(REALM=='Freshwater') %>%
  distinct(Biome) %>%
  mutate(Biome = gsub("_", " ", Biome))

# filter spatialdataframe to these biomes only
fresh_spdf <- fresh_spdf[which(fresh_spdf$Biome %in% fresh_biomes$Biome),]

# fix names in ranef dataframe
fresh_biome_re <- bind_cols(BT_biome_ranef %>% 
                              filter(model=='S_pois') %>% 
                              mutate(S_biome_re = Slope_ranef) %>%
                              select(S_biome_re, Biome),
                            BT_biome_ranef %>% 
                              filter(model=='Jtu_norm') %>% 
                              mutate(Jtu_biome_re = Slope_ranef) %>%
                              select(Jtu_biome_re),
                            BT_biome_ranef %>% 
                              filter(model=='Jne_norm') %>% 
                              mutate(Jne_biome_re = Slope_ranef) %>%
                              select(Jne_biome_re)) %>%
  mutate(Biome = gsub("_", " ", Biome),
         # fix some other random ones up
         Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
         Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 
                        'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
         Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 
                        'Mediterranean Forests, Woodlands and Scrub', Biome),
         Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 
                        'Temperate Grasslands, Savannas and Shrublands', Biome))

# fresh_spdf <- tmaptools::append_data(shp = fresh_spdf, data = fresh_biome_re, key.shp = 'Biome', key.data = 'Biome')
# or if you can't install tmaptools (I can't on one of my machines!)
fresh_spdf@data = data.frame(fresh_spdf@data, 
                            fresh_biome_re[match(as.character(fresh_spdf@data[,'Biome']), as.character(unlist(fresh_biome_re[,'Biome']))),])

fresh_spdf$Biome %>% levels()

fresh_spdf@data$id = rownames(fresh_spdf@data)
fresh_spdf.points = fortify(fresh_spdf, region="id")
fresh_spdf2 = plyr::join(fresh_spdf.points, fresh_spdf@data, by="id")


# all terrestrial/freshwater
terr_df <- bind_rows(terr_spdf2, fresh_spdf2) %>%
  mutate(Realm2 = 'Terrestrial/Freshwater') %>%
  as_tibble()

# slope ranef to qualitative (using deciles)
S_re <- BT_biome_ranef %>% filter(model=='S_pois') %>% select(Slope_ranef) %>% .$Slope_ranef
S_re_quantile <- quantile(S_re, probs = seq(0,1,by=0.2))

Jtu_re <- BT_biome_ranef %>% filter(model=='Jtu_norm') %>% select(Slope_ranef) %>% .$Slope_ranef
Jtu_re_quantile <- quantile(Jtu_re, probs = seq(0,1,by=0.2))

Jne_re <- BT_biome_ranef %>% filter(model=='Jne_norm') %>% select(Slope_ranef) %>% .$Slope_ranef
Jne_re_quantile <- quantile(Jtu_re, probs = seq(0,1,by=0.2))

# join terrestrial and marine data
all_df <- bind_rows(marine_df %>% select(-ECO_CODE), terr_df)

# cut the slope ranefs into bins
alldf <- all_df %>%
  mutate(S_re=cut(S_biome_re, breaks = c(S_re_quantile[1:3], 0, S_re_quantile[4:6]),
                  labels = c(1:6), include.lowest = TRUE),
         Jtu_re=cut(Jtu_biome_re, breaks = c(Jtu_re_quantile[1:3], 0, Jtu_re_quantile[4:6]),
                    labels = c(1:6), include.lowest = TRUE),
         Jne_re=cut(Jne_biome_re, breaks = c(Jne_re_quantile[1:3], 0, Jne_re_quantile[4:6]),
                    labels = c(1:6), include.lowest = TRUE)) %>%
  as_tibble()

# add these qualitative data to the location of each rarefyID
coords2 <- left_join(coords %>% 
                       mutate(Biome = gsub("_", " ", Biome),
                              # fix some other random ones up
                              Biome = ifelse(Biome=='Boreal Forests Taiga', 'Boreal Forests/Taiga', Biome),
                              Biome = ifelse(Biome=='Tropical and Subtropical Grasslands Savannas and Shrublands', 
                                             'Tropical and Subtropical Grasslands, Savannas and Shrublands', Biome),
                              Biome = ifelse(Biome=='Mediterranean Forests Woodlands and Scrub', 
                                             'Mediterranean Forests, Woodlands and Scrub', Biome),
                              Biome = ifelse(Biome=='Temperate Grasslands Savannas and Shrublands', 
                                             'Temperate Grasslands, Savannas and Shrublands', Biome)),
                      alldf %>% distinct(Biome, S_re, Jtu_re, Jne_re), by = 'Biome')


ab_concept_taxa_plot_allPoints <- inner_join(BT_taxa_estimate %>% select(Biome, taxa_mod, n_cells),
                                             ab_concept_taxa_plot_allPoints, by = c('Biome', 'taxa_mod')) %>%
  distinct(Biome, taxa_mod, hc_x, hc_y, .keep_all=TRUE)

ab_concept_taxa_plot <- inner_join(BT_taxa_estimate %>% select(Biome, taxa_mod, n_cells),
                                   ab_concept_taxa_plot, by = c('Biome', 'taxa_mod')) %>%
  distinct(Biome, taxa_mod, hc_x, hc_y, .keep_all=TRUE)

ab_concept_taxa_plot_allPoints <- ab_concept_taxa_plot_allPoints %>%
  mutate(realm_label = ifelse(REALM=='Marine', 'a.   Marine', 'b.   Terrestrial/Freshwater'))

