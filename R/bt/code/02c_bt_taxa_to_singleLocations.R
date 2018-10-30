# code to simplify geography for plotting taxa estimates: get one or more points for biome-taxa combinations
# one where the biome is geographically cohesive, >1 when biomes are geographically disjointed (terrestrial mainly)

# code to plot panel D of figure 5 (map of the relationship between alpha- and beta-diversity change)


# wrangle coefficients & calculate relationships between change in the different metrics
source('~/Dropbox/BiogeoBioTIME/Nature_Manuscript/R/bt/code/02b_bt_alpha_beta_relationships_wrangle.R')
  
world <- map_data('world')

# function to find the convex hull of a polygon
find_hull <- function(df) df[chull(df$rarefyID_x, df$rarefyID_y), ]
# function to find centre of hull
hull_centre <- function(rarefyID_x, rarefyID_y){
  hc_x = geosphere::geomean(cbind(x = rarefyID_x, y = rarefyID_y))[1]
  hc_y = geosphere::geomean(cbind(x = rarefyID_x, y = rarefyID_y))[2]
  return(cbind.data.frame(hc_x, hc_y))
}  

##  get unique coords for each biome, taxa combination
coords <- rarefied_medians %>% distinct(REALM, Biome, taxa_mod, rarefyID, rarefyID_x, rarefyID_y)

# visual inspection
# coords %>%
#   filter(REALM == 'Terrestrial') %>%
#   ggplot() +
#   facet_wrap(~Biome, scales ='free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(aes(x = rarefyID_x, y = rarefyID_y), size = 0.5) +
#   theme(legend.position = 'none')


# coords %>%
#   filter(REALM == 'Marine') %>%
#   ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   # geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(aes(x = rarefyID_x, y = rarefyID_y)) +
#   theme(legend.position = 'none')



##--------marine realm wrangle------------------
marine_hulls <- plyr::ddply(filter(coords, REALM=='Marine'),
                     plyr::.(REALM, Biome, taxa_mod), find_hull) %>%
  as_tibble()


marine_nest <- marine_hulls %>%
  group_by(REALM, Biome, taxa_mod) %>%
  filter(n_distinct(rarefyID) > 1) %>%
  nest(rarefyID_x, rarefyID_y) %>% 
  mutate(hull_centre = purrr::map(data, ~ hull_centre(.x$rarefyID_x, .x$rarefyID_y)))
  

mar_hull_cent <- marine_nest %>% unnest(hull_centre)
         

shapes = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
           'Plant' = 5, 'All' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)

shapes_mod2 = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
                'Plant' = 5, 'Multiple taxa' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8)
# ggplot() +
#   facet_wrap(~Biome) +
#   geom_polygon(data = marine_hulls, alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(data = filter(coords, REALM=='Marine'),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = mar_hull_cent,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   # coord_map('gilbert', xlim=c(-180, 180)) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')
# 
# # check tropics
# ggplot() +
#   facet_wrap(~Biome) +
#   geom_polygon(data = filter(marine_hulls, abs(rarefyID_y) < 23.5),  alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(data = filter(coords, REALM=='Marine' &  abs(rarefyID_y) < 23.5),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = filter(mar_hull_cent, abs(hc_y) < 23.5),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   coord_map('mollweide', xlim=c(-180, 180)) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')
# 
# # there is a handful of invertebrate data that cross -180/180 longitude,
# # that would be a problem for plotting polygons (hulls), the centres (points) look ok
# 
# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(marine_hulls, Biome=='Arctic'), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM=='Marine' & Biome=='Arctic'),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = filter(mar_hull_cent, Biome=='Arctic'),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')
# 
# 
# # check a tropical location
# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_polygon(data = filter(marine_hulls, Biome=='Tropical_Northwestern_Atlantic'), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM=='Marine' & Biome=='Tropical_Northwestern_Atlantic'),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = filter(mar_hull_cent, Biome=='Tropical_Northwestern_Atlantic'),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')
# 
# # 3 separate points for arctic inverts (or 1 pt )
arctic_invert_centre <- marine_nest %>% filter(Biome=='Arctic' & taxa_mod=='Invertebrates') %>%
  unnest(data) %>%
  # filter to region with most data
  filter(rarefyID_x < -100) %>%
  # nest and recalculate centre of small hull
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~ hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(marine_nest %>% filter(Biome=='Arctic' & taxa_mod=='Invertebrates') %>%
              unnest(data) %>%
              # filter to region with most data
              filter(rarefyID_x > -100) %>%
              mutate(
                hc_x = rarefyID_x,
                hc_y = rarefyID_y
              ))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(marine_hulls, Biome=='Arctic'), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   # 
#   geom_point(data = filter(coords, REALM=='Marine' & Biome=='Arctic'),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   # majority of the data are from north america
#   geom_point(data = arctic_invert_centre,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')


# combine with rest of data (after removing other point)
mar_hull_cent <- mar_hull_cent %>%
  filter(!(Biome=='Arctic' & taxa_mod=='Invertebrates')) %>%
  # keep only one point for the arctic invertebrates
  bind_rows(slice(arctic_invert_centre, 1))

mar_hull_cent_allPoints <- mar_hull_cent %>%
  filter(!(Biome=='Arctic' & taxa_mod=='Invertebrates')) %>%
  # keep only one point for the arctic invertebrates
  bind_rows(arctic_invert_centre)

# check
# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(marine_hulls, Biome=='Arctic'), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM=='Marine' & Biome=='Arctic'),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = filter(mar_hull_cent, Biome=='Arctic'),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

#---marine map...coming soon .gif?, leaflet?, shiny the whole model?
# ggplot() +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(data = mar_hull_cent,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   coord_equal(xlim=c(-180,180)) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

##----------terrestrial/freshwater-------------
# coords %>%
#   filter(REALM == 'Terrestrial' & Biome=='Temperate_Conifer_Forests') %>%
#   ggplot() +
#   facet_wrap(~Biome, scales ='free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5) +
#   theme(legend.position = 'top', legend.direction = 'horizontal') 

terr_hulls <- plyr::ddply(filter(coords, REALM!='Marine'),
                          plyr::.(REALM, Biome, taxa_mod), find_hull) %>%
  as_tibble()

terr_nest <- terr_hulls %>%
  group_by(REALM, Biome, taxa_mod) %>%
  filter(n_distinct(rarefyID) > 1) %>%
  nest(rarefyID_x, rarefyID_y) %>% 
  mutate(hull_centre = purrr::map(data, ~ hull_centre(.x$rarefyID_x, .x$rarefyID_y)))


terr_hull_cent <- terr_nest %>% unnest(hull_centre)

# # visual inspection
# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   # geom_polygon(data = terr_hulls, alpha = 0.5,
#   #              aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   # geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_point(data = filter(coords, REALM!='Marine'),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = terr_hull_cent,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   stat_density2d(data = terr_hulls, alpha = 0.5, geom = 'raster',
#                aes(x = rarefyID_x, rarefyID_y, fill=taxa_mod, group = taxa_mod), contour = FALSE) +
#   scale_shape_manual(values = shapes) +
#   # coord_map('gilbert', xlim=c(-180, 180)) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

# lots of biome/taxa combinations need > 1 point for each taxa
# these ones don't
good_terr_hulls <- terr_hulls %>%
  filter(Biome=='large_lakes' | Biome == 'Mangroves' | Biome=='temperate_coastal_rivers')

good_terr_hullCent <- terr_hulls %>%
  filter(Biome=='large_lakes' | Biome == 'Mangroves' | Biome=='temperate_coastal_rivers') %>%
  select(-rarefyID) %>%
  group_by(REALM, Biome, taxa_mod) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre)

# check these good hulls? Only one centre in each.
# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(good_terr_hulls), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome %in% good_terr_hulls$Biome),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = filter(good_terr_hullCent),
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

# fix bad ones one at a time because I am slow
bad_hulls <- terr_hulls %>% distinct(Biome) %>% filter(!(Biome=='large_lakes' | Biome == 'Mangroves' | Biome=='temperate_coastal_rivers'))


#------bad hull 1-----
b1_fix_hc <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[1]) %>%
  mutate(hc_x = rarefyID_x, 
         hc_y = rarefyID_y)

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[1]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   # there are two points in the antarctic, use it for single biome/taxa representative
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[1]), 
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = b1_fix_hc,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b1_hull1 <- b1_fix_hc %>% slice(1)


#----bad hull 2-----------
b2_fix_hc <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[2]) %>%
  select(-rarefyID) %>%
  filter(rarefyID_x < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[2] & rarefyID_x > 0) %>% 
    mutate(hc_x = rarefyID_x, 
           hc_y = rarefyID_y))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[2]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   # most of the data are from north america
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[2]), 
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = b2_fix_hc,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b2_hull1 <- b2_fix_hc %>% slice(1)


#----bad hull 3-----------
b3_fix_hc <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[3]) %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > -84) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[3] & rarefyID_x < -84) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[3]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   # most of the data are clustered in the southern point
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[3]), 
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   geom_point(data = b3_fix_hc,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b3_hull1 <- b3_fix_hc %>% slice(1)


#----bad hull 4-----------

b4_fix_hc_plant <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[4] & taxa_mod=='Plant') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x < -100) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[4] & taxa_mod=='Plant' & rarefyID_x > -84) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b4_fix_hc_birds <- terr_hull_cent %>% 
  filter(Biome==bad_hulls$Biome[4] & taxa_mod=='Birds') %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[4] & taxa_mod=='Birds' & rarefyID_x > 0 ) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y)) %>%
  # plus two cells that were captured inside the convex hull, but are geographically isolated
  bind_rows(coords %>% filter(Biome==bad_hulls$Biome[4] & taxa_mod=='Birds' & rarefyID_x>0 & rarefyID_x < 50) %>%
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b4_fix_hc_amph <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[4] & taxa_mod=='Amphibians') %>%
  select(-rarefyID) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) 

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[4]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[4]), 
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   # plant data are mostly from the most easterly observations
#   geom_point(data = b4_fix_hc_plant,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   # most of the bird data are from north america
#   geom_point(data = b4_fix_hc_birds,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   # only one amphibian centre
#   geom_point(data = b4_fix_hc_amph,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b4_hull1_plant <- b4_fix_hc_plant %>% slice(1)
b4_hull1_bird <- b4_fix_hc_birds %>% slice(1)
b4_hull1_amp <- b4_fix_hc_amph %>% slice(1)

#----bad hull 5-----------
b5_fix_hc <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[5]) %>%
  select(-rarefyID) %>%
  filter(rarefyID_y < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[5] & rarefyID_y > 0) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[5]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[5]), 
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   # 21 north america vs 18 africa
#   geom_point(data = b5_fix_hc,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

# north america
b5_hull1 <- b5_fix_hc %>% slice(2)

#----bad hull 6-----------
b6_fix_hc_plant <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[6] & taxa_mod=='Plant') %>%
  select(-rarefyID) %>%
  filter(rarefyID_y < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[6] & taxa_mod == 'Plant' & rarefyID_y > 0) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b6_fix_hc_bird <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[6] & taxa_mod=='Birds') %>%
  select(-rarefyID) %>%
  filter(rarefyID_y < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[6] & taxa_mod == 'Birds' & rarefyID_y > 0) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[6]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[6]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  most of the plant data are from africa
#   geom_point(data = b6_fix_hc_plant,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   # most of the bird data are from africa
#   geom_point(data = b6_fix_hc_bird,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b6_hull1_plant <- b6_fix_hc_plant %>% slice(1)
b6_hull1_bird <- b6_fix_hc_bird %>% slice(1)

#----bad hull 7-----------
b7_fix_hc_bird <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[7]) %>%
  select(-rarefyID) %>%
  filter(rarefyID_y < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[7] & rarefyID_y > 0) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[7]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[7]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  most of the bird data are from africa
#   geom_point(data = b7_fix_hc_bird,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b7_hull1_bird <- b7_fix_hc_bird %>% slice(1)


#----bad hull 8-----------
b8_fix_hc_amph <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Amphibians') %>%
  select(-rarefyID) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre)

b8_fix_hc_birds <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Birds') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 100) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Birds' & rarefyID_x < 0) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre)) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Birds' & rarefyID_y > 50) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b8_fix_hc_invert <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Invertebrates') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 100) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Invertebrates' & rarefyID_x < 100) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b8_fix_hc_mamm <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Mammals') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 100) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Mammals' & rarefyID_x < 100) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b8_fix_hc_plant <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Plant') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[8] & taxa_mod=='Plant' & rarefyID_x < 0) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[8]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[8]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  only one amphibian
#   geom_point(data = b8_fix_hc_amph,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the bird data are from north america
#   geom_point(data = b8_fix_hc_birds,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  NA = 2, -50 < x < 0 = 23; 0 < x < 50 = 16; x > 50 = 22.
#   geom_point(data = b8_fix_hc_invert,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the mammal data are from japan
#   geom_point(data = b8_fix_hc_mamm,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the plant data are from europe
#   geom_point(data = b8_fix_hc_plant,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b8_hull1_amph <- b8_fix_hc_amph %>% slice(1)
b8_hull1_birds <- b8_fix_hc_birds %>% slice(2)
b8_hull1_invert <- b8_fix_hc_invert %>% slice(4)
b8_hull1_mamm <- b8_fix_hc_mamm %>% slice(1)
b8_hull1_plant <- b8_fix_hc_plant %>% slice(1)

#----bad hull 9-----------

b9_fix_hc_invert <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Invertebrates') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 140) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Invertebrates' & rarefyID_x < 140 ) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b9_fix_hc_plants <-terr_hulls %>%
  filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Plant') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Plant' & rarefyID_x > 0 ) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre))

b9_fix_hc_birds <-terr_hulls %>%
  filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Birds') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Birds' & rarefyID_x > 0 & rarefyID_y > 40) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre)) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[9] & taxa_mod=='Birds' & rarefyID_x > 0 & rarefyID_y < 40) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[9]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[9]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  most of the bird data are from north america
#   geom_point(data = b9_fix_hc_birds,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the invert data are from the north of Japan
#   geom_point(data = b9_fix_hc_invert,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the plant data are from north america
#   geom_point(data = b9_fix_hc_plants,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b9_hull1_birds <- b9_fix_hc_birds %>% slice(1)
b9_hull1_invert <- b9_fix_hc_invert %>% slice(1)
b9_hull1_plant <- b9_fix_hc_plants %>% slice(1)

#----bad hull 10-----------
b10_fix_hc_birds <-terr_hulls %>%
  filter(Biome==bad_hulls$Biome[10]) %>%
  select(-rarefyID) %>%
  filter(rarefyID_x < 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[10] & rarefyID_x > 0) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[10]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[10]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  most of the bird data are from north america
#   geom_point(data = b10_fix_hc_birds,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b10_hull1_birds <- b10_fix_hc_birds %>% slice(1)


#----bad hull 11-----------

b11_fix_hc_birds <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[11] & taxa_mod=='Birds') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[11] & taxa_mod=='Birds' & rarefyID_x < 0) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y))

b11_fix_hc_plants <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[11] & taxa_mod=='Plant') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 0) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[11] & taxa_mod=='Plant' & rarefyID_x < 0) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre))

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[11]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[11]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  most of the bird data are from africa
#   geom_point(data = b11_fix_hc_birds,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the plant data are from south america
#   geom_point(data = b11_fix_hc_plants,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b11_hull1_birds <- b11_fix_hc_birds %>% slice(1)
b11_hull1_plant<- b11_fix_hc_plants %>% slice(2)


#-------bad hull 12-------
b12_fix_hc_amph <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Amphibians') %>%
  select(-rarefyID) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) 

b12_fix_hc_plants <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Plant') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 120) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Plant' & rarefyID_x > 100 & rarefyID_x < 120) %>% 
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y)) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Plant' & rarefyID_x > 0 & rarefyID_x < 100) %>%             
              mutate(hc_x = rarefyID_x, 
                     hc_y = rarefyID_y)) %>%
  # need to get inside the hull to find some new centres/points
  bind_rows(coords %>% filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Plant' & rarefyID_x < -45 & rarefyID_x > -52) %>%             
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre)) %>%
  # need to get inside the hull to find some new centres/points
  bind_rows(coords %>% filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Plant' & rarefyID_x < -60) %>%             
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre)) 

b12_fix_hc_birds <- terr_hulls %>%
  filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Birds') %>%
  select(-rarefyID) %>%
  filter(rarefyID_x > 0 & rarefyID_x < 50) %>%
  nest(rarefyID_x, rarefyID_y) %>%
  mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
  unnest(hull_centre) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Birds' & rarefyID_x < 0 ) %>% 
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre)) %>%
  bind_rows(terr_hulls %>% filter(Biome==bad_hulls$Biome[12] & taxa_mod=='Birds' & rarefyID_x > 100) %>%             
              select(-rarefyID) %>%
              nest(rarefyID_x, rarefyID_y) %>%
              mutate(hull_centre = purrr::map(data, ~hull_centre(.x$rarefyID_x, .x$rarefyID_y))) %>%
              unnest(hull_centre)) 

# ggplot() +
#   facet_wrap(~Biome, scales = 'free') +
#   geom_map(data=world, map=world, aes(long, lat, map_id=region), colour='white', fill='#7f7f7f', size=0.05, alpha=0.5) +
#   geom_polygon(data = filter(terr_hulls, Biome==bad_hulls$Biome[12]), alpha = 0.5,
#                aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
#   geom_point(data = filter(coords, REALM!='Marine' & Biome==bad_hulls$Biome[12]),
#              aes(x = rarefyID_x, y = rarefyID_y, colour = taxa_mod), size = 0.5, alpha = 0.8) +
#   #  only one amphibian centre
#   geom_point(data = b12_fix_hc_amph,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the bird data are from africa
#   geom_point(data = b12_fix_hc_birds,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   #  most of the plant data are from australia
#   geom_point(data = b12_fix_hc_plants,
#              aes(x = hc_x, y = hc_y, shape = taxa_mod)) +
#   scale_shape_manual(values = shapes) +
#   theme(legend.position = 'top', legend.direction = 'horizontal')

b12_hull1_amph <- b12_fix_hc_amph %>% slice(1)
b12_hull1_birds <- b12_fix_hc_birds %>% slice(1)
b12_hull1_plant<- b12_fix_hc_plants %>% slice(1)

#-------- problem hulls potentially remain graphically, e.g., more than one taxa per biome---------
# terr_taxa_hullCent <- bind_rows(good_terr_hullCent,
#                                 b1_fix_hc, b2_fix_hc, b3_fix_hc, b4_fix_hc_amph, b4_fix_hc_birds, b4_fix_hc_plant, b5_fix_hc, 
#           b6_fix_hc_bird, b6_fix_hc_plant, b7_fix_hc_bird, b8_fix_hc_amph, b8_fix_hc_birds, b8_fix_hc_invert, b8_fix_hc_mamm, b8_fix_hc_plant, 
#           b9_fix_hc_invert, b9_fix_hc_plants, b9_fix_hc_birds, b10_fix_hc_birds,
#           b11_fix_hc_birds, b11_fix_hc_plants,
#           b12_fix_hc_amph, b12_fix_hc_plants, b12_fix_hc_birds)   

# one point per taxa per biome
terr_taxa_hullCent <- bind_rows(good_terr_hullCent,
                                b1_hull1, b2_hull1, b3_hull1, b4_hull1_amp, b4_hull1_bird, b4_hull1_plant,
                                b5_hull1, b6_hull1_bird, b6_hull1_plant, b7_hull1_bird, 
                                b8_hull1_plant, b8_hull1_mamm, b8_hull1_invert, b8_hull1_birds, b8_hull1_amph,
                                b9_hull1_plant, b9_hull1_invert, b9_hull1_birds,
                                b10_hull1_birds, b11_hull1_plant, b11_hull1_birds, 
                                b12_hull1_plant, b12_hull1_birds, b12_hull1_amph)   

# keep all locations for each taxa (per biome)
terr_taxa_hullCent_allPoints <- bind_rows(good_terr_hullCent,
                                b1_fix_hc, b2_fix_hc, b3_fix_hc, 
                                b4_fix_hc_amph, b4_fix_hc_birds, b4_fix_hc_plant,
                                b5_fix_hc, b6_fix_hc_bird, b6_fix_hc_plant,
                                b7_fix_hc_bird, b8_fix_hc_amph, b8_fix_hc_birds, b8_fix_hc_invert, b8_fix_hc_mamm, b8_fix_hc_plant,
                                b9_fix_hc_birds, b9_fix_hc_invert, b9_fix_hc_plants,
                                b10_fix_hc_birds, b11_fix_hc_birds, b11_fix_hc_plants, 
                                b12_fix_hc_amph, b12_fix_hc_birds, b12_fix_hc_plants)

# fix biome names for cleaner plot
coords <- coords %>%
  mutate(Biome2 = gsub("_", " ", Biome))
terr_taxa_hullCent <- terr_taxa_hullCent %>%
  mutate(Biome2 = gsub("_", " ", Biome))

terr_taxa_hullCent_allPoints <- terr_taxa_hullCent_allPoints %>%
  mutate(Biome2 = gsub("_", " ", Biome))


##------combined marine and terrestrial map--------------

# want to combine the biome/taxa correlation data with these geographic data
# the difference between these two dataframes is that _allPoints show taxa-level estimates in all 
# parts of the biomes in which they were observed (rather than reducing to a single point, as for the other dataframe)
biome_taxa_simpleGeogr <- bind_rows(terr_taxa_hullCent, mar_hull_cent) %>% select(REALM, Biome, taxa_mod, hc_x, hc_y)
biome_taxa_simpleGeogr_allPoints <- bind_rows(terr_taxa_hullCent_allPoints, mar_hull_cent_allPoints) %>% select(REALM, Biome, taxa_mod, hc_x, hc_y)

ab_concept_taxa_plot <- inner_join(taxa_corr %>% 
                                     select(Biome, taxa_mod, deltaJbeta, deltaJbeta_lower, deltaJbeta_upper, 
                                            deltaJne, deltaJne_lower, deltaJne_upper, deltaJtu, deltaJtu_lower,deltaJtu_upper,
                                            deltaS, deltaS_lower,deltaS_upper,
                                            quad, alphaS, alphaJtu, alphaJne), 
                                   biome_taxa_simpleGeogr)

ab_concept_taxa_plot_allPoints <- inner_join(taxa_corr %>% 
                                     select(Biome, taxa_mod, deltaJbeta, deltaJbeta_lower, deltaJbeta_upper, 
                                            deltaJne, deltaJne_lower, deltaJne_upper, deltaJtu, deltaJtu_lower,deltaJtu_upper,
                                            deltaS, deltaS_lower,deltaS_upper,
                                            quad, alphaS, alphaJtu, alphaJne),
                                   biome_taxa_simpleGeogr_allPoints)
# add indicator to change point stroke as a function of significant trends
ab_concept_taxa_plot <- ab_concept_taxa_plot %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 1.1, 1),
         sizeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 2.5, .5),
         sizeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 2.5, .5))

ab_concept_taxa_plot_allPoints <- ab_concept_taxa_plot_allPoints %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         # set stroke and size for points where both alpha and beta differ from zero
         strokeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 1.1, 1),
         strokeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 1.1, 1),
         sizeBLUE = ifelse(((quad=='c1' | quad=='c2') & alphaS==1 & alphaJtu==1), 2.5, .25),
         sizeGREEN = ifelse(((quad=='c3' | quad=='c4') & alphaS==1 & alphaJne==1), 2.5, .25))


# combine terrestrial and freshwater for plotting
ab_concept_taxa_plot <- ab_concept_taxa_plot %>%
  mutate(realm2 = ifelse(REALM=='Marine', 'Marine', 'Terrestrial/Freshwater'))
ab_concept_taxa_plot_allPoints <- ab_concept_taxa_plot_allPoints %>%
  mutate(realm2 = ifelse(REALM=='Marine', 'Marine', 'Terrestrial/Freshwater'))

# tidy taxa names: All to Multiple
ab_concept_taxa_plot$taxa_mod2 <- factor(ab_concept_taxa_plot$taxa_mod, 
                                         levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "All", "Marine invertebrates/plants", "Mammals"),
                                         labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "Multiple taxa", "Marine invertebrates/plants", "Mammals"))
ab_concept_taxa_plot_allPoints$taxa_mod2 <- factor(ab_concept_taxa_plot_allPoints$taxa_mod, 
                                         levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "All", "Marine invertebrates/plants", "Mammals"),
                                         labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                    "Multiple taxa", "Marine invertebrates/plants", "Mammals"))

# join all the unique rarefyIDs with 
coords_quad <- inner_join(coords, taxa_corr %>% select(Biome, taxa_mod, quad, alphaS))


# put the alpha-beta diversity concept on a map 
alpha_beta_map <- ggplot() +
  # facet_wrap(~Biome) +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#cccccc', size=0) +
  # geom_polygon(data = filter(terr_hulls, Biome=='Boreal_Forests_Taiga'), alpha = 0.5,
  #              aes(x = rarefyID_x, rarefyID_y, fill = taxa_mod)) +
  # geom_point(data = filter(coords),
  #            aes(x = rarefyID_x, y = rarefyID_y, shape = taxa_mod), size = 0.2, alpha = 0.1) +
  geom_point(data = filter(ab_concept_taxa_plot_allPoints, quad != 'c3' & quad != 'c4'),
             aes(x = hc_x, y = hc_y, shape = taxa_mod2, colour = quad, stroke = strokeBLUE, size = sizeBLUE),
             position = position_jitter(width = 0.25, height = 0.25)) +
  geom_point(data = filter(ab_concept_taxa_plot_allPoints, quad == 'c3' | quad == 'c4'),
             aes(x = hc_x, y = hc_y, shape = taxa_mod2, colour = quad, stroke = strokeGREEN, size = sizeGREEN),
             position = position_jitter(width = 0.25, height = 0.25)) +
  # geom_hline(yintercept = 23.5, lty = 2) +
  # geom_hline(yintercept = -23.5, lty = 2) +
  scale_shape_manual(name = 'Taxa', values = shapes_mod2) +
  scale_color_manual(guide = FALSE, values = quad_col) +
  scale_size_area(guide = FALSE) +
  xlab('') +
  ylab('') +
  theme_bw() +
  scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(breaks = c(0, -60, -23.5, 23.5, 60)) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  labs(subtitle = 'd') +
  # coord_map('mollweide', xlim = c(-180, 180)) +
  theme(axis.text = element_text(size = 14), panel.grid.minor = element_blank(),
        legend.position = c(0.2, 0.3), legend.direction = 'horizontal', 
        legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = 'transparent'),
        plot.subtitle = element_text(size = 14, face = 'bold')) +
  guides(shape = guide_legend(title.position = 'top', ncol = 2, override.aes = list(size = 2)))
