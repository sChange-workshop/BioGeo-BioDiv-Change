# code to create and check new regional grouping structure:
# marine realm to combine biome_taxa
# terrestrial and freshwater to combine continent and taxa
library(tidyverse)
library(sp)
library(rworldmap)

# Want to know how many studies in each group 
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')


# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

continents <- rarefied_medians %>% 
  distinct(REALM, Biome, STUDY_ID, rarefyID, rarefyID_x, rarefyID_y) %>% 
  nest(rarefyID_x, rarefyID_y) %>% 
  mutate(continent = purrr::map(data, ~coords2continent(data.frame(.x$rarefyID_x, .x$rarefyID_y))))

continents <- continents %>% 
  unnest(data, continent)

world = map_data('world')
# visual check
ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#f0f0f0', size=0) +
  geom_point(data = filter(continents, REALM == 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = Biome), size = 0.5, stroke = 1.2)+
  scale_color_viridis_d(guide = F)

# no problems for the marine biomes
filter(continents, REALM == 'Marine') %>% 
  filter(is.na(Biome) | is.na(rarefyID_x) | is.na(rarefyID_y))

ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#d9d9d9', size=0) +
  geom_point(data = filter(continents, REALM != 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = continent), size = 0.5, stroke = 1.2) 

# need to fix the NAs by hand
ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#d9d9d9', size=0) +
  geom_point(data = filter(continents, REALM != 'Marine' ) %>% #& is.na(continent)) %>% 
               # filter(is.na(continent) & REALM !='Marine' & (rarefyID_y > 0 & rarefyID_x < -50)),
             # filter(is.na(continent) & REALM !='Marine' & (rarefyID_y < -25 & (rarefyID_x < 50 & rarefyID_x > 0))),
             # filter(is.na(continent) & REALM !='Marine' & (rarefyID_y) > 50 & abs(rarefyID_x) < 10),
             # filter(is.na(continent) & REALM !='Marine' & (rarefyID_y > 25 & rarefyID_x > 50)),
             # filter(is.na(continent) & REALM !='Marine' & (rarefyID_y < -25 & rarefyID_x > 150)),
               # filter(continent=='South America' & rarefyID_y > 5),
               filter(REALM!='Marine' & continent=='Europe' & rarefyID_y > 25 & rarefyID_x > 50),
             aes(x = rarefyID_x, y = rarefyID_y), size = 0.5, stroke = 1.2)

# fix NAs in north american
na1 <- continents %>% 
  filter(is.na(continent) & REALM !='Marine' & (rarefyID_y > 0 & rarefyID_x < -50)) %>% 
  mutate(continent = 'North America')

# fix Africa
na2 <- continents %>% 
  filter(is.na(continent) & REALM !='Marine' & (rarefyID_y < -25 & (rarefyID_x < 50 & rarefyID_x > 0))) %>% 
  mutate(continent = 'Africa')

# fix Europe
na3 <- continents %>% 
  filter(is.na(continent) & REALM !='Marine' & (rarefyID_y) > 50 & abs(rarefyID_x) < 10) %>% 
  mutate(continent = 'Europe')

# fix Asia
na4 <- continents %>% 
  filter(is.na(continent) & REALM !='Marine' & (rarefyID_y > 25 & rarefyID_x > 50)) %>% 
  mutate(continent = 'Asia')

# fix Oceania
na5 <- continents %>% 
  filter(is.na(continent) & REALM !='Marine' & (rarefyID_y < -25 & rarefyID_x > 150)) %>% 
  mutate(continent = 'Australia')

# fix the central american points to be north instead of south america
c2n <- continents %>% 
  filter(REALM!='Marine' & continent=='South America' & rarefyID_y > 5) %>% 
  mutate(continent = 'North America')

# fix the european points that are in asia
e2a <- continents %>% 
  filter(REALM!='Marine' & continent=='Europe' & rarefyID_y > 25 & rarefyID_x > 50) %>% 
  mutate(continent = 'Asia')

# replace the rows 
continent_fix <- continents %>% 
  filter(!(rarefyID %in% na1$rarefyID)) %>% 
  filter(!(rarefyID %in% na2$rarefyID)) %>% 
  filter(!(rarefyID %in% na3$rarefyID)) %>% 
  filter(!(rarefyID %in% na4$rarefyID)) %>% 
  filter(!(rarefyID %in% na5$rarefyID)) %>% 
  filter(!(rarefyID %in% c2n$rarefyID)) %>% 
  filter(!(rarefyID %in% e2a$rarefyID)) %>% 
  bind_rows(na1, na2, na3, na4, na5, c2n, e2a)

# row check
nrow(continent_fix)==nrow(continents)

# visual check
ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#d9d9d9', size=0) +
  geom_point(data = filter(continent_fix, REALM != 'Marine'),
             aes(x = rarefyID_x, y = rarefyID_y, colour = continent), size = 0.5, stroke = 1.2) 

# make new region column: biomes for marine, continents for terrestrial and freshwater 
rarefied_medians <- rarefied_medians %>% 
  left_join(continent_fix %>% select(Biome, continent, rarefyID), 
            by = c('Biome', 'rarefyID'))

rarefied_medians <- rarefied_medians %>% 
  mutate(region = ifelse(REALM=='Marine', Biome, continent))

save(rarefied_medians, file = '~/Desktop/revision_code/rarefied_medians_continents.Rdata')

ggplot() +
  geom_polygon(data=world, aes(long, lat, group = group), colour=NA, fill='#d9d9d9', size=0) +
  geom_point(data = distinct(rarefied_medians, rarefyID, .keep_all = T),
             aes(x = rarefyID_x, y = rarefyID_y, colour = rlm_region_taxa), size = 0.5, stroke = 1.2) +
  theme(legend.position = 'none')
