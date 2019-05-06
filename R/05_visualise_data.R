rm(list=ls())
# some plots of teh data the model were fit to
library(tidyverse)
library(brms)

#--------the data
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')


cell_count <- rarefied_medians %>%
  group_by(Biome, taxa_mod) %>%
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('Biome', 'taxa_mod'))

##	filter to count data and biome/taxa combinations with >3 cells
# these are the data that the model was fit to
rarefied_medians <- rarefied_medians %>%
  filter(BROAD_TYPE=='count' & n_cells > 3)

# do any cells have multiple rarefyID's
rarefied_medians %>% 
  group_by(cell) %>% 
  summarise(n_rarefyID = n_distinct(rarefyID),
            n_studyID = n_distinct(STUDY_ID)) %>% 
  filter(n_rarefyID > 2)

# create df with the unique rarefyIDs
newdat = rarefied_medians %>% 
               distinct(REALM, Biome, taxa_mod, STUDY_ID, rarefyID, 
                        Manual, SamplePool,
                        SampleN, num_years, 
                        duration, startYear, endYear, Distance, Manual)

# how many time series with duration > xx
newdat %>% filter(duration > 20)
newdat %>% filter(duration > 10)

# cell-counts per taxa
newdat %>% 
  mutate(total_cells = n_distinct(rarefyID)) %>% 
  group_by(REALM, Biome, taxa_mod) %>% 
  summarise(n_cell = n_distinct(rarefyID),
            percent_of_total = n_cell/unique(total_cells))# %>% 
  # write.csv(file = '~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Revision/figures/taxa_cell_count.csv')

# want counts of SL and ML studies 
load("/Users/sb25gaqy/Dropbox/1current/BioTime/local_code/hierarchical/1data/BioTIME_grid_filtered_011017.Rdata")

bt_grid_filtered %>% 
  filter(STUDY_ID %in% newdat$STUDY_ID) %>% 
  distinct(STUDY_ID, .keep_all = T) %>% 
  summarise(n.sl = sum(StudyMethod=='SL'),
            n.ml = sum(StudyMethod=='ML'))

# how many cells for SL versus ML studies
bt_grid_filtered %>% 
  filter(rarefyID %in% newdat$rarefyID) %>% 
  group_by(StudyMethod) %>% 
  summarise(n_timeSeries = n_distinct(rarefyID))


rm(bt_grid_filtered)

hist_duration <- ggplot() +
  geom_histogram(data = newdat %>% distinct(rarefyID, .keep_all = T),
                 aes(x = duration),
                 binwidth = 1) +
  scale_x_continuous(name = 'Duration of sampling (years)',
                     breaks = c(2,3,4,8,16,32,48,64,96),
                     #position = 'top'
                     labels = c('',3,4,8,16,32,48,64,96)) +
  # scale_y_continuous('Number of cells',
  #                    breaks = c(30000, 20000, 10000)) +
  # ylim(c(12000, 0)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()#,
        # axis.title.y.right = element_text(angle = 90)
        ) + 
  labs(tag = 'C',
       y = 'Number of cells') #+
  # coord_flip() 

ggplot() +
  geom_histogram(data = newdat %>% distinct(rarefyID, .keep_all = T) %>% 
                   filter(num_years==2),
                 aes(x = duration), binwidth = 1) +
  scale_x_continuous(name = 'Duration (years)',
                     breaks = c(2,3,4,8,16,32,48),
                     labels = c('',3,4,8,16,32,48)) +
  scale_y_continuous(name = 'Number of cells',
                     breaks = c(0, 500, 1000, 1500, 2000)) +
  labs(subtitle = 'Duration when number of years sampled = 2') +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14)) 
ggsave('~/Dropbox/BiogeoBioTIME/Biogeo Science submission/Biogeo Science Rev_2/figures/duration_when_num_years_two.png',
       width = 150, height = 150, units = 'mm')
hist_numYears <- ggplot() +
  geom_histogram(data = newdat %>% distinct(rarefyID, .keep_all = T),
                 aes(x = num_years),
                 binwidth = 2) +
  scale_x_continuous(name = 'Number of years sampled',
                     breaks = c(2,4,8,16,32,48,64,96)) +
  scale_y_continuous(breaks = c(0,2500,5000,7500,10000,12500, 15000, 17500, 20000,22500)) +
  labs(tag = 'B',
       y = 'Number of cells') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) 

hist_startYear <- ggplot() +
  geom_histogram(data = newdat %>% distinct(rarefyID, .keep_all = T),
                 aes(x = startYear),
                 binwidth = 2) +
  scale_y_continuous(breaks = c(0,2500,5000,7500)) +
  labs(tag = 'A',
       y = 'Number of cells',
       x = 'First year sampled') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) 


# ggplot() + 
#   geom_point(data = newdat,
#              aes(x = num_years, y = duration),
#              alpha = 0.1) +
#   scale_x_continuous('Number of years sampled',
#                      breaks = c(2,4,8,16,32,64,128)) +
#   scale_y_continuous('Duration of sampling (years)',
#                      breaks = c(2,4,8,16,32,64,128)) +
#   theme(panel.grid.minor = element_blank())

xy_density <- ggplot() + 
  # show density (see also geom_hex)
  geom_bin2d(data = newdat %>% distinct(rarefyID, .keep_all = T),
             aes(x = num_years, y = duration)) +
  scale_x_continuous('Number of years sampled',
                     breaks = c(2,4,8,16,32,48,64,96)) +
  scale_y_continuous('Duration of sampling (years)',
                     breaks = c(2,4,8,16,32,48,64,96),
                     labels = c(2,'',8,16,32,48,64,96)) +
  scale_fill_gradientn(name = 'Number of cells', 
                       trans = 'log2',
                      # breaks = c(2,16,128,1024,4112),
                      colours = c('#fdd49e',
                                  '#fdbb84', '#fc8d59',
                                  '#e34a33', '#b30000')) +
  labs(tag = 'D') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.79, 0.3),
        legend.box.background = element_rect(linetype = 1, size = 0.25, fill = 'white'),
        legend.background = element_blank())


cowplot::plot_grid(hist_startYear, hist_numYears, hist_duration, xy_density, nrow = 2, align = 'hv')
# ggsave('FigS2.png', width = 240, height = 180, units = 'mm')
# ggsave('FigS2.pdf', width = 240, height = 180, units = 'mm')

# how many rarefyID's had their biome assigned manually
newdat %>% 
  distinct(rarefyID, .keep_all = T) %>% 
  filter(Distance==0 & Manual==1)

