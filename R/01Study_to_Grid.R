##======================================================================
##	Before rarefaction we classify studies as having samples 
##	from one or more locations in space: Single Location (SL) and 
##	many location (ML). We use this classification, and the mean size of
##  SL studies to grid all other ML studies to a similar extent, using dggrid.
##  New cell centres are estimated based on rarefyID (study_cell).
##  Gridded studies are then further filtered based on coverage, omitting years
##  below the threshold for abundance data, and entire "rarefyID"s below the 
##  threshold for presence and biomass data.

# Primary Code Authors: Shane Blowes and Sarah Supp
# Email: sablowes@gmail.com, sarah@weecology.org
# DISCLAIMER: code is under development
# 07/03/2017

# Input is the BioTIME database and metadata

# Coverage: 
# Abundance-based coverage is calculated as C-hat corrected (Eqn 4a, Chao & Jost, Ecology)
# Incidence-based coverage is calculated as C-hat corrected, using years as samples 
#   (Eqn Chat_sample(T) from Table 2, Chao et al. 2014)
# Note that the resolution of coverage (and omission) differs for abundance data vs. presence and biomass data

# Output is a gridded version of the BioTIME database, with a new column for rarefyID (the "studyID" to use henceforth)
#  that has been filtered for adequate abundance- or incidence-based coverage.
#   BioTIME_grid_filtered.Rdata (data.frame to be used for rarefaction in rarefy_griddedData_allmetrics.R)
##======================================================================

rm(list=ls())
##	
library(tidyverse)
library(dggridR)
library(scales)
library(vegan)
library(iNEXT)

##	get the data and metadata
##	Get the raw data locally 
bt <- read_csv('~/Dropbox/BioTIMELatest/BioTIMEQOctober.csv')
#bt <- read_csv('/home/sarah/Dropbox/BioTIMELatest/bioTIMEFeb.csv')
#bt <- read_csv('/Users/HotStuff/Dropbox/BioTIMELatest/bioTIMEFeb.csv')

##	Get the meta data locally
meta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEmetadataJune.csv')
#meta <- read.csv('/home/sarah/Dropbox/BioTIMELatest/bioTIMEmetadataFeb.csv')
#meta <- read.csv('/Users/HotStuff/Dropbox/BioTIMELatest/bioTIMEmetadataFeb.csv')

##	join abundance records with the metadata
bt <- inner_join(meta, bt, by='STUDY_ID')

# Add columns for Species species name 
bt <- bt %>% unite(col=Species, GENUS, SPECIES, remove=FALSE)

##======================================================================
## 	IDENTIFY SL & ML studies in the data: 
##	Single Location [SL] have only 1 geographic coordinate for the whole study
##	Many Locations [ML] have many coordinates
meta <- meta %>%
  mutate(StudyMethod = ifelse(NUMBER_LAT_LONG == 1, "SL", NA))

bt <- bt %>%
  mutate(StudyMethod = ifelse(NUMBER_LAT_LONG == 1, 'SL', 'ML'))

##	change ML studies with small extent to SL 
##	(i.e., ML studies with extent < mean(extent)+sd(extent) of SL studies) 

##  First, look at frequency distribution of SL studies, and check for any extreme outliers
##	  SB: The extent of STUDY_ID==337 is 15259km2. All other SL have extent ≤ 500km2
##	  Including this study increases the mean by an order of magnitude (110km2 vs 11km2)
##	  and the sd goes from 53km2 to 1233km2. Our grid increases from ~32km2 to ~864km2 
ggplot(meta[meta$StudyMethod=="SL",], aes(AREA_SQ_KM)) + geom_histogram()
ggplot(meta[meta$StudyMethod=="SL",], aes(AREA_SQ_KM)) + geom_histogram(aes(fill = as.numeric(AREA_SQ_KM) > 500 )) + scale_x_log10()
meta %>% filter(StudyMethod=='SL' & AREA_SQ_KM > 500) %>% select(TITLE, STUDY_ID, NUMBER_LAT_LONG, AREA_SQ_KM)

##  Calculate the extent and mean for SL studies, without the outlier
SL_extent_mean <- meta %>% filter(StudyMethod=='SL' & AREA_SQ_KM<=500) %>% 
	summarise(extent_mean = mean(AREA_SQ_KM, na.rm=TRUE)) %>% .$extent_mean
SL_extent_sd <- meta %>% filter(StudyMethod=='SL' & AREA_SQ_KM<=500) %>% 
	summarise(extent_sd = sd(AREA_SQ_KM, na.rm=TRUE)) %>% .$extent_sd
	
##	change the MLs into SLs that satisfy the criterion (< mean + sd)
bt$StudyMethod <- with(bt, ifelse(AREA_SQ_KM < (SL_extent_mean+SL_extent_sd), 'SL', StudyMethod))

##	We'll want to 'grid' SL and ML studies differently, add new coords to dataframe
## 	If StudyMethod=='SL', want to use the central lat and long, else
##	ML uses observed coords for each observation
bt <- bt %>% 
	mutate(lon_to_grid = ifelse(StudyMethod=='SL', CENT_LONG, LONGITUDE),
		lat_to_grid = ifelse(StudyMethod=='SL', CENT_LAT, LATITUDE))

##======================================================================
##	other checks to filter data before gridding?
## 	studies with only ONE YEAR of data (since we do rarefaction on a yearly scale)
oneyear <- bt %>% 
  group_by(STUDY_ID) %>%
  filter(max(YEAR)-min(YEAR)==0) %>%
  summarise() %>%
  collect %>% .[["STUDY_ID"]]
bt <- bt %>% filter(!(STUDY_ID %in% oneyear))

##======================================================================
##	create a global grid with cells approximately equal to extent +/- sd of the 'true' SL studies?

dgg <- dgconstruct(res=12)

## 	determine the resolution closest to our cutoff point for SL vs ML studies 
res <- dg_closest_res_to_area(dgg, SL_extent_mean+SL_extent_sd)

##	set the resolution 
dgg <- dgsetres(dgg, res)

##	get the corresponding grid cells for all observations
bt <- bt %>% mutate(cell = dgtransform(dgg, lat=lat_to_grid, lon=lon_to_grid))

##	what just happened?
check <- bt %>% group_by(StudyMethod, STUDY_ID) %>% summarise(n_cell = n_distinct(cell))

##	do all SL studies have one grid cell?
if (sum(dplyr::filter(check, StudyMethod=='SL') %>% .$n_cell != 1)==0) { 
  print("all SL studies have 1 grid cell") 
  } else { print("ERROR: some SL studies have > 1 grid cell") }

##	ok, how many cells/year/study are there? (e.g. how spread out were samples in a given study and year?)
check2 <- bt %>% 
  group_by(StudyMethod, STUDY_ID, YEAR) %>% 
  summarise(n_cell = n_distinct(cell))

range(check2$n_cell)

##	Some of these studies are spread out across MANY cells (>5000!?)
ggplot(filter(check2, StudyMethod=='ML')) +
	geom_histogram(aes(n_cell, fill = n_cell > 1000), binwidth=500) + xlab("Number of cells in a study per year") + scale_y_log10()
	
##======================================================================
##	in order to calculate new centres we need this new bt object
##	add rarefyID:
bt <- bt %>% unite(col=rarefyID, STUDY_ID, cell, sep="_", remove=FALSE)
#save(bt, dgg, file='biotime_cells.Rdata')
#load('~/Desktop/current/BioTime/data/biotime_cells_res11.Rdata')

##	want to calculate a single coordinate (i.e., the centre) for each rarefyID to place rarefyID's
##	within ecoregions....
##	also calculate cell_extent = area in convex hull (polygon) of points within a rarefyID. We can use this
##	to calculate a new 'extent' for each study AreaSum = sum(cell_extent) Issue #7 github
rarefyID_coords_nest <- ungroup(bt) %>%
	##	we don't need to do anything with the SL studies
	filter(StudyMethod!='SL') %>%
	##	select columns
	select(STUDY_ID, rarefyID, LONGITUDE, LATITUDE) %>%
	##	retain only uniqe locations within rarefyIDs (may want to change this if we want to weight the calculation of the centre)
	distinct(rarefyID, LONGITUDE, LATITUDE, .keep_all=TRUE) %>% 
	##	there are also some rarefyID's with only one unique geographic
	group_by(rarefyID) %>%
	mutate(n_locations = n_distinct(LONGITUDE,LATITUDE)) %>%
	ungroup() %>%
	##	drop rarefyIDs with only one location
	filter(n_locations > 1) %>%
	##	drop our location counter
	select(-n_locations) %>%
	##	group & nest 
	group_by(STUDY_ID, rarefyID) %>%
	nest()

##	i can't get purrr::map and chull to play nice so a loop it is! This takes awhile....
cell_extent <- numeric()
centre_rarefyID_x <- numeric()
centre_rarefyID_y <- numeric()
vertices_check <- data.frame()
for(i in 1:nrow(rarefyID_coords_nest)){
	##	sanity check
    print(paste('rarefyID', i, 'out of', length(unique(rarefyID_coords_nest$rarefyID))))
    ##	put a convex hull around the coords  
	hull = chull(x=unlist(rarefyID_coords_nest$data[[i]][,'LONGITUDE']), y=unlist(rarefyID_coords_nest $data[[i]][,'LATITUDE']))	
	##	get the vertices of the convex hull
	vertices = rarefyID_coords_nest$data[[i]][hull,c('LONGITUDE', 'LATITUDE')]
	##	put some metadata together for checking later
	info = cbind.data.frame(Realm=rep(rarefyID_coords_nest$STUDY_ID[i], times=nrow(vertices)), rarefyID=rep(rarefyID_coords_nest$rarefyID[i], times=nrow(vertices)), vertices)
	vertices_check = rbind.data.frame(vertices_check, info)	# this could be used to check all these convex hulls
	##	calculate the extent and centres (NB cell_extent==0 if there are only two points)
	cell_extent[i] = geosphere::areaPolygon(data.frame(x=vertices$LONGITUDE, y=vertices$LATITUDE))	# km2
	centre_rarefyID_x[i] = geosphere::geomean(cbind(x=vertices$LONGITUDE, y=vertices$LATITUDE))[1]
	centre_rarefyID_y[i] = geosphere::geomean(cbind(x=vertices$LONGITUDE, y=vertices$LATITUDE))[2]
}

##	combine STUDY_ID, rarefyID and the new cell_extent, and geographic centres
rarefyID_cell_centre <- cbind.data.frame(rarefyID_coords_nest[,1:2], cell_extent, rarefyID_x=centre_rarefyID_x, rarefyID_y=centre_rarefyID_y)		
rarefyID_cell_centre <- as_tibble(rarefyID_cell_centre)				  	

##	need to combine with the SL studies and ML with only 1 location per rarefyID
SL_coords <- ungroup(bt) %>%
	##	get the SL studies
	filter(StudyMethod=='SL') %>%
	##	select columns (use the central coords)
	select(STUDY_ID, rarefyID, CENT_LONG, CENT_LAT) %>%
	##	create cell_extent column, and rename coords for joining with rarefyID_cell_centre
	mutate(cell_extent = 0,
		rarefyID_x = CENT_LONG,
		rarefyID_y = CENT_LAT) %>%
	select(-CENT_LONG, -CENT_LAT)
	
ML_coords <- ungroup(bt) %>%
	filter(StudyMethod!='SL') %>%	
	##	select columns
	select(STUDY_ID, rarefyID, LONGITUDE, LATITUDE) %>%
	##	retain only uniqe locations within rarefyIDs (may want to change this if we want to weight the calculation of the centre)
	distinct(rarefyID, LONGITUDE, LATITUDE, .keep_all=TRUE) %>% 
	##	there are also some rarefyID's with only one unique geographic
	group_by(rarefyID) %>%
	mutate(n_locations = n_distinct(LONGITUDE,LATITUDE)) %>%
	ungroup() %>%
	##	retain rarefyIDs with only one location
	filter(n_locations == 1) %>%
	##	create cell_extent column, and rename coords for joining with rarefyID_cell_centre
	mutate(cell_extent = 0,
		rarefyID_x = LONGITUDE,
		rarefyID_y = LATITUDE) %>%
	select(-LONGITUDE, -LATITUDE, -n_locations)

##	put them together
rarefyID_cell_centre <- bind_rows(rarefyID_cell_centre, SL_coords, ML_coords)
##	not sure why but I have multiple entries for each rarefyID!
rarefyID_cell_centre <- rarefyID_cell_centre %>% distinct(STUDY_ID, rarefyID, cell_extent, rarefyID_x, rarefyID_y)
#save(rarefyID_cell_centre, file='rarefyID_cell_centre_011017.Rdata')
##======================================================================
##	reduce to data required for rarefying, and rename a couple of columns
##	NB: we will rarefy to the smallest number of ObsEventID's within studies
bt_grid <- bt %>% 
  dplyr::select(CLIMATE, REALM, TAXA, StudyMethod, ABUNDANCE_TYPE, BIOMASS_TYPE, STUDY_ID, YEAR, PLOT,
         cell, Species, sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS)
names(bt_grid)[12:13] <- c('Abundance', 'Biomass')

# add column for rarefyID (STUDY_ID + cell = samples within a study that are spatially grouped)
bt_grid <- bt_grid %>% unite(col=rarefyID, STUDY_ID, cell, sep="_", remove=FALSE)

# add column for ObsEventID (STUDY_ID + PLOT + YEAR = discrete sampling events within a year)
bt_grid <- bt_grid %>% unite(col=ObsEventID, rarefyID, PLOT, YEAR, sep="_", remove=FALSE)

##	combine with bt_grid (and we are ready to rarefy)
# bt_grid <- inner_join(ungroup(bt_grid), select(SampEvent_per_cell, STUDY_ID, YEAR, cell, n_samps))

#save(dgg, bt_grid, file='BioTIME_grid.Rdata')


##==================================== EXPLORE AND FILTER ON COVERAGE ========================================
##	first collate taxa within cells, to calculate coverage for count data (see below for incidence) 
bt_grid_collate <- bt_grid %>%
  group_by(CLIMATE, REALM, TAXA, StudyMethod, ABUNDANCE_TYPE, rarefyID, STUDY_ID, YEAR, cell, Species) %>%
  summarise(
    Abundance = sum(as.numeric(Abundance), na.rm=TRUE),
    Biomass = sum(as.numeric(Biomass), na.rm=TRUE)) %>% 
  ungroup()

abund_coverage <- bt_grid_collate %>% 
  # get only rows representing count data (abundance)
  filter(ABUNDANCE_TYPE!='Presence/Absence' & ABUNDANCE_TYPE!='<NA>') %>%
  # remove zeroes and NAs(some studies are designated count, but record Biomass estimates)
  filter(Abundance > 0 & !is.na(Abundance)) %>%
  group_by(CLIMATE, REALM, TAXA, StudyMethod, ABUNDANCE_TYPE, rarefyID, STUDY_ID, YEAR, cell) %>%
  summarise(
    # how many singletons
    singletons = sum(Abundance==1),
    # how many doubletons
    doubletons = sum(Abundance==2),
    # how many individuals in total sample
    N = sum(Abundance),
    # eqn 4a in Chao & Jost 2012 Ecology (==eqn 12 in Chao et al 2014 Ecol Monogr)
    Chat = 1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2*doubletons)),
    # with fix from Chao et al 2014 Subroutine for iNEXT code (appendix)
    # correction for communities with no doubletons (this prevents NaN results for either singletons==0 or doubletons==0)
    Chat_corrected = ifelse(doubletons>0, 
                            1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2*doubletons)), 
                            1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2))),
    # the iNEXT coverage calculation has some extras in it that I don't understand
    # e.g., it corrects using f0.hat, the number of unobserved species (Chao1 estimator)? Why? Exactly how? Ref?
    f1_iNEXT = DataInfo(Abundance)$f1,
    f2_iNEXT = DataInfo(Abundance)$f2,
    #n_iNEXT = DataInfo(Abundance)$n,
    coverage = DataInfo(Abundance)$SC) %>%
  ungroup()

# are my estimates of singletons and doubletons equal to the iNEXT estimates?
sum(abund_coverage$singletons!=abund_coverage$f1_iNEXT)		# yes, good
sum(abund_coverage$doubletons!=abund_coverage$f2_iNEXT)		# yes, good

#png('count_coverage_comparison_cell_level', width=800, height=600)
with(abund_coverage, plot(x=Chat, y=coverage, type='n', xlim=c(0,1), ylim=c(0, 1),
                                xlab='Chat or Chat_corrected', ylab='Coverage (iNEXT)', main='Count data coverage calculation check'))
with(abund_coverage, points(x=Chat, y=coverage))
with(abund_coverage, points(x=Chat_corrected, y=coverage, col=2))
abline(c(0,1), lty=2)
legend('bottomright', legend=c('eqn4', 'eqn4 corrected for f2=0'), col=c(1,2), pch=1, lty=1)
#dev.off()

mn=mean(abund_coverage$Chat_corrected, na.rm=TRUE)
sd=sd(abund_coverage$Chat_corrected, na.rm=TRUE)
ggplot(abund_coverage, aes(Chat_corrected)) + geom_histogram(aes(fill=Chat_corrected>=mn-sd), binwidth=0.05) + geom_vline(xintercept=mn)

#-------------------
## PRESENCE/ABSENCE coverage (for INCIDENCE DATA)... by rarefyID (year as samples)
bt_grid_collate_incidence <- bt_grid %>%
  # incidence data and remove NAs
  filter(ABUNDANCE_TYPE=='Presence/Absence' & ABUNDANCE_TYPE!='<NA>') %>%
  filter(!is.na(Abundance)) %>%
  #took out ObsEventID and PLOT as groups b/c no distinct plots for P/A data (Each year has only 1 sample)
  group_by(CLIMATE, REALM, TAXA, StudyMethod, ABUNDANCE_TYPE, rarefyID, STUDY_ID, YEAR, cell, Species) %>%
  summarise(
    Abundance = sum(Abundance, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::select(-CLIMATE, -REALM, -TAXA, -StudyMethod, -ABUNDANCE_TYPE) %>%
  # create incidence column
  mutate(incidence = ifelse(Abundance==0, 0, 1))

pa_coverage <- data.frame()
for (r in unique(bt_grid_collate_incidence$rarefyID)){
  dat <- bt_grid_collate_incidence[bt_grid_collate_incidence$rarefyID == r,]
  if(length(unique(dat$YEAR))<2){ next }
  dat_pa <- dat %>%
    dplyr::select(YEAR, Species, incidence) %>%
    complete(YEAR, Species, fill = list(incidence = 0)) %>%
    #each row is a unique cell and Species, each column is a year
    spread(YEAR, incidence)
  # how many singletons (present in only 1 year)    #FIXME: This is a *really* ugly way to count
  singletons <- length(which(rowSums(dat_pa[,2:ncol(dat_pa)])==1))
  # how many doubletons (present in only 2 years)
  doubletons <- length(which(rowSums(dat_pa[,2:ncol(dat_pa)])==2))
  # how many incidences in the whole matrix
  N <- length(which(dat_pa[,2:ncol(dat_pa)] == 1))
  # how many samples (years) in the matrix
  T <- ncol(dat_pa) - 1
  # Chat for incidence: eqn Chat_sample(T) from Table 2, Chao et al. 2014, similar to eqn 4a in Chao & Jost Ecology
  Chat_i <- 1 - (singletons/N) * (((T-1)*singletons)/((T-1)*singletons + 2*doubletons))
  # with fix from Chao et al 2014 Subroutine for iNEXT code (appendix)
  # correction for communities with no doubletons (this prevents NaN results for
  # either singletons==0 or doubletons==0)
  if (doubletons > 0) {
    Chat_corrected_i <- Chat_i  } 
  else {
    Chat_corrected_i <- 1 - (singletons/N) * (((T-1)*singletons)/((T-1)*singletons + 2))  }
  calcs <- data.frame(rarefyID=r, singletons, doubletons, N, Chat_i, Chat_corrected_i)
  pa_coverage <- rbind(pa_coverage, calcs)
  rm(dat_pa)
}

#what is the mean and sd of the presence data?
mean(pa_coverage$Chat_corrected_i)
sd(pa_coverage$Chat_corrected_i)
#plot the presence coverage data using the abundance-based mean and standard deviation as a guide
ggplot(pa_coverage, aes(Chat_corrected_i)) + geom_histogram(aes(fill=Chat_corrected_i>=mn-sd), binwidth=0.05) + geom_vline(xintercept=mn)

#-------------------
# BIOMASS DATA coverage (as INCIDENCE)... by rarefyID (year as samples)
bt_grid_collate_biomass <- bt_grid %>%
  # incidence data and remove NAs
  filter(is.na(ABUNDANCE_TYPE)) %>%
  filter(!is.na(Biomass)) %>%
  #took out ObsEventID and PLOT as groups b/c no distinct plots for biomass data (Few years have > 1 sample)
  group_by(CLIMATE, REALM, TAXA, StudyMethod, BIOMASS_TYPE, rarefyID, STUDY_ID, YEAR, cell, Species) %>%
  summarise(
    Biomass = sum(Biomass, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::select(-CLIMATE, -REALM, -TAXA, -StudyMethod, -BIOMASS_TYPE) %>%
  # create incidence column
  mutate(incidence = ifelse(Biomass==0, 0, 1))

bm_coverage <- data.frame()
for (r in unique(bt_grid_collate_biomass$rarefyID)){
  dat <- bt_grid_collate_biomass[bt_grid_collate_biomass$rarefyID == r,]
  if(length(unique(dat$YEAR))<2){ next }
  dat_pa <- dat %>%
    dplyr::select(YEAR, Species, incidence) %>%
    complete(YEAR, Species, fill = list(incidence = 0)) %>%
    #each row is a unique cell and Species, each column is a year
    spread(YEAR, incidence)
  # how many singletons (present in only 1 year)    #FIXME: This is a *really* ugly way to count
  singletons <- length(which(rowSums(dat_pa[,2:ncol(dat_pa)])==1))
  # how many doubletons (present in only 2 years)
  doubletons <- length(which(rowSums(dat_pa[,2:ncol(dat_pa)])==2))
  # how many incidences in the whole matrix
  N <- length(which(dat_pa[,2:ncol(dat_pa)] == 1))
  # how many samples (years) in the matrix
  T <- ncol(dat_pa) - 1
  # Chat for incidence: eqn Chat_sample(T) from Table 2, Chao et al. 2014, similar to eqn 4a in Chao & Jost Ecology
  Chat_i <- 1 - (singletons/N) * (((T-1)*singletons)/((T-1)*singletons + 2*doubletons))
  # with fix from Chao et al 2014 Subroutine for iNEXT code (appendix)
  # correction for communities with no doubletons (this prevents NaN results for
  # either singletons==0 or doubletons==0)
  if (doubletons > 0) {
    Chat_corrected_i <- Chat_i  } 
  else {
    Chat_corrected_i <- 1 - (singletons/N) * (((T-1)*singletons)/((T-1)*singletons + 2))  }
  calcs <- data.frame(rarefyID=r, singletons, doubletons, N, Chat_i, Chat_corrected_i)
  bm_coverage <- rbind(bm_coverage, calcs)
  rm(dat_pa)
}

mean(bm_coverage$Chat_corrected_i)
sd(bm_coverage$Chat_corrected_i)
#plot the presence coverage data using the abundance-based mean and standard deviation as a guide
ggplot(bm_coverage, aes(Chat_corrected_i)) + geom_histogram(aes(fill=Chat_corrected_i>=mn-sd), binwidth=0.05)


## Make a list of the rarefyIDs to keep for analysis based on coverage threshold (>= mn-sd of count data)
##  NOTE: Count data will drop individual years that don't meet the criteria, and Presence and Biomass data will drop entire rarefyIDs 
countkeep <- unique(abund_coverage[abund_coverage$Chat_corrected>=mn-sd,c('rarefyID', 'YEAR')])
countkeep <- unite(countkeep, col=keep, rarefyID, YEAR, sep="_")
countkeep <- as.vector(countkeep$keep)

pakeep <- unique(pa_coverage[pa_coverage$Chat_corrected_i>=mn-sd,'rarefyID']) # presence
bmkeep <- unique(bm_coverage[bm_coverage$Chat_corrected_i>=mn-sd,'rarefyID']) # biomass


# Filter gridded studies for the coverage cutoff, prior to rarefaction
bt_count_filtered <- bt_grid %>%
  unite(col=keep, rarefyID, YEAR, sep="_", remove=FALSE) %>%
  filter(keep %in% countkeep) %>%
  dplyr::select(-keep)

bt_pabm_filtered <- bt_grid %>%
  filter(rarefyID %in% pakeep | rarefyID %in% bmkeep)

# DATASET filtered for coverage (>= mean-sd of count data) to be used for rarefaction
#   Note: count data was filtered at rarefyID + YEAR, and presence and biomass data was filtered at rarefyID
bt_grid_filtered <- rbind(bt_count_filtered, bt_pabm_filtered)


#save(dgg, bt_grid_filtered, file='BioTIME_grid_filtered_011017.Rdata')


##==================================== VISUALIZE DATA ===========================================
##	want to examine coverage and numbers of samples per cell through time
##	calculate diversity metrics within cells for each study through time

##	Calculate biodiversity metrics for grid cells (count data only)
bt_gridMetrics_count <- bt_grid_collate %>%
	# count data first...
	filter(ABUNDANCE_TYPE!='Presence/Absence' & ABUNDANCE_TYPE!='<NA>') %>%
	# remove zeroes and NAs(some studies are designated count, but record Biomass estimates)
	filter(Abundance > 0 & !is.na(Abundance)) %>%
	group_by(CLIMATE, REALM, TAXA, StudyMethod, ABUNDANCE_TYPE, STUDY_ID, YEAR, cell) %>%
	summarise(
		N = sum(Abundance),
		S = n_distinct(Species),
		S_check = specnumber(Abundance),
		PIE = diversity(Abundance, index='simpson'),
		ENSPIE = diversity(Abundance, index='invsimpson'),
		pielou = diversity(Abundance)/log(S),
		singletons = sum(Abundance==1),
		doubletons = sum(Abundance==2), 
		# eqn 4a in Chao & Jost 2012 Ecology (==eqn 12 in Chao et al 2014 Ecol Monogr)
		Chat = 1 - singletons/N * (N-1)*singletons/((N-1)*singletons + 2*doubletons),
		# with one wrinkle  that makes a correction
		# for communities with no doubletons (this prevents NaN results when singletons==0 & doubletons==0)
		# (from code associated with Chao et al 2014 - see appendix)
		Chat_corrected = ifelse(doubletons>0, 
			1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2*doubletons)), 
			1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2)))) %>%
	ungroup()

##	how many cells for all the count data?
N_total_cells_countData <- bt_grid_collate %>%
	# restrict to count data that we calculated metrics for
	filter(ABUNDANCE_TYPE!='Presence/Absence' & ABUNDANCE_TYPE!='<NA>') %>%
	filter(Abundance > 0 & !is.na(Abundance)) %>%
	# calculate how many cells in total
	summarise(total_cells = n_distinct(cell)) %>% .$total_cells

bt_gridMetrics_count <- bt_gridMetrics_count %>% mutate(total_cells = N_total_cells_countData)

##	want a plot of % total cells retained for different coverage cutoffs
cov_threshold <- data.frame(threshold = seq(0,1,by=0.05),
	retained_Chat = rep(NA, times=21),
	retained_Chat_corrected = rep(NA, times=21))

for(i in 1:nrow(cov_threshold)) {
	cells_retained_Chat <- bt_gridMetrics_count %>% filter(Chat >= cov_threshold$threshold[i]) %>% 
		summarise(cells_retained = n_distinct(cell)) %>% .$cells_retained
	cells_retained_Chat_corrected <- bt_gridMetrics_count %>% filter(Chat_corrected >= cov_threshold$threshold[i]) %>% 
		summarise(cells_retained = n_distinct(cell)) %>% .$cells_retained	
	cov_threshold$retained_Chat[i] <- cells_retained_Chat/N_total_cells_countData
	cov_threshold$retained_Chat_corrected[i] <- cells_retained_Chat_corrected/N_total_cells_countData
}

#png('count_coverage_proportion_cell_retained', width=800, height=600)
with(cov_threshold, plot(x=threshold, y=retained_Chat, type='n', xlim=c(0,1), ylim=c(min(cov_threshold$retained_Chat), 1),
	xlab='Coverage', ylab='Proportion of cells retained', main='Count data coverage'))
with(cov_threshold, points(x=threshold, y=retained_Chat))
with(cov_threshold, lines(x=threshold, y=retained_Chat))
with(cov_threshold, points(x=threshold, y=retained_Chat_corrected, col=2))
with(cov_threshold, lines(x=threshold, y=retained_Chat_corrected, col=2))
legend('bottomleft', legend=c('eqn4', 'eqn4 corrected for f2=0'), col=c(1,2), pch=1, lty=1)
#dev.off()
	



#-------------------------- PLOT COVERAGE SEPARATELY BY SL, ML, REALM, AND CLIMATE--------------------

##	Plot coverage (within cells) through time for the SL? 
##	These are studies that have observations from only one cell
#png('SL coverage through time', width=800, height=600)
ggplot(filter(bt_gridMetrics_count, StudyMethod=='SL')) +
	facet_wrap(CLIMATE~REALM, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=interaction(STUDY_ID, cell)), alpha=0.5) +
	theme_bw()
#dev.off()

#png('SL coverage_over_80 through time', width=800, height=600)
ggplot(filter(bt_gridMetrics_count, StudyMethod=='SL' & Chat_corrected>0.8)) +
	facet_wrap(CLIMATE~REALM, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=interaction(STUDY_ID, cell)), alpha=0.5) +
	theme_bw()
#dev.off()


##	coverage (within cells) through time for the ML studies (MESSY when plotted all at once!)
ggplot(filter(bt_gridMetrics_count, StudyMethod=='ML')) +
	facet_wrap(CLIMATE~REALM, scales='free') +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=interaction(STUDY_ID, cell)), alpha=0.3) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()
	
##	Studies with good coverage first...
#png('ML coverage through time_good', width=800, height=600)
ggplot(filter(bt_gridMetrics_count,  
	(StudyMethod=='ML' & CLIMATE=='Polar' & REALM=='Freshwater') | (StudyMethod=='ML' & CLIMATE=='Polar' & REALM=='Marine') | (StudyMethod=='ML' & CLIMATE=='Tropical' & REALM=='Marine') | (StudyMethod=='ML' & CLIMATE=='Tropical' & REALM=='Terrestrial'))) +
	facet_wrap(CLIMATE~REALM, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=interaction(STUDY_ID, cell)), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()
#dev.off()

##	now, lots of variation in coverage...

##	closer look at noisy CLIMATE/REALM combinations
##	temperate/marine
#png('ML coverage through time_temp_mar', width=800, height=600)
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Temperate' & REALM=='Marine'))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw() +
	ggtitle('Coverage through time for temperate marine studies')
#dev.off()

##	what happens if we filter to coverage>0.8
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Temperate' & REALM=='Marine' & Chat_corrected>0.8))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()

##	temperate/terrestrial	
#png('ML coverage through time_temp_terr', width=800, height=600)
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Temperate' & REALM=='Terrestrial'))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw() +
	ggtitle('Coverage through time for temperate terrestrial studies')
#dev.off()

##	what happens if we filter to coverage>0.8
#png('ML coverage_over_80 through time_temp_terr', width=800, height=600)
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Temperate' & REALM=='Terrestrial' & Chat_corrected>0.8))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw() +
	ggtitle('Coverage through time for temperate terrestrial studies (only cells with coverage > 0.8)')
#dev.off()	

##	polar-temperate/marine	
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Polar/Temperate' & REALM=='Marine'))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()

##	what happens if we filter to coverage>0.8
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Polar/Temperate' & REALM=='Marine' & Chat_corrected>0.8))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()

##	temperate-tropical/marine	
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Temperate/Tropical' & REALM=='Marine'))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()

##	what happens if we filter to coverage>0.8
ggplot(filter(bt_gridMetrics_count, (StudyMethod=='ML' & CLIMATE=='Temperate/Tropical' & REALM=='Marine' & Chat_corrected>0.8))) +
	facet_wrap(~STUDY_ID, scales='free') +
	geom_point(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), size=0.75, alpha=0.25) +
	geom_line(aes(x=YEAR, y=Chat_corrected, colour=TAXA, group=cell), alpha=0.5) +
	scale_x_continuous(breaks=pretty_breaks()) +
	theme_bw()

#-------------------------------------------------------------------------------
##  frequency distribution of coverage by climate and realm
#png('Coverage per cell (histogram)', width=800, height=600)
ggplot(filter(bt_gridMetrics_count)) +
  facet_wrap(CLIMATE~REALM, scales='free') +
  geom_histogram(aes(Chat_corrected))
#dev.off()	

##  frequency distribution of samples per year by climate and realm
#png('Samples per cell (histogram)', width=800, height=600)
ggplot(filter(bt_gridMetrics_count)) +
	facet_wrap(CLIMATE~REALM, scales='free') +
	geom_histogram(aes(n_samps)) +
	ylab('Samples per cell (per year)')
#dev.off()	
