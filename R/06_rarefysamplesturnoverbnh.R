# Maria Dornelas 13.05.2013
# rarefying for equal number of samples and calculating temporal turnover

rarefysamplesturnoverbnh<-function(Year, SampleID, Species, Abundance, resamps) {
#######################################################################
  # takes as input a  Year, SampleID, Species, Abundance and number of resamples
  # which should be in dataframe so that elements match
  # calculates turnover:
  # 1) between each year and the first year 
  # 2) between pairs of adjacent years 
  # 3) between each year and the last year of the time series
  # for the rarefied pooled samples
###########################################################################
library(vegan)
counter<-1
simbaseline<-data.frame(array(NA,dim=c(length(unique(Year)),7)))
names(simbaseline)<-c("Year", "Jaccard","Horn","Chao","Pearson", "Jtu", "Jne")

simnext<-data.frame(array(NA,dim=c(length(unique(Year)),7)))
names(simnext)<-c("Year", "Jaccard","Horn","Chao","Pearson", "Jtu", "Jne")

simhind<-data.frame(array(NA,dim=c(length(unique(Year)),7)))
names(simhind)<-c("Year", "Jaccard","Horn","Chao","Pearson", "Jtu", "Jne")

counter2<-1

# getting vector with number of samples per year
nsamples<-c()
for(y in unique(Year)){
  nsamples<-c(nsamples, length(unique(SampleID[Year==y])))
}
t<-1
minsample<-min(nsamples)
for(repeats in 1:resamps){
  raref<-data.frame(array(NA,dim=c(1,3)))
  names(raref)<-c("Year","Species","Abundance")
  for(y in unique(Year)){
    #getting samples for this year
    samps<-unique(SampleID[Year==y])
    # re-sampling samples to equalize number of samples
    sam<-as.character(sample(samps,minsample,replace=T))
    # getting data that belongs to bootstraped samples
    rarefyear<-data.frame(SampleID[which(SampleID %in% sam & Year == y)],
                          Species[which(SampleID %in% sam & Year == y)],
                          Abundance[which(SampleID %in% sam & Year == y)])
    names(rarefyear)<-c("SampleID", "Species", "Abundance")
    # calculating pooled abundances of eahc species to store
    spabun<-tapply(as.numeric(rarefyear[,3]),rarefyear[,2],sum)
    spar<-data.frame(rep(y, length(spabun)),names(spabun),spabun, row.names=NULL)
    names(spar)<-c("Year","Species","Abundance")
    raref<-rbind(raref,spar)
    counter<-counter+1
  }
  # calculating year by species table of abundance
  rareftabtemp<-with(raref,tapply(Abundance,list(Year,Species),function(x)x))
  rareftabtemp[is.na(rareftabtemp)]<-0
  Pearsoncor<-cor(t(log(rareftabtemp+1)), method="pearson")
  # calculating between year similarities (NOT DISTANCE!) with Jaccard, Morisita-Horn, Chao and Pearson correlations
  Jacsim<-as.matrix(1-vegdist(rareftabtemp, method="jaccard"))
  Hornsim<-as.matrix(1-vegdist(rareftabtemp, method="horn"))
  Chaosim<-as.matrix(1-vegdist(rareftabtemp, method="chao"))
  # two steps for Jaccard components (so as calculation is done only once)
  # rare_comm_binary <- with(rareftabtemp, ifelse(rareftabtemp > 0, 1, 0))
  J_components <- betapart::beta.pair(rareftabtemp, index.family='jaccard')	# distance
  Jbeta <- as.matrix(J_components$beta.jac)
  Jtu <- as.matrix(J_components$beta.jtu)
  Jne <- as.matrix(J_components$beta.jne)
  n<-length(unique(Year))
  
  # comparisons to baseline
  simbaseline[counter2:(counter2+n-2),] <- cbind(unique(Year)[2:n],
                                                 Jacsim[2:n],
                                                 Hornsim[2:n],
                                                 Chaosim[2:n],
                                                 Pearsoncor[2:n],
                                                 Jtu[2:n],
                                                 Jne[2:n])
  
  simnext[counter2:(counter2+n-2),] <- cbind(unique(Year)[2:n],
                                             Jacsim[row(Jacsim)-col(Jacsim)==1],
                                             Hornsim[row(Hornsim)-col(Hornsim)==1],
                                             Chaosim[row(Chaosim)-col(Chaosim)==1],
                                             Pearsoncor[row(Pearsoncor)-col(Pearsoncor)==1],
                                             Jtu[row(Jtu)-col(Jtu)==1],
                                             Jne[row(Jne)-col(Jne)==1])
  
  # added hindcasting 
  simhind[counter2:(counter2+n-2),] <- cbind(unique(Year)[1:(n-1)],
                                             Jacsim[row(Jacsim)%in%1:(max(row(Jacsim))-1) & col(Jacsim)==max(col(Jacsim))], 
                                             Hornsim[row(Hornsim)%in%1:(max(row(Hornsim))-1) & col(Hornsim)==max(col(Hornsim))],
                                             Chaosim[row(Chaosim)%in%1:(max(row(Chaosim))-1) & col(Chaosim)==max(col(Chaosim))], 
                                             Pearsoncor[row(Pearsoncor)%in%1:(max(row(Pearsoncor))-1) & col(Pearsoncor)==max(col(Pearsoncor))],
                                             Jtu[row(Jtu)%in%1:(max(row(Jtu))-1) & col(Jtu)==max(col(Jtu))],
                                             Jne[row(Jne)%in%1:(max(row(Jne))-1) & col(Jne)==max(col(Jne))])
  counter2<-counter2+n
}

# calculate means
baselinesim <- data.frame(unique(Year)[2:n],
                        tapply(simbaseline$Jaccard,simbaseline$Year,mean),
                        tapply(simbaseline$Horn,simbaseline$Year,mean),
                        tapply(simbaseline$Chao,simbaseline$Year,mean),
                        tapply(simbaseline$Pearson,simbaseline$Year,mean),
                        tapply(simbaseline$Jtu,simbaseline$Year,mean),
                        tapply(simbaseline$Jne,simbaseline$Year,mean))
names(baselinesim) <- c("Year", "Jaccard","Horn","Chao","Pearson", "Jtu", "Jne")

nextsim <- data.frame(unique(Year)[2:n],
                      tapply(simnext$Jaccard,simnext$Year,mean),
                      tapply(simnext$Horn,simnext$Year,mean),
                      tapply(simnext$Chao,simnext$Year,mean),
                      tapply(simnext$Pearson,simnext$Year,mean),
                      tapply(simnext$Jtu,simnext$Year,mean),
                      tapply(simnext$Jne,simnext$Year,mean))

names(nextsim) <- c("Year", "Jaccard","Horn","Chao","Pearson", "Jtu", "Jne")

hindcastsim <- data.frame(unique(Year)[1:(n-1)],
                          tapply(simhind$Jaccard,simhind$Year,mean),
                          tapply(simhind$Horn,simhind$Year,mean),
                          tapply(simhind$Chao,simhind$Year,mean),
                          tapply(simhind$Pearson,simhind$Year,mean),
                          tapply(simhind$Jtu,simhind$Year,mean),
                          tapply(simhind$Jne,simhind$Year,mean))
names(hindcastsim) <- c("Year", "Jaccard","Horn","Chao","Pearson", "Jtu", "Jne")

a <- list(baselinesim,
          nextsim,
          hindcastsim)

return(a)
}
