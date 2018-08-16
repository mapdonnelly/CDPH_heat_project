
setwd('~/Desktop/GitHub/CDPH_heat_project/')
require("acs")
xwalk<-read.csv("Tract_UR_key.csv", header=TRUE)
api.key.install(key="564b38dac3a753efb9e6541ae2414678652018d6")

lookup<-acs.lookup(endyear = 2015, span = 5,keyword="income",case.sensitive = F)
lookup

devtools::install_github("collectivemedia/tictoc")
###Create function to fetch ACS data with API
library(tictoc)
geo_function<-function(geoid){
  tic("time")
geo<-geo.make(state=geoid$STATE, county=geoid$COUNTY, tract=geoid$TRACT)

sex.by.age<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                              table.number = "B01001", col.names = "pretty", key="564b38dac3a753efb9e6541ae2414678652018d6")

median.household.income<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                              table.number = "B19013", col.names = "pretty", key="564b38dac3a753efb9e6541ae2414678652018d6")
  
race<-acs.fetch(endyear = 2015, span = 5, geography = geo, 
                                     table.number = "B02001", col.names = "pretty", key = "564b38dac3a753efb9e6541ae2414678652018d6")
  
  
hispanic.origin.race<-acs.fetch(endyear = 2014, span = 5, geography = geo, 
                          table.number = "B03002", col.names = "pretty")
  
 
population<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                      table.number = "B01003", case.sensitive=F)

  attr(sex.by.age, "acs.colnames")
  attr(median.household.income, "acs.colnames")
  attr(race, "acs.colnames")
  attr(hispanic.origin.race, "acs.colnames")
  attr(population, "acs.colnames")
  
    ACS_tracts_CA<- data.frame( geoid$TRACT, sex.by.age@estimate[,1],sex.by.age@estimate[,2],sex.by.age@estimate[,3],
                             median.household.income@estimate[,1],
                             race@estimate[,1],race@estimate[,2],race@estimate[,3],race@estimate[,4],race@estimate[,5],race@estimate[,6],race@estimate[,7],
                             hispanic.origin.race@estimate[,1], hispanic.origin.race@estimate[,2], hispanic.origin.race@estimate[,12],
                             population@estimate[,1],
                             stringsAsFactors = FALSE)  
 # colnames(ACS.GEO.LA.df)[1]<-"GEOID"
  #ACS.GEO.LA.df$address<-address
  #ACS.GEO.LA.df$lon<-g1$lon
  #ACS.GEO.LA.df$lat<-g1$lat
  return(ACS_tracts_CA)
   time<-toc()
}

test.df<-geo_function(xwalk[1,])
