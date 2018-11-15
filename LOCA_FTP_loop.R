rm(list = ls())
library(ncdf4)
library(tidyverse)
library(parallel)
setwd('~/Desktop/GitHub/CDPH_heat_project/')


#rcpMat <- c("45","85")
#names(rcpMat) <- c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")

modelMat <- c("ACCESS1-0","CanESM2","CESM1-BGC","CMCC-CMS","CNRM-CM5","GFDL-CM3","HadGEM2-CC","HadGEM2-ES","MIROC5")
names(modelMat) <- c("ACCESS1-0","CanESM2","CESM1-BGC","CMCC-CMS","CNRM-CM5","GFDL-CM3","HadGEM2-CC","HadGEM2-ES","MIROC5")

download_nc <- function(modelVar,YearStartVar,YearStopVar){
  # n<-5
  # modelVar<-n
  model <- modelVar
  #rcp <- rcpVar
  #temp <- tempVar 
  yearStart<-YearStartVar
  yearStop<-YearStopVar
  
  # ncdfURL.45.tasmax <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/rcp45/tasmax/tasmax_day_",model,"_rcp45_r6i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  # dest.45.tasmax <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_rcp45_tasmax_",yearStart,"_",yearStop,"_","CA_NV.nc")
  # download.file(url=ncdfURL.45.tasmax,destfile=dest.45.tasmax, mode = "wb")
  # 
  #  ncdfURL.45.tasmin <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/rcp45/tasmin/tasmin_day_",model,"_rcp45_r6i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  #  dest.45.tasmin <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_rcp45_tasmin_",yearStart,"_",yearStop,"_","CA_NV.nc") 
  #  download.file(url=ncdfURL.45.tasmin,destfile=dest.45.tasmin, mode = "wb") 
  # 
  # ncdfURL.85.tasmax <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/rcp85/tasmax/tasmax_day_",model,"_rcp85_r6i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  # dest.85.tasmax <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_rcp85_tasmax_",yearStart,"_",yearStop,"_","CA_NV.nc")
  # download.file(url=ncdfURL.85.tasmax,destfile=dest.85.tasmax, mode = "wb")
  # 
  #  ncdfURL.85.tasmin <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/rcp85/tasmin/tasmin_day_",model,"_rcp85_r6i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  #  dest.85.tasmin <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_rcp85_tasmin_",yearStart,"_",yearStop,"_","CA_NV.nc") 
  #  download.file(url=ncdfURL.85.tasmin,destfile=dest.85.tasmin, mode = "wb") 
  # 
  ##for CCSM4 you need to change r1i1p1 to r6i1p1

  ncdfURL.hist.tasmax <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/historical/tasmax/tasmax_day_",model,"_historical_r1i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  dest.hist.tasmax <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_historical_tasmax_",yearStart,"_",yearStop,"_","CA_NV.nc")
  download.file(url=ncdfURL.hist.tasmax,destfile=dest.hist.tasmax, mode = "wb")

  ncdfURL.hist.tasmin <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/historical/tasmin/tasmin_day_",model,"_historical_r1i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  dest.hist.tasmin <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_historical_tasmin_",yearStart,"_",yearStop,"_","CA_NV.nc")
  download.file(url=ncdfURL.hist.tasmin,destfile=dest.hist.tasmin, mode = "wb")
}


#rcpVar<- c("historical")##inputs are "rcp45" "rcp85" and "historical"
#tempVar<-c("tasmax")
YearStartVar<-c("20050101")
YearStopVar<-c("20051231")
#modelMat <- c("CCSM4")
#names(modelMat) <- c("CCSM4")


##Download data year by year splitting it up on multiple cores. 
mclapply(modelMat, download_nc,YearStartVar=YearStartVar,YearStopVar=YearStopVar) 
##need to do CCSM$ separately because it has r6i1p1 instead of r1i1p1



