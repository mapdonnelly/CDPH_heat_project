rm(list = ls())
library(ncdf4)
library(tidyverse)
setwd('~/Desktop/GitHub/CDPH_heat_project/')

tempMat <- c("tasmax")
names(tempMat) <- c("Daily max temp")

rcpMat <- c("45","85")
names(rcpMat) <- c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")

modelMat <- c("ACCESS1-0","CanESM2","CCSM4","CESM1-BGC","CMCC-CMS","CNRM-CM5","GFDL-CM3","HadGEM2-CC","HadGEM2-ES","MIROC5")
names(modelMat) <- c("ACCESS1-0","CanESM2","CCSM4","CESM1-BGC","CMCC-CMS","CNRM-CM5","GFDL-CM3","HadGEM2-CC","HadGEM2-ES","MIROC5")

yearStartMat <- c("20300101")
names(yearStartMat) <- c("20300101")

yearStopMat <- c("20301231")
names(yearStopMat) <- c("20301231")

process_single_nc <- function(modelVar, rcpVar, popVar){
  n<-3
  modelVar<-n
  rcpVar<-1
  tempVar<-1
  yearStartVar<-1
  yearStopVar<-1
  model <- modelMat[modelVar]
  rcp <- rcpMat[rcpVar]
  temp <- tempMat[tempVar] 
  yearStart<-yearStartMat[yearStartVar]
  yearStop<-yearStopMat[yearStopVar]
  ###What is dname???
  dname = "hectares"
  
  ##for CCSM$ you need to change r1i1p1 to r6i1p1
  ncdfURL <- paste0("http://albers.cnr.berkeley.edu/data/scripps/loca/",model,"/rcp",rcp,"/",temp,"/",temp,"_day_",model,"_rcp",rcp,"_r1i1p1_",yearStart,"-",yearStop,".LOCA_2016-04-02.16th.CA_NV.nc")
  dest <-  paste0("~/Desktop/GitHub/CDPH_heat_project/data/",model,"_rcp",rcp,"_",temp,"_",yearStart,"_",yearStop,"_","CA_NV.nc") 
  download.file(url=ncdfURL,destfile=dest, mode = "wb") 
  
  ncin <- nc_open(dest)
  # store values from variables and atributes
  attributes(ncin$dim)$names
  nc_lat <- ncvar_get(ncin, "lat")
  n_lat <- dim(nc_lat)
  nc_lon <- ncvar_get(ncin, "lon")
  n_lon <- dim(nc_lon)
  nc_year <- ncvar_get(ncin, "time")+1953
  n_year <- dim(nc_year)
  
  # get hectares
  tmp_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(tmp_array)
  
  # get global attributes
  # title <- ncatt_get(ncin,0,"title")
  # institution <- ncatt_get(ncin,0,"institution")
  # datasource <- ncatt_get(ncin,0,"source")
  # references <- ncatt_get(ncin,0,"references")
  # history <- ncatt_get(ncin,0,"history")
  # Conventions <- ncatt_get(ncin,0,"Conventions")
  
  nc_close(ncin)
  
  # replace netCDF fill values with NA's
  tmp_array[tmp_array==fillvalue$value] <- NA
  
  # check number of missing data values
  # length(na.omit(as.vector(tmp_array[,,1])))
  # dimnames(tmp_array) <-list(lon=nc_lon, lat=nc_lat, year=nc_year)
  
  bar <- data.frame(expand.grid(lon = nc_lon, lat = nc_lat)) %>%
    cbind(matrix(as.vector(tmp_array), nrow = n_lon*n_lat, ncol = n_year)) %>% 
    mutate(model=model, scenario = rcp, population = pop) %>% 
    select(model, scenario, population, lon, lat, 3:150)
  
  names(bar) <- c("model","scenario","population","lon","lat",1954:2100) 
  
  
  CAonly <- inner_join(bar, locationData) %>%
    gather(6:152, key = "year", value = "hectares") %>% 
    mutate(year = as.integer(as.character(year))) %>% 
    mutate(period = factor(ifelse(year %in% c(1961:1990),"baseline (1961-1990)", 
                                  ifelse(year %in% c(2000:2020), "early (2000-2020)",
                                         ifelse(year %in% c(2040:2060),"mid-century (2040-2060)",
                                                ifelse(year %in% c(2080:2100), "late-century (2080-2100)", "between")))),
                           levels = c("baseline (1961-1990)","early (2000-2020)","mid-century (2040-2060)","late-century (2080-2100)","between"))) %>%
    mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                        ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                               ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                                 levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs)")),
           
           RCP = factor(ifelse(scenario == "45", "RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)"),
                        levels = c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","emissions continue to rise throughout the 21st century")), 
           
           PopulationGrowth = factor(ifelse(population == "AA.all.bau.mu.nc","Central Projection",
                                            ifelse(population == "AA.all.L.mu.nc","Low Projection","High Projection")),
                                     levels = c("Low Projection", "Central Projection","High Projection")))
  return(CAonly)
}

