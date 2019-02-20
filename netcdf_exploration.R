##Opening NetCDF files
##tutorial from this website: http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
setwd('~/Google Drive/CDPH_internship/data/')

ncpath<-"~/Google Drive/CDPH_internship/data/RCP45/"
ncname<-"ACCESS1-0_rcp45_tasmax_20060101_20061231_CA_NV"

ncfname<-paste(ncpath,ncname, ".nc",sep = "")
dname<-"tasmax"
ncin<-nc_open(ncfname)
print(ncin)
##time units are days since 1900-01-01
lon<-ncvar_get(ncin, "lon")-360
nlon<-dim(lon)
head(lon) ##we should subtract 360 from longitude to get in the correct units. 
lat<-ncvar_get(ncin, "lat")
nlat<-dim(lat)
head(lat)
print(c(nlon,nlat))
time<-ncvar_get(ncin,"time")
time
tunits<-ncatt_get(ncin,"time","units")
nt<-dim(time)
nt
tunits
  
##get temperature variables
tmp_array<-ncvar_get(ncin,dname)
dlname<-ncatt_get(ncin, dname, "long_name")
dunits<-ncatt_get(ncin, dname,"units")
fillvalue<-ncatt_get(ncin, dname, "_FillValue")
dim(tmp_array)


##get the global attributes
title<-ncatt_get(ncin, 0, "title")
institution<-ncatt_get(ncin, 0, "institution")
datasource<-ncatt_get(ncin, 0, "source")
references<-ncatt_get(ncin, 0, "references")
history<-ncatt_get(ncin,0,"history")
conventions<-ncatt_get(ncin, 0, "Conventions")

##close the netcdf file
#nc_close()
#ls()

##convert time by first pulling apart components of time since 1900-01-01
tustr<-strsplit(tunits$value," ")
tdstr<-strsplit(unlist(tustr)[3],"-")
tmonth<-as.integer(unlist(tdstr)[2])
tday<-as.integer(unlist(tdstr)[3])
tyear<-as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth,tday,tyear))

##replace netCDF fill values with NAs
tmp_array[tmp_array==fillvalue$value]<-NA
length(na.omit(head(as.vector(tmp_array[,,1]))))

##pull out a single slice of the array
m<-1
tmp_slice<-tmp_array[,,m]-273
dim(tmp_slice)
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

grid<-expand.grid(lon=lon,lat=lat)
cutpts<-c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice~lon*lat, data=grid, at=cutpts, cuts=11, pretty=T, col.regions = (rev(brewer.pal(10,"RdBu"))))

##reshape the entire array
tmp_vec_long<-as.vector(tmp_array)
length(tmp_vec_long)
##reshape this vector into a matrix
tmp_mat<-matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)
head(na.omit(tmp_mat)) ##still in Kelvin
##create a dataframe
lonlat<-as.matrix(expand.grid(lon,lat))
tmp_df02<-data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02)<-c("lon","lat",1:365)
head(na.omit(tmp_df02, 10))
library(tidyr)
library(dplyr)
library(ggplot2)
d<- tmp_df02
d<-d %>% gather(key=day, value=temp, -c(lat,lon))

d %>% 
  filter(day %in% seq(1,365,30)[-13]) %>% 
  ggplot(aes(lon, lat, color = temp)) + 
  geom_point() + 
  facet_wrap(~day, nrow = 3) + 
  scale_color_viridis_c() +
  theme_void()
