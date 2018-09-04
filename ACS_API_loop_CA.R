##Script to loop over every county and retrieve census data
rm(list=ls())
library(jsonlite)
library(tidyverse)
setwd('~/Desktop/GitHub/CDPH_heat_project/')
xwalk<-read.csv("Tract_UR_key.csv", header=TRUE)
api.key.install(key="564b38dac3a753efb9e6541ae2414678652018d6")

county = 01
state= 06
getTractAgeSex <- function(state, county) {
  varString <-
    "B01001_003E,B01001_004E,B01001_005E,B01001_006E,B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,B01001_027E,B01001_028E,B01001_029E,B01001_030E,B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E"
  
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACSpop <-
    as.data.frame(fromJSON(
      paste(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=tract:*&in=state:",
        state,
        "+county:",
        county,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038",
        sep = ""
      )
    ))
  ACSpop <- ACSpop[-1,]
  
  colnames(ACSpop) <-
    c(
      "name",
      "M_Under 5 years",
      "M_5 to 9 years",
      "M_10 to 14 years",
      "M_15 to 17 years",
      "M_18 and 19 years",
      "M_20 years",
      "M_21 years",
      "M_22 to 24 years",
      "M_25 to 29 years",
      "M_30 to 34 years",
      "M_35 to 39 years",
      "M_40 to 44 years",
      "M_45 to 49 years",
      "M_50 to 54 years",
      "M_55 to 59 years",
      "M_60 and 61 years",
      "M_62 to 64 years",
      "M_65 and 66 years",
      "M_67 to 69 years",
      "M_70 to 74 years",
      "M_75 to 79 years",
      "M_80 to 84 years",
      "M_85 years and over",
      "F_Under 5 years",
      "F_5 to 9 years",
      "F_10 to 14 years",
      "F_15 to 17 years",
      "F_18 and 19 years",
      "F_20 years",
      "F_21 years",
      "F_22 to 24 years",
      "F_25 to 29 years",
      "F_30 to 34 years",
      "F_35 to 39 years",
      "F_40 to 44 years",
      "F_45 to 49 years",
      "F_50 to 54 years",
      "F_55 to 59 years",
      "F_60 and 61 years",
      "F_62 to 64 years",
      "F_65 and 66 years",
      "F_67 to 69 years",
      "F_70 to 74 years",
      "F_75 to 79 years",
      "F_80 to 84 years",
      "F_85 years and over",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:47,key = variable, value = value)

  #ACSpop$gender <-  matrix(unlist(strsplit(as.character(ACSpop$variable), "_")), ncol = 2, byrow =T)[, 1]
  #ACSpop$acsAge <-  matrix(unlist(strsplit(as.character(ACSpop$variable), "_")), ncol = 2, byrow =T)[, 2]

  # ITHIMageKey <-
  #   c(
  #     "ageClass1",
  #     "ageClass2",
  #     "ageClass2",
  #     "ageClass3",
  #     "ageClass3",
  #     "ageClass3",
  #     "ageClass3",
  #     "ageClass3",
  #     "ageClass3",
  #     "ageClass4",
  #     "ageClass4",
  #     "ageClass4",
  #     "ageClass5",
  #     "ageClass5",
  #     "ageClass5",
  #     "ageClass6",
  #     "ageClass6",
  #     "ageClass6",
  #     "ageClass6",
  #     "ageClass7",
  #     "ageClass7",
  #     "ageClass8",
  #     "ageClass8"
  #   )
  # 
  # names(ITHIMageKey) <- unique(ACSpop$acsAge)
  # ACSpop$ITHIMage <- ITHIMageKey[as.character(ACSpop$acsAge)]

  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

agelist<-list()

for (i in 1:length(counties)) {
  agelist[[i]]<-getTractAgeSex(state = 06,county = counties[i])
}

age_df = do.call(rbind, agelist)

################
###Median income
################

getTractMedInc <- function(state, county) {
  varString <-"B19013_001E"
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACSpop <-
    as.data.frame(fromJSON(
      paste(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=tract:*&in=state:",
        state,
        "+county:",
        county,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038",
        sep = ""
      )
    ))
  ACSpop <- ACSpop[-1,]
  
  colnames(ACSpop) <-
    c(
      "name",
      "median_household_income",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

med_inc_list<-list()

for (i in 1:length(counties)) {
  med_inc_list[[i]]<-getTractMedInc(state = 06,county = counties[i])
}

med_inc_df = do.call(rbind, med_inc_list)

###############
###Race
##############
state = 06
county = 01
getTractRace <- function(state, county) {
  varString <-"B02001_001E,B02001_002E,B02001_003E,B02001_004E,B02001_005E,B02001_006E,B02001_007E,B02001_008E,B02001_009E,B02001_010E"
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACSpop <-
    as.data.frame(fromJSON(
      paste(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=tract:*&in=state:",
        state,
        "+county:",
        county,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038",
        sep = ""
      )
    ))
  ACSpop <- ACSpop[-1,]
  
  colnames(ACSpop) <-
    c(
      "name",
      "race_total",
      "white_alone",
      "black_af_am_alone",
      "am_ind_alone",
      "asian_alone",
      "nat_haw_pac_isl",
      "some_other_alone",
      "two_or_more_races",
      "two_or_more_races_incl_other",
      "two_or_more_races_excl_other",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

race_list<-list()

for (i in 1:length(counties)) {
  race_list[[i]]<-getTractRace(state = 06,county = counties[i])
}

race_df = do.call(rbind, race_list)

###############
###Hispanic latino by race
##############
state = 06
county = 01
getTractHispLatRace <- function(state, county) {
  varString <-"B03002_001E,B03002_002E,B03002_003E,B03002_004E,B03002_005E,B03002_006E,B03002_007E,B03002_008E,B03002_009E,B03002_010E,B03002_011E,B03002_012E,B03002_013E,B03002_014E,B03002_015E,B03002_016E,B03002_017E,B03002_018E,B03002_019E,B03002_020E,B03002_021E"
# https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACSpop <-
    as.data.frame(fromJSON(
      paste(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=tract:*&in=state:",
        state,
        "+county:",
        county,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038",
        sep = ""
      )
    ))
  ACSpop <- ACSpop[-1,]
  
  colnames(ACSpop) <-
    c(
      "name",
      "hisp_lat_race_total",
      "not_hisp_lat_total",
      "not_hisp_lat_white_alone",
      "not_hisp_lat_black_af_am_alone",
      "not_hisp_lat_am_ind_alone",
      "not_hisp_lat_asian_alone",
      "not_hisp_lat_nat_haw_pac_isl_alone",
      "not_hisp_lat_some_other_alone",
      "not_hisp_lat_two_or_more",
      "not_hisp_lat_two_or_more_incl_some_other",
      "not_hisp_lat_two_or_more_excl_some_other",
      "yes_hisp_lat_total",
      "yes_hisp_lat_white_alone",
      "yes_hisp_lat_black_af_am_alone",
      "yes_hisp_lat_am_ind_alone",
      "yes_hisp_lat_asian_alone",
      "yes_hisp_lat_nat_haw_pac_isl_alone",
      "yes_hisp_lat_some_other_alone",
      "yes_hisp_lat_two_or_more",
      "yes_hisp_lat_two_or_more_incl_some_other",
      "yes_hisp_lat_two_or_more_excl_some_other",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

hisp_lat_race_list<-list()

for (i in 1:length(counties)) {
  hisp_lat_race_list[[i]]<-getTractHispLatRace(state = 06,county = counties[i])
}

hisp_lat_race_df = do.call(rbind, hisp_lat_race_list)

###############
###Pop density
##############
state = 06
county = 01
getTractPopDens <- function(state, county) {
  varString <-"B01003_001E"
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACSpop <-
    as.data.frame(fromJSON(
      paste(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=tract:*&in=state:",
        state,
        "+county:",
        county,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038",
        sep = ""
      )
    ))
  ACSpop <- ACSpop[-1,]
  
  colnames(ACSpop) <-
    c(
      "name",
      "population_density",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

pop_dens_list<-list()

for (i in 1:length(counties)) {
  pop_dens_list[[i]]<-getTractPopDens(state = 06,county = counties[i])
}

pop_dens_df = do.call(rbind, pop_dens_list)


###############
###Household type including living alone pop >65
##############
state = 06
county = 01
getTractHouseholdType65 <- function(state, county) {
  varString <-"B09020_001E,B09020_002E,B09020_003E,B09020_014E,B09020_015E,B09020_016E,B09020_017E,B09020_018E,B09020_019E,B09020_021E"
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACSpop <-
    as.data.frame(fromJSON(
      paste(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=tract:*&in=state:",
        state,
        "+county:",
        county,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038",
        sep = ""
      )
    ))
  ACSpop <- ACSpop[-1,]
  
  colnames(ACSpop) <-
    c(
      "name",
      "household_type_over_65_total",
      "household_type_over_65_in_households",
      "household_type_over_65_in_households_family",
      "M_65_in_households_nonfamily_householder_total",
      "M_65_in_households_nonfamily_householder_alone",
      "M_65_in_households_nonfamily_householder_not_alone",
      "F_65_in_households_nonfamily_householder_total",
      "F_65_in_households_nonfamily_householder_alone",
      "F_65_in_households_nonfamily_householder_not_alone",
      "over_65_group_quarters",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

house_type_65_list<-list()

for (i in 1:length(counties)) {
  house_type_65_list[[i]]<-getTractHouseholdType65(state = 06,county = counties[i])
}

house_type_65_df = do.call(rbind, house_type_65_list)
join_df<-dplyr::right_join(race_df, pop_dens_df) %>% 
  right_join(.,med_inc_df) %>%
  right_join(.,house_type_65_df) %>% 
  right_join(., hisp_lat_race_df)

census_df<-join_df[,c(12:14, 1:11, 15:26,27,28,38)]

census_df$county<-as.character(census_df$county)
census_df$county<-as.factor(census_df$county)

county_names<-read.csv("county_names.csv", header=TRUE)
county_names<-county_names[,-3]
colnames(county_names)[1]<-"county"
county_names[,1]<-as.factor(county_names[,1])
library(stringr)
county_names[,1]<-str_pad(county_names[,1], 3, pad = "0")
county_names[,1]<-as.factor(county_names[,1])

census_df<- dplyr::right_join(census_df, county_names, by="county")
census_df<-census_df[,c(1:3, 30, 4:29)]
saveRDS(census_df, "census_tract_CA_heat.rds")

x<-readRDS("census_tract_CA_heat.rds")
