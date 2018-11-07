##Script to loop over every county and retrieve census data
rm(list=ls())
library(jsonlite)
library(tidyverse)
library(acs)
setwd('~/Desktop/GitHub/CDPH_heat_project/')
xwalk<-read.csv("Tract_UR_key.csv", header=TRUE)
api.key.install(key="564b38dac3a753efb9e6541ae2414678652018d6")

county = 01
state= 06



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
  varString <-"B03002_001E,B03002_002E,B03002_003E,B03002_012E"
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
      "hisp_lat_total",
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


################
###Median income by race/ethnicity
################

getTractMedIncRace <- function(state, county) {
  varString <-"B19013A_001E,B19013B_001E,B19013C_001E,B19013D_001E,B19013E_001E,B19013F_001E,B19013H_001E,B19013I_001E"
  
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
      "median_h_income_white_alone",
      "median_h_income_black_alone",
      "median_h_income_am_ind_alone",
      "median_h_income_asian_alone",
      "median_h_income_nat_haw_alone",
      "median_h_income_other_alone",
      "median_h_income_non_hisp_white_alone",
      "median_h_income_hisp_lat_alone",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

med_inc_race_list<-list()

for (i in 1:length(counties)) {
  med_inc_race_list[[i]]<-getTractMedIncRace(state = 06,county = counties[i])
}

med_inc_race_df = do.call(rbind, med_inc_race_list)
saveRDS(med_inc_race_df, "median_income_by_race.rds")


###############
###Below poverty line by sex and age
##############
state = 06
county = 01
getTractPovertyAgeSex <- function(state, county) {
  varString <-"B17001_001E,B17001_002E,B17001_003E,B17001_004E,B17001_005E,B17001_006E,B17001_007E,B17001_008E,B17001_009E,B17001_010E,B17001_011E,B17001_012E,B17001_013E,B17001_014E,B17001_015E,B17001_016E,B17001_017E,B17001_018E,B17001_019E,B17001_020E,B17001_021E,B17001_022E,B17001_023E,B17001_024E,B17001_025E,B17001_026E,B17001_027E,B17001_028E,B17001_029E,B17001_030E"
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
      "poverty_total",
      "Income_in_the_past_12_months_below_poverty_level_total",
      "Income_in_the_past_12_months_below_poverty_level_Male",
      "Income_in_the_past_12_months_below_poverty_level_Male_Under_5_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_5_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_6_to_11_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_12_to_14_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_15_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_16_and_17_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_18_to_24_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_25_to_34_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_35_to_44_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_45_to_54_years",
      "Income in the past 12 months below poverty level_Male_55 to 64 years",
      "Income_in_the_past_12_months_below_poverty_level_Male_65_to_74_years",
      "Income_in_the_past_12_months_below_poverty_level_Male_75_years_and_over",
      "Income_in_the_past_12_months_below_poverty_level_Female",
      "Income_in_the_past_12_months_below_poverty_level_Female_Under_5_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_5_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_6_to_11_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_12_to_14_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_15_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_16_and_17_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_18_to_24_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_25_to_34_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_35_to_44_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_45_to_54_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_55_to_64_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_65_to_74_years",
      "Income_in_the_past_12_months_below_poverty_level_Female_75_years_and_over",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

poverty_age_sex_list<-list()

for (i in 1:length(counties)) {
  poverty_age_sex_list[[i]]<-getTractPovertyAgeSex(state = 06,county = counties[i])
}

poverty_age_sex_df = do.call(rbind, poverty_age_sex_list)
saveRDS(poverty_age_sex_df, "poverty_age_sex.RDS")


################
####Sex by Age classes
##############
state = 06
county = 01
getTractAgeSex <- function(state, county) {
  varString <-"B01001_001E,B01001_002E,B01001_003E,B01001_004E,B01001_005E,B01001_006E,B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,B01001_026E,B01001_027E,B01001_028E,B01001_029E,B01001_030E,B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E"
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
      "sex_age_total",
      "male_sex_age_total",
      "Male:!!Under 5 years",
      "Male:!!5 to 9 years",
      "Male:!!10 to 14 years",
      "Male:!!15 to 17 years",
      "Male:!!18 and 19 years",
      "Male:!!20 years",
      "Male:!!21 years",
      "Male:!!22 to 24 years",
      "Male:!!25 to 29 years",
      "Male:!!30 to 34 years",
      "Male:!!35 to 39 years",
      "Male:!!40 to 44 years",
      "Male:!!45 to 49 years",
      "Male:!!50 to 54 years",
      "Male:!!55 to 59 years",
      "Male:!!60 and 61 years",
      "Male:!!62 to 64 years",
      "Male:!!65 and 66 years",
      "Male:!!67 to 69 years",
      "Male:!!70 to 74 years",
      "Male:!!75 to 79 years",
      "Male:!!80 to 84 years",
      "Male:!!85 years and over",
      "sex_age_female_total",
      "Female:!!Under 5 years",
      "Female:!!5 to 9 years",
      "Female:!!10 to 14 years",
      "Female:!!15 to 17 years",
      "Female:!!18 and 19 years",
      "Female:!!20 years",
      "Female:!!21 years",
      "Female:!!22 to 24 years",
      "Female:!!25 to 29 years",
      "Female:!!30 to 34 years",
      "Female:!!35 to 39 years",
      "Female:!!40 to 44 years",
      "Female:!!45 to 49 years",
      "Female:!!50 to 54 years	",
      "Female:!!55 to 59 years",
      "Female:!!60 and 61 years	",
      "Female:!!62 to 64 years",
      "Female:!!65 and 66 years",
      "Female:!!67 to 69 years",
      "Female:!!70 to 74 years",
      "Female:!!75 to 79 years",
      "Female:!!80 to 84 years",
      "Female:!!85 years and over",
      "state",
      "county",
      "tract"
    )
  
  #ACSpop <- ACSpop %>% gather(2:5,key = variable, value = value)
  
  return(ACSpop)
  
}

counties<-unique(xwalk$COUNTY)

age_sex_list<-list()

for (i in 1:length(counties)) {
  age_sex_list[[i]]<-getTractAgeSex(state = 06,county = counties[i])
}

age_sex_df = do.call(rbind, age_sex_list)
saveRDS(age_sex_df, "age_sex.RDS")

#####Join all vars into one dataframe
join_df<-dplyr::right_join(race_df, pop_dens_df) %>% 
  right_join(.,med_inc_df) %>%
  right_join(.,house_type_65_df) %>% 
  right_join(., hisp_lat_race_df) %>%
  right_join(.,med_inc_race_df) %>%
  right_join(.,poverty_age_sex_df) %>%
  right_join(.,age_sex_df)




census_df<-join_df[,c(12:14, 1:11, 15:117)]

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
census_df<-census_df[,c(1:3, 118, 4:117)]
saveRDS(census_df, "census_tract_CA_heat2.rds")

x<-readRDS("census_tract_CA_heat2.rds")



