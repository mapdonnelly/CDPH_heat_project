rm(list=ls())
setwd('~/Desktop/GitHub/CDPH_heat_project/')

df<-readRDS("census_tract_CA_heat.rds")
xwalk<-read.csv("Tract_UR_key.csv", header=TRUE)
df$ct9<-paste(df$county, df$tract, sep='')
library(stringr)
xwalk$ct9<-str_pad(xwalk$ct9, 9, pad="0")
library(dplyr)
df <- xwalk %>% 
  dplyr::select(ct9 ,MSSAdef, NCHS_2013code,NCHS_2006code, NCHS_1990basedcode,NCHS2013_text) %>%
  right_join(df, by="ct9")
colnames(df)[22]<-"population"
###aggregating be regions in the 
##Knowlton et al. paper
##central_coast: Alameda, Contra Costa, Monterey, San Benito, San Francisco, San Luis Obispo, San Mateo, Santa Clara, Santa Cruz
df<- df %>% mutate( kn_region = ifelse(county_name %in% c("Alameda", "Contra Costa", "Monterey", "San Benito", "San Francisco",
                                                          "San Luis Obispo", "San Mateo", "Santa Clara", "Santa Cruz"), 
                                       "central_coast", 
                                       ifelse(county_name %in% c("Amador", "Calaveras", "Fresno", "Kern", "Kings", "Madera", "Mariposa", "Merced", "Placer", "Sacramento", "San Joaquin", "Stanislaus", "Tulare", "Tuolumne"), 
                                              "central_valley", 
                                              ifelse(county_name %in% c("Alpine", "Butte", "Colusa", "El Dorado",
                                                                        "Glenn", "Lassen", "Modoc", "Mono", "Nevada", "Plumas", "Shasta", "Sierra", "Siskiyou", "Sutter", "Tehama", "Yolo", "Yuba"), 
                                                     "north_central", 
                                                     ifelse(county_name %in% c("Del Norte", "Humboldt", "Lake", "Marin", "Mendocino", "Napa", "Solano", "Sonoma",
                                                                               "Trinity"), 
                                                            "north_coast", 
                                                            ifelse(county_name %in% c("Los Angeles", "Orange", "San Diego", "Santa Barbara", "Ventura"), 
                                                                   "south_coast", 
                                                                   ifelse(county_name %in% c("Imperial", "Inyo", "Riverside", "San Bernardino"),
                                                                          "southeast_dessert", 
                                                                          "none")))))))

df$kn_region<-as.factor(df$kn_region)

df[12:36] <- sapply(df[12:36],as.character)
df[12:36] <- sapply(df[12:36],as.numeric)
df$over_65_living_alone<-df$F_65_in_households_nonfamily_householder_alone+df$M_65_in_households_nonfamily_householder_alone

###calculate race proportions
df$prop.white<-df$white_alone/df$race_total
df$prop.af.am<-df$black_af_am_alone/df$race_total
df$prop.asian<-df$asian_alone/df$race_total
df$prop.am.ind<-df$am_ind_alone/df$race_total
df$prop.nat.haw<-df$nat_haw_pac_isl/df$race_total
df$prop.hisp.lat<-df$yes_hisp_lat_total/df$hisp_lat_race_total

##reduce dataframe
df<-df[,-c(4,5,19:21,27:32)]
inc.race<-readRDS("median_income_by_race.rds")

library(dplyr)
library(ggplot2)
df.urban.rural<-df %>% group_by(kn_region, MSSAdef) %>%
  tally()

df.urban.rural.pop<-df %>% dplyr::group_by(kn_region, MSSAdef) %>% dplyr::summarise(population_sum=sum(population, na.rm=TRUE))

labels<-c(central_coast="Central Coast", central_valley="Central Valley", north_central="North Central", north_coast="North Coast", south_coast="South Coast", southeast_dessert="Southeast Dessert")
library(scales)

png(file="mssa_population.png", width=1500,height=800)

  df.urban.rural.pop [which(!is.na(df.urban.rural.pop $MSSAdef)),] %>%
  ggplot() +
  geom_bar(mapping = aes(x=MSSAdef,y = population_sum, fill=MSSAdef), stat = "identity") +
  theme_bw()+
  facet_wrap(~kn_region,labeller = labeller(kn_region=labels))+
  ylab("Total population")+
  xlab("MSSA Definition")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = scales::comma)
dev.off()

png(file="mssa_population_boxplots.png", width = 1500, height = 800)
df[which(!is.na(df$MSSAdef)),] %>%
ggplot() +
  geom_boxplot(mapping = aes(x=MSSAdef,y = population, fill=MSSAdef)) +
  theme_bw()+
  facet_wrap(~kn_region,labeller = labeller(kn_region=labels))+
  ylab("Census tract population")+
  xlab("MSSA Definition")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) +
  scale_y_continuous(labels = scales::comma)
dev.off()

library(reshape2)
df.race<-df[,c(2,7,17,18,11:16,25,26)]
df.melt<-melt(df.race, id=c("MSSAdef", "tract", "kn_region", "population", "median_household_income"))
df.melt$variable<-factor(df.melt$variable, levels =c( "white_alone", "yes_hisp_lat_total","black_af_am_alone","am_ind_alone","asian_alone","nat_haw_pac_isl","some_other_alone" ))

labels<-c(central_coast="Central Coast", central_valley="Central Valley", north_central="North Central", north_coast="North Coast", south_coast="South Coast", southeast_dessert="Southeast Dessert")

png(file="pop_race_region.png", width=1500,height=800)
df.melt[which(!is.na(df.melt$MSSAdef)),] %>%
  ggplot() +
  geom_boxplot(mapping = aes(x=variable,y = value, fill=variable))+
  theme_bw()+
  facet_wrap(~kn_region, labeller=labeller(kn_region=labels)) +
  theme(axis.text.x=element_blank()) +
  ylim(0,15000)+
  xlab("Race/ethnicity")+
  ylab("Population by census tract")+
  scale_fill_discrete(name="Race/ethnicity", labels=c("White", "Hispanic","Af. America", "Am. Indian","Asian","Pac. Islander", "Other"))+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) 
dev.off()


labels<-c(central_coast="Central Coast", central_valley="Central Valley", north_central="North Central", north_coast="North Coast", south_coast="South Coast", southeast_dessert="Southeast Dessert")

png( file="race_mssa.png", width=1500,height=800)
df.melt[which(!is.na(df.melt$MSSAdef)),] %>%
  ggplot() +
  geom_boxplot(mapping = aes(x=variable,y = value, fill=MSSAdef))+
  theme_bw()+
  ylim(0,13000)+
  facet_wrap(~kn_region, labeller = labeller(kn_region=labels)) +
  scale_x_discrete(labels=c("Wh.", "Hisp.","Afr. Am.","Am. Ind.", "As.","Pac. Isl.", "Other"))+
  xlab("Race/ethnicity")+
  ylab("Population by census tract")+
  scale_fill_discrete(name="MSSA Definition")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) 
dev.off()


labels<-c(central_coast="Central Coast", central_valley="Central Valley", north_central="North Central", north_coast="North Coast", south_coast="South Coast", southeast_dessert="Southeast Dessert")

png( file="income_mssa.png", width=1500,height=800)

df[which(!is.na(df$MSSAdef)),] %>%
  ggplot() +
  geom_boxplot(mapping = aes(x=MSSAdef,y = median_household_income, fill=MSSAdef))+
  theme_bw()+
  facet_wrap(~kn_region, labeller = labeller(kn_region=labels)) +
  ylab("Median household income by census tract")+
  xlab("MSSA Definition")+
  scale_fill_discrete(name="MSSA Definition")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) 
dev.off()

png( file="over_65_mssa.png", width=1500,height=800)

df[which(!is.na(df$MSSAdef)),] %>%
  ggplot() +
  geom_boxplot(mapping = aes(x=MSSAdef,y = over_65_living_alone, fill=MSSAdef))+
  theme_bw()+
  facet_wrap(~kn_region, labeller = labeller(kn_region=labels)) +
  ylab("Population >65 living alone")+
  xlab("MSSA Definition")+
  scale_fill_discrete(name="MSSA Definition")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) 
dev.off()


#######
###median income by race
df.inc.race<- df %>% 
  right_join(inc.race, by="name")

df.race2<-df.inc.race[,c(2,7,17,18,26,34:41)]
df.melt2<-melt(df.race2, id=c("MSSAdef", "tract.x", "kn_region", "population", "median_household_income"))
df.melt2<-df.melt2[which(!is.na(df.melt2$MSSAdef)&!is.na(df.melt2$value)),]
df.melt2$value<-as.numeric(as.character(df.melt2$value))

png( file="income_by_race.png", width=1500,height=800)

df.melt2%>%
  ggplot() +
  geom_boxplot(mapping = aes(x=variable,y = value, fill=MSSAdef))+
  theme_bw()+
  #facet_wrap(~kn_region, labeller=labeller(kn_region=labels)) +
  scale_x_discrete(labels=c("Wh","AA","AI.", "As","PI", "Ot.", "NH Wh.", "Hi"))+
  xlab("Race/ethnicity")+
  ylab("Median Income by census tract")+
  scale_fill_discrete(name="MSSA Definition")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) 
dev.off()

png( file="income_by_race_urban.png", width=1500,height=800)

df.melt2%>%
  ggplot() +
  geom_boxplot(mapping = aes(x=MSSAdef,y = value, fill=variable))+
  theme_bw()+
  #facet_wrap(~kn_region, labeller=labeller(kn_region=labels)) +
  #scale_x_discrete(labels=c())+
  xlab("MSSA definition")+
  ylab("Median Income by census tract")+
  scale_fill_discrete(name="Income by race/ethnicity")+
  theme(legend.title=element_text(size=18), legend.text=element_text(size=18),strip.text.x=element_text(size=18), axis.text=element_text(size=16), axis.title = element_text(size = 18)) 
dev.off()
