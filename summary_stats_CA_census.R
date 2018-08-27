###This script is to start looking at summary statistics for the counties and region in CA
##for the heat-related morbidity and mortality project at CDPH
rm(list=ls())
df<-readRDS("census_tract_CA_heat.rds")
xwalk<-read.csv("Tract_UR_key.csv", header=TRUE)
df$ct9<-paste(df$county, df$tract, sep='')
library(stringr)
xwalk$ct9<-str_pad(xwalk$ct9, 9, pad="0")


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

df[12:33] <- sapply(df[12:33],as.character)
df[12:33] <- sapply(df[12:33],as.numeric)

###Summary stats
library(dplyr)

##median income plots
df %>% filter(kn_region=="central_coast") %>% 
  dplyr::select(median_household_income) %>%  
  ggplot(aes(median_household_income)) +                     
  geom_histogram() + 
  theme_minimal()

df %>% filter(kn_region=="central_valley") %>% 
  dplyr::select(median_household_income) %>%  
  ggplot(aes(median_household_income)) +                     
  geom_histogram() + 
  theme_minimal()

df %>% filter(kn_region=="north_central") %>% 
  dplyr::select(median_household_income) %>%  
  ggplot(aes(median_household_income)) +                     
  geom_histogram() + 
  theme_minimal()

df %>% filter(kn_region=="north_coast") %>% 
  dplyr::select(median_household_income) %>%  
  ggplot(aes(median_household_income)) +                     
  geom_histogram() + 
  theme_minimal()

df %>% filter(kn_region=="south_coast") %>% 
  dplyr::select(median_household_income) %>%  
  ggplot(aes(median_household_income)) +                     
  geom_histogram() + 
  theme_minimal()

df %>% filter(kn_region=="southeast_dessert") %>% 
  dplyr::select(median_household_income) %>%  
  ggplot(aes(median_household_income)) +                     
  geom_histogram() + 
  theme_minimal()

###Facet plot
df %>% 
  ggplot(aes(median_household_income)) +                     
  geom_histogram( fill="darkturquoise", alpha=.5) + 
  theme_minimal() +
  facet_wrap(~ kn_region)


df %>% 
  ggplot(aes(population)) +                     
  geom_histogram(fill="violet", alpha=.5) + 
  theme_minimal() +
  xlim(0,20000) +
  facet_wrap(~ kn_region)

df %>% 
  ggplot() +
  geom_histogram(aes(white_alone), fill="darkturquoise", alpha=.5) + 
    theme_minimal() +
  xlim(0,15000) +
  facet_wrap(~ kn_region)

df %>% 
  ggplot() +
  geom_histogram(aes(black_af_am_alone), fill="violet",alpha=.5) +
  theme_minimal() +
  xlim(0,6000) +
  ylim(0,1250) +
  facet_wrap(~ kn_region)

df %>% 
  ggplot() +
  geom_histogram(aes(am_ind_alone), fill="slateblue1",alpha=.5) +
  theme_minimal() +
  xlim(0,500) +
  ylim(0,1000) +
  facet_wrap(~ kn_region)

df %>% 
  ggplot() +
  geom_histogram(aes(asian_alone), fill="palevioletred1",alpha=.5) +
  theme_minimal() +
  xlim(0,7000) +
  ylim(0,1500) +
  facet_wrap(~ kn_region)

df %>% 
  ggplot() +
  geom_histogram(aes(nat_haw_pac_isl), fill="darkseagreen4",alpha=.5) +
  theme_minimal() +
  xlim(0,1000) +
  ylim(0,400) +
  facet_wrap(~ kn_region)


##sums of urban and rural pops per region
df.urban.rural<-df %>% group_by(kn_region, MSSAdef) %>%
  tally()

df.urban.rural.pop<-df %>% dplyr::group_by(kn_region, MSSAdef) %>% dplyr::summarise(population_sum=sum(population, na.rm=TRUE))
 
df.urban.rural.pop %>%
  ggplot() +
  geom_bar(mapping = aes(x=MSSAdef,y = population_sum, fill=MSSAdef), stat = "identity") +
  theme_minimal()+
  facet_wrap(~kn_region)
