
#-----------------------------------------------------------------------------------------
# Output: long form dataset with all variables used in the IMiC manuscript analysis
#
# Author: Andrew Mertens (amertens@berkeley.edu)
#-----------------------------------------------------------------------------------------



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(lubridate)

misame <- readRDS("/data/imic/data/harmonized_datasets/clean/misame_clean.RDS")
vital <- readRDS("/data/imic/data/harmonized_datasets/clean/vital_clean.RDS")
elicit <- readRDS("/data/imic/data/harmonized_datasets/clean/elicit_clean.RDS")



#-------------------------------------------------------------------
#combine data
#-------------------------------------------------------------------

# head(vital)
# head(elicit)
# 
# class(vital$date)
# class(elicit$date)
# class(misame$date)

#Combine datasets
dfull <- bind_rows(vital, elicit, misame)
colnames(dfull) <- tolower(colnames(dfull))
table(dfull$country)

table(dfull$studyid,is.na(dfull$bmc_collection_date))

#Save full dataset
saveRDS(dfull, "/data/imic/data/combined_raw_data.rds")

dfull %>% group_by(studyid) %>%
  summarise(min(anthro_date, na.rm=T), max(anthro_date, na.rm=T))


#Subset to rows with growth measures
dim(dfull)
d <- dfull %>% filter(!is.na(waz) | !is.na(haz) | !is.na(whz) | !is.na(baz) | !is.na(muaz))
dim(d)

#number of kids
dkids <- d %>% group_by(subjido) %>% slice(1)
length(dkids$subjido)
table(dkids$studyid)
table(dkids$studyid, dkids$arm)

#visit numbers
table(d$visit[d$studyid=="ELICIT"])
table(d$visit[d$studyid=="VITAL-Lactation"])
table(d$visit[d$studyid=="MISAME-3"])


#Read rds file and drop unneeded columns that are either used elsewhere in covariate creation or 
# were too rare to include as exposures (to avoid memory allocation issues)
# d <- readRDS(paste0(BV_dir,"mock_imic_data.RDS"))
# colnames(d) <- tolower(colnames(d))



#Check measurement frequency
meas_freq_tab <- d %>% filter(!is.na(waz)|!is.na(haz)) %>% group_by(studyid, country, subjid) %>% mutate(lagage=agedays-lag(agedays)) %>% group_by(studyid) %>% summarize(mn=mean(lagage,na.rm=T), md=median(lagage,na.rm=T))
meas_freq_tab



saveRDS(d, "/data/imic/data/imic_combined_anthro.rds")
gc()




#can code into improved or not
#https://washdata.org/monitoring/drinking-water
unique(d$h2osrcp)
table(d$studyid, d$h2osrcp)



# The JMP also differentiates populations using unimproved sources such as unprotected wells or springs,
# and populations drinking surface water collected directly from a river, dam, lake, stream or irrigation canal. 
d <- d %>%
  mutate(improved_water=case_when(h2osrcp %in% c("Piped", "Piped to yard/plot", "Public tap/stand pipe","Protected well","Tube well or borehole" ,"Tap in yard",  
                                                 "Pump well or borehole","Well dug: Protected pit",  "Public tap/public drinking fountain" ,"Boring" , "Community tap water"  ) ~ "Improved",
                                  h2osrcp %in% c("Unprotected well",  "Surface water(river/dam/lake/pond/stream",     "Well dug: Non Protected pit" ,
                                                 "Water tanker","Bottle"  ) ~ "Unimproved",
                                  h2osrcp=="" ~ "missing"))
table(d$studyid, d$improved_water)




#floor code by improved material versus natural
table(d$studyid, d$floor)
d <- d %>%
  mutate(improved_floor=case_when(floor %in% c("Cement", "Linoleum (plastic)",  "Tiles", "Tiles, parquet", "Wood") ~ 1,
                                  floor %in% c("Natural/mud",  "Clay") ~ 0))
table(d$studyid, d$improved_floor)




#elicit breastfeeding - had just crudely coded it as if DUR_EBF>AGEDAYS, but age=183 at 6 months but DUR_BF is coded in 30 day chunks (180 days) 

#date origin in misame

#anthro visit number in vital (vno)
