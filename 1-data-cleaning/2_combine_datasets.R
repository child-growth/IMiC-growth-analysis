
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

#Save full dataset
saveRDS(dfull, "/data/imic/data/combined_raw_data.rds")


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

