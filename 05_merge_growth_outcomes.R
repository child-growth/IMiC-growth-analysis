rm(list=ls())

source(paste0(here::here(), "/0-config.R"))

load(file=paste0(ghapdata_dir,"wast_prev.RData"))
load(file=paste0(ghapdata_dir,"wast_meanZ_outcomes.RData"))
load(file=paste0(ghapdata_dir,"wast_cuminc.rdata"))
load(file=paste0(ghapdata_dir,"pers_wast.rdata"))
load(file=paste0(ghapdata_dir,"wast_rec.rdata"))
load(file=paste0(ghapdata_dir,"st_prev_outcomes.RData"))
load(file=paste0(ghapdata_dir,"st_meanZ_outcomes.RData"))
load(file=paste0(ghapdata_dir,"st_cuminc_outcomes.rdata"))
load(file=paste0(ghapdata_dir,"haz_vel_outcomes.RData"))
load(file=paste0(ghapdata_dir,"waz_vel_outcomes.RData"))
load(file=paste0(ghapdata_dir,"len_vel_outcomes.RData"))
load(file=paste0(ghapdata_dir,"weight_vel_outcomes.RData"))
load(file=paste0(ghapdata_dir,"co_cuminc.rdata"))


cuminc_stunt <- cuminc_stunt %>% subset(., select = -c(arm, haz, agedays, measid, minhaz, Nobs,N))
cuminc_wast <- cuminc_wast %>% subset(., select = c(studyid, subjid, country, agecat, ever_wasted, ever_swasted))
pers_wast <- pers_wast %>% subset(., select = c(studyid, subjid, country, agecat, pers_wast))
cuminc_co <- cuminc_co %>% subset(., select = c(studyid, subjid, country, agecat, ever_co))
#rec_wast <- rec_wast %>% subset(., select = c(studyid, subjid, country, agecat, ever_co))


d <- left_join(meanHAZ, meanWHZ, by=c("studyid", "subjid", "country","agecat" ))
d <- left_join(d, prev_stunt, by=c("studyid", "subjid", "country","agecat" ))
d <- left_join(d, prev_wast, by=c("studyid", "subjid", "country","agecat" ))
d <- left_join(d, cuminc_stunt, by=c("studyid", "subjid", "country","agecat" ))
d <- left_join(d, cuminc_wast, by=c("studyid", "subjid", "country","agecat" ))
d <- left_join(d, cuminc_co, by=c("studyid", "subjid", "country","agecat" ))
d <- left_join(d, pers_wast, by=c("studyid", "subjid", "country","agecat" ))
#d <- left_join(d, rec_wast, by=c("studyid", "subjid", "country","agecat" ))
head(d)

#merge velocities
vel_haz <- vel_haz %>% rename(haz_rate=y_rate)
vel_waz <- vel_waz %>% rename(waz_rate=y_rate)
vel_lencm <- vel_lencm %>% rename(lencm_rate=y_rate)
vel_wtkg <- vel_wtkg %>% rename(wtkg_rate=y_rate)
vel <- left_join(vel_haz, vel_waz, by=c("studyid", "subjid", "country","agecat" ))
vel <- left_join(vel, vel_lencm, by=c("studyid", "subjid", "country","agecat" ))
vel <- left_join(vel, vel_wtkg, by=c("studyid", "subjid", "country","agecat" ))


saveRDS(d, file=paste0(ghapdata_dir,"imic_growth_outcomes.RDS"))
saveRDS(vel, file=paste0(ghapdata_dir,"imic_growth_trajectory_outcomes.RDS"))

