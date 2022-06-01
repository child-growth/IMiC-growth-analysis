


rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(),"/0-project-functions/0_wast_inc_functions.R"))

d <- readRDS(paste0(ghapdata_dir,"wasting_data.rds"))



dfull <- d
df <- d
d <- df %>% group_by(studyid, country) %>% do(WastIncCalc(.))
d_noBW <- df %>% group_by(studyid, country) %>% do(WastIncCalc(., dropBornWasted=T))

save(d, d_noBW, file=paste0(ghapdata_dir, "Wasting_inc_data.RData"))
