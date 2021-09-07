
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

d <- readRDS("/data/KI/UCB-SuperLearner/Manuscript analysis data/FINAL_only_included_studies.rds")

d <- d %>% filter(measurefreq=="monthly")
table(d$studyid)

d <- d %>% filter(studyid %in% c("TanzaniaChild2","GMS-Nepal","MAL-ED","CMC-V-BCS-2002"))
d$country[d$studyid=="TanzaniaChild2"] <- "Tanzania"
d$country[d$studyid=="GMS-Nepal"] <- "Canada"
d$country[d$studyid=="MAL-ED"] <- "Pakistan"
d$country[d$studyid=="CMC-V-BCS-2002"] <- "Burkina Faso"

d$studyid[d$studyid=="TanzaniaChild2"] <- "ELICIT"
d$studyid[d$studyid=="GMS-Nepal"] <- "CHILD"
d$studyid[d$studyid=="MAL-ED"] <- "VITAL"
d$studyid[d$studyid=="CMC-V-BCS-2002"] <- "MISAME-3"

d$haz <- d$haz + rnorm(nrow(d), mean=0, sd=0.1)
d$waz <- d$waz + rnorm(nrow(d), mean=0, sd=0.1)
d$whz <- d$whz + rnorm(nrow(d), mean=0, sd=0.1)
 
saveRDS(d, file=paste0(BV_dir,"mock_imic_data.RDS"))
 





