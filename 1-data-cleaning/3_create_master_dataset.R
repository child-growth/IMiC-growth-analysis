
#NOTE: THIS SCRIPT AND THESE COVARIATES WILL CHANGE FOR IMiC

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#load longform anthropometry and mortality data
Zscores <- readRDS(included_studies_path)


# Check how many at-birth measurements have
# length < 45cm and therefore no Z-scores
df <- Zscores %>% filter(agedays <= 7)
dim(df)
mean(df$lencm < 45, na.rm=T)*100
table(1*(df$lencm < 45), is.na(df$whz))
table(df$country, 1*(df$lencm < 45))

#keep only Z-scores and mortality variables, and month of measurement
dput(colnames(Zscores))
Zscores <- Zscores %>% 
  subset(., select = c(studyid, country, measurefreq, subjid, sex, agedays, 
                       waz, haz, whz, muaz, lencm, wtkg, htcm,  tr, arm,  month, brthweek, brthyr,  dead, agedth, 
                       latitude, longitud, causedth))


#load covariate dataset (one row per child)
cov <- readRDS(clean_covariates_path)

dput(colnames(cov))

cov <- cov %>% subset(., select = c(country, studyid, siteid, subjid, subjido, studytyp, 
                                    arm, armcd, sexn, sex, brthyr, brthweek, mage, 
                                    parity, nlchild, nperson, nrooms, meducyrs, h2osrcp, 
                                    cookplac, inctot, inctotu, ageimpfl, ageimpcm, visitnum, 
                                    visit, epochn, epoch, mhtcm, mwtkg, mbmi, mhgb, 
                                    pregout, delivrdt, dlvloc, dvseason, wtkg, lencm, 
                                    bmi, hcircm, muaccm, baz, muaz, feeding, dur_bf, 
                                    dur_ebf, visit_r_fl, dur_r, bfedfl_r, bfdu_r, exbfed_r, 
                                    exbfdu_r, bmcol_fl, bmid, fever_r, cough_r, diarr_r, 
                                    anti_r, citytown, gagebrth, gagecm, birthwt, birthlen, 
                                    birthord, gravida, nlivbrth, floor, gagedays, visitimpcm, 
                                    postbmi, mmuaccm, delivery, hgb, exbfdef, bfinittm, 
                                    cmfdint, bfmode, bfedfl, exbfedfl, formlkfl, sldfedfl, 
                                    anmlk_r, formlk_r, sldfed_r, fever, cough, diarr, 
                                    vomit, vomit_r, physican, hosp, antibiot, anti_oral, 
                                    anti_inj, anti_or_r, anti_in_r, mcrp, mferritin, mstrf, 
                                    magp, enstunt, enwast, birthLAZ, birthWAZ, birthmeas_age, 
                                    birthlen2, birthwt2, id))




table(Zscores$country)
table(cov$country)

dim(Zscores)
dim(cov)
d <- left_join(Zscores, cov, by = c("studyid", "subjid", "country"))
dim(d)

rm(Zscores, cov)
gc()

##Drop observations missing all anthropometry
dim(d)
d <- d %>% filter(!is.na(haz) | !is.na(whz) | !is.na(waz) | !is.na(muaz))
dim(d)


#Fill in missing month of measurement for cohorts that measure birth month  
table(is.na(d$month), is.na(d$brthmon))
table(is.na(d$month))
d$month[is.na(d$month)] <- ceiling((as.numeric(d$brthmon[is.na(d$month)]) + d$agedays[is.na(d$month)]/30.4167)%%12) 
table(is.na(d$month))
table(d$month)

#Rename tanzania
d$country[d$country=="TANZANIA, UNITED REPUBLIC OF"]<-"TANZANIA"
table(d$country)

# Save dataset
saveRDS(d, ki_manuscript_dataset_path)

