
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))


#--------------------------------------------
# Read in master data file
#--------------------------------------------

d <- readRDS(paste0(ghapdata_dir, "FINAL_only_included_studies.rds"))
#d <- readRDS(paste0(ghapdata_dir, "ki-manuscript-dataset.rds"))
dim(d)


unique(d$studyid)

unique(paste0(d$studyid,"-",d$country))
length(unique(paste0(d$studyid,d$country,d$subjid)))


#--------------------------------------------
# Subset to  just identifying and anthro data
#--------------------------------------------

d <- d %>% subset(., select=c(studyid, subjid, country, arm, sex, agedays, haz, whz, waz, muaz))

#Check for duplicate agedays
dup_age <- d %>% group_by(studyid, subjid, agedays) %>%
  summarize(N=n())
mean(dup_age$N)

# count number of studies
length(names(table(d$studyid)))

# table of studies
table(d$studyid)
table(d$studyid,d$country)



#Children dropped for outlier exclusions
df <- d %>% filter(!is.na(haz)|!is.na(whz)|!is.na(waz))
nchild_cc <- nrow(df %>% distinct(studyid, subjid))
no_outliers_df <- df %>% filter(haz >= -6 & haz <=6, 
                               whz >= -5 & whz <=5,
                               waz >= -6 & waz <=5) %>%
                        mutate(id=paste0(studyid, "_",subjid))
outliers_df <- df %>% filter( !(haz >= -6 & haz <=6 &
                               whz >= -5 & whz <=5 &
                               waz >= -6 & waz <=5)) %>%
                        mutate(id=paste0(studyid, "_",subjid))
outliers_df <- outliers_df[!(outliers_df$id %in% no_outliers_df$id),]
nrow(outliers_df %>% distinct(id))
outliers_df<- outliers_df %>% group_by(id) %>% summarise(N=n()) 
prop.table(table(outliers_df$N))
outliers_df %>%  ungroup() %>% summarise(mean(N), median(N))


dropped <- nchild_cc - nrow(no_outliers_df %>% distinct(studyid, subjid))
dropped
dropped/nchild_cc * 100

#--------------------------------------------
# order data, create measurement id, and 
# drop unrealistic measures depending on 
# anthropometry measure
#--------------------------------------------

stunt <- d %>% filter(haz >= -6 & haz <=6, !is.na(haz)) %>%
  subset(., select = - c(whz, waz, muaz)) %>%
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(measid=seq_along(subjid)) 


wast <- d %>% filter(whz >= -5 & whz <=5) %>%
  subset(., select = - c(haz, waz, muaz)) %>%
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(measid=seq_along(subjid)) 

waz <- d %>% filter(waz >= -6 & waz <=5) %>%
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(measid=seq_along(subjid)) 

#double check calculations above
co <- d %>% filter(haz >= -6 & haz <=6 & whz >= -5 & whz <=5) %>%
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(measid=seq_along(subjid)) 

stunt <- droplevels(stunt)
wast <- droplevels(wast)
waz <- droplevels(waz)
co <- droplevels(co)


saveRDS(stunt, stunting_data_path)
saveRDS(wast, wasting_data_path)
saveRDS(waz, underweight_data_path)
saveRDS(co, co_occurrence_data_path)


#--------------------------------------------
# Get Stunting manuscript 
#--------------------------------------------
Ndf <- stunt 
length(unique(paste0(Ndf$studyid, Ndf$country))) #cohorts
length(unique(Ndf$country)) #Countries
length(unique(paste0(Ndf$studyid,"_", Ndf$subjid))) #Children
length(unique(paste0(Ndf$studyid, "_",Ndf$subjid, "_",Ndf$agedays))) #Observations
nrow(Ndf) #Observations


#--------------------------------------------
# Get Wasting N's
#--------------------------------------------
Ndf <- rbind(stunt, wast, waz) 
length(unique(paste0(Ndf$studyid, Ndf$country))) #cohorts
length(unique(Ndf$country)) #Countries
length(unique(paste0(Ndf$studyid,"_", Ndf$subjid))) #Children
length(unique(paste0(Ndf$studyid, "_", Ndf$subjid, "_", Ndf$agedays))) #Observations

