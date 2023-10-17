
#-----------------------------------------------------------------------------------------
# Output: long form dataset with all variables used in the IMiC manuscript analysis
#
# Author: Andrew Mertens (amertens@berkeley.edu)
#-----------------------------------------------------------------------------------------



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(lubridate)




#-------------------------------------------------------------------
# VITAL
#-------------------------------------------------------------------

# VITAL cleaned dataset
vital <- read.csv("/data/imic/data/harmonized_datasets/VITAL_Lactation_IMiC_analysis.csv")
head(vital)

unique(vital$CITYTOWN)


#use to merge in!!!

#make anthropometry observation number
vital_vno <- vital %>% group_by(SUBJIDO) %>% arrange(AGEDAYS) %>% 
  filter(!is.na(LENCM)
         #NOTE! THIS is not filtering on any weight but there are many more weight measurements than other anthro that 
         #are not in the raw anthro dataset so the dates wouldn't line up. Check in the future
         #|!is.na(WTKG)
         |!is.na(BMI)|!is.na(MUACCM)) %>% mutate(vno=row_number()-1) %>% 
  subset(., select=c(SUBJIDO, AGEDAYS, vno)) %>% distinct()

dim(vital)
dim(vital_vno)
vital_anthro <- left_join(vital_vno, vital, by=c("SUBJIDO","AGEDAYS"))
dim(vital_anthro)

table(is.na(vital_anthro$vno))
table(is.na(vital_anthro$HAZ))

# VITAL RAW DATA

#This one seems to have all the data needed, including date of visit, dob, and anthropometry metrics
vital_raw <- read.csv("/data/imic/data/raw_field_data/vital_raw/ZSCORE_FOR_EACH_VISIT.csv")


#Vital_raw is the Z-scores for all 6 visits concatenated, but doesn't have a subject ID that links to the main dataset
#I think both this date of birth and baseline anthro have both ID's to merge to both datasets to allow them to merge
dob <- read.csv("/data/imic/data/raw_field_data/vital_raw/DOB.csv")
vital_baseline_anthro <- read.csv("/data/imic/data/raw_field_data/vital_raw/CRF3C_BANTHRO.csv")

vital_baseline_anthro <- vital_baseline_anthro %>%
  rename(ID = crf3c_q2) %>%
  rename(SUBJIDO = assid)
  
vital_ids <- inner_join(dob, vital_baseline_anthro, by = c("ID", "SUBJIDO")) %>%
  subset(., select=c(ID, SUBJIDO))




#include date of visit & date of birth
#To do: subset to just the needed variables and merge with the main data with the ID variable and the sample date
vital_raw <- vital_raw %>%
  subset(., select=c(studyid, dov, dob, visit, vno)) %>%
  rename(ID = studyid)

vital_raw <- left_join(vital_raw, vital_ids, by = "ID")



temp <- vital %>% filter(SUBJIDO=="EMP6:BH:6261") %>% filter(!is.na(LENCM)|!is.na(WTKG)|!is.na(BMI)|!is.na(MUACCM)) 
temp_raw <- vital_raw %>% filter(SUBJIDO=="EMP6:BH:6261")

#Merge in with harmonized data set
table(vital_anthro$vno)
table(vital_raw$vno)
#NOTE! Look into the high visit numbers in vital


#merge in vital anthro


dim(vital_anthro)
dim(vital_raw)
vitalmerged_anthro <- left_join(vital_anthro, vital_raw, by = c("SUBJIDO","vno"))
dim(vitalmerged_anthro)

#merge anthro dates back into main dataset
dim(vitalmerged_anthro)
vitalmerged_anthro <- vitalmerged_anthro %>% 
  mutate(anthro_date=mdy(dov),
         dob=mdy(dob)) %>% 
  subset(., select=c(SUBJIDO, VISIT, anthro_date, dob)) %>% distinct()
dim(vitalmerged_anthro)

dim(vital)
vital <- left_join(vital, vitalmerged_anthro, by = c("SUBJIDO","VISIT"))
dim(vital)

#TO DO:
#double check that baseline anthro dataset is being used
saveRDS(vital,file="/data/imic/data/harmonized_datasets/clean/vital_clean.RDS")

