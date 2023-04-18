
#-----------------------------------------------------------------------------------------
# Output: long form dataset with all variables used in the IMiC manuscript analysis
#
# Author: Andrew Mertens (amertens@berkeley.edu)
#-----------------------------------------------------------------------------------------



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(lubridate)


#-------------------------------------------------------------------
# Elicit
#-------------------------------------------------------------------


#Elicit clean data
elicit <- read.csv("/data/imic/data/harmonized_datasets/ELICIT_IMiC_analysis.csv")

# ELICIT RAW DATA

elicit_raw_anthro <- readxl::read_excel("/data/imic/data/raw_field_data/elicit_raw/ELICIT anthro measurements for IMiC 1-2022.xlsx")
head(elicit_raw_anthro)

elicit_raw_bm <- read.csv("/data/imic/data/raw_field_data/elicit_raw/ELICIT breastmilk collection data 4-2022.csv")
head(elicit_raw_bm)

#This one seems to have all the data needed, including date of birth ("dob") and child age. I think the DDF-FINAL-IMiC file is the metadata file
elicit_raw <- readxl::read_excel("/data/imic/data/raw_field_data/elicit_raw/ELICIT meta-data IMiC 4-2021.xlsx")
colnames(elicit_raw)

#Calculate LAZ & WAZ dates 
lazprefix <- "agedays_laz"
wazprefix <- "agedays_waz"

samplelaz <- "sampledate_laz"
samplewaz <- "sampledate_waz"

rounds <- c("0","3","6","9","12","15")
for (val in rounds){
  
laz_string <- paste(lazprefix, val, sep="_") 
waz_string <- paste(wazprefix, val, sep="_") 
sample_laz_string <- paste(samplelaz, val, sep="_")
sample_waz_string <- paste(samplewaz, val, sep="_")

elicit_raw[sample_laz_string] <- ymd(elicit_raw$dob) + days(as.numeric(unlist(elicit_raw[laz_string])))
elicit_raw[sample_waz_string] <- ymd(elicit_raw$dob) + days(as.numeric(unlist(elicit_raw[waz_string])))
}

# # #To do: subset to just the needed variables and merge with the main data with the ID variable and the sample date
elicit_raw <- elicit_raw %>% select(pid, dob, agedays_laz_0, sampledate_laz_0, agedays_laz_3, sampledate_laz_3, agedays_laz_6, sampledate_laz_6, agedays_laz_9, sampledate_laz_9, agedays_laz_12, sampledate_laz_12, agedays_laz_15, sampledate_laz_15, agedays_waz_0, sampledate_waz_0, agedays_waz_3, sampledate_waz_3, agedays_waz_6, sampledate_waz_6, agedays_waz_9, sampledate_waz_9, agedays_waz_12, sampledate_waz_12, agedays_waz_15, sampledate_waz_15)
elicit_raw_bm <- elicit_raw_bm %>% 
  select(pid, bmc_date_collected) 

#Convert BM dataset to include visit round (1 month & 5 months)
elicit_bm <- elicit_raw_bm %>%
  mutate(VISIT = rep(c("1 Month Visit", "5 Months Visit"), times = 200)) %>%
  rename(BMC_Collection_Date = bmc_date_collected) %>% 
  rename(SUBJIDO = pid) 

#anthro 0,3,6,9,12,15

#Convert to longform dataset
lazpivot1 <- elicit_raw %>% 
  select(pid,dob,agedays_laz_0, agedays_laz_3, agedays_laz_6, agedays_laz_9, agedays_laz_12, agedays_laz_15) %>%
  pivot_longer(
    cols = starts_with("agedays_laz"),
    names_to = "VISIT",
    names_prefix = "agedays_laz_",
    values_to = "Age (days)",
    values_drop_na = TRUE
  )

lazpivot2 <- elicit_raw %>%
  select(pid,dob, sampledate_laz_0, sampledate_laz_3, sampledate_laz_6, sampledate_laz_9, sampledate_laz_12, sampledate_laz_15) %>%
  pivot_longer(
    cols = starts_with("sampledate_laz"),
    names_to = "VISIT",
    names_prefix = "sampledate_laz_",
    values_to = "anthro_date", #name sampling date to anthro dates
    values_drop_na = TRUE
  )


#Final elicit_raw longform data
lazmerged <- full_join(lazpivot1,lazpivot2, by=c("pid","dob","VISIT")) %>% 
  rename(SUBJIDO = pid) 

lazmerged <- lazmerged %>% mutate(VISIT = ifelse(VISIT == "0", "Enrolment Visit", 
                                   ifelse(VISIT == "3", "3 Months Visit",
                                          ifelse(VISIT == "6", "6 Months Visit",
                                                 ifelse(VISIT == "9", "9 Months Visit",
                                                        ifelse(VISIT == "12", "12 Months Visit",
                                                               ifelse(VISIT == "15", "15 Months Visit", VISIT)))))))

#Merge in with harmonized data set
dim(elicit)
dim(elicit_bm)
unique(elicit$VISIT)
unique(elicit_bm$VISIT)
elicit <- left_join(elicit, elicit_bm, by = c("SUBJIDO", "VISIT"))
dim(elicit)
table(!is.na(elicit$BMC_Collection_Date))

# dim(lazpivot1)
# dim(distinct(lazpivot1))
# dim(lazpivot2)
# dim(distinct(lazpivot2))
# lazmerged <- merge(lazpivot1,lazpivot2, by=c("pid", "Sampling Round (LAZ)"))
# lazmerged
# dim(lazmerged)

elicit <- left_join(elicit,lazmerged, by = c("SUBJIDO", "VISIT"))

#make single date variable
elicit$date <- elicit$anthro_date
elicit$date[is.na(elicit$date)] <- elicit$BMC_Collection_Date[is.na(elicit$date)]
table(is.na(elicit$date))

#-------------------------------------------------------------------
# VITAL
#-------------------------------------------------------------------

# VITAL cleaned dataset
vital <- read.csv("/data/imic/data/harmonized_datasets/VITAL_IMiC_analysis.csv")


#make anthropometry observation number
vital_vno <- vital %>% group_by(SUBJIDO) %>% arrange(AGEDAYS) %>% 
  filter(!is.na(LENCM)
         #NOTE! THIS is not filtering on any weight but there are many more weight measurements than other anthro that 
         #are not in the raw anthro dataset so the dates wouldn't line up. Check in the future
         #|!is.na(WTKG)
         |!is.na(BMI)|!is.na(MUACCM)) %>% mutate(vno=row_number()-1) %>% 
  select(SUBJIDO, AGEDAYS, vno) %>% distinct()

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
  select(ID, SUBJIDO)




#include date of visit & date of birth
#To do: subset to just the needed variables and merge with the main data with the ID variable and the sample date
vital_raw <- vital_raw %>%
  select(studyid, dov, dob, visit, vno) %>%
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
  select(SUBJIDO, VISIT, anthro_date, dob) %>% distinct()
dim(vitalmerged_anthro)

dim(vital)
vital <- left_join(vital, vitalmerged_anthro, by = c("SUBJIDO","VISIT"))
dim(vital)

#TO DO:
#double check that baseline anthro dataset is being used

#-------------------------------------------------------------------
# misame 
#-------------------------------------------------------------------

# misame clean data
misame_clean <- read.csv("/data/imic/data/harmonized_datasets/MISAME_3_IMiC_analysis.csv")

# misame raw data
misame_raw <- haven::read_sas("/data/imic/data/raw_field_data/misame_raw/misame3_imic.sas7bdat")
#need to transform the raw data from wide to long first

misame= NULL

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#-------------------------------------------------------------------
#combine data
#-------------------------------------------------------------------

head(vital)
head(elicit)

#Combine datasets
dfull <- bind_rows(vital, elicit, misame)
colnames(dfull) <- tolower(colnames(dfull))

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


#Read rds file and drop unneeded columns that are either used elsewhere in covariate creation or 
# were too rare to include as exposures (to avoid memory allocation issues)
# d <- readRDS(paste0(BV_dir,"mock_imic_data.RDS"))
# colnames(d) <- tolower(colnames(d))



#Check measurement frequency
meas_freq_tab <- d %>% filter(!is.na(waz)|!is.na(haz)) %>% group_by(studyid, country, subjid) %>% mutate(lagage=agedays-lag(agedays)) %>% group_by(studyid) %>% summarize(mn=mean(lagage,na.rm=T), md=median(lagage,na.rm=T))
meas_freq_tab



saveRDS(d, included_studies_path)
gc()

