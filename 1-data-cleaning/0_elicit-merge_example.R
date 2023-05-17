
#Elicit example data merging

rm(list=ls())
library(tidyverse)
library(lubridate)


#-------------------------------------------------------------------
# Elicit
#-------------------------------------------------------------------


#Elicit clean data
elicit <- read.csv("/data/imic/data/harmonized_datasets/ELICIT_IMiC_analysis.csv")
head(elicit)

# ELICIT RAW DATA
elicit_raw_anthro <- readxl::read_excel("/data/imic/data/raw_field_data/elicit_raw/ELICIT anthro measurements for IMiC 1-2022.xlsx")
head(elicit_raw_anthro)

#This one seems to have all the data needed, including date of birth ("dob") and child age. I think the DDF-FINAL-IMiC file is the metadata file
elicit_raw <- readxl::read_excel("/data/imic/data/raw_field_data/elicit_raw/ELICIT meta-data IMiC 4-2021.xlsx")
colnames(elicit_raw)

#Calculate LAZ & WAZ dates 
#The raw dataset doesn't have dates of anthropomentry measurement, so add ages at each measurement to the data of birth to get the sample date
lazprefix <- "agedays_laz"
wazprefix <- "agedays_waz"

samplelaz <- "sampledate_laz"
samplewaz <- "sampledate_waz"

rounds <- c("0","3","6","9","12","15","18")
for (val in rounds){
  
  laz_string <- paste(lazprefix, val, sep="_") 
  waz_string <- paste(wazprefix, val, sep="_") 
  sample_laz_string <- paste(samplelaz, val, sep="_")
  sample_waz_string <- paste(samplewaz, val, sep="_")
  
  elicit_raw[sample_laz_string] <- ymd(elicit_raw$dob) + days(as.numeric(unlist(elicit_raw[laz_string])))
  elicit_raw[sample_waz_string] <- ymd(elicit_raw$dob) + days(as.numeric(unlist(elicit_raw[waz_string])))
}

# subset to just the needed variables and merge with the main data with the ID variable and the sample date
elicit_raw <- elicit_raw %>% subset(., select=c(pid, dob, 
                                                agedays_laz_0, sampledate_laz_0, 
                                                agedays_laz_3, sampledate_laz_3, 
                                                agedays_laz_6, sampledate_laz_6, 
                                                agedays_laz_9, sampledate_laz_9,
                                                agedays_laz_12, sampledate_laz_12, 
                                                agedays_laz_15, sampledate_laz_15, 
                                                agedays_laz_18, sampledate_laz_18, 
                                                agedays_waz_0, sampledate_waz_0, 
                                                agedays_waz_3, sampledate_waz_3, 
                                                agedays_waz_6, sampledate_waz_6, 
                                                agedays_waz_9, sampledate_waz_9,
                                                agedays_waz_12, sampledate_waz_12, 
                                                agedays_waz_15, sampledate_waz_15,
                                                agedays_waz_18, sampledate_waz_18)) %>%
                              mutate(agedays_laz_18 = as.character(agedays_laz_18))

head(elicit_raw)

table(elicit_raw$agedays_laz_3 == elicit_raw$agedays_waz_3)
elicit_raw[elicit_raw$agedays_laz_3 != elicit_raw$agedays_waz_3 , ]


#Convert to longform dataset
lazpivot1 <- elicit_raw %>% 
  subset(., select=c(pid,dob,agedays_laz_0, agedays_laz_3, agedays_laz_6, agedays_laz_9, agedays_laz_12, agedays_laz_15, agedays_laz_18)) %>%
  pivot_longer(
    cols = starts_with("agedays_laz"),
    names_to = "VISIT",
    names_prefix = "agedays_laz_",
    values_to = "Age (days)",
    values_drop_na = TRUE
  )

lazpivot2 <- elicit_raw %>%
  subset(., select=c(pid,dob, sampledate_laz_0, sampledate_laz_3, sampledate_laz_6, sampledate_laz_9, sampledate_laz_12, sampledate_laz_15, sampledate_laz_18)) %>%
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

head(lazmerged, 20)

lazmerged <- lazmerged %>% mutate(VISIT = case_when(
                               VISIT == "0" ~ "Enrolment Visit",
                               VISIT == "3" ~ "3 Months Visit",
                               VISIT == "6" ~ "6 Months Visit",
                               VISIT == "9" ~ "9 Months Visit",
                               VISIT == "12" ~ "12 Months Visit",
                               VISIT == "15" ~ "15 Months Visit",
                               VISIT == "18" ~ "18 Months Visit"))
  
  
  

#Merge in with harmonized data set
elicit <- left_join(elicit,lazmerged, by = c("SUBJIDO", "VISIT"))


ggplot(elicit, aes(x = month(anthro_date))) +
  geom_histogram(binwidth = 1)


# 2a) Make spline plot of mean whz by day of the year
elicit <- elicit %>% rename(`Anthro Date` = anthro_date)

#look by calendar day
ggplot(data=elicit, aes(x=yday(`Anthro Date`), y=WHZ)) + geom_smooth() + geom_point()
ggplot(data=elicit, aes(x=yday(`Anthro Date`), y=WHZ)) + geom_smooth() 

#look by date
ggplot(data=elicit, aes(x=`Anthro Date`, y=WHZ)) + geom_smooth() + geom_point()



elicit %>% filter(!is.na(`Anthro Date`)) %>%
  mutate(month=month(`Anthro Date`)) %>%
  group_by(month) %>%
  summarise(N_measurements=n(), mean_whz=mean(WHZ, na.rm=T), mean_age=mean(AGEDAYS))


df <- elicit %>% filter(is.na(`Anthro Date`), !is.na(WHZ) & !is.na(HAZ))
df

dim(df)
table(df$VISIT)




