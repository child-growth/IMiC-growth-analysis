
#-----------------------------------------------------------------------------------------
# Output: long form dataset with all variables used in the IMiC manuscript analysis
#
# Author: Andrew Mertens (amertens@berkeley.edu)
#-----------------------------------------------------------------------------------------



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(lubridate)


#-------------------------------------------------------------------
# misame 
#-------------------------------------------------------------------

# misame clean data
misame_clean <- read.csv("/data/imic/data/harmonized_datasets/MISAME_3_IMiC_analysis.csv")
table(misame_clean$VISIT, !is.na(misame_clean$HAZ))
head(misame_clean)


ggplot(misame_clean, aes(x=AGEDAYS,y=HAZ)) + geom_smooth() 
ggplot(misame_clean, aes(x=VISIT,y=HAZ)) + geom_jitter() + geom_smooth() 

misame_raw_long <- haven::read_sas("/data/imic/data/harmonized_datasets/full_misame_3.sas7bdat") 
summary(misame_clean$HAZ)
summary(misame_raw_long$HAZ)

colnames(misame_raw_long)

# misame raw data
#misame_raw <- haven::read_sas("/data/imic/data/raw_field_data/misame_raw/misame3_imic.sas7bdat")
misame_raw <- haven::read_sas("/data/imic/data/raw_field_data/misame_raw/misame3_wide.sas7bdat") 
colnames(misame_raw)
#need to transform the raw data from wide to long first

# # create data dictionary ----
# misame_dictionary <- data.frame(labelled::generate_dictionary(misame_raw))
# write.csv(misame_dictionary[,c(1:3)],file="/data/imic/data/raw_field_data/misame_raw/misame3_imic_variable_dictionary.csv", row.names = FALSE)


misame= NULL

head(misame_clean)
head(misame_raw)

colnames(misame_raw)

unique(misame_clean$SUBJIDO)
unique(misame_raw$idnew )



misame_raw_long_dates<- misame_raw %>% 
  rename(SUBJIDO=idnew ) %>%
  subset(., select=c(SUBJIDO, pn_date_1,
                     pn_date_2,
                     pn_date_3,
                     pn_date_4,
                     pn_date_5,
                     pn_date_6,
                     pn_date_9,
                     pn_date_12)) %>%
  pivot_longer(!SUBJIDO, names_to = "round", names_prefix = "pn_date_",  values_to = "date")

# [1] Delivery          Post Natal FU M01 Post Natal FU M02 Post Natal FU M03 Post Natal FU M04 Post Natal FU M05
# [7] Post Natal FU M06 Post Natal FU M09
table(misame_raw_long_dates$round)
misame_raw_long_dates <- misame_raw_long_dates %>%
  mutate(VISIT=case_when(
    round==0 ~ "Delivery",
    round==1 ~ "Post Natal FU M01",
    round==2 ~ "Post Natal FU M02",
    round==3 ~ "Post Natal FU M03",
    round==4 ~ "Post Natal FU M04",
    round==5 ~ "Post Natal FU M05",
    round==6 ~ "Post Natal FU M06",
    round==9 ~ "Post Natal FU M09",
    ))
table(misame_raw_long_dates$VISIT)
#Where is delivery round? may need to get birth anthropometry seperately.

# misame_raw_long_ages<- misame_raw %>% 
#   rename(SUBJIDO=idnew ) %>%
#   subset(., select=c(SUBJIDO,
#                      pn_cage_1,
#                      pn_cage_2,
#                      pn_cage_3,
#                      pn_cage_4,
#                      pn_cage_5,
#                      pn_cage_6,
#                      pn_cage_9,
#                      pn_cage_12)) %>%
#   pivot_longer(!SUBJIDO, names_to = "round", names_prefix = "pn_cage_",  values_to = "age")
# misame_raw_long <- left_join(misame_raw_long_dates, misame_raw_long_ages, by=c("SUBJIDO", "round")) %>% filter(!is.na(date))
# 
# #temp subset to anthro only:
# misame_anthro <- misame_clean %>% filter(!is.na(HAZ))  %>%
#   subset(., select=c(STUDYID, SUBJIDO, VISITNUM, AGEDAYS, WAZ, HAZ, WHZ, BAZ, MUAZ)) %>% distinct() %>%
#   mutate(agemonth=round(AGEDAYS/31))
# misame_raw_long <- misame_raw_long %>% mutate(agemonth=round(age))
# dim(misame_anthro)
# dim(misame_raw)
# misame <- left_join(misame_anthro, misame_raw_long, by=c("SUBJIDO","agemonth"))
# dim(misame)


# Add in: #IMIC collection dates and ID's
# IMiC_date_1421
# IMiC_date_12
# IMiC_date_34
# bmid1
# bmid2
# bmid3



#NOTE! Need to merge in birth anthro dates (or back calculate from 1 month date and age)

dim(misame_clean)
dim(misame_raw)
misame <- left_join(misame_clean, misame_raw_long_dates, by=c("SUBJIDO","VISIT"))
dim(misame)

table(is.na(misame$SEX))
table(misame$VISIT, is.na(misame$date))
table(is.na(misame$anthro_date))



#NOTE! Need to find the right origin
#misame$anthro_date = misame$date = as.Date(misame$date, origin = "1957-01-01")
misame$anthro_date = misame$date = as.Date(misame$date)

misame <- misame %>% mutate(SUBJIDO=as.character(SUBJIDO)) 
misame$COUNTRY <- "BURKINA FASO"


saveRDS(misame,file="/data/imic/data/harmonized_datasets/clean/misame_clean.RDS")
table(misame$VISIT, !is.na(misame$HAZ))


#to do: 
#just replace raw data anthro date visit number with the name of the visit number from the clean data

visit_tab <- as.data.frame(table(misame_clean$VISIT, !is.na(misame_clean$HAZ))) %>% filter(Var2==TRUE, Freq > 0)
unique(visit_tab$Var1)
