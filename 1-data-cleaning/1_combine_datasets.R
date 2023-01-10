
#-----------------------------------------------------------------------------------------
# Load FINAL dataset and drop studies 
# Output: long form dataset with all variables used in the IMiC manuscript analysis, minus those 
# covariates created from raw SAS datasets
#
# Author: Andrew Mertens (amertens@berkeley.edu)
#-----------------------------------------------------------------------------------------



# Instructions for downloading FINAL dataset

# Go to [insert git location]
# click clone button
# Copy link 
# Open Sourcetree (Click window icon in bottom left, then search magnifying glass icon
# in the top right, and search Sourcetree to find)
# Click clone button in source tree 
# Paste link in source path box
# Add U:/data/FINAL/ to the destination path (make sure FINAL folder is empty)
# Click clone



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

elicit <- read.csv("/data/imic/data/harmonized_datasets/ELICIT_IMiC_analysis.csv")
vital <- read.csv("/data/imic/data/harmonized_datasets/VITAL_IMiC_analysis.csv")

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Esther work here:
# Merge in date and location info
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# elicit_raw_anthro <- readxl::read_excel("/data/imic/data/raw_field_data/elicit_raw/ELICIT anthro measurements for IMiC 1-2022.xlsx")
# head(elicit_raw_anthro)

# elicit_raw_bm <- read.csv("/data/imic/data/raw_field_data/elicit_raw/ELICIT breastmilk collection data 4-2022.csv")
# head(elicit_raw_bm)

#This one seems to have all the data needed, including date of birth ("dob") and child age. I think the DDF-FINAL-IMiC file is the metadata file
elicit_raw <- readxl::read_excel("/data/imic/data/raw_field_data/elicit_raw/ELICIT meta-data IMiC 4-2021.xlsx")
head(elicit_raw)


# #To do: subset to just the needed variables and merge with the main data with the ID variable and the sample date
# elicit_raw <- elicit_raw %>% select(XXXXX)
# elicit <- left_join(elicit, elicit_raw_bm, by=c("XXXXX"))

#Check that each anthro observation has a calendar date associated with it
# head(elicit)

#look at metadata documentation and files here: /data/imic/data/raw_field_data/elicit_raw/
# to repeat the same process for elicit

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


head(vital)
head(elicit)

#Combine datasets
dfull <- bind_rows(vital, elicit)
colnames(dfull) <- tolower(colnames(dfull))

#Save full dataset
saveRDS(dfull, "/data/KI/imic/data/combined_raw_data.rds")


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

