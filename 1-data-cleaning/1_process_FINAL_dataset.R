
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



#Read rds file and drop unneeded columns that are either used elsewhere in covariate creation or 
# were too rare to include as exposures (to avoid memory allocation issues)
d <- readRDS(paste0(BV_dir,"mock_imic_data.RDS"))


colnames(d) <- tolower(colnames(d))



#Check measurement frequency
meas_freq_tab <- d %>% filter(!is.na(waz)|!is.na(haz)) %>% group_by(studyid, country, subjid) %>% mutate(lagage=agedays-lag(agedays)) %>% group_by(studyid) %>% summarize(mn=mean(lagage,na.rm=T), md=median(lagage,na.rm=T))
meas_freq_tab




saveRDS(d, included_studies_path)
gc()
