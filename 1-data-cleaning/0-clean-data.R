
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

source(paste0(here(),"/1-data-cleaning/1_combine_datasets.R"))
source(paste0(here(),"/1-data-cleaning/2_clean_FINAL_covariates.R"))
source(paste0(here(),"/1-data-cleaning/3_create_master_dataset.R"))
source(paste0(here(),"/1-data-cleaning/4_anthro_prepdata.R"))
source(paste0(here(),"/1-data-cleaning/5_biomarker.R"))


