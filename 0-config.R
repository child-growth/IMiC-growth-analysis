#-------------------------------------
# ki longitudinal analysis manuscripts

# configure data directories
# source base functions
# load libraries
#-------------------------------------
kiPath <- c("/data/KI/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths())
.libPaths(kiPath)

library(tidyverse)

#suppress grouping messages
options(dplyr.summarise.inform=F) 

# library(tidyverse)
# library(metafor)
# library(here)
# library(data.table)
# library(ggthemes)

# Define directories
# results that can be pushed to github:
res_dir                           = "/data/KI/imic/results/"
data_dir                          = "/data/KI/imic/data/"
ghapdata_dir                      = "/data/KI/imic/data/"

#Bluevelvet directory
BV_dir                            = "/data/KI/imic/"


project_functions_dir             = paste0(here::here(),"/0-project-functions")
data_cleaning_dir                 = paste0(here::here(),"/1-data-cleaning")
descriptive_outcomes_dir          = paste0(here::here(),"/2-descriptive-outcomes")
prep_tmle_analysis_dir            = paste0(here::here(),"/3-prep-tmle-analysis")
longbow_tmle_analysis_dir         = paste0(here::here(),"/4-longbow-tmle-analysis")
visualizations_dir                = paste0(here::here(),"/5-visualizations")


fig_dir                           = "/data/KI/imic/figures/"
figdata_dir_stunting              = paste0(fig_dir,"stunting/figure-data/")
figdata_dir_wasting               = paste0(fig_dir,"wasting/figure-data/")


#################################
# Data Cleaning Scripts
included_studies_path             = paste0(ghapdata_dir, "FINAL_only_included_studies.rds")
clean_covariates_path             = paste0(ghapdata_dir,"FINAL_clean_covariates.rds")
ki_manuscript_dataset_path        = paste0(ghapdata_dir,"ki-manuscript-dataset.rds")
rf_stunting_data_path             = paste0(ghapdata_dir, "rf_stunting_data.rds")
rf_wasting_data_path              = paste0(ghapdata_dir, "rf_wasting_data.rds")
rf_underweight_path               = paste0(ghapdata_dir, "rf_underweight_data.rds")
rf_co_occurrence_path             = paste0(ghapdata_dir, "rf_co_occurrence_data.rds")
stunting_data_path                = paste0(ghapdata_dir, "stunting_data.rds")
wasting_data_path                 = paste0(ghapdata_dir, "wasting_data.rds")
underweight_data_path             = paste0(ghapdata_dir, "underweight_data.rds")
co_occurrence_data_path           = paste0(ghapdata_dir, "co_occurrence_data.rds")


##################################

# Source base functions
source(paste0(project_functions_dir, "/0_clean_study_data_functions.R"))
source(paste0(project_functions_dir, "/0_descriptive_epi_shared_functions.R"))
source(paste0(project_functions_dir, "/0_descriptive_epi_stunt_functions.R"))
source(paste0(project_functions_dir, "/0_descriptive_epi_wast_functions.R"))
source(paste0(project_functions_dir, "/0_risk_factor_functions.R"))
source(paste0(project_functions_dir, "/0_shift_intervention_functions.R"))
source(paste0(project_functions_dir, "/0_pca_functions.R"))
source(paste0(project_functions_dir, "/0_preprocessing_functions.R"))


# Set theme
source(paste0(here::here(), "/5-visualizations/0-plot-themes.R"))
theme_set(theme_ki())





