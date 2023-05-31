#-------------------------------------------------------------------------------
# Stochastic Treatment Regimes
# Inputs:
#   Wide dataset + biomarker data
# Outputs:
#   A dataframe of results
# Author:
#   Sajia Darwish
#-------------------------------------------------------------------------------
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

library(tidyverse)
library(data.table)
library(haldensify)
library(sl3)
library(tmle3)
library(tmle3shift)
library(speedglm)
library(nnls)
library(dplyr)
library(MASS)

## Load data
# vitalWide <- readRDS("/data/KI/imic/results/wideVital.RDS")
# # Read in comma-separated dataset.
# vitalBio <- read.table("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Allen_Bvit_VITAL.csv",
#                        sep = ",", header = TRUE)
# 
# # Since the wide vital dataset and the vital biomarker datasets are not mergable
# # yet, we randomly select 150 observations from the vital biomarker dataset and 
# # combine it with the wide data for testing the models.
# 
# # Randomly sample 150 observations from the biomarker dataset.
# rand_df <- vitalBio[sample(nrow(vitalBio), nrow(vitalWide)), ]
# 
# # Combine datasets
# data <- as.data.frame(cbind(vitalWide, rand_df))

#hmoE <- readRDS("/data/KI/imic/data/raw_lab_data/elicit/merged/hmo.RDS")
hmoE <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/hmoClean.RDS")
names(hmoE)
try(hmoE <- readRDS("C:/Users/andre/Downloads/hmo.RDS"))



#temp: complete case
hmoE <- hmoE[complete.cases(hmoE),]


# Get rid of ID columns and baseline measures of outcomes.
delete <- c("country", "studyid", "siteid", "subjid", "subjido", "studytyp")
hmoE <- hmoE[, !names(hmoE) %in% delete]

#drop missing outcome


## Baseline covariates.
W = c("sex_base", "mage_base", "nlchild_base", "Secretor", "Diversity", "Evenness")


## Create treatment based on baseline W.
# A <- hmoE %>%
#   dplyr::select(ends_with("ug.mL"))
colnames(hmoE)
colnames(hmoE) = gsub("_nmol.mL","",colnames(hmoE))

A = c("X2.FL",       "X3FL",        "DFLac",       "X3.SL",       "X6.SL",      
       "LNT",         "LNnT",        "LNFP.I",      "LNFP.II",     "LNFP.III",    "LSTb",        "LSTc",        "DFLNT",      
       "LNH",         "DSLNT",       "FLNH",        "DFLNH",       "FDSLNH",      "DSLNH",       "SUM",         "Sia",        
      "Fuc")

A <- hmoE[,colnames(hmoE) %in% A]



## Create outcome as a linear function of A, W + white noise.
set.seed(12345)
hmoE$haz_6m <- hmoE$haz_6m + rnorm(nrow(hmoE))
Y <- hmoE$haz_6m
<<<<<<< HEAD

summary(A[, 1])
sd(A[, 1])
=======
>>>>>>> 5f2031815621daff7152c9d69734f5432fb366ed

# Use the function
res <- shiftFunc(covariates = W, treat = A[, 1], outcome = Y, shift = sd(A[, 1]), data = hmoE)
saveRDS(res, file=paste0(here::here(),"/results/example_shift_HMO.RDS"))


# # Run the function over each biomarker
# for (i in 1:ncol(A)) {
#   shiftFunc(covariates = W, treat = A[, i], outcome = Y, shift = 0.05, data = hmoE)
# }
# 
# class(A $ X2.FL_ug.mL)


# FOR FUTURE--------------------------------------------------------------------

# If we wanted a stable stochastic intervention (i.e., avoid positivity violations)
# we could make a choice of the shift based on the impact of the candidate values
# on the estimator.

# We specify the grid of shifts we wish to consider.
delta_grid <- seq(-1, 1, 1)

# Initialize a TMLE specification
tmle_spec <- tmle_vimshift_delta(
  shift_grid = delta_grid,
  max_shifted_ratio = 2
)

# Targeted estimation of stochastic intervention effects.
tmle_fit <- tmle3(tmle_spec, data2, node_list, learner_list)
#-------------------------------------------------------------------------------







