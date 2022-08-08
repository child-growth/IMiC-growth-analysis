#-------------------------------------------------------------------------------
# Stochastic Treatment Regimes
# Inputs:
#   Wide dataset + biomarker data
# Outputs:
#   A dataframe of results
#-------------------------------------------------------------------------------
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

library(data.table)
library(haldensify)
library(sl3)
library(tmle3)
library(tmle3shift)
library(speedglm)
library(nnls)
library(dplyr)
library(MASS)

# Try to merge biomarker with the covariates dataset [DELETE AFTER]-------------

elicitWide <- readRDS("/data/KI/imic/results/wideElicit.RDS")
bvitE <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Allen_Bvit_ELICIT.csv")

# Remove the first 5 elements of the X column in bvitE
bvitE $ X = substring(bvitE $ X, 6)

# Check for unique ID's in bvitE
sum(table(unique(bvitE $ X)))

# Check for similarities between the ID's in elicitWide and bvitE
intersect(elicitWide $ subjid, bvitE $ X) # Only 6 shared obs. Not a great way to merge.
setdiff(bvitE $ X, elicitWide $ subjid)
#-------------------------------------------------------------------------------

## Load data
vitalWide <- readRDS("/data/KI/imic/results/wideVital.RDS")
# Read in comma-separated dataset.
vitalBio <- read.table("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Allen_Bvit_VITAL.csv",
                       sep = ",", header = TRUE)

# Since the wide vital dataset and the vital biomarker datasets are not mergable
# yet, we randomly select 150 observations from the vital biomarker dataset and 
# combine it with the wide data for testing the models.

# Randomly sample 150 observations from the biomarker dataset.
rand_df <- vitalBio[sample(nrow(vitalBio), nrow(vitalWide)), ]

# Combine datasets
data <- as.data.frame(cbind(vitalWide, rand_df))

## Baseline covariates.
W <- as.data.frame(cbind(data $ nlchild_base, data $ nperson_base, data $ nrooms_base))

## Create treatment based on baseline W.
A <- c(data $ TPP)

## Create outcome as a linear function of A, W + white noise.
Y <- c(data $ haz_m6)

# Use the function
shiftFunc(covariates = W, treat = A, outcome = Y, shift = 0.05)

# Run the function over each biomarker
vitalBio $ X = NULL
for (i in 1:ncol(vitalBio)) {
  shiftFunc(covariates = W, treat = vitalBio[, i], outcome = Y, shift = 0.05)
}


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







