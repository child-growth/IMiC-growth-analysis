#-------------------------------------------------------------------------------
# TMLE script calling functions from 0-project-functions.
#
# Inputs:
#   ELICIT HMO dataset.
# Outputs:
#   A dataframe of results.
# Authors:
#   Andrew Mertens and Sajia Darwish
#-------------------------------------------------------------------------------

# Install packages + load libraries
#remotes::install_github("tlverse/tmle3")
library(sl3)
library(tmle3)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
registerDoParallel(cores=50)
library(Rsolnp)

# Load data
data <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/hmoClean.RDS")
#dput(names((data)))

# Write a TMLE function

#write a wrapper function called tmle_wrapper_function (or something more clever) 
#that runs a TMLE3 analysis and outputs a 1-row data.frame with all the results we want
#this is important to help loop through a bunch of A variables
#https://tlverse.org/tlverse-handbook/tmle3.html
#Need res to be a row of a dataframe with all information we'd want out of a tmle analysis.
#The name of Y and A, the ATE, 95% CI, pvalue, size of the dataset


tmleFunc <- function(W, A, Y, data) {
  # Define the variable roles
  node_list <- list(W = W, A = A, Y = Y)
  
  # Choose base learners
  # Instantiate a main terms generalized linear model
  lrnr_glm <- make_learner(Lrnr_glm)
  # Define metalearners appropriate to data types
  ls_metalearner <- make_learner(Lrnr_nnls)
  mn_metalearner <- make_learner(Lrnr_solnp, metalearner_linear_multinomial,
    loss_loglik_multinomial)
  sl_Y <- Lrnr_sl $ new(learners = list(lrnr_glm),
                        metalearner = ls_metalearner)
  sl_A <- Lrnr_sl $ new(learners = list(lrnr_glm),
                        metalearner = mn_metalearner)
  learner_list <- list(A = sl_A, Y = sl_Y)
  
  # Fit the TMLE
  tmle_fit <- tmle3(tmle_TSM_all(), data, node_list, learner_list)
  
  # Extract estimates
  return(tmle_fit $ summary)
}

# Set parameters for tmleFunc
W = c("Secretor", "Diversity", "Evenness")
A = c("X2.FL", "X3FL", "DFLac", "X3.SL", "X6.SL", "LNT", "LNnT", "LNFP.I",
      "LNFP.II", "LNFP.III", "LSTb", "LSTc", "DFLNT", "LNH", "DSLNT", "FLNH",
      "DFLNH", "FDSLNH", "DSLNH", "SUM", "Sia", "Fuc")
Y = data $ haz_6m

# Test: does not run. Will fix output after it runs.
tmleFunc(W = W, A = A[1], Y = Y, data = data)





# TO DO-------------------------------------------------------------------------
# Make a for loop to run over each A.
res_df <- NULL
for(i in 1:length(A)) {
  
  res <- NULL
  try(res <- tmleFunc(W = W, A = A[i], Y = Y, data = data))
  bind_rows(res_df[i, ], res)
  
}

res_df


#Vignette
#https://cran.r-project.org/web/packages/foreach/vignettes/foreach.html

#Note you may need to figure out which data/parameters/packages you need to pass in
res_df <- foreach(i = 1:length(Avar), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  

  try(res <- tmle_wrapper_function(data=d, A=Avar[i], ...))
  return(res)
}



#After running the for loop for all exposures, then apply P-value correction:|

