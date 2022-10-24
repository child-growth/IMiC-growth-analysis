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
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
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
  
  #process data
  d_W <- data %>% select(all_of(W))
  d_YA <- data %>% select(all_of(Y),all_of(A))
  
  #impute W and add missingness indicator
  d_W<-impute_missing_values(d_W)$data
  data <- cbind(d_YA,d_W)
  #complete case for Y and A
  data <- data[complete.cases(data),]
  
  #discretize A
  data[[A]] <- factor(ifelse(data[[A]]>median(data[[A]]),1,0))
  
  # Define the variable roles
  node_list <- list(W = colnames(d_W), A = A, Y = Y)
  
  #tmle spec
  ate_spec <- tmle_ATE(
    treatment_level = "1",
    control_level = "0")
  
  # choose base learners
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_rf <- make_learner(Lrnr_ranger)
  
  # define metalearners appropriate to data types
  ls_metalearner <- make_learner(Lrnr_nnls)
  mn_metalearner <- make_learner(
    Lrnr_solnp, metalearner_linear_multinomial,
    loss_loglik_multinomial
  )
  sl_Y <- Lrnr_sl$new(
    learners = list(lrnr_mean, lrnr_rf),
    metalearner = ls_metalearner
  )
  sl_A <- Lrnr_sl$new(
    learners = list(lrnr_mean, lrnr_rf),
    metalearner = mn_metalearner
  )
  learner_list <- list(A = sl_A, Y = sl_Y)
  
  
  # Fit the TMLE
  tmle_fit <- tmle3(ate_spec, data, node_list, learner_list)
  
  # Extract estimates
  return(tmle_fit $ summary)
}

# Set parameters for tmleFunc
W = c("Secretor", "Diversity", "Evenness")
A = c("X2.FL", "X3FL", "DFLac", "X3.SL", "X6.SL", "LNT", "LNnT", "LNFP.I",
      "LNFP.II", "LNFP.III", "LSTb", "LSTc", "DFLNT", "LNH", "DSLNT", "FLNH",
      "DFLNH", "FDSLNH", "DSLNH", "SUM", "Sia", "Fuc")
Y = "haz_6m"

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

