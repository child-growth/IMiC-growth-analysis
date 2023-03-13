---
title: |
  SL Model Selection
author: "Sajia Darwish"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
---

```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(sl3)
library(kableExtra)
library(knitr)
library(skimr)
library(tidyverse)
library(data.table)
library(sl3)
library(SuperLearner)
library(origami)
library(hal9001)
library(polspline)
```

```{r message=FALSE, warning=FALSE}
slModelFunc <- function(data, covars, outcome) {
  
  # create the sl3 task
  task <- make_sl3_Task(
  data = data,
  covariates = covars,
  outcome = outcome
  )
  
  #dput(sl3_list_learners(c("continuous")))
  
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_glmtree <- make_learner(Lrnr_glmtree)
  lrnr_ranger100 <- make_learner(Lrnr_ranger, num.trees = 100)
  lrnr_gam <- Lrnr_pkg_SuperLearner $ new("SL.gam")
  lrnr_bayesglm <- Lrnr_pkg_SuperLearner $ new("SL.bayesglm")
  lrn_ridge <- Lrnr_glmnet $ new(alpha = 0)
  lrn_lasso <- Lrnr_glmnet $ new(alpha = 1)
  # spline regressions:
  lrn_earth <- Lrnr_earth $ new()
  # fast highly adaptive lasso (HAL) implementation
  lrn_hal <- Lrnr_hal9001 $ new(max_degree = 2, num_knots = c(3, 2), nfolds = 5)
  # tree-based methods
  lrn_xgb <- Lrnr_xgboost $ new()
  
  # Use Discrete SL instead of ensemble
  
  stack <- make_learner(
    Stack,
    lrnr_glm, lrnr_mean, lrnr_glmtree, 
    lrnr_ranger100, lrnr_gam,
    lrnr_gam, lrnr_bayesglm, lrn_ridge,
    lrn_lasso, lrn_earth,
    lrn_hal, lrn_xgb
  )
  
  metalearner <- make_learner(Lrnr_nnls)
  
  sl <- make_learner(Lrnr_sl,
    learners = stack,
    metalearner = metalearner
  )
  
  sl_fit <- sl $ train(task)
  
  sl_preds <- sl_fit $ predict()
  results <- sl_fit $ print()

  return(results)
}
```

# Using discrete SL
```{r}
library(sl3)

# Define the learners
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmtree <- make_learner(Lrnr_glmtree)
lrnr_ranger100 <- make_learner(Lrnr_ranger, num.trees = 100)
lrnr_gam <- Lrnr_pkg_SuperLearner$new("SL.gam")
lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
lrn_ridge <- Lrnr_glmnet$new(alpha = 0)
lrn_lasso <- Lrnr_glmnet$new(alpha = 1)
lrn_earth <- Lrnr_earth$new()
lrn_hal <- Lrnr_hal9001$new(max_degree = 2, num_knots = c(3, 2), nfolds = 5)
lrn_xgb <- Lrnr_xgboost$new()

# Define the super learner
superlearner <- Lrnr_superlearner$new()

# Add the learners to the super learner
superlearner$add_learner(lrnr_glm)
superlearner$add_learner(lrnr_mean)
superlearner$add_learner(lrnr_glmtree)
superlearner$add_learner(lrnr_ranger100)
superlearner$add_learner(lrnr_gam)
superlearner$add_learner(lrnr_bayesglm)
superlearner$add_learner(lrn_ridge)
superlearner$add_learner(lrn_lasso)
superlearner$add_learner(lrn_earth)
superlearner$add_learner(lrn_hal)
superlearner$add_learner(lrn_xgb)

# Use discrete super learner
discrete_superlearner <- make_learner(Lrnr_discrete_superlearner)

# Combine the super learner and discrete super learner into a meta-learner
metalearner <- Lrnr_mean$new()

# Define the super learner pipeline
pipeline <- Lrnr_pipeline$new()
pipeline$add_step(superlearner)
pipeline$add_step(discrete_superlearner)
pipeline$add_step(metalearner)

# Define the sl3 task
task <- Lrnr_task$new(data = my_data, target = "my_target")

# Fit the model
fit <- pipeline$train(task)

# Make predictions
predictions <- predict(fit, new_data = my_new_data)
```

# Function to loop over each biomarker for making predictions and summarize
```{r}
library(sl3)

super_learner_featurewise <- function(data, outcome_col, learners) {
  # Initialize empty list to store results
  preds_list <- list()
  
  # Loop over each feature column in the data
  for (i in 1:(ncol(data) - 1)) {
    # Extract the feature and outcome columns
    feature_col <- names(data)[i]
    outcome_col <- names(data)[ncol(data)]
    
    # Subset data to include only non-missing values of the feature
    sub_data <- data[!is.na(data[, feature_col]), ]
    
    # Extract the feature and outcome values
    X <- as.matrix(sub_data[, feature_col, drop = FALSE])
    y <- as.numeric(sub_data[, outcome_col])
    
    # Fit a super learner model using the specified learners
    sl <- SuperLearner$new(learners)
    sl$train(X, y)
    
    # Make predictions using the super learner for the current feature
    preds <- sl$predict(X)
    
    # Store the predictions in the list with the feature name as the key
    preds_list[[feature_col]] <- preds
  }
  
  # Combine the predictions into a data frame
  preds_df <- data.frame(preds_list)
  
  # Calculate summary statistics for the predictions
  summary_df <- data.frame(feature = names(preds_df),
                           min = apply(preds_df, 2, min),
                           median = apply(preds_df, 2, median),
                           max = apply(preds_df, 2, max),
                           mean = apply(preds_df, 2, mean),
                           sd = apply(preds_df, 2, sd))
  
  return(summary_df)
}

```

# Call the function
```{r}
data <- read.csv("my_data.csv")
learners <- list(lrnr_glm, lrnr_mean, lrnr_glmtree, lrnr_ranger100, lrnr_gam, lrnr_bayesglm, lrn_ridge, lrn_lasso, lrn_earth, lrn_hal, lrn_xgb)
summary_df <- super_learner_featurewise(data, "outcome_col", learners)

```

# Use the function on datasets
```{r}
# Load data
hmo <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/hmoClean.RDS")
#dput(names((hmo)))
#head(data)

covars = c("X2.FL_nmol.mL", "X3FL_nmol.mL", "DFLac_nmol.mL", "X3.SL_nmol.mL",
       "X6.SL_nmol.mL", "LNT_nmol.mL", "LNnT_nmol.mL", "LNFP.I_nmol.mL", 
       "LNFP.II_nmol.mL", "LNFP.III_nmol.mL", "LSTb_nmol.mL", "LSTc_nmol.mL",
       "DFLNT_nmol.mL", "LNH_nmol.mL", "DSLNT_nmol.mL", "FLNH_nmol.mL", 
       "DFLNH_nmol.mL", "FDSLNH_nmol.mL", "DSLNH_nmol.mL", "SUM_nmol.mL", 
       "Sia_nmol.mL", "Fuc_nmol.mL")
outcome = "haz_6m_simulated"

set.seed(4197)
start_time <- proc.time() # start time
slModelFunc(hmo, covars = covars, outcome = outcome)
runtime_sl_fit <- proc.time() - start_time # end time - start time = run time
runtime_sl_fit
```


