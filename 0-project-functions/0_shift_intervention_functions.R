#https://tlverse.org/tlverse-handbook/shift.html

# In order to construct a TML estimator, we need 2 components.
# First: outcome regression - can be constructed using SL algorithm.
# Second: generalized propensity score - estimate of the treatment mechanism.
# When the intervention = continuous: conditional density. A list of learners
# suited for conditional density estimation can be extracted from the sl3 package.

# Make a wrapper function-------------------------------------------------------

shiftFunc <- function (covariates, treat, outcome, shift, data) {
  
  # 1. OUTCOME REGRESSION
  
  # learners used for conditional mean of the continuous outcome.
  mean_lrnr <- Lrnr_mean $ new()
  fglm_lrnr <- Lrnr_glm_fast $ new()
  rf_lrnr <- Lrnr_ranger $ new()
  hal_lrnr <- Lrnr_hal9001 $ new(max_degree = 3, n_folds = 3)
  
  # SL for the outcome regression.
  sl_reg_lrnr <- Lrnr_sl $ new(
    learners = list(mean_lrnr, fglm_lrnr, rf_lrnr, hal_lrnr),
    metalearner = Lrnr_nnls $ new()
  )
  
  # 2. ESTIMATE OF TREATMENT MECHANISM
  
  # Conditional density learners: pick whichever is desired.
  sl3_list_learners("density")
  
  # Here, we select two of the learners in sl3.
  
  # learners used for conditional densities for (g_n).
  haldensify_lrnr <- Lrnr_haldensify $ new(
    n_bins = c(5, 10, 20),
    lambda_seq = exp(seq(-1, -10, length = 200))
  )
  
  # semiparametric density estimator with homoscedastic errors (HOSE).
  hose_hal_lrnr <- make_learner(Lrnr_density_semiparametric,
                                mean_learner = hal_lrnr
  )
  
  # semiparametric density estimator with heteroscedastic errors (HESE).
  hese_rf_glm_lrnr <- make_learner(Lrnr_density_semiparametric,
                                   mean_learner = rf_lrnr,
                                   var_learner = fglm_lrnr
  )
  
  # SL for the conditional treatment density.
  sl_dens_lrnr <- Lrnr_sl $ new(
    learners = list(hose_hal_lrnr, hese_rf_glm_lrnr),
    metalearner = Lrnr_solnp_density $ new()
  )
  
  # Make an object for use in TML estimator construction.
  # Here, Y = outcome regression learner.
  # A = treatment mechanism learner.
  learner_list <- list(Y = sl_reg_lrnr, A = sl_dens_lrnr)
  
  # 3. Apply
  
  # Organize data and nodes for tmle3.
  data <- data.table(covariates, treat, outcome)
  
  # Filter out NA's from the dataset.
  data <- data %>%
    drop_na()
  
  # Change column names while accommodating any number of covariates.
  num = ncol(data) - 2
  setnames(data, c(paste0("W", 1:num), "A", "Y"))
  
  node_list <- list(
    W = dput(names(data[, 1:num])),
    A = "A",
    Y = "Y"
  )
  
  # Initialize a TMLE specification.
  tmle_spec <- tmle_shift(
    shift_val = shift, # shift on the scale of treatment A.
    shift_fxn = shift_additive,
    shift_fxn_inv = shift_additive_inv
  )
  
  # Targeted estimation of stochastic intervention effects.
  tmle_fit <- tmle3(tmle_spec, data, node_list, learner_list)
  
  # Return a dataframe
  return(tmle_fit)
}