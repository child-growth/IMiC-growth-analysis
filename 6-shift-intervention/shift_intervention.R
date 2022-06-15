library(data.table)
library(haldensify)
library(sl3)
library(tmle3)
library(tmle3shift)

#https://tlverse.org/tlverse-handbook/shift.html

# learners used for conditional mean of the outcome
mean_lrnr <- Lrnr_mean $ new()
fglm_lrnr <- Lrnr_glm_fast $ new()
rf_lrnr <- Lrnr_ranger $ new()
hal_lrnr <- Lrnr_hal9001 $ new(max_degree = 3, n_folds = 3)

# SL for the outcome regression
sl_reg_lrnr <- Lrnr_sl $ ne(
  learners = list(mean_lrnr, fglm_lrnr, rf_lrnr, hal_lrnr),
  metalearner = Lrnr_nnls $ new()
)

sl3_list_learners("density")


## baseline covariates -- simple, binary
W <- c()

## create treatment based on baseline W
A <- c()

## create outcome as a linear function of A, W + white noise
Y <- c()


# initialize a tmle specification
tmle_spec <- tmle_shift(
  shift_val = 0.5,
  shift_fxn = shift_additive,
  shift_fxn_inv = shift_additive_inv
)







