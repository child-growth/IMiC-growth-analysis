---
title: |
  TMLE Parallelization
author: "Sajia Darwish"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
---

```{r, include=FALSE}
#rm(list=ls())
#source(paste0(here::here(), "/0-config.R"))
library(tidyverse)
library(sl3)
library(tmle3)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
registerDoParallel(cores=50)
library(Rsolnp)
library(dplyr)
```

# Make a TMLE function and a table function: without added learners
```{r, include=FALSE}
tmleFunc <- function(W, A, Y, data) {
  # Define the variable roles
  node_list <- list(W = W, A = A, Y = Y)

  processed <- process_missing(data, node_list)
  data2 <- processed $ data # Remember that now data is data2 now.
  node_list <- processed $ node_list

  # Dichotomize A
  for (i in 1:nrow(data2)) {
    data2[[A]][i] <- ifelse(data2[[A]][i] > median(data2[[A]]), 1, 0)
  }

  ate_spec <- tmle_ATE(treatment_level = 1, control_level = 0)

  # Choose base learners
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_mean <- make_learner(Lrnr_mean)

  # Define metalearners appropriate to data types
  # non-negative least squares regression for continuous outcomes
  ls_metalearner <- make_learner(Lrnr_nnls)

  # mn_metalearner <- make_learner(Lrnr_solnp, metalearner_linear_multinomial,
  #                                loss_loglik_multinomial)

  # Plot Y to get a sense of distribution
  #hist(data2[[Y]])

  sl_Y <- Lrnr_sl $ new(learners = list(lrnr_mean, lrnr_glm),
                      metalearner = ls_metalearner)

  # Plot A to get a sense of distribution
  #hist(data[[A]]) # Actual A
  #hist(data2[[A]]) # Dichotomized A

  sl_A <- Lrnr_sl $ new(learners = list(lrnr_mean, lrnr_glm),
                      metalearner = ls_metalearner)#mn_metalearner)

  learner_list <- list(A = sl_A, Y = sl_Y)

  tmle_fit <- tmle3(ate_spec, data2, node_list, learner_list)
  print(tmle_fit)
}

# Make a table function
tableFunc <- function (A, W, Y, data) {
# Parallelize:
int.start.time <- Sys.time()
resultTable  <- foreach(i = 1:length(A), .combine = 'bind_rows',
                        .errorhandling = 'remove') %dopar% {
  tmleResult <- tmleFunc(W, A[i], Y, data)
  resultTable <- tmleResult $ summary
  resultTable $ A <- A[i]
  resultTable $ Y <- Y
  return(resultTable)
                        }

int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units = "mins")

return(resultTable)
}
```

# Function with added learners
```{r}
# tmleFunc <- function(W, A, Y, data) {
#   # Define the variable roles
#   node_list <- list(W = W, A = A, Y = Y)
# 
#   processed <- process_missing(data, node_list)
#   data2 <- processed $ data # Remember that now data is data2 now.
#   node_list <- processed $ node_list
# 
#   # Dichotomize A
#   for (i in 1:nrow(data2)) {
#     data2[[A]][i] <- ifelse(data2[[A]][i] > median(data2[[A]]), 1, 0)
#   }
# 
#   ate_spec <- tmle_ATE(treatment_level = 1, control_level = 0)
# 
#   lrnr_glmnet <- Lrnr_glmnet$new()
#   random_forest <- Lrnr_randomForest$new()
#   glm_fast <- Lrnr_glm_fast$new()
#   nnls_lrnr <- Lrnr_nnls$new()
#   
#   xgboost_lrnr <- Lrnr_xgboost$new()
#   ranger_lrnr <- Lrnr_ranger$new()
#   gbm_lrnr <- Lrnr_gbm$new()
#   earth_lrnr <- Lrnr_earth$new()
#   dbarts_lrnr <- Lrnr_dbarts$new()
#   hal_lrnr <- Lrnr_hal9001$new()
#   gam_lrnr <- Lrnr_gam$new()
#   polyspline<-Lrnr_polspline$new()
#   
# 
#   screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
#   screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
#   screen_corRank <- Lrnr_screener_corRank$new(rank=3)
#   
#   cor_glm <- make_learner(Pipeline, screen_cor, glm_fast)
#   cor_spline <- make_learner(Pipeline, screen_cor, polyspline)
#   screened_hal <- make_learner(Pipeline, screen_glmnet, hal_lrnr)
#   
#   stack <- make_learner(
#     Stack,
#     lrnr_mean,
#     glm_fast,
#     cor_glm,
#     lrnr_glmnet,
#     ranger_lrnr,
#     xgboost_lrnr,
#     gam_lrnr,
#     cor_spline,
#     screened_hal
#   )
# 
#   # Define metalearners appropriate to data types
#   # non-negative least squares regression for continuous outcomes
#   ls_metalearner <- make_learner(Lrnr_nnls)
# 
#   # mn_metalearner <- make_learner(Lrnr_solnp, metalearner_linear_multinomial,
#   #                                loss_loglik_multinomial)
# 
#   # Plot Y to get a sense of distribution
#   #hist(data2[[Y]])
# 
#   sl_Y <- Lrnr_sl $ new(learners = stack,
#                       metalearner = ls_metalearner)
# 
#   # Plot A to get a sense of distribution
#   #hist(data[[A]]) # Actual A
#   #hist(data2[[A]]) # Dichotomized A
# 
#   sl_A <- Lrnr_sl $ new(learners = stack,
#                       metalearner = ls_metalearner)#mn_metalearner)
# 
#   learner_list <- list(A = sl_A, Y = sl_Y)
# 
#   tmle_fit <- tmle3(ate_spec, data2, node_list, learner_list)
#   print(tmle_fit)
# }
# 
# # # Make a for loop function
# tableFunc <- function (A, W, Y, data) {
# # Parallelize:
# int.start.time <- Sys.time()
# resultTable  <- foreach(i = 1:length(A), .combine = 'bind_rows',
#                         .errorhandling = 'remove') %dopar% {
#   tmleResult <- tmleFunc(W, A[i], Y, data)
#   resultTable <- tmleResult $ summary
#   resultTable $ A <- A[i]
#   resultTable $ Y <- Y
#   return(resultTable)
#   }
# 
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units = "mins")
# 
# return(resultTable)
# }
```

# HMO ELICIT
```{r, include=FALSE}
# Load data
hmo <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/hmoClean.RDS")
#dput(names((hmo)))
#head(data)

# Set parameters for tmleFunc
W = c("Secretor", "Diversity", "Evenness")
A1 = c("X2.FL_nmol.mL", "X3FL_nmol.mL", "DFLac_nmol.mL", "X3.SL_nmol.mL",
       "X6.SL_nmol.mL", "LNT_nmol.mL", "LNnT_nmol.mL", "LNFP.I_nmol.mL", 
       "LNFP.II_nmol.mL", "LNFP.III_nmol.mL", "LSTb_nmol.mL", "LSTc_nmol.mL",
       "DFLNT_nmol.mL", "LNH_nmol.mL", "DSLNT_nmol.mL", "FLNH_nmol.mL", 
       "DFLNH_nmol.mL", "FDSLNH_nmol.mL", "DSLNH_nmol.mL", "SUM_nmol.mL", 
       "Sia_nmol.mL", "Fuc_nmol.mL")
Y1 = "haz_base"
Y2 = "haz_6m_simulated"

tableHmo1 <- tableFunc(A, W, Y1, hmo)
tableHmo2 <- tableFunc(A, W, Y2, hmo)

# Save dataset
#write.csv(tableHmo, file = "/data/imic/results/tmle/hmoElicitTMLE.csv")
```

# Biocrates normalized ELICIT
```{r}
# Load data
bioc <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/biocNormClean.RDS")
# head(bioc)
# dput(names((bioc)))

# Set parameters for tmleFunc
W = c("sex_base", "mage_base")
A = bioc %>%
  select(!c("bmid_base", "country", "studyid", "subjid", "subjido", "arm_base", 
            "sex_base", "brthyr_base", "brthweek_base", "mage_base", "parity_base", 
            "nlchild_base", "nperson_base", "nrooms_base", "meducyrs_base", 
            "h2osrcp_base", "cookplac_base", "inctot_base", "inctotu_base", 
            "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", "dlvloc_base", 
            "dvseason_base", "wtkg_base", "lencm_base", "bmi_base", "hcircm_base", 
            "waz_base", "haz_base", "whz_base", "baz_base", "agedays_base", 
            "dur_bf_base", "dur_ebf_base", "lencm_3m", "lencm_6m", "lencm_9m", 
            "lencm_12m", "lencm_15m", "lencm_18m", "bmi_3m", "bmi_6m", "bmi_9m", 
            "bmi_12m", "bmi_15m", "bmi_18m", "hcircm_3m", "hcircm_6m", "hcircm_9m", 
            "hcircm_12m", "hcircm_15m", "hcircm_18m", "waz_3m", "waz_6m", 
            "waz_9m", "waz_12m", "waz_15m", "waz_18m", "haz_3m", "haz_6m", 
            "haz_9m", "haz_12m", "haz_15m", "haz_18m", "whz_3m", "whz_6m", 
            "whz_9m", "whz_12m", "whz_15m", "whz_18m", "baz_3m", "baz_6m", 
            "baz_9m", "baz_12m", "baz_15m", "baz_18m", "exbfdu_r_0to6m", 
            "fever_r_0to6m", "fever_r_6to18m", "cough_r_0to6m", "cough_r_6to18m", 
            "diarr_r_0to6m", "diarr_r_6to18m", "mhgb_18m", "muaccm_18m", 
            "muaz_18m", "bfdu_r_0to18m", "anti_r_0to18m", "haz_6m_simulated"))
A = colnames(A)
Y = "haz_6m_simulated"

tableBioc1 <- tableFunc(A, W, Y, bioc)

# Get mean and variance of the outcome
hist(bioc $ haz_base)
mean <- mean(bioc $ haz_base)
sd <- sd(bioc $ haz_base)

# Generate new data from this distribution
bioc $ haz_base_simulated <- rnorm(n = 199, mean = mean, sd = sd)
bioc $ haz_base_simulated <- as.numeric(format(round(bioc $ haz_base_simulated, 2), 
                                            nsmall = 2))

# Set parameters for tmleFunc
W = c("sex_base", "mage_base")
A = bioc %>%
  select(!c("bmid_base", "country", "studyid", "subjid", "subjido", "arm_base", 
            "sex_base", "brthyr_base", "brthweek_base", "mage_base", "parity_base", 
            "nlchild_base", "nperson_base", "nrooms_base", "meducyrs_base", 
            "h2osrcp_base", "cookplac_base", "inctot_base", "inctotu_base", 
            "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", "dlvloc_base", 
            "dvseason_base", "wtkg_base", "lencm_base", "bmi_base", "hcircm_base", 
            "waz_base", "haz_base", "whz_base", "baz_base", "agedays_base", 
            "dur_bf_base", "dur_ebf_base", "lencm_3m", "lencm_6m", "lencm_9m", 
            "lencm_12m", "lencm_15m", "lencm_18m", "bmi_3m", "bmi_6m", "bmi_9m", 
            "bmi_12m", "bmi_15m", "bmi_18m", "hcircm_3m", "hcircm_6m", "hcircm_9m", 
            "hcircm_12m", "hcircm_15m", "hcircm_18m", "waz_3m", "waz_6m", 
            "waz_9m", "waz_12m", "waz_15m", "waz_18m", "haz_3m", "haz_6m", 
            "haz_9m", "haz_12m", "haz_15m", "haz_18m", "whz_3m", "whz_6m", 
            "whz_9m", "whz_12m", "whz_15m", "whz_18m", "baz_3m", "baz_6m", 
            "baz_9m", "baz_12m", "baz_15m", "baz_18m", "exbfdu_r_0to6m", 
            "fever_r_0to6m", "fever_r_6to18m", "cough_r_0to6m", "cough_r_6to18m", 
            "diarr_r_0to6m", "diarr_r_6to18m", "mhgb_18m", "muaccm_18m", 
            "muaz_18m", "bfdu_r_0to18m", "anti_r_0to18m", "haz_base_simulated"))
A = colnames(A)
Y = "haz_base_simulated"

tableBioc2 <- tableFunc(A, W, Y, bioc)

# Add _6 and _base to dataframces + cbind
colnames(tableBioc2) <- paste(colnames(tableBioc2),"base",sep="_")
colnames(tableBioc1) <- paste(colnames(tableBioc1),"6",sep="_")

tableBioc <- cbind(tableBioc2, tableBioc1)

# Save dataset
write.csv(tableBioc, file = "/data/imic/results/tmle/biocElicitTMLE.csv")
```

# Bvitamin ELICIT
```{r}
# Load data
bvit <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/bvitClean.RDS")
#dput(names((bvit)))

# Set parameters for tmleFunc
W = c("sex_base", "mage_base")
A = bvit %>%
  select(!c("bmid_base", "country", "studyid", "siteid", "subjid", "subjido", 
            "studytyp", "arm_base", "sex_base", "brthyr_base", "brthweek_base", 
            "mage_base", "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
            "meducyrs_base", "h2osrcp_base", "cookplac_base", "inctot_base", 
            "inctotu_base", "epochn_base", "epoch_base", "mhtcm_base", "mwtkg_base", 
            "mbmi_base", "pregout_base", "dlvloc_base", "dvseason_base", 
            "wtkg_base", "lencm_base", "bmi_base", "hcircm_base", "waz_base", 
            "haz_base", "whz_base", "baz_base", "agedays_base", "feeding_base", 
            "dur_bf_base", "dur_ebf_base", "bmcol_fl_1m", "bmcol_fl_5m", 
            "agedays_1m", "agedays_5m", "lencm_3m", "lencm_6m", "lencm_9m", 
            "lencm_12m", "lencm_15m", "lencm_18m", "bmi_3m", "bmi_6m", "bmi_9m", 
            "bmi_12m", "bmi_15m", "bmi_18m", "hcircm_3m", "hcircm_6m", "hcircm_9m", 
            "hcircm_12m", "hcircm_15m", "hcircm_18m", "waz_3m", "waz_6m", 
            "waz_9m", "waz_12m", "waz_15m", "waz_18m", "haz_3m", "haz_6m", 
            "haz_9m", "haz_12m", "haz_15m", "haz_18m", "whz_3m", "whz_6m", 
            "whz_9m", "whz_12m", "whz_15m", "whz_18m", "baz_3m", "baz_6m", 
            "baz_9m", "baz_12m", "baz_15m", "baz_18m", "agedays_3m", "agedays_6m", 
            "agedays_9m", "agedays_12m", "agedays_15m", "agedays_18m", "visit_r_fl_0to6m", 
            "visit_r_fl_6to18m", "dur_r_0to6m", "dur_r_6to18m", "bfedfl_r_0to6m", 
            "exbfed_r_0to6m", "exbfdu_r_0to6m", "fever_r_0to6m", "fever_r_6to18m", 
            "cough_r_0to6m", "cough_r_6to18m", "diarr_r_0to6m", "diarr_r_6to18m", 
            "mhgb_18m", "muaccm_18m", "muaz_18m", "agedays_18m.1", "bfdu_r_0to18m", 
            "anti_r_0to18m", "haz_6m_simulated"))
A = colnames(A)
Y = "haz_6m_simulated"

tableBvit <- tableFunc(A, W, Y, bvit)

# Save dataset
#write.csv(tableBvit, file = "/data/imic/results/tmle/BvitElicitTMLE.csv")
```

# Metabolomic Indicators ELICIT
```{r}
# Load data
metabol <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/metabolClean.RDS")

#dput(names((metabol)))

# Set parameters for tmleFunc
W = c("sex_base", "mage_base")
A = metabol %>%
  select(!c("bmid_base", "country", "studyid", "siteid", "subjid", "subjido", 
            "studytyp", "arm_base", "sex_base", "brthyr_base", "brthweek_base", 
            "mage_base", "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
            "meducyrs_base", "h2osrcp_base", "cookplac_base", "inctot_base", 
            "inctotu_base", "epochn_base", "epoch_base", "mhtcm_base", "mwtkg_base", 
            "mbmi_base", "pregout_base", "dlvloc_base", "dvseason_base", 
            "wtkg_base", "lencm_base", "bmi_base", "hcircm_base", "waz_base", 
            "haz_base", "whz_base", "baz_base", "agedays_base", "feeding_base", 
            "dur_bf_base", "dur_ebf_base", "bmcol_fl_1m", "bmcol_fl_5m", 
            "agedays_1m", "agedays_5m", "lencm_3m", "lencm_6m", "lencm_9m", 
            "lencm_12m", "lencm_15m", "lencm_18m", "bmi_3m", "bmi_6m", "bmi_9m", 
            "bmi_12m", "bmi_15m", "bmi_18m", "hcircm_3m", "hcircm_6m", "hcircm_9m", 
            "hcircm_12m", "hcircm_15m", "hcircm_18m", "waz_3m", "waz_6m", 
            "waz_9m", "waz_12m", "waz_15m", "waz_18m", "haz_3m", "haz_6m", 
            "haz_9m", "haz_12m", "haz_15m", "haz_18m", "whz_3m", "whz_6m", 
            "whz_9m", "whz_12m", "whz_15m", "whz_18m", "baz_3m", "baz_6m", 
            "baz_9m", "baz_12m", "baz_15m", "baz_18m", "agedays_3m", "agedays_6m", 
            "agedays_9m", "agedays_12m", "agedays_15m", "agedays_18m", "visit_r_fl_0to6m", 
            "visit_r_fl_6to18m", "dur_r_0to6m", "dur_r_6to18m", "bfedfl_r_0to6m", 
            "exbfed_r_0to6m", "exbfdu_r_0to6m", "fever_r_0to6m", "fever_r_6to18m", 
            "cough_r_0to6m", "cough_r_6to18m", "diarr_r_0to6m", "diarr_r_6to18m", 
            "mhgb_18m", "muaccm_18m", "muaz_18m", "agedays_18m.1", "bfdu_r_0to18m", 
            "anti_r_0to18m","haz_6m_simulated"))
A = colnames(A)
Y = "haz_6m_simulated"

tableMetabol <- tableFunc(A, W, Y, metabol)

# Save dataset
#write.csv(tableMetabol, file = "/data/imic/results/tmle/MetabolElicitTMLE.csv")
```

# HMO VITAL
```{r, include=FALSE}
# Load data
hmo <- readRDS("/data/imic/data/raw_lab_data/vital/merged_vital/hmoClean.RDS")
#dput(names((hmo)))
#head(data)

# Set parameters for tmleFunc
W = c("Secretor", "Diversity", "Evenness")
A = c("X2.FL_nmol.mL", "X3FL_nmol.mL", "DFLac_nmol.mL", 
      "X3.SL_nmol.mL", "X6.SL_nmol.mL", "LNT_nmol.mL", "LNnT_nmol.mL", 
      "LNFP.I_nmol.mL", "LNFP.II_nmol.mL", "LNFP.III_nmol.mL", "LSTb_nmol.mL", 
      "LSTc_nmol.mL", "DFLNT_nmol.mL", "LNH_nmol.mL", "DSLNT_nmol.mL", 
      "FLNH_nmol.mL", "DFLNH_nmol.mL", "FDSLNH_nmol.mL", "DSLNH_nmol.mL", 
      "SUM_nmol.mL", "Sia_nmol.mL", "Fuc_nmol.mL")
Y = "haz_6m_simulated"

tableHmo <- tableFunc(A, W, Y, hmo)

# Save dataset
#write.csv(tableHmo, file = "/data/imic/results/tmle/hmoVitalTMLE.csv")
```

# Biocrates normalized VITAL
```{r}
# Load data
bioc <- readRDS("/data/imic/data/raw_lab_data/vital/merged_vital/biocNormClean.RDS")
# head(bioc)
#dput(names((bioc)))

# Set parameters for tmleFunc
W = c("sex_base", "mage_base")
A = bioc %>%
  select(!c("bmid_base", "subjid", "country", "studyid", "siteid", "subjido", 
            "studytyp", "arm_base", "sex_base", "brthyr_base", "brthweek_base", 
            "mage_base", "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
            "meducyrs_base", "h2osrcp_base", "cookplac_base", "agedays_base", 
            "epochn_base", "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", 
            "pregout_base", "dlvloc_base", "wtkg_base", "lencm_base", "bmi_base", 
            "muaccm_base", "waz_base", "haz_base", "whz_base", "baz_base", 
            "feeding_base", "dur_bf_base", "dur_ebf_base", "visit_r_fl_base", 
            "dur_r_base", "fever_r_base", "cough_r_base", "anti_r_base", 
            "citytown_base", "gagebrth_base", "gagecm_base", "birthwt_base", 
            "birthlen_base", "birthord_base", "gravida_base", "nlivbrth_base", 
            "floor_base", "gagedays_base", "postbmi_base", "mmuaccm_base", 
            "delivery_base", "bfinittm_base", "cmfdint_base", "exbfedfl_base", 
            "formlkfl_base", "fever_base", "cough_base", "diarr_base", "vomit_base", 
            "physican_base", "hosp_base", "antibiot_base", "anti_oral_base", 
            "anti_inj_base", "agedays_m1", "mhtcm_m1", "mwtkg_m1", "mbmi_m1", 
            "wtkg_m1", "lencm_m1", "bmi_m1", "muaccm_m1", "waz_m1", "haz_m1", 
            "whz_m1", "baz_m1", "feeding_m1", "dur_bf_m1", "dur_ebf_m1", 
            "visit_r_fl_m1", "dur_r_m1", "fever_r_m1", "cough_r_m1", "diarr_r_m1", 
            "anti_r_m1", "gagedays_m1", "postbmi_m1", "mmuaccm_m1", "cmfdint_m1", 
            "bfmode_m1", "bfedfl_m1", "exbfedfl_m1", "sldfedfl_m1", "fever_m1", 
            "cough_m1", "diarr_m1", "vomit_m1", "physican_m1", "hosp_m1", 
            "antibiot_m1", "anti_oral_m1", "anti_inj_m1", "agedays_m2", "mhtcm_m2", 
            "mwtkg_m2", "mbmi_m2", "mhgb_m2", "wtkg_m2", "lencm_m2", "bmi_m2", 
            "muaccm_m2", "waz_m2", "haz_m2", "whz_m2", "baz_m2", "feeding_m2", 
            "dur_bf_m2", "dur_ebf_m2", "visit_r_fl_m2", "dur_r_m2", "fever_r_m2", 
            "cough_r_m2", "diarr_r_m2", "anti_r_m2", "gagedays_m2", "postbmi_m2", 
            "mmuaccm_m2", "hgb_m2", "cmfdint_m2", "bfmode_m2", "bfedfl_m2", 
            "exbfedfl_m2", "sldfedfl_m2", "fever_m2", "cough_m2", "diarr_m2", 
            "vomit_m2", "physican_m2", "hosp_m2", "antibiot_m2", "anti_oral_m2", 
            "anti_inj_m2", "mcrp_m2", "mferritin_m2", "mstrf_m2", "magp_m2", 
            "agedays_m3", "mhtcm_m3", "mwtkg_m3", "mbmi_m3", "wtkg_m3", "lencm_m3", 
            "bmi_m3", "muaccm_m3", "waz_m3", "haz_m3", "whz_m3", "baz_m3", 
            "muaz_m3", "feeding_m3", "dur_bf_m3", "dur_ebf_m3", "visit_r_fl_m3", 
            "dur_r_m3", "fever_r_m3", "cough_r_m3", "diarr_r_m3", "anti_r_m3", 
            "gagedays_m3", "postbmi_m3", "mmuaccm_m3", "cmfdint_m3", "bfmode_m3", 
            "bfedfl_m3", "exbfedfl_m3", "sldfedfl_m3", "fever_m3", "cough_m3", 
            "diarr_m3", "vomit_m3", "physican_m3", "hosp_m3", "antibiot_m3", 
            "anti_oral_m3", "anti_inj_m3", "agedays_m4", "mhtcm_m4", "mwtkg_m4", 
            "mbmi_m4", "wtkg_m4", "lencm_m4", "bmi_m4", "muaccm_m4", "waz_m4", 
            "haz_m4", "whz_m4", "baz_m4", "muaz_m4", "feeding_m4", "dur_bf_m4", 
            "dur_ebf_m4", "visit_r_fl_m4", "dur_r_m4", "fever_r_m4", "cough_r_m4", 
            "diarr_r_m4", "anti_r_m4", "gagedays_m4", "postbmi_m4", "mmuaccm_m4", 
            "cmfdint_m4", "bfmode_m4", "bfedfl_m4", "exbfedfl_m4", "sldfedfl_m4", 
            "fever_m4", "cough_m4", "diarr_m4", "vomit_m4", "physican_m4", 
            "hosp_m4", "antibiot_m4", "anti_oral_m4", "anti_inj_m4", "agedays_m5", 
            "mhtcm_m5", "mwtkg_m5", "mbmi_m5", "wtkg_m5", "lencm_m5", "bmi_m5", 
            "muaccm_m5", "waz_m5", "haz_m5", "whz_m5", "baz_m5", "muaz_m5", 
            "feeding_m5", "dur_bf_m5", "dur_ebf_m5", "visit_r_fl_m5", "dur_r_m5", 
            "fever_r_m5", "cough_r_m5", "diarr_r_m5", "anti_r_m5", "gagedays_m5", 
            "postbmi_m5", "mmuaccm_m5", "cmfdint_m5", "bfmode_m5", "bfedfl_m5", 
            "exbfedfl_m5", "sldfedfl_m5", "fever_m5", "cough_m5", "diarr_m5", 
            "vomit_m5", "physican_m5", "hosp_m5", "antibiot_m5", "anti_oral_m5", 
            "anti_inj_m5", "agedays_m6", "mhtcm_m6", "mwtkg_m6", "mbmi_m6", 
            "wtkg_m6", "lencm_m6", "bmi_m6", "muaccm_m6", "waz_m6", "haz_m6", 
            "whz_m6", "baz_m6", "muaz_m6", "feeding_m6", "dur_bf_m6", "dur_ebf_m6", 
            "visit_r_fl_m6", "dur_r_m6", "fever_r_m6", "cough_r_m6", "diarr_r_m6", 
            "anti_r_m6", "gagedays_m6", "postbmi_m6", "mmuaccm_m6", "cmfdint_m6", 
            "bfmode_m6", "bfedfl_m6", "exbfedfl_m6", "sldfedfl_m6", "fever_m6", 
            "cough_m6", "diarr_m6", "vomit_m6", "physican_m6", "hosp_m6", 
            "antibiot_m6", "anti_oral_m6", "anti_inj_m6","haz_6m_simulated"))
A = colnames(A)
Y = "haz_6m_simulated"

tableBioc <- tableFunc(A, W, Y, bioc)

# Save dataset
#write.csv(tableBioc, file = "/data/imic/results/tmle/biocVitalTMLE.csv")
```

# Metabolomic Indicators VITAL
```{r}
# Load data
metabol <- readRDS("/data/imic/data/raw_lab_data/vital/merged_vital/metabolClean.RDS")

dput(names((metabol)))

# Set parameters for tmleFunc
W = c("sex_base", "mage_base")
A = metabol %>%
  select(c("X2.Methylbutyrylglycinuria..NBS.", 
          "X7.Alpha.Dehydroxylation.of.Cholic.Acid", "Alpha.Aminobutyric.Acid.Synthesis", 
          "Anserine.Synthesis", "Asparagine.Synthesis", "Asymmetrical.Arginine.Methylation", 
          "Beta.Alanine.Synthesis", "Beta.Oxidation", "Betaine.Synthesis", 
          "Carnosine.Synthesis", "Citrulline.Synthesis", "Cortisone.Synthesis", 
          "Carbamoyl.Phosphate.Synthase.Deficiency..NBS.", "Cysteine.Synthesis", 
          "Cystine.Synthesis", "Dihydrolipoamide.Dehydrogenase.Deficiency..NBS.", 
          "Fischer.Ratio", "Gamma.Aminobutyric.Acid.Synthesis",
          "Global.Arginine.Bioavailability.Ratio",
          "Glycodeoxycholic.Acid.Synthesis.from.Cholic.Acid", "Glutaminase.Activity", 
          "Glutaminolysis.Rate", "Glycine.Conjugation.of.Cholic.Acid", 
          "Glycine.Conjugation.of.Chenodeoxycholic.Acid",
          "Glycine.Conjugation.of.Deoxycholic.Acid", 
          "Glycine.Conjugation.of.Primary.Bile.Acids", "Glycine.Synthesis", 
          "Glutathione.Constituents", "Homoarginine.Synthesis", "Homocysteine.Synthesis", 
          "Hippuric.Acid.Synthesis", "Isobutyryl.Coenzyme.A.Dehydrogenase.Deficiency..NBS.", 
          "Indoleamine.2.3.Dioxygenase.Activity", "Isovaleric.Acidemia..NBS.", 
          "Lactate.Dehydrogenase.Activity", "Malonic.Aciduria..NBS.", "Methionine.Oxidation", 
          "Methylmalonic.Acidemia..NBS.", "Methylenetetrahydrofolate.Reductase.Deficiency..NBS.", 
          "Nitric.Oxide.Synthase.Activity", "Ornithine.Synthesis",
          "Ornithine.Transcarbamylase.Deficiency..NBS.", 
          "para.Cresol.Sulfate.Synthesis", "Phenylethylamine.Synthesis", 
          "Phenylketonuria..NBS.", "Phospholipase.A2.Activity..2.",
          "Phospholipase.A2.Activity..3.", 
          "Phospholipase.A2.Activity..4.", "Phospholipase.A2.Activity..5.", 
          "Phospholipase.A2.Activity..6.", "Polyamine.Synthesis",
          "Ratio.of.Conjugated.Primary.Bile.Acids.to.Unconjugated.Primary.Bile.Acids", 
          "Putrescine.Synthesis", "Ratio.of.Acetylcarnitine.to.Carnitine", 
          "Ratio.of.Chenodeoxycholic.Acid.to.Cholic.Acid",
          "Ratio.of.Docosahexaenoic.Acid.to.Arachidonic.Acid", 
          "Ratio.of.Docosahexaenoic.Acid.to.Eicosapentaenoic.Acid", 
          "Ratio.of.Eicosapentaenoic.Acid.to.Arachidonic.Acid", 
          "Ratio.of.Homoarginine.to.Asymmetric.Dimethylarginine",
          "Ratio.of.Homoarginine.to.Symmetric.Dimethylarginine", 
          "Ratio.of.Non.Essential.to.Essential.Amino.Acids", "Ratio.of.Proline.to.Citrulline", 
          "Ratio.of.Hydroxylated.Sphingomyelins.to.Non.Hydroxylated.Sphingomyelins", 
          "Sarcosine.Synthesis.from.Choline", "Sarcosine.Synthesis.from.Glycine", 
          "Short.Branched.Chain.Acyl.Coenzyme.A.Dehydrogenase.Deficiency..NBS.", 
          "Short.Chain.Acyl.Coenzyme.A.Dehydrogenase.Deficiency..NBS.", 
          "Spermidine.Synthesis", "Spermine.Synthesis", "Sum.of.12.Alpha.Hydroxylated.Bile.Acids", 
          "Sum.of.Amino.Acids", "Sum.of.Aromatic.Amino.Acids", 
          "Sum.of.Asymmetrical.and.Symmetrical.Arginine.Methylation", 
          "Sum.of.Betaine.and.Related.Metabolites", "Sum.of.Betaine.Related.Metabolites", 
          "Sum.of.Branched.Chain.Amino.Acids", "Sum.of.Conjugated.Primary.Bile.Acids", 
          "Sum.of.Dimethylated.Arginines", "Sum.of.Essential.Amino.Acids", 
          "Sum.of.Long.Chain.Fatty.Acid.Lysophosphatidylcholines", 
          "Sum.of.Long.Chain.Fatty.Acid.Sphingomyelins", 
          "Sum.of.Measured.Omega.3.Fatty.Acids", 
          "Sum.of.Monounsaturated.Fatty.Acid.Lysophosphatidylcholines", 
          "Sum.of.Non.Essential.Amino.Acids", "Sum.of.Polyamines", "Sum.of.Primary.Bile.Acids", 
          "Sum.of.Polyunsaturated.Fatty.Acid.Lysophosphatidylcholines", 
          "Sum.of.Purine.Derivatives", "Sum.of.Non.Hydroxylated.Sphingomyelins", 
          "Sum.of.Hydroxylated.Sphingomyelins", "Sum.of.Sphingomyelins", 
          "Sum.of.Solely.Glucogenic.Amino.Acids", "Sum.of.Solely.Ketogenic.Amino.Acids", 
          "Sum.of.Steroid.Hormones", "Sum.of.Sulfur.Containing.Amino.Acids", 
          "Sum.of.Unconjugated.Bile.Acids", "Sum.of.Unconjugated.Primary.Bile.Acids", 
          "Sum.of.Very.Long.Chain.Fatty.Acid.Sphingomyelins", "Symmetrical.Arginine.Methylation", 
          "Taurine.Conjugation.of.Cholic.Acid", "Taurine.Conjugation.of.Chenodeoxycholic.Acid", 
          "Taurine.Conjugation.of.Deoxycholic.Acid", "Taurine.Conjugation.of.Primary.Bile.Acids", 
          "Taurine.Synthesis", "Taurodeoxycholic.Acid.Synthesis.from.Cholic.Acid", 
          "Trimethylamine.N.Oxide.Synthesis", "Trimethylamine.N.Oxide.Synthesis..direct.", 
          "Valinemia..NBS.", "Xanthine.Synthesis", "Ratio.of.Serine.and.Glycine.to.Hexose", 
          "Ratio.of.Serine..Glycine..and.Alanine.to.Hexose"))
A = colnames(A)
Y = "haz_6m_simulated"

tableMetabol <- tableFunc(A, W, Y, metabol)

# Save dataset
#write.csv(tableMetabol, file = "/data/imic/results/tmle/MetabolVitalTMLE.csv")
```


















