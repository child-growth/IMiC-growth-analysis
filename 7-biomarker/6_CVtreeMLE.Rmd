---
title: |
  CVtreeMLE
author: "Sajia Darwish"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
---

https://github.com/blind-contours/CVtreeMLE/blob/main/vignettes/intro_CVtreeMLE.Rmd
https://github.com/blind-contours/CVtreeMLE

```{r message=FALSE, warning=FALSE, include=FALSE}
# library(devtools) # For install_github().
# remotes::install_github("blind-contours/CVtreeMLE@main")
# remotes::install_github("tlverse/sl3@devel")
# install.packages("partykit")
# install.packages("pre")
# install.packages("namespace")

library(CVtreeMLE)
library(sl3)
library(pre)
library(partykit)
library(kableExtra)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(namespace)


# Load data
data <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/hmo.RDS")

#dput(names(data))

hmo <- data %>%
select(c("Secretor", "Diversity", "Evenness", "X2.FL_nmol.mL", 
         "X3FL_nmol.mL", "DFLac_nmol.mL", "X3.SL_nmol.mL", "X6.SL_nmol.mL", 
         "LNT_nmol.mL", "LNnT_nmol.mL", "LNFP.I_nmol.mL", "LNFP.II_nmol.mL", 
         "LNFP.III_nmol.mL", "LSTb_nmol.mL", "LSTc_nmol.mL", "DFLNT_nmol.mL", 
         "LNH_nmol.mL", "DSLNT_nmol.mL", "FLNH_nmol.mL", "DFLNH_nmol.mL", 
         "FDSLNH_nmol.mL", "DSLNH_nmol.mL", "SUM_nmol.mL", "Sia_nmol.mL", 
         "Fuc_nmol.mL","sex_base", "mage_base", "nlchild_base", "haz_6m"))

# Scramble outcome variable.
hmo $ haz_6m <- sample(hmo $ haz_6m)

# Remove _nmol.mL
# Keep everything before and up to ":": 
names(hmo) <- gsub("_nmol.mL.*", "", names(hmo))

# Convert sex_base to binary.
hmo $ sex_base = ifelse(hmo $ sex_base == "Male", 0, 1)

# Delete all NAs in the outcome var.
hmo <- hmo %>%
  filter(!is.na(haz_6m))

# Save clean dataset
saveRDS(hmo, file = "/data/imic/data/raw_lab_data/elicit/merged_elicit/hmoClean.RDS")

# Delete all NA for CVtreeMLE
hmo <- hmo %>%
  na.omit()
```

# Implement CVtreeMLE
```{r, echo=FALSE}
# Save the names of the mixture variables.
mixVars <- c("X2.FL", "X3FL", "DFLac", "X3.SL", "X6.SL", "LNT", "LNnT",
             "LNFP.I", "LNFP.II", "LNFP.III", "LSTb", "LSTc", "DFLNT",
             "LNH", "DSLNT", "FLNH", "DFLNH", "FDSLNH", "DSLNH", "SUM", 
             "Sia", "Fuc")

covars = c("sex_base", "mage_base", "nlchild_base", "Secretor", "Diversity", 
           "Evenness")

ptm <- proc.time()
results <- CVtreeMLE(data = hmo,
                     w = covars,
                     a = mixVars,
                     y = "haz_6m",
                     n_folds = 10,
                     seed = 1000,
                     parallel_cv = TRUE,
                     parallel_type = "multi_session",
                     family = "continuous",
                     num_cores = 10)
proc.time() - ptm


#save results
saveRDS(results, file=paste0(here::here(),"/results/CVtreeMLE_example_model.RDS"))

```

## Pooled TMLE results
```{r, echo=FALSE}
mixture_results <- results $ `Pooled TMLE Mixture Results`
#write.csv(mixture_results, file = "/data/imic/results/CVtreeMLE_mixtureResults.csv")

# Mixture present in highest proportion of folds
max(mixture_results $ Proportion_Folds)

# Look at the most stable partitions 
mixture_results %>%
   dplyr::filter(Proportion_Folds == 0.5)
```

The estimated mixture ATE is interpreted as the average counterfactual mean outcome if all individuals were exposed to the rule shown in Union Rule compared to if all individuals were unexposed. That is, those individuals who are exposed to this rule have an outcome that is different by the estimate amount compared to those that are not exposed to this rule.

Children exposed to concentrations of 74.279 =< X6.SL <= 211.091 and 552.188 =< Sia <= 1242.157 had 0.112 higher HAZ at six months than those that were not exposed to these concentrations of X6.SL and Sia.

## V-fold specific results
```{r}
mixture_v_results <- results $ `V-Specific Mix Results`
mixture_v_results

# All mixtures
names(mixture_v_results)

# Again, look at v-fold specific results present in highest proportion of folds
specificResult <- mixture_v_results $ "Sia-X6.SL"
write.csv(specificResult, file = "/data/imic/results/CVtreeMLE_specificResults.csv")
```

The v-fold specific results also give a pooled estimate. This is different than the pooled TMLE estimate. Here we simply take the weighted average of the fold specific ATEs and the harmonic mean of the variances. This is similar to meta-analysis approaches.

## Plot of the v-fold mixture results: interactions
```{r}
mixture_plots <- plot_mixture_results(v_intxn_results =
                                      results $ `V-Specific Mix Results`,
                                      hjust = 1.05)
# Plots of most stable partitions
mixture_plots $ "Sia-X6.SL"
```

This plot shows the ATE specific for each fold and for the weighted-mean results over the fold with corresponding pooled variance. The rule is the union rule which includes all observations that were indicated by the fold specific rules.

# Implement on biocrates dataset
```{r}
rm(list=ls())
source('~/IMiC-growth-analysis/0-config.R')

# Load data
data <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/biocratesNorm.RDS")

#dput(names(bioc))

# Convert sex_base to binary.
data $ sex_base = ifelse(data $ sex_base == "Male", 0, 1)

bioc <- data %>%
select(-c("bmid_base", "country", "studyid", "siteid", "subjid", "subjido", 
          "studytyp", "arm_base", "brthyr_base", "brthweek_base", "parity_base", 
          "nperson_base", "nrooms_base", "meducyrs_base", "h2osrcp_base",
          "cookplac_base", "inctot_base", "inctotu_base", "epochn_base", 
          "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", "pregout_base",
          "dlvloc_base", "dvseason_base", "wtkg_base", "lencm_base", "bmi_base",
          "hcircm_base", "waz_base", "haz_base", "whz_base", "baz_base",
          "agedays_base", "feeding_base", "dur_bf_base", "dur_ebf_base", 
          "bmcol_fl_1m", "bmcol_fl_5m", "agedays_1m", "agedays_5m", "lencm_3m",
          "lencm_6m", "lencm_9m", "lencm_12m", "lencm_15m", "lencm_18m",
          "bmi_3m", "bmi_6m", "bmi_9m", "bmi_12m", "bmi_15m", "bmi_18m",
          "hcircm_3m", "hcircm_6m", "hcircm_9m", "hcircm_12m", "hcircm_15m",
          "hcircm_18m", "waz_3m", "waz_6m", "waz_9m", "waz_12m", "waz_15m",
          "waz_18m", "haz_3m", "haz_9m", "haz_12m", "haz_15m", "nlchild_base",
          "haz_18m", "whz_3m", "whz_6m", "whz_9m", "whz_12m", "whz_15m", 
          "whz_18m", "baz_3m", "baz_6m", "baz_9m", "baz_12m", "baz_15m", "baz_18m",
          "agedays_3m", "agedays_6m", "agedays_9m", "agedays_12m", "agedays_15m",
          "agedays_18m", "visit_r_fl_0to6m", "visit_r_fl_6to18m", "dur_r_0to6m",
          "dur_r_6to18m", "bfedfl_r_0to6m", "exbfed_r_0to6m", "exbfdu_r_0to6m",
          "fever_r_0to6m", "fever_r_6to18m", "cough_r_0to6m", "cough_r_6to18m",
          "diarr_r_0to6m", "diarr_r_6to18m", "mhgb_18m", "muaccm_18m", "muaz_18m",
          "agedays_18m.1", "bfdu_r_0to18m", "anti_r_0to18m", "mage_base"))

table(is.na(bioc))

# Scramble outcome variable.
bioc $ haz_6m <- sample(bioc $ haz_6m)

# Delete all NAs.
bioc <- bioc %>%
  filter(!is.na(haz_6m))

# Call cleaning function from the pcs script
cleanData <- cleanFunc(bioc)
head(cleanData)
```

# Implement CVtreeMLE
```{r}
# Save the names of the mixture variables.
mixVars <- cleanData %>%
  select(-c(haz_6m))

mixVars <- names(mixVars)

covars <- c("sex_base")

ptm <- proc.time()
results <- CVtreeMLE(data = cleanData,
                     w = covars,
                     a = mixVars,
                     y = "haz_6m",
                     n_folds = 10,
                     seed = 1000,
                     parallel_cv = TRUE,
                     parallel_type = "multi_session",
                     family = "continuous",
                     num_cores = 10)
proc.time() - ptm
```

## Pooled TMLE results
```{r, echo=FALSE}
mixture_results <- results $ `Pooled TMLE Mixture Results`
#write.csv(mixture_results, file = "/data/imic/results/CVtreeMLE_mixtureResults.csv")

# Look at the most stable partitions 
mixture_results %>%
   dplyr::filter(max(Proportion_Folds))
```

## V-fold specific results
```{r}
mixture_v_results <- results $ `V-Specific Mix Results`
mixture_v_results

# # Again, look at the partitions with most consistency
# mixture_v_results $ "DFLac-Fuc"
# mixture_v_results $ "LNFP.I-X3.SL"
```

## Plot of the v-fold mixture results: interactions
```{r}
mixture_plots <- plot_mixture_results(v_intxn_results =
                                      results $ `V-Specific Mix Results`,
                                      hjust = 1.05)
# Plots of most stable partitions
mixture_plots $ "DFLac-Fuc"
mixture_plots $ "LNFP.I-X3.SL"
```





