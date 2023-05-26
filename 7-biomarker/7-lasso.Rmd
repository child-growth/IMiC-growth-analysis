---
title: |
  Lasso Regression
author: "Sajia Darwish"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
output:
  html_document:
    toc: yes
    toc_depth: 4
  pdf_document:
    toc: yes
    toc_depth: '4'
keep_tex: yes
theme: bclear
fontsize: 10pt
geometry: margin=1in
header-includes:
- \usepackage{enumitem}
- \usepackage{indentfirst}
- \usepackage[default, scale=1]{raleway}
- \usepackage[T1]{fontenc}
- \usepackage{inconsolata}
always_allow_html: yes
---

```{r, include=FALSE}
library(tidyverse)
library(glmnet)

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

# Remove _nmol.mL
# Keep everything before and up to ":": 
names(hmo) <- gsub("_nmol.mL.*", "", names(hmo))

# Convert sex_base to binary
hmo $ sex_base = ifelse(hmo $ sex_base == "Male", 0, 1)

# Delete all NAs.
hmo <- hmo %>%
  na.omit()
```

# ELICIT: HMO
```{r}
# Define response variable
y <- hmo $ haz_6m

#define matrix of predictor variables
x <- data.matrix(hmo[, c("X2.FL", "X3FL", "DFLac", "X3.SL", "X6.SL", "LNT", "LNnT",
                         "LNFP.I", "LNFP.II", "LNFP.III", "LSTb", "LSTc", "DFLNT",
                         "LNH", "DSLNT", "FLNH", "DFLNH", "FDSLNH", "DSLNH", "SUM", "Sia", "Fuc",
                         "Secretor", "Diversity", "Evenness", "sex_base", "mage_base", "nlchild_base")])

# Perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# Find optimal lambda value that minimizes test MSE
best_lambda <- cv_model $ lambda.min
best_lambda

# Produce plot of test MSE by lambda value
plot(cv_model)

# Find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
```

# ELICIT: B-vitamins
```{r}
# Load data
data <- readRDS("/data/imic/data/raw_lab_data/elicit/merged_elicit/bvit.RDS")

bvit <- data %>%
select(c("B12", "TPP", "TMP", "T", "B1", "Ribo", "FMN", 
         "FAD", "NAM", "NAD", "NMN", "NR", "Nufa", "PA", "PL", "PM", "PN", 
         "PLP", "Bio", "TRP", "B2", "B3", "B6", "haz_6m",
         "sex_base", "mage_base", "nlchild_base"))

# Delete all NAs.
bvit <- bvit %>%
  na.omit()

# Define response variable
y <- bvit $ haz_6m

#define matrix of predictor variables
x <- data.matrix(bvit[, c("B12", "TPP", "TMP", "T", "B1", "Ribo", "FMN",
                         "FAD", "NAM", "NAD", "NMN", "NR", "Nufa", "PA", "PL", "PM", "PN", 
                         "PLP", "Bio", "TRP", "B2", "B3", "B6", "sex_base", "mage_base", "nlchild_base")])

# Perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# Find optimal lambda value that minimizes test MSE
best_lambda <- cv_model $ lambda.min
best_lambda

# Produce plot of test MSE by lambda value
plot(cv_model)

# Find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
```



