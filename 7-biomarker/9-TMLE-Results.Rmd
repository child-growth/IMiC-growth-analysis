---
title: |
  TMLE Results
author: "Sajia Darwish"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
---

# Overview

- Implemented TMLE over each group of biomarkers separately for each study, controlling for only a couple of baseline covariates for simplicity (sex and mother's age for most).
- The following graphs show TMLE estimates for each group of biomarkers to understand how they are associated with the outcome (haz_6m).
- The outcome for these analyses were simualted from the distrubutions of the actual outcomes.

```{r, include=FALSE}
library(tidyverse)

hmo <- read.csv("/data/imic/results/tmle/hmoElicitTMLE.csv")
bioc <- read.csv("/data/imic/results/tmle/biocElicitTMLE.csv")
bvit <- read.csv("/data/imic/results/tmle/BvitElicitTMLE.csv")
metabol <- read.csv("/data/imic/results/tmle/MetabolElicitTMLE.csv")
```

# ELICIT (N = 199)

## HMO (22 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
hmo %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```

## Biocrates (329 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
bioc %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```

## B-vitamins (23 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
bvit %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```

## Metabolomcis (111 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
metabol %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```

# VITAL (N = 147)
```{r, include=FALSE}
hmo <- read.csv("/data/imic/results/tmle/hmoVitalTMLE.csv")
bioc <- read.csv("/data/imic/results/tmle/biocVitalTMLE.csv")
metabol <- read.csv("/data/imic/results/tmle/MetabolVitalTMLE.csv")
```

## HMO (22 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
hmo %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```

## Biocrates (630 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
bioc %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```

## Metabolomcis (111 features)
```{r, echo=FALSE, fig.show="hold", out.width="75%", fig.align = 'center'}
metabol %>%
    ggplot(aes(x =tmle_est)) +
    geom_histogram(binwidth = 0.1, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
    theme_classic()
```
















