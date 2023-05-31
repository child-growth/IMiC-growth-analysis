

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(knitr)
library(kableExtra)
library(CVtreeMLE)


model <- readRDS(paste0(here::here(),"/results/CVtreeMLE_example_model.RDS"))
res <- readRDS(paste0(here::here(),"/results/CVtreeMLE_specificResults.RDS"))

head(res)



# mixture_results <- model$`Pooled TMLE Mixture Results`
# mixture_results %>%
#   dplyr::filter(Proportion_Folds == 1.0) %>%
#   kbl(caption = "Mixture Results") %>%
#   kable_classic(full_width = FALSE, html_font = "Cambria")


## Pooled TMLE results
mixture_results <- model$`Pooled TMLE Mixture Results`
#write.csv(mixture_results, file = "/data/imic/results/CVtreeMLE_mixtureResults.csv")

# Mixture present in highest proportion of folds
max(mixture_results $ Proportion_Folds)

# Look at the most stable partitions 
mixture_results %>%
  dplyr::filter(Proportion_Folds == 0.5)

# The estimated mixture ATE is interpreted as the average counterfactual mean outcome if all individuals were exposed to the rule shown in Union Rule compared to if all individuals were unexposed. That is, those individuals who are exposed to this rule have an outcome that is different by the estimate amount compared to those that are not exposed to this rule.
# Children exposed to concentrations of 74.279 =< X6.SL <= 211.091 and 552.188 =< Sia <= 1242.157 had 0.112 higher HAZ at six months than those that were not exposed to these concentrations of X6.SL and Sia.

## V-fold specific results
mixture_v_results <- model $ `V-Specific Mix Results`
mixture_v_results

# All mixtures
names(mixture_v_results)

# Again, look at v-fold specific results present in highest proportion of folds
specificResult <- mixture_v_results $ "Sia-X6.SL"


mixture_plots <- plot_mixture_results(
  v_intxn_results =
    model$`V-Specific Mix Results`,
  hjust = 1
)

mixture_plots$LNnT

