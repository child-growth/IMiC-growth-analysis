

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))


bvit_fit <- readRDS(paste0(here::here(),"/results/bvitEr2weight.RDS"))
hmo_fit <- readRDS(paste0(here::here(),"/results/hmoEr2weight.RDS"))
try(hmoE <- readRDS("C:/Users/andre/Downloads/hmo.RDS"))

head(hmo_fit)


hmo_results <- hmo_fit$results
hmo_results$predictors <- c("nlchild_base", "nperson_base", "nrooms_base", 
      "meducyrs_base", "h2osrcp_base", "cookplac_base",
      "inctot_base")



