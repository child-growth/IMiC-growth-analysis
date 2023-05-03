
#-------------------------------------------------------------------------------
# GRF causal forest treatment heterogeneity
# Inputs:
#   Wide dataset + biomarker data for VITAL trial
# Outputs:
#   A dataframe of results
#-------------------------------------------------------------------------------

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(caret)
library(grf)


#load dataset of HMO biomarkers and baseline variables
dfull <- readRDS("/data/imic/data/raw_lab_data/vital/merged_vital/hmoClean.RDS")
head(dfull)

table(dfull$arm_base)

#drop control arm and just contrast nutritional supplementation vs  nutritional supplementation + AZT
df <- dfull %>% filter(arm_base!="Control") %>%
                mutate(tr=ifelse(arm_base=="Nutrient supplement+Ex.BreastFeed+AZT",1,0))

#Set outcome, treatment, and biomarkers

Y = "haz_6m_simulated" #scrambles outcome assignment
W = "tr" #treatment assignment (called W in grf), 0= Control arm



#baseline variables:
base_vars <- c("bmid_base", "subjid", "country", "studyid", "subjido", 
        "sex_base", "brthyr_base", "brthweek_base", "mage_base", 
        "parity_base", "nlchild_base", "nperson_base", "nrooms_base", 
        "meducyrs_base", "h2osrcp_base", "agedays_base", "epochn_base", 
        "epoch_base", "mhtcm_base", "mwtkg_base", "mbmi_base", "dlvloc_base", 
        "wtkg_base", "lencm_base", "bmi_base", "muaccm_base", "waz_base", 
        "haz_base", "whz_base", "baz_base", "feeding_base", "dur_bf_base", 
        "dur_ebf_base", "visit_r_fl_base", "dur_r_base", "fever_r_base", 
        "cough_r_base", "anti_r_base", "citytown_base", "gagebrth_base", 
        "gagecm_base", "birthwt_base", "birthlen_base", "birthord_base", 
        "gravida_base", "nlivbrth_base", "floor_base", "gagedays_base", 
        "postbmi_base", "mmuaccm_base", "delivery_base", "bfinittm_base", 
        "cmfdint_base", "formlkfl_base", "fever_base", "diarr_base", 
        "physican_base", "hosp_base", "antibiot_base", "haz_m6", "anti_inj_base",
        "anti_oral_base")

biomarkers <- c("X2.FL_pct",       "X3FL_pct",        "DFLac_pct",       "X3.SL_pct",       "X6.SL_pct",       "LNT_pct",        
                "LNnT_pct",        "LNFP.I_pct",      "LNFP.II_pct",     "LNFP.III_pct",    "LSTb_pct",        "LSTc_pct",        "DFLNT_pct",       "LNH_pct",        
                "DSLNT_pct",       "FLNH_pct",        "DFLNH_pct",       "FDSLNH_pct",      "DSLNH_pct")


base_vars[!(base_vars %in% colnames(df))]

#process predictors
X_df <- df %>% subset(., select = c(base_vars,biomarkers))

#remove near zero variance predictors
X_df = X_df[, -nzv(X_df)]

#Convert factors to indicators:
X_df = model.matrix(~. , data=X_df)


grf_model <- causal_forest(X=X_df, 
                           Y=df$haz_6m_simulated, 
                           W=df$tr)
grf_model