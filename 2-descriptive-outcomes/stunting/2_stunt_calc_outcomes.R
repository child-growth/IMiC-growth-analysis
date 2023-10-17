################################################################################
# The ELCIIT and VITAL datasets have different time frames (0-18 months vs 0-6 
# months, respectively). The following code is hard-coded and uses a specific 
# time frame (e.g., 24 months) to calculate cumulative measures. As such, 
# I (Sajia) have calculated these measures for each dataset separately and then 
# combined them. The final file saved is named exactly as the previous one so it 
# should not interfere with future usage.
################################################################################
# ki longitudinal manuscripts
# stunting analysis

# Calculate mean LAZ, prevalence, incidence, 
# and recovery, repeated for fixed effects models 
# and sensitivity analysis in monthly cohorts
# with measurements up to 18 months for ELICIT and up to 6 months for VITAL.

# Inputs:
#   0-config.R : configuration file
#   0_descriptive_epi_shared_functions.R
#   0_descriptive_epi_stunt_functions.R
#   stunting_data.RData

# Outputs:
#   meanlaz_velocity.RDS
#   meanlaz_velocity_fe.RDS
#   quantile_data_stunting.RDS
#   quantile_data_stunting_fe.RDS
#   shiny_desc_data_stunting_objects.RDS
#   shiny_desc_data_stunting_objects_fe.RDS
################################################################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

# reloading because some overlap with stunting
source(paste0(here::here(), "/0-project-functions/0_descriptive_epi_shared_functions.R"))
source(paste0(here::here(), "/0-project-functions/0_descriptive_epi_stunt_functions.R"))

agelst3 = list(
  "0-3 months",
  "3-6 months",
  "6-9 months",
  "9-12 months",
  "12-15 months",
  "15-18 months"
)

agelst6 = list(
  "0-6 months", 
  "6-12 months", 
  "12-18 months"
)

agelst3_birthstrat = list(
  "Birth",
  "8 days-3 months",
  "3-6 months",
  "6-9 months",
  "9-12 months",
  "12-15 months",
  "15-18 months"
)

agelst6_birthstrat = list(
  "Birth",
  "8 days-6 months", 
  "6-12 months", 
  "12-18 months"
)

# data=d
# calc_method="REML"
# output_file_suffix=""

calc_outcomes = function(data, calc_method, output_file_suffix){
  dprev <<- calc.prev.agecat(data)
  d3 <<- calc.ci.agecat(data, range = 3, birth="yes")
  d6 <<- calc.ci.agecat(data, range = 6, birth="yes")
  d3_birthstrat <<- calc.ci.agecat(data, range = 3, birth="no")
  d6_birthstrat <<- calc.ci.agecat(data, range = 6, birth="no")
  
  ##############################################################################
  # Prevalence
  ##############################################################################
  calc_prevalence = function(severe){
    prev.data <- summary.prev.haz(dprev, severe.stunted = severe, method = calc_method)
    prev.cohort <- prev.data$prev.cohort %>% 
      subset(., select = c(cohort, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
      rename(est = prev,  lb = ci.lb,  ub = ci.ub)
    
    prev <- bind_rows(
      prev <- data.frame(cohort = "pooled", prev.data$prev.res),
      prev.cohort
    )
    return(prev)
  }
  #-----------------------------------------------------------------------------
  # Prevalence and WHZ  - not including yearly studies
  #-----------------------------------------------------------------------------
  prev = calc_prevalence(severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Severe stunting prevalence
  #-----------------------------------------------------------------------------
  sev.prev = calc_prevalence(severe = TRUE)
  #-----------------------------------------------------------------------------
  # mean haz
  #-----------------------------------------------------------------------------
  haz.data <- summary.haz(dprev, method = calc_method)
  haz.cohort <- haz.data$haz.cohort %>% 
    subset(., select = c(cohort, agecat, nmeas, meanhaz,  ci.lb,  ci.ub)) %>%
    rename(est = meanhaz,  lb = ci.lb,  ub = ci.ub)
  
  haz <- bind_rows(data.frame(cohort = "pooled", haz.data$haz.res), haz.cohort)
  
  #-----------------------------------------------------------------------------
  # mean haz for growth velocity age categories
  #-----------------------------------------------------------------------------
  d_vel = data %>% 
    mutate(agecat=ifelse(agedays<3*30.4167,"0-3",
                         ifelse(agedays>=3*30.4167 & agedays<6*30.4167,"3-6",
                                ifelse(agedays>=6*30.4167 & agedays<9*30.4167,"6-9",
                                       ifelse(agedays>=9*30.4167 & agedays<12*30.4167,"9-12",
                                              ifelse(agedays>=12*30.4167 & agedays<15*30.4167,"12-15",
                                                     ifelse(agedays>=15*30.4167,"15-18", ""))))))) %>%
    mutate(agecat=factor(agecat,levels=c("0-3","3-6","6-9","9-12",
                                         "12-15","15-18")))
  
  haz.data.vel <- summary.haz.age.sex(d_vel, method = calc_method) 
  haz.cohort.vel <- haz.data.vel$haz.cohort %>% 
    subset(., select = c(cohort, agecat, sex, nmeas,  meanhaz, ci.lb,  ci.ub)) %>% 
    rename(est = meanhaz,  lb = ci.lb,  ub = ci.ub)
  
  haz.vel.elicit <- bind_rows(data.frame(cohort = "pooled", haz.data.vel$haz.res), haz.cohort.vel)
  
  saveRDS(haz.vel.elicit, file = paste0(res_dir, "stunting/meanlaz_velocity_E", 
                                        calc_method, output_file_suffix, ".RDS"))
  
  ##############################################################################
  # Incidence proportion
  ##############################################################################
  calc_ip = function(datatable, age_list, severe){
    ip.data <- summary.stunt.incprop(datatable, agelist = age_list, 
                                     severe.stunted = severe, method = calc_method)
    ip.cohort <- ip.data$ip.cohort %>% 
      subset(., select = c(cohort, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
      rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)
    
    ip <- bind_rows(
      data.frame(cohort = "pooled", ip.data$ip.res),
      ip.cohort
    )
    return(ip)
  }
  #-----------------------------------------------------------------------------
  # Incidence proportion 3 month intervals
  #-----------------------------------------------------------------------------
  ip_3 = calc_ip(d3, agelst3, severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Incidence proportion 3 month intervals
  # stratify by birth
  #-----------------------------------------------------------------------------
  ip_3.birthstrat = calc_ip(d3_birthstrat, agelst3_birthstrat, severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Incidence proportion 6 month intervals
  #-----------------------------------------------------------------------------
  ip_6 = calc_ip(d6, agelst6, severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Incidence proportion of severe stunting 
  # 3 month interval
  #-----------------------------------------------------------------------------
  sev.ip3 = calc_ip(d3, agelst3, severe = TRUE)
  
  #-----------------------------------------------------------------------------
  # Incidence proportion of severe stunting
  # 6 month interval
  #-----------------------------------------------------------------------------
  sev.ip6 = calc_ip(d6, agelst6, severe = TRUE)
  
  ##############################################################################
  # Cumulative incidence
  ##############################################################################
  
  calc_ci = function(datatable, age_list, birth_strat, severe){
    ci.data <- summary.ci(datatable, birthstrat = birth_strat, agelist = age_list,
                          severe.stunted = severe, method = calc_method)
    ci.cohort <-
      ci.data$ci.cohort %>% 
      subset(., select = c(cohort, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
      rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)
    
    cuminc <- bind_rows(
      data.frame(cohort = "pooled", ci.data$ci.res),
      ci.cohort
    )
    return(cuminc)
  }
  
  #-----------------------------------------------------------------------------
  # Cumulative Incidence  - 3 month intervals
  #-----------------------------------------------------------------------------
  cuminc3 = calc_ci(d3, agelst3, birth_strat = FALSE, severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Cumulative Incidence  - 3 month intervals
  # stratify by birth 
  #-----------------------------------------------------------------------------
  cuminc3.birthstrat = calc_ci(d3_birthstrat, agelst3_birthstrat,
                               birth_strat = TRUE, severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Cumulative Incidence  - 6 month intervals
  #-----------------------------------------------------------------------------
  cuminc6 <- calc_ci(d6, agelst6, birth_strat = FALSE, severe = FALSE)
  
  #-----------------------------------------------------------------------------
  # Cumulative Incidence  - 3 month intervals 
  # severe
  #-----------------------------------------------------------------------------
  sev.cuminc3 <- calc_ci(d3, agelst3, birth_strat = FALSE, severe = TRUE)
  
  #-----------------------------------------------------------------------------
  # Cumulative Incidence  - 6 month intervals
  # severe
  #-----------------------------------------------------------------------------
  sev.cuminc6 <- calc_ci(d6, agelst6, birth_strat = FALSE, severe = TRUE)
  
  shiny_desc_data <- bind_rows(
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="no", measure= "Prevalence", prev),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="yes", measure= "Prevalence", sev.prev),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="no", measure= "Mean LAZ",  haz),
    # data.frame(disease = "Stunting", age_range="1 month",
    #            birth="yes", severe="no", measure= "Mean LAZ",  monthly.haz),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="no", measure= "Cumulative incidence", cuminc3),
    data.frame(disease = "Stunting", age_range="3 months", 
               birth="strat", severe="no", measure= "Cumulative incidence", cuminc3.birthstrat),
    data.frame(disease = "Stunting", age_range="6 months", 
               birth="yes", severe="no", measure= "Cumulative incidence", cuminc6),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="yes", measure= "Cumulative incidence", sev.cuminc3),
    data.frame(disease = "Stunting", age_range="6 months",
               birth="yes", severe="yes", measure= "Cumulative incidence", sev.cuminc6),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="no", measure= "Incidence_proportion", ip_3),
    data.frame(disease = "Stunting", age_range="3 months",  
               birth="strat", severe="no", measure= "Incidence_proportion", ip_3.birthstrat),
    data.frame(disease = "Stunting", age_range="6 months",
               birth="yes", severe="no", measure= "Incidence_proportion", ip_6),
    data.frame(disease = "Stunting", age_range="3 months", 
               birth="yes", severe="yes", measure= "Incidence_proportion",  sev.ip3),
    data.frame(disease = "Stunting", age_range="6 months",
               birth="yes", severe="yes", measure= "Incidence_proportion",  sev.ip6)
  )
  
  shiny_desc_data <- shiny_desc_data %>% subset(., select = -c(se, nmeas.f,  ptest.f))
  
  shiny_desc_data$agecat <- as.factor(shiny_desc_data$agecat)
  
  return(shiny_desc_data)
}


d <- readRDS(paste0(ghapdata_dir,"stunting_data.rds"))

stunt_outcomes = calc_outcomes(data = d, calc_method = "REML", output_file_suffix = "")
stunt_outcomes_fe = calc_outcomes(data = d, calc_method = "FE", output_file_suffix = "_fe")

# # Add a studyid var to each dataset
# stunt_outcomes_V $ studyid = "VITAL-Lactation"
# stunt_outcomes_fe_V $ studyid = "VITAL-Lactation"
# stunt_outcomes_E $ studyid = "ELICIT"
# stunt_outcomes_fe_E $ studyid = "ELICIT"

# Save datasets
saveRDS(stunt_outcomes, file = paste0(res_dir,"stunting/stunting_desc_data.RDS"))
saveRDS(stunt_outcomes_fe, file = paste0(res_dir,"stunting/stunting_desc_data_FE.RDS"))


