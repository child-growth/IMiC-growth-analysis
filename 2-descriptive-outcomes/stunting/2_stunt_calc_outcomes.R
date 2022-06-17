##########################################
# ki longitudinal manuscripts
# stunting analysis

# Calculate mean LAZ, prevalence, incidence, 
# and recovery, repeated for fixed effects models 
# and sensitivity analysis in monthly cohorts
# with measurements up to 18 months

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
##########################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

# reloading because some overlap with stunting
source(paste0(here::here(), "/0-project-functions/0_descriptive_epi_shared_functions.R"))
source(paste0(here::here(), "/0-project-functions/0_descriptive_epi_stunt_functions.R"))

d <- readRDS(paste0(ghapdata_dir, "stunting_data.rds"))

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

calc_outcomes = function(data, calc_method, output_file_suffix){
  dprev <<- calc.prev.agecat(data)
  d3 <<- calc.ci.agecat(data, range = 3, birth="yes")
  d6 <<- calc.ci.agecat(data, range = 6, birth="yes")
  d3_birthstrat <<- calc.ci.agecat(data, range = 3, birth="no")
  d6_birthstrat <<- calc.ci.agecat(data, range = 6, birth="no")
  
  ######################################################################
  # Prevalence
  ######################################################################
  calc_prevalence = function(severe){
    prev.data <- summary.prev.haz(dprev, severe.stunted = severe, method = calc_method)
    
    prev <- data.frame(prev.data$prev.res)
    return(prev)
  }
  #----------------------------------------
  # Prevalence and WHZ  - not including yearly studies
  #----------------------------------------
  prev = calc_prevalence(severe = FALSE)
  
  #----------------------------------------
  # Severe stunting prevalence
  #----------------------------------------
  sev.prev = calc_prevalence(severe = TRUE)
  
  ######################################################################
  # Mean HAZ
  ######################################################################
  #----------------------------------------
  # mean haz
  #----------------------------------------
  haz.data <- summary.haz(dprev, method = calc_method)
  
  haz <- data.frame(haz.data$haz.res)
  
  #----------------------------------------
  # mean haz for growth velocity age categories
  #----------------------------------------
  d_vel = data %>% 
    mutate(agecat=ifelse(agedays<3*30.4167,"0-3",
                         ifelse(agedays>=3*30.4167 & agedays<6*30.4167,"3-6",
                                ifelse(agedays>=6*30.4167 & agedays<9*30.4167,"6-9",
                                       ifelse(agedays>=9*30.4167 & agedays<12*30.4167,"9-12",
                                              ifelse(agedays>=12*30.4167 & agedays<15*30.4167,"12-15",
                                                     ifelse(agedays>=15*30.4167,"15-18", ""))))))) %>%
                                                            #ifelse(agedays>=18*30.4167 & agedays<21*30.4167,"18-21",
                                                                   #ifelse(agedays>=21*30.4167& agedays<24*30.4167,"21-24",""
    mutate(agecat=factor(agecat,levels=c("0-3","3-6","6-9","9-12",
                                         "12-15","15-18")))
  
  haz.data.vel <- summary.haz.age.sex(d_vel, method = calc_method)
    rename(est = meanhaz,  lb = ci.lb,  ub = ci.ub)
  
  haz.vel <- data.frame(haz.data.vel$haz.res)
  
  saveRDS(haz.vel, file = paste0(res_dir, "stunting/meanlaz_velocity", 
                                 calc_method, output_file_suffix, ".RDS"))
  
  ######################################################################
  # Incidence proportion
  ######################################################################
  calc_ip = function(datatable, age_list, severe){
    ip.data <- summary.stunt.incprop(datatable, agelist = age_list, 
                                     severe.stunted = severe, method = calc_method)
    
    ip <- data.frame(ip.data$ip.res)
    return(ip)
  }
  #----------------------------------------
  # Incidence proportion 3 month intervals
  #----------------------------------------
  ip_3 = calc_ip(d3, agelst3, severe = FALSE)
  
  #----------------------------------------
  # Incidence proportion 3 month intervals
  # stratify by birth
  #----------------------------------------
  ip_3.birthstrat = calc_ip(d3_birthstrat, agelst3_birthstrat, severe = FALSE)
  
  #----------------------------------------
  # Incidence proportion 6 month intervals
  #----------------------------------------
  ip_6 = calc_ip(d6, agelst6, severe = FALSE)
  
  #----------------------------------------
  # Incidence proportion of severe stunting 
  # 3 month interval
  #----------------------------------------
  sev.ip3 = calc_ip(d3, agelst3, severe = TRUE)
  
  #----------------------------------------
  # Incidence proportion of severe stunting
  # 6 month interval
  #----------------------------------------
  sev.ip6 = calc_ip(d6, agelst6, severe = TRUE)
  
  ######################################################################
  # Cumulative incidence
  ######################################################################
  
  calc_ci = function(datatable, age_list, birth_strat, severe){
    ci.data <- summary.ci(datatable, birthstrat = birth_strat, agelist = age_list,
                          severe.stunted = severe, method = calc_method)
      rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)
    
    cuminc <- data.frame(ci.data$ci.res)
    return(cuminc)
  }
  
  #----------------------------------------
  # Cumulative Incidence  - 3 month intervals
  #----------------------------------------
  cuminc3 = calc_ci(d3, agelst3, birth_strat = FALSE, severe = FALSE)
  
  #----------------------------------------
  # Cumulative Incidence  - 3 month intervals
  # stratify by birth 
  #----------------------------------------
  cuminc3.birthstrat = calc_ci(d3_birthstrat, agelst3_birthstrat,
                               birth_strat = TRUE, severe = FALSE)
  
  #----------------------------------------
  # Cumulative Incidence  - 6 month intervals
  #----------------------------------------
  cuminc6 <- calc_ci(d6, agelst6, birth_strat = FALSE, severe = FALSE)
  
  #----------------------------------------
  # Cumulative Incidence  - 3 month intervals 
  # severe
  #----------------------------------------
  sev.cuminc3 <- calc_ci(d3, agelst3, birth_strat = FALSE, severe = TRUE)
  
  #----------------------------------------
  # Cumulative Incidence  - 6 month intervals
  # severe
  #----------------------------------------
  sev.cuminc6 <- calc_ci(d6, agelst6, birth_strat = FALSE, severe = TRUE)
  
  shiny_desc_data <- bind_rows(
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="no", measure= "Prevalence", prev),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="yes", measure= "Prevalence", sev.prev),
    data.frame(disease = "Stunting", age_range="3 months",
               birth="yes", severe="no", measure= "Mean LAZ",  haz),
    data.frame(disease = "Stunting", age_range="1 month",
               birth="yes", severe="no", measure= "Mean LAZ",  monthly.haz),
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
  
  assert_that(names(table(shiny_desc_data$method.used)) == calc_method)
  
  shiny_desc_data <- shiny_desc_data %>% subset(., select = -c(se, nmeas.f,  ptest.f))
  
  shiny_desc_data$agecat <- as.factor(shiny_desc_data$agecat)
  
  return(shiny_desc_data)
}

data = d
calc_method = "REML"
output_file_suffix = ""

stunt_outcomes = calc_outcomes(data = d, calc_method = "REML", output_file_suffix = "")
saveRDS(stunt_outcomes, file = paste0(res_dir,"stunting/shiny_desc_data_stunting_objects.RDS"))

stunt_outcomes_fe = calc_outcomes(data = d, calc_method = "FE", output_file_suffix = "_fe")
saveRDS(stunt_outcomes_fe, file = paste0(res_dir,"stunting/shiny_desc_data_stunting_objects_fe.RDS"))