################################################################################
# IMiC longitudinal manuscripts
# stunting analysis

# Calculate mean LAZ, prevalence, incidence, 
# and recovery, repeated for fixed effects models 
# and sensitivity analysis 
# with measurements up to 18 months for Elicit and 6 months for Vital.

# Inputs:
#   0-config.R : configuration file
#   0_descriptive_epi_shared_functions.R
#   0_descriptive_epi_stunt_functions.R
#   stunting_data.RData

# Outputs:
#   meanlaz_velocity.RDS
#   meanlaz_velocity_monthly.RDS
#   meanlaz_velocity_fe.RDS
#   quantile_data_stunting.RDS
#   quantile_data_stunting_monthly.RDS
#   quantile_data_stunting_fe.RDS
#   shiny_desc_data_stunting_objects.RDS
#   shiny_desc_data_stunting_objects_monthly.RDS
#   shiny_desc_data_stunting_objects_fe.RDS
################################################################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

# reloading because some overlap with stunting
source(paste0(here::here(), "/0-project-functions/0_descriptive_epi_shared_functions.R"))
source(paste0(here::here(), "/0-project-functions/0_descriptive_epi_stunt_functions.R"))

d <- readRDS(paste0(ghapdata_dir, "stunting_data.rds"))

data = d
calc_method = "REML"
output_file_suffix = ""

calc_outcomes = function(data, calc_method, output_file_suffix) {

dprev <- calc.prev.agecat(d)

d3 <<- calc.ci.agecat(d, range = 3, baseenv() == "yes")
  d6 <<- calc.ci.agecat(d, range = 6, birth == "yes")
  d3_birthstrat <<- calc.ci.agecat(d, range = 3, birth == "no")
  d6_birthstrat <<- calc.ci.agecat(d, range = 6, birth == "no")
}

#------------------------------------------------------------------------------#
#                                    Prevalence                                #
#------------------------------------------------------------------------------#
calc_prevalence = function(severe) {
    prev.data <- summary.prev.haz(dprev) #, severe.stunted = severe, method = calc_method)
    
    prev <- bind_rows(
      data.frame(prev.data $ prev.res)
    )
    
  # mean haz
    
  haz.data <- summary.haz(dprev)
  
  haz <- bind_rows(
    data.frame(haz.data $ haz.res)
  )
  
  # mean haz for growth velocity age categories
  
  d_vel = d %>% 
    mutate(agecat=ifelse(agedays<3*30.4167,"0-3",
                         ifelse(agedays>=3*30.4167 & agedays<6*30.4167,"3-6",
                                ifelse(agedays>=6*30.4167 & agedays<9*30.4167,"6-9",
                                       ifelse(agedays>=9*30.4167 & agedays<12*30.4167,"9-12",
                                              ifelse(agedays>=12*30.4167 & agedays<15*30.4167,"12-15",
                                                     ifelse(agedays>=15*30.4167 & agedays<18*30.4167,"15-18",
                                                            ifelse(agedays>=18*30.4167 & agedays<21*30.4167,"18-21",
                                                                   ifelse(agedays>=21*30.4167& agedays<24*30.4167,"21-24",""))))))))) %>%
    mutate(agecat=factor(agecat,levels=c("0-3","3-6","6-9","9-12",
                                         "12-15","15-18","18-21","21-24"))) 
  
  haz.data.vel <- summary.haz.age.sex(d_vel)
  
  haz.vel <- bind_rows(
    data.frame(haz.data.vel $ haz.res)
  )
  
  saveRDS(haz.vel, file = paste0(res_dir, "stunting/meanlaz_velocity", 
                                  output_file_suffix = ".RDS"))
  
 
  ######################################################################
  # Incidence proportion
  ######################################################################
  #calc_ip = function(datatable, age_list, severe){
  dage <- create_age_categories(d)
   ip.data <- summary.stunt.incprop(dage)
    
    ip <- bind_rows(
      data.frame(ip.data $ ip.res)
    )
    return(ip)
}

  ######################################################################
  # Cumulative incidence
  ######################################################################
  
  calc_ci = function(datatable, age_list, birth_strat, severe) {
    ci.data <- summary.ci(datatable, birthstrat = birth_strat,
                          agelist = age_list, severe.stunted = severe,
                          method = calc_method)
    
    cuminc <- bind_rows(
      data.frame(ci.data$ci.res)
    )
    return(cuminc)
  }

stunt_outcomes = calc_outcomes(data = d, calc_method = "REML", output_file_suffix = "")
saveRDS(stunt_outcomes, file = paste0(res_dir,"stunting/shiny_desc_data_stunting_objects.RDS"))

stunt_outcomes_fe = calc_outcomes(data = d, calc_method = "FE", output_file_suffix = "_fe")
saveRDS(stunt_outcomes_fe, file = paste0(res_dir,"stunting/shiny_desc_data_stunting_objects_fe.RDS"))
