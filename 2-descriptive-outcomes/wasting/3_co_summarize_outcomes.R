################################################################################
# The ELCIIT and VITAL datasets have different time frames (0-18 months vs 0-6 
# months, respectively). The following code is hard-coded and uses a specific 
# time frame (e.g., 24 months) to calculate cumulative measures. As such, 
# I (Sajia) have calculated these measures for each dataset separately and then 
# combined them. The final file saved is named exactly as the previous one so it 
# should not interfere with future usage.
################################################################################

rm(list = ls())

source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(),"/0-project-functions/0_descriptive_epi_co_functions.R"))

d <- readRDS(co_occurrence_data_path)
waz <- readRDS(underweight_data_path)


#Overall absolute counts
df <- d %>% filter(agedays < 19*30.4167) %>%
  mutate(co = 1*(whz < (-2) & haz < (-2)),
         sevco = 1*(whz < (-3) & haz < (-3))) %>%
  group_by(studyid, country, subjid) %>%
  mutate(co=max(co), sevco=max(sevco)) %>% slice(1)
table(df$co)
prop.table(table(df$co))
table(df$sevco)
prop.table(table(df$sevco))

#get the pooled CI
cuminc.data= df%>%
  group_by(studyid,country) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(co),
    N=sum(length(co))) %>%
  filter(N>=50)

cuminc.data$agecat <- "0-18 months"
co.ci.res=fit.rma(data=cuminc.data,ni="N", xi="ncases",age="0-18 months",
                  measure="PLO",nlab=" measurements", method="REML")
co.ci.res

#get the pooled CI
sevcuminc.data= df%>%
  group_by(studyid,country) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(sevco),
    N=sum(length(sevco))) %>%
  filter(N>=50)
sevcuminc.data$agecat <- "0-18 months"
sev.co.ci.res=fit.rma(data=sevcuminc.data,ni="N", xi="ncases",age="0-18 months",
                      measure="PLO",nlab=" measurements", method="REML")
sev.co.ci.res

#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev.co(d)
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, country, agecat, nmeas,
                                                 prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

prev <- bind_rows(
  data.frame(cohort = "pooled", prev.data$prev.res),
  prev.cohort
)

#cumulative incidence
d <- calc.ci.agecat(d)
ci.data <- summary.co.ci(d)
ci.cohort <-
  ci.data$ci.cohort %>% subset(., select = c(cohort, agecat, yi, ci.lb, ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ci <- bind_rows(
  data.frame(cohort = "pooled", ci.data$ci.res),
  ci.cohort
)

#Severe wasting and stunting prevalence
d <- calc.prev.agecat(d)
sev.prev.data <- summary.prev.co(d, severe = T)

sev.prev.cohort <-
  sev.prev.data$prev.cohort %>%
  subset(., select = c(cohort, country, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

sev.prev <- bind_rows(
  data.frame(cohort = "pooled", sev.prev.data$prev.res),
  sev.prev.cohort
)

#Underweight Prevalence
df <- waz %>% subset(., select = -c(whz)) %>% mutate(whz=waz)
summary(df$whz)

df <- calc.prev.agecat(df)
prev.data <-  summary.prev.whz(df)
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, country, agecat, nmeas,
                                                 prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

underweight.prev <- bind_rows(
  data.frame(cohort = "pooled", prev.data$prev.res),
  prev.cohort
)


#mean waz
waz.data <- summary.waz(df)
waz.cohort <-
  waz.data$waz.cohort %>% subset(., select = c(cohort, country, agecat, nmeas,
                                               meanwaz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwaz, lb = ci.lb, ub = ci.ub)

waz <- bind_rows(
  data.frame(cohort = "pooled", waz.data$waz.res),
  waz.cohort
)


co_desc_data <- bind_rows(
  data.frame(disease = "co-occurrence", age_range="3 months", birth="yes",
             severe="no", measure= "Prevalence", prev),
  data.frame(disease = "co-occurrence", age_range="3 months", birth="yes", 
             severe="no", measure= "Incidence proportion", ci),
  data.frame(disease = "co-occurrence", age_range="3 months", birth="yes", 
             severe="yes", measure= "Prevalence", sev.prev),
  data.frame(disease = "Underweight", age_range="3 months", birth="yes", 
             severe="no", measure= "Mean WAZ",  waz),
  #data.frame(disease = "Underweight", age_range="1 month", birth="yes", 
  #severe="no", measure= "Mean WAZ",  monthly.waz),
  data.frame(disease = "Underweight", age_range="3 months", birth="yes", 
             severe="no", measure= "Prevalence",  underweight.prev)#,
  #data.frame(disease = "Wasting", age_range="3 months", birth="yes", 
  #severe="no", measure= "MUAC Prevalence",  muaz.prev),
  #data.frame(disease = "Wasting", age_range="3 months", birth="yes", 
  #severe="no", measure= "MUAC WHZ Prevalence",  m.whz.prev)
)

co_desc_data <- co_desc_data %>% subset(., select = -c(se, nmeas.f,  ptest.f))

unique(co_desc_data$agecat)
co_desc_data$agecat <- factor(co_desc_data$agecat, levels=unique(co_desc_data$agecat))

co_desc_data <- co_desc_data %>% 
  mutate(pooling = case_when(
    cohort != "pooled" ~ "no pooling",
    cohort == "pooled" & !is.na(country) ~ "country"
  ))



# Save the final dataset
saveRDS(co_desc_data, file = paste0(BV_dir,"/results/co_desc_data.rds"))
