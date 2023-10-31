


rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(washb)
d <- readRDS(co_occurrence_data_path) %>% 
      group_by(studyid, arm, subjid) %>%
      filter(abs(agedays - 6*30.4167) == min(abs(agedays - 6*30.4167)))

covars <- readRDS(paste0(ghapdata_dir, "imic_combined_anthro.rds")) %>%
            select(studyid, subjid, agedays, gagebrth, sex, birthwt, mbmi, mage, dur_ebf) %>%
            mutate(ebf = 1*(dur_ebf >= agedays),
                   lbw=1*(birthwt < 2500),
                   munderweight=1*(mbmi < 18.5)) %>% 
  distinct(studyid, subjid, agedays, .keep_all=T)
head(covars)
      table(covars$ebf)
      
dim(d)

d <- left_join(d, covars, by=c("studyid", "subjid", "agedays"))
table(d$ebf)
table(d$arm)

res <- d %>% group_by(studyid, arm) %>%
            summarise(mean(whz))

#ARM
res_arm_haz <- d %>% group_by(studyid, arm) %>%
                    do(as.data.frame(washb_mean(Y=.$haz, id=.$subjid, print=F)) ) %>%
                    rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=arm) %>%
                    mutate(outcome="LAZ", subgroup=as.character(subgroup), subgroup_var="arm")
res_arm_whz <- d %>% group_by(studyid, arm) %>%
            do(as.data.frame(washb_mean(Y=.$whz, id=.$subjid, print=F)) ) %>%
            rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=arm) %>%
            mutate(outcome="WLZ", subgroup=as.character(subgroup), subgroup_var="arm")
#EBF
res_bf_haz <- d %>% group_by(studyid, ebf) %>% filter(!is.na(ebf)) %>%
            do(as.data.frame(washb_mean(Y=.$haz, id=.$subjid, print=F)) ) %>%
            rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=ebf) %>%
            mutate(outcome="LAZ", subgroup=as.character(subgroup), subgroup_var="EBF")
res_bf_whz <- d %>% group_by(studyid, ebf) %>% filter(!is.na(ebf)) %>%
              do(as.data.frame(washb_mean(Y=.$whz, id=.$subjid, print=F)) ) %>%
              rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=ebf) %>%
              mutate(outcome="WLZ", subgroup=as.character(subgroup), subgroup_var="EBF")
#LBW
res_lbw_haz <- d %>% group_by(studyid, lbw) %>% filter(!is.na(lbw)) %>%
  do(as.data.frame(washb_mean(Y=.$haz, id=.$subjid, print=F)) ) %>%
  rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=lbw) %>%
  mutate(outcome="LAZ", subgroup=as.character(subgroup), subgroup_var="LBW")
res_lbw_whz <- d %>% group_by(studyid, lbw) %>% filter(!is.na(lbw)) %>%
  do(as.data.frame(washb_mean(Y=.$whz, id=.$subjid, print=F)) ) %>%
  rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=lbw) %>%
  mutate(outcome="WLZ", subgroup=as.character(subgroup), subgroup_var="LBW")
#maternal underweight
res_munderweight_haz <- d %>% group_by(studyid, munderweight) %>% filter(!is.na(munderweight)) %>%
do(as.data.frame(washb_mean(Y=.$haz, id=.$subjid, print=F)) ) %>%
  rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=munderweight) %>%
  mutate(outcome="LAZ", subgroup=as.character(subgroup), subgroup_var="maternal underweight")
res_munderweight_whz <- d %>% group_by(studyid, munderweight) %>% filter(!is.na(munderweight)) %>%
  do(as.data.frame(washb_mean(Y=.$whz, id=.$subjid, print=F)) ) %>%
  rename(Y=Mean, lb=`Lower 95%CI`, ub=`Upper 95%CI`, subgroup=munderweight) %>%
  mutate(outcome="WLZ", subgroup=as.character(subgroup), subgroup_var="maternal underweight")

plot_df <- bind_rows(res_arm_haz, res_arm_whz,
                     res_bf_haz, res_bf_whz,
                     res_lbw_haz, res_lbw_whz,
                     res_munderweight_haz, res_munderweight_whz)
table(plot_df$subgroup)

p_arm <- ggplot(plot_df %>% filter(subgroup_var=="arm"),
       aes(x=subgroup, y=Y)) +
  geom_point() + 
  geom_linerange(aes(ymin=lb, ymax=ub)) +
  facet_wrap(studyid~outcome, scales="free", ncol=2) + 
  coord_flip()

p_ebf <- ggplot(plot_df %>% filter(subgroup_var=="EBF"),
                aes(x=subgroup, y=Y)) +
  geom_point() + 
  geom_linerange(aes(ymin=lb, ymax=ub)) +
  facet_wrap(studyid~outcome, scales="free", ncol=2) + 
  coord_flip()

p_lbw <- ggplot(plot_df %>% filter(subgroup_var=="LBW"),
                aes(x=subgroup, y=Y)) +
  geom_point() + 
  geom_linerange(aes(ymin=lb, ymax=ub)) +
  facet_wrap(studyid~outcome, scales="free", ncol=2) + 
  coord_flip()

p_munderweight <- ggplot(plot_df %>% filter(subgroup_var=="maternal underweight"),
                aes(x=subgroup, y=Y)) +
  geom_point() + 
  geom_linerange(aes(ymin=lb, ymax=ub)) +
  facet_wrap(studyid~outcome, scales="free", ncol=2) + 
  coord_flip()

#Growth measures
# Look at maternal age/size

