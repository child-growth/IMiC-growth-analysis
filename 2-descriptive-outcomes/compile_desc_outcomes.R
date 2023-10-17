
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

#Primary outcomes
wast <- readRDS(paste0(res_dir,"wasting_desc_data.RDS")) %>% 
  mutate(analysis = "Primary")
stunt = readRDS(paste0(res_dir,"stunting/stunting_desc_data.RDS")) %>% 
  mutate(analysis = "Primary")
co_desc_data <- readRDS(paste0(res_dir,"co_desc_data.rds")) %>%
  mutate(analysis = "Primary", pooling=ifelse(!is.na(country) & pooling!="no pooling", "country",NA))



d <- bind_rows(stunt, wast, co_desc_data#, 
               # stunt_fe, wast_fe, co_fe,
               # stunt_monthly24
               )
table(d$cohort, d$disease)


d$agecat <- factor(d$agecat, levels=unique(d$agecat))


#Convert incidence rate to per 1000 days
d$est[grepl("Incidence rate", d$measure)] <- d$est[grepl("Incidence rate", d$measure)] * 1000
d$lb[grepl("Incidence rate", d$measure)] <- d$lb[grepl("Incidence rate", d$measure)] * 1000
d$ub[grepl("Incidence rate", d$measure)] <- d$ub[grepl("Incidence rate", d$measure)] * 1000


#Clean up measure labels
d$measure <- gsub("Incidence_proportion", "Incidence proportion", d$measure)


#Check for duplicates
dim(d)
# Return all duplicated elements
dups <- d %>% filter(duplicated(.) | duplicated(., fromLast = TRUE))
head(dups)
#drop any duplicated analyses
d<-distinct(d)
dim(d)


d <- droplevels(d)

table(d$disease, d$measure)
table(d$cohort, d$disease)



saveRDS(d, file=paste0(BV_dir,"/results/desc_data_cleaned.rds"))
