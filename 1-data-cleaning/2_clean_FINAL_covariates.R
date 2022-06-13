
#-------------------------------------------------------------------------------
# Process FINAL dataset into a dataset of covariates to be used in the exposure/
# risk factor analysis. 
#
# Output: Single dataset with one row per child and all baseline covariates
#         Time-varying covariates and anthropometry measures processed in a 
#         seperate script.
#
# Authors: Andrew Mertens (amertens@berkeley.edu)
#          Sajia Darwish (sajdarwish@berkeley.edu)

#NOTE: THIS SCRIPT AND THESE COVARIATES ARE CHANGING FOR IMiC
#-------------------------------------------------------------------------------

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(growthstandards)
#install.packages(naniar)
library(naniar) # For missingness
library(table1)

#------------------------------------------------------------------------------#
#           High level clean up of the joint dataset + subset [DONE]           #                                                #
#------------------------------------------------------------------------------#

d <- readRDS("/data/KI/imic/data/combined_raw_data.rds")


# Fill out empty cells with NA
d <- d %>% mutate_all(na_if,"")

# Get rid of the word visit from each row of the visit column
d $ visit <- gsub(" Visit", "", d $ visit)

# Shorten words in the visit column
d $ visit <- gsub("Months", "m", d $ visit)
d $ visit <- gsub("months", "m", d $ visit)
d $ visit <- gsub("month", "m", d $ visit)
d $ visit <- gsub("Month", "m", d $ visit)
d $ visit <- gsub(" ", "", d $ visit)
d $ visit <- gsub("Enrolment", "base", d $ visit)

## Look at visit and ageday and figure out how to clean the visit variable
visitAgedays <- d %>%
  select(visit, agedays)

# Subset the data into 2 sites. This is because when pivoting to wide, 
# only 200 got included. This is because some specific measures are exclusively
# included in the VITAL dataset and not in ELICIT. So, we change strategy and
# first subset the data and then deal with time-varying variables.

# Also, later I found that variables are measured at different times for ELICIT 
# and VITAL. So, we cleanup the visit variable within each site separately.

vital <- d %>%
  filter(studyid == "VITAL-Lactation")

sum(table(unique(vital $ subjid))) #150

elicit <- d %>%
  filter(studyid == "ELICIT")

sum(table(unique(elicit $ subjid))) #200

#------------------------------------------------------------------------------#
#               Reshape the ELICIT data from long to wide [DONE]               #                                                #
#------------------------------------------------------------------------------#

# Cleanup the visit variable: messy entries: scattered follow ups
elicit $ visit2 <- case_when(elicit $ agedays < 30 ~ "base",
                             elicit $ agedays >= 30 &
                               elicit $ agedays < 61 ~ "1m",
                             elicit $ agedays >= 61 &
                               elicit $ agedays < 91 ~ "Lab1",
                             elicit $ agedays >= 91 &
                               elicit $ agedays < 152 ~ "3m",
                             elicit $ agedays >= 152 &
                               elicit $ agedays < 183 ~ "5m",
                             elicit $ agedays >= 183 &
                               elicit $ agedays < 244 ~ "6m",
                             elicit $ agedays >= 244 &
                               elicit $ agedays < 274 ~ "Lab2",
                             elicit $ agedays >= 274 &
                               elicit $ agedays < 365 ~ "9m",
                             elicit $ agedays >= 365 &
                               elicit $ agedays < 457 ~ "12m",
                             elicit $ agedays >= 457 &
                               elicit $ agedays < 548 ~ "15m",
                             elicit $ agedays >= 548 ~ "18m",
                             TRUE ~ elicit $ visit)
# Test
visitAgedays <- elicit %>%
  select(visit, visit2, agedays) # It worked!

# Check
table(elicit $ visit2)

# Clean up the dataset: delete unnecessary variables
delete <- c("visitimpcm", "visitnum", "visit", "ageimpcm", 
            "ageimpfl", "sexn", "delivrdt", "bmid", "armcd", "exbfdef")

elicit <- elicit[, !names(d) %in% delete]

## Make id a values vectors
id = c("country", "studyid", "siteid", "subjid", "subjido", "studytyp")

valuesBaselineE = c("arm", "sex", "brthyr", "brthweek", "mage", "parity", 
                    "nlchild", "nperson", "nrooms", "meducyrs", "h2osrcp", 
                    "cookplac", "inctot", "inctotu", "epochn", "epoch", 
                    "mhtcm", "mwtkg", "mbmi",  "pregout", "dlvloc", "dvseason",
                    "wtkg", "lencm", "bmi", "hcircm", "waz", "haz", "whz", 
                    "baz", "agedays", "feeding", "dur_bf", "dur_ebf")

valuesOneFiveE = c("bmcol_fl", "agedays")

valuesIntervalsOf3E = c("lencm", "bmi", "hcircm", "waz", "haz", "whz", "baz", 
                        "agedays")

valuesIntervalsOf6E = c("visit_r_fl", "dur_r", "bfedfl_r", "exbfed_r", 
                       "exbfdu_r", "fever_r", "cough_r", "diarr_r", "agedays")

valuesEndlineE <- c("mhgb", "muaccm", "muaz", "agedays")

valuesOverallE <- c("bfdu_r", "anti_r", "agedays")

# Subset data for different times
baselineE <- elicit %>%
  filter(visit2 == "base")

oneFiveE <- elicit %>%
  filter(visit2 == "1m" | visit2 == "5m")

intrvlsOf3E <- elicit %>%
  filter(visit2 == "3m"| visit2 == "6m" | visit2 == "9m" | visit2 == "12m" |
           visit2 == "15m" | visit2 == "18m")

intrvlsOf6E <- elicit %>%
  filter(visit2 == "0to6m" | visit2 == "6to18m")

overallE <- elicit %>%
  filter(visit2 == "0to18m")

endlineE <- elicit %>%
  filter(visit2 == "18m")

# Reshape data to wide
wideBaselineE <- baselineE %>% 
  pivot_wider(id_cols = id,
              names_from = visit2,
              values_from = all_of(valuesBaselineE))

# Make a function to pivot all other data to wide
other <- function (data1, data2) {
  data1 %>% 
    pivot_wider(id_cols = id,
                names_from = visit2,
                values_from = data2) %>%
    select(-id)
}

# Use the function
wideOneFiveE <- other(oneFiveE, valuesOneFiveE)
wideIntrvlsOf3E <- other(intrvlsOf3E, valuesIntervalsOf3E)
wideIntrvlsOf6E <- other(intrvlsOf6E, valuesIntervalsOf6E)
wideEndlineE <- other(endlineE, valuesEndlineE)
wideOverallE <- other(overallE, valuesOverallE)

# Combine all these 6 datasets
combinedWideElicit <- cbind(wideBaselineE, wideOneFiveE, wideIntrvlsOf3E, 
                            wideIntrvlsOf6E, wideEndlineE, wideOverallE)

# Remove columns with 100% NA in this dataset
combinedWideElicit <- 
  combinedWideElicit[, -which(colMeans(is.na(combinedWideElicit)) == 1)]

# Back to the full ELICIT dataset

# Delete all the time-variant variables already taken care of.
dStatic <- elicit %>%
  select(-c(valuesBaselineE, valuesOneFiveE, valuesIntervalsOf3E, 
            valuesIntervalsOf6E, valuesEndlineE, valuesOverallE))

# Look at missingness in dStatic
gg_miss_var(dStatic[, 1:ncol(dStatic)], show_pct = T)

# Now we can see that the remaining 50 variables aside from id variables
# were not measured in the ELICIT study and are all at 100% NA. As such, we
# disregard them, which means that the combinedWideElicit dataset is our final
# dataset for ELICIT.

# Save the dataset
#write.csv(combinedWideElicit, file = "wideElicit.csv")

# Anonymized data
combinedWideElicit <- combinedWideElicit %>%
  select(-id)

# Save the anonymized dataset
#write.csv(combinedWideElicit, file = "wideElicitAnonymized.csv")

#------------------------------------------------------------------------------#
#                       Make Summary Table - ELICIT [DONE]                     #                                                #
#------------------------------------------------------------------------------#
table1E <- table1(~ . | arm_base, data = combinedWideElicit)

# Save the table as a csv file
#write.csv(table1E, file = "table1Elicit.csv")

#------------------------------------------------------------------------------#
#               Reshape the VITAL data from long to wide [DONE]                #                                                #
#------------------------------------------------------------------------------#

## Get a list of names separated with a comma
#cat(paste(shQuote(names(d), type="cmd"), collapse=", "))

# Cleanup the visit variable: messy entries: scattered follow ups
table(vital $ visit)

# Get rid of the word follow-up in the visit variable
vital $ visit <- gsub("Followup", "", vital $ visit)

# Look at the specific days used to mark time frames
visitAgedays <- vital %>%
  select(visit, agedays)

# Here, we will stick to the monthly measurements only and disregard the daily
# follow-ups. We can include those later once we have a plan for them.

vital $ visit2 <- case_when(vital $ visit == "Baseline" |
                              vital $ visit == "Baseline(Birthrecall)" ~ "base",
                            vital $ visit == "m1" ~ "m1",
                            vital $ visit == "m2" ~ "m2",
                            vital $ visit == "m3" ~ "m3",
                            vital $ visit == "m4" ~ "m4",
                            vital $ visit == "m5" ~ "m5",
                            vital $ visit == "m6" ~ "m6")
# Test
visitAgedays <- vital %>%
  select(subjid, visit, visit2, agedays) # It worked!

# Check
table(vital $ visit2, vital $ agedays)

# Clean up the dataset: delete unnecessary variables
delete <- c("visitimpcm", "visitnum", "visit", "ageimpcm", 
            "ageimpfl", "sexn", "delivrdt", "bmid", "armcd", "exbfdef")

vital <- vital[, !names(vital) %in% delete]

# Look at missing values in this dataset
gg_miss_var(vital[, 1:45], show_pct = T)
gg_miss_var(vital[, 46:95], show_pct = T)

# Save variables not measured in this study
notMeasured <- vital[, which(colMeans(is.na(vital)) == 1)]
#names(notMeasured)

# Remove variables at 100% NA since they were not measured in this study
vital <- vital[, -which(colMeans(is.na(vital)) == 1)]

# Recheck missingness
gg_miss_var(vital[, 1:45], show_pct = T)
gg_miss_var(vital[, 46:87], show_pct = T)

# Subset data for different times
subset <- function (data) {
  vital %>%
    filter(visit2 == data)
}

baselineV <- subset("base")
m1 <- subset("m1")
m2 <- subset("m2")
m2 <- subset("m3")
m2 <- subset("m4")
m2 <- subset("m5")
m2 <- subset("m6")

# Here we can see that in the monthly datasets, there are multiple rows per 
# subject, which means that we need to fix those before we pivot to wide. 
# Otherwise it will cause complications.

table(m1 $ subjid) # Here we can see that 554 has 3, 2435 has 2, & 3170 has 2.

# Group by subjid and visit2 and fill out all rows with existing info within
# each sunject and visit
test <- vital %>%
  group_by(subjid, visit2) %>%
  fill(c("mwtkg", "mbmi", "wtkg", "lencm", "bmi", "muaccm", "waz", "haz", "whz",
         "baz", "visit_r_fl", "dur_r", "bmcol_fl", "fever_r", "cough_r",
         "diarr_r", "anti_r")) %>%
  fill(c("mwtkg", "mbmi", "wtkg", "lencm", "bmi", "muaccm", "waz", "haz", "whz",
         "baz", "visit_r_fl", "dur_r", "bmcol_fl", "fever_r", "cough_r",
         "diarr_r", "anti_r"), .direction = "down")

test <- test %>%
  group_by(subjid, visit2) %>%
  fill(c("mwtkg", "mbmi", "wtkg", "lencm", "bmi", "muaccm", "waz", "haz", "whz",
         "baz", "visit_r_fl", "dur_r", "bmcol_fl", "fever_r", "cough_r",
         "diarr_r", "anti_r")) %>%
  fill(c("mwtkg", "mbmi", "wtkg", "lencm", "bmi", "muaccm", "waz", "haz", "whz",
         "baz", "visit_r_fl", "dur_r", "bmcol_fl", "fever_r", "cough_r", 
         "diarr_r", "anti_r"), .direction = "up")

# Pick one row per subject and per visit2
test <- test %>%
  group_by(subjid, visit2) %>%
  slice(n = 1)

# Subset data again to check
subset <- function (data) {
  test %>%
    filter(visit2 == data)
}

baselineV <- subset("base")
m1 <- subset("m1")
m2 <- subset("m2")
m2 <- subset("m3")
m2 <- subset("m4")
m2 <- subset("m5")
m2 <- subset("m6")

# This is good for baseline and m1: we have exactly 150 observations but it is
# not good for other months because they are all less than 150, especially m2.
# This needs to be addressed in the meeting.

## Make values vectors for pivoting
#cat(paste(shQuote(names(vital), type="cmd"), collapse=", "))

valuesBase <- c("arm", "sex", "brthyr", "brthweek", "mage", "parity", "nlchild", 
            "nperson", "nrooms", "meducyrs", "h2osrcp", "cookplac", "agedays", 
            "epochn", "epoch", "mhtcm", "mwtkg", "mbmi", "mhgb", "pregout", 
            "dlvloc", "wtkg", "lencm", "bmi", "muaccm", "waz", "haz", "whz",
            "baz", "muaz", "feeding", "dur_bf", "dur_ebf", "visit_r_fl", 
            "dur_r", "bmcol_fl", "fever_r", "cough_r", "diarr_r", "anti_r", 
            "citytown", "gagebrth", "gagecm", "birthwt", "birthlen", "birthord",
            "gravida", "nlivbrth", "floor", "gagedays", "postbmi", "mmuaccm", 
            "delivery", "hgb", "bfinittm", "cmfdint", "bfmode", "bfedfl",
            "exbfedfl", "formlkfl", "sldfedfl", "anmlk_r", "formlk_r", 
            "sldfed_r", "fever", "cough", "diarr", "vomit", "vomit_r",
            "physican", "hosp", "antibiot", "anti_oral", "anti_inj",
            "anti_or_r", "anti_in_r", "mcrp", "mferritin", "mstrf", "magp")

valuesMonthly = c("agedays", "mhtcm", "mwtkg", "mbmi", "mhgb", "wtkg", "lencm",
             "bmi", "muaccm", "waz", "haz", "whz","baz", "muaz", "feeding",
             "dur_bf", "dur_ebf", "visit_r_fl", "dur_r", "bmcol_fl", "fever_r",
             "cough_r", "diarr_r", "anti_r", 
             "gagedays", "postbmi", "mmuaccm", "hgb", "cmfdint", "bfmode",
             "bfedfl", "exbfedfl", "formlkfl", "sldfedfl", "anmlk_r",
             "formlk_r", "sldfed_r", "fever", "cough", "diarr", "vomit",
             "vomit_r", "physican", "hosp", "antibiot", "anti_oral", "anti_inj",
             "anti_or_r", "anti_in_r", "mcrp", "mferritin", "mstrf", "magp")

# Reshape baseline data to wide
wideBaselineV <- baselineV %>% 
  pivot_wider(id_cols = id,
              names_from = visit2,
              values_from = all_of(valuesBase))

# Make a function to pivot data to wide
monthly <- function (data) {
  data %>%
    pivot_wider(id_cols = id,
                names_from = visit2,
                values_from = all_of(valuesMonthly)) %>%
    select(-id)
}

# Pivot all monthly data to wide
widem1 <- monthly(m1)
widem2 <- monthly(m2)
widem3 <- monthly(m3)
widem4 <- monthly(m4)
widem5 <- monthly(m5)
widem6 <- monthly(m6)

# Combine all these 6 datasets
combinedWideV <- merge(wideBaselineV, widem1, by = "subjid", all = TRUE)
combinedWideV <- merge(combinedWideV, widem2, by = "subjid", all = TRUE)
combinedWideV <- merge(combinedWideV, widem3, by = "subjid", all = TRUE)
combinedWideV <- merge(combinedWideV, widem4, by = "subjid", all = TRUE)
combinedWideV <- merge(combinedWideV, widem5, by = "subjid", all = TRUE)
combinedWideV <- merge(combinedWideV, widem6, by = "subjid", all = TRUE)

# Remove columns with 100% NA in this dataset
combinedWideV <- combinedWideV[, -which(colMeans(is.na(combinedWideV)) == 1)]

# Save the dataset
#write.csv(combinedWideV, file = "wideVital.csv")

# Anonymized data
combinedWideVital <- combinedWideV %>%
  select(-id)

# Save the anonymized dataset
#write.csv(combinedWideVital, file = "wideVitalAnonymized.csv")

#------------------------------------------------------------------------------#
#                       Make Summary Table - VITAL [DONE]                      #                                                #
#------------------------------------------------------------------------------#
table1V <- table1(~ . | arm_base, data = combinedWideVital)

# Save the table as a csv file
#write.csv(table1V, file = "table1Vital.csv")

#------------------------------------------------------------------------------#
#                       Plot Outcome Variables: ELICIT [DONE]                  #                                                #
#------------------------------------------------------------------------------#

# Filter out unwanted time points
elicit2 <- elicit %>%
  filter(visit2 == "base" | visit2 == "3m" | visit2 == "6m" | visit2 == "9m" |
           visit2 == "12m" | visit2 == "15m" | visit2 == "18m")

# Re order levels
elicit2 $ visit2 <- factor(elicit2 $ visit2, 
                           level = c("base", "1m", "3m", "5m", "6m", "9m",
                                     "12m", "15m", "18m"))

# Make a function for showing number of observations in plot
stat_box_data <- function (y) {
  return( 
    data.frame(
      y = 0.5 + 1.1 * max(y),  #may need to modify this depending on your data
      label = paste('n=',length(y), '\n')
                    #'mean =', round(mean(y), 1), '\n')
      )
    )
}

# Make plots
elicit2 %>%
  group_by("subjid") %>%
  ggplot(aes(x = visit2, y = baz)) +
  geom_boxplot() +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  geom_jitter(width = 0.05, alpha = 0.2) +
  xlab("Duration") +
  ylab("BMI-for-Age Z-Score") +
  facet_wrap(~ arm) +
  theme(strip.text.x = element_text(size = 10)) #+
  #scale_color_brewer(palette = "Spectral")

#------------------------------------------------------------------------------#
#                       Plot Outcome Variables: VITAL [DONE]                   #                                                #
#------------------------------------------------------------------------------#

# Re order levels: visit2
vital $ visit2 <- factor(vital $ visit2, 
                           level = c("base", "m1", "m2", "m3", "m4", "m5",
                                     "m6"))
# Recode the arm variable
vital $ arm <- case_when(vital $ arm == "Nutrient supplement+Ex.BreastFeed"
                         ~ "Nutrient+Ex",
                         vital $ arm == "Nutrient supplement+Ex.BreastFeed+AZT"
                         ~ "Nutrient+Ex+AZT",
                         vital $ arm == "Control" ~ "Control")

# Make plots
vital %>%
  filter(!is.na(visit2)) %>%
  group_by("subjid") %>%
  ggplot(aes(x = visit2, y = whz)) +
  geom_boxplot() +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  geom_jitter(width = 0.05, alpha = 0.2) +
  xlab("Duration") +
  ylab("Weight-for-Height Z-Score") +
  #facet_wrap(~ arm) +
  theme(strip.text.x = element_text(size = 10)) #+
#scale_color_brewer(palette = "Spectral")





#--------------------------------------------------------
#Calculate stunting and wasting at enrollment and keep one observation per child
#Also check if children without a recorded birthweight or birthlength have WAZ or HAZ in the first year of life
#--------------------------------------------------------
d <- d %>% group_by(studyid, country, subjid) %>% 
  arrange(studyid, subjid, agedays) %>% 
  mutate(enstunt= as.numeric(ifelse(length(first(haz[complete.cases(haz)]))==0,NA,first(haz[complete.cases(haz)])) < -2),
         enwast= as.numeric(ifelse(length(first(whz[complete.cases(whz)]))==0,NA,first(whz[complete.cases(whz)])) < -2),
         birthLAZ= haz,
         birthWAZ= waz) %>%
  #keep one observation per child
  slice(1) 

table(is.na(d$birthwt), d$agedays > 7)


#keep where anthro is measured on first 7 days, but birth anthro is not recorded
d$birthLAZ[d$agedays>7] <- NA 
d$birthWAZ[d$agedays>7] <- NA
d$birthmeas_age <- NA
d$birthmeas_age[d$agedays <= 7] <- d$agedays[d$agedays <= 7]

#Drop anthropometry measures (seperate long-form dataset used to calculate anthropometry outcomes)
d <- d %>% subset(., select=-c(agedays, haz, waz, whz)) 

table(paste0(d$studyid,"-",d$country), d$enwast)
table(paste0(d$studyid,"-",d$country), d$enstunt)



# 
# #--------------------------------------------------------
# # Merge in household assets-based wealth index
# # (Calculated from first principal component of a PCA analysis)
# # of asset indicators
# #--------------------------------------------------------
# 
# #convert subjid to character for the merge with covariate dataset
# d$subjid <- as.character(d$subjid)
# 
# #load in pca results
# pca <- readRDS(paste0(deriveddata_dir,"allGHAPstudies-HHwealth.rds"))
# 
# #Strip grant identifiers from study id's
# pca$studyid<- gsub("^k.*?-" , "", pca$studyid)
# 
# table(pca$studyid, pca$hhwealth_quart)
# 
# #Merge into main dataframe
# pca <- as.data.frame(pca)
# pca$subjid <-as.character(pca$subjid)
# 
# dim(pca)
# dim(d)
# d <- left_join(d, pca, by=c("studyid", "country", "subjid"))
# dim(d)
# #Note, only the COHORTS study has SES categories from a PCA, but no/incomplete indicators to calculate PCA from
# #Clean and merge that data here:
# #merge in ses variable for COHORTS for all countries except INDIA. The other countries have wealth based on 
# #an asset-based PCA index, but India is based on father's occupation.
# d$hhwealth_quart <- as.character(d$hhwealth_quart)
# chtses<- d$ses[is.na(d$hhwealth_quart) & d$studyid=="COHORTS" & d$country!="INDIA"]
# chtses[chtses==""] <- NA
# chtses[chtses=="Low"] <- "Wealth Q1"
# chtses[chtses=="Lower-mi"] <- "Wealth Q2"
# chtses[chtses=="Middle"] <- "Wealth Q3"
# chtses[chtses=="Upper-mi"] <- "Wealth Q4"
# chtses[chtses=="Upper"] <- "Wealth Q4"
# 
# d$hhwealth_quart[is.na(d$hhwealth_quart) & d$studyid=="COHORTS" & d$country!="INDIA"] <-chtses
# d$hhwealth_quart <- factor(d$hhwealth_quart)
# 
# #Check and make sure all merged correctly
# df <- d %>% filter(!is.na(hhwealth_quart)) %>% group_by(studyid, subjid) %>% slice(1)
# pca_unique <- pca %>% filter(!is.na(hhwealth_quart)) %>% group_by(studyid, subjid) %>% slice(1)
# table(pca_unique$studyid, pca_unique$hhwealth_quart)
# table(df$studyid, df$hhwealth_quart)
# 
# #remove space for longbow
# d$hhwealth_quart <- as.character(d$hhwealth_quart)
# d$hhwealth_quart <- gsub(" ", "", d$hhwealth_quart)
# d$hhwealth_quart <- factor(d$hhwealth_quart, levels=c("WealthQ4","WealthQ3","WealthQ2","WealthQ1"))
# table(d$hhwealth_quart)



#--------------------------------------------------------------------------
# birth characteristics
#--------------------------------------------------------------------------



#Use agedays-1 as function codes birth age=0
d$birthlen2 <- who_zscore2value(d$birthmeas_age-1, d$birthLAZ, y_var = "lenhtcm" , x_var = "agedays", sex = d$sex)
d$birthwt2 <- who_zscore2wtkg(d$birthmeas_age-1, d$birthWAZ, sex = d$sex) * 1000
d$birthlen2[!is.finite(d$birthlen2)] <- NA
d$birthwt2[!is.finite(d$birthwt2)] <- NA



#Check if children without a recorded birthweight or birthlength have WAZ or HAZ in the first 7 days of life
#and add into birthweight variable

summary(d$birthlen)
summary(d$birthlen2)
summary(d$birthwt)
summary(d$birthwt2)

table(is.na(d$birthlen), is.na(d$birthlen2))
table(is.na(d$birthwt), is.na(d$birthwt2))

d$birthlen[is.na(d$birthlen)] <- d$birthlen2[is.na(d$birthlen)]
d$birthwt[is.na(d$birthwt)] <- d$birthwt2[is.na(d$birthwt)]


table(d$studyid, is.na(d$birthlen))
table(d$studyid, is.na(d$birthwt))


# #--------------------------------------------------------------------------
# # parental characteristics
# #--------------------------------------------------------------------------
# 
# #Calculate bmi from height and weight, and vice versa, for when only 2 of 3 are measured
# #bmi
# flag <- is.na(d$mbmi) & !is.na(d$mhtcm) & !is.na(d$mwtkg)
# d$mbmi[flag] <- d$mwtkg[flag] / (d$mhtcm[flag] / 100)^2
# 
# #weight
# flag <- is.na(d$mwtkg) & !is.na(d$mhtcm) & !is.na(d$mbmi)
# d$mwtkg[flag] <- d$mbmi[flag] * (d$mhtcm[flag] / 100)^2
# 
# #height
# flag <- is.na(d$mhtcm) & !is.na(d$mwtkg) & !is.na(d$mbmi)
# d$mhtcm[flag] <- sqrt(d$mwtkg[flag] / d$mbmi[flag]) * 100
# 
# summary(d$mbmi)
# summary(d$mwtkg)
# summary(d$mhtcm)






#--------------------------------------------------------
# create id variable for unit of independent observation
# (At level of child for most studies, but some trials are cluster-randomized)
#--------------------------------------------------------

d$id <- NA
table(is.na(d$id))


#--------------------------------------------------------
# Classify intervention arms (used in initially-planned analysis)
# Of intervention effects that was scrapped, but also as adjustment variables
#--------------------------------------------------------


#--------------------------------------------------------
# Drop risk factors without enough studies or unneeded/temporary variables 
#--------------------------------------------------------

colnames(d)
#d <- subset(d, select = c(studyid, subjid, sex... ))


#--------------------------------------------------------
# Convert continious variables to quartiled categorical 
# variables for use as primary exposures
#--------------------------------------------------------



#quantiling functions
quantile_rf <- function(data, A, labs=NULL, Acuts=NULL, units=NULL){
  A<-as.numeric(A)
  if(sum(is.na(A))!=length(A)){
    if(is.null(Acuts)){
      Acuts=c(0, as.numeric(quantile(A, probs = c(.25,.5,.75), na.rm=T)), max(A, na.rm=T))
    }
    
    if(length(Acuts)==3){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0(">=",round(Acuts[2],2))) 
    }    
    if(length(Acuts)==4){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0(">=",round(Acuts[3],2))) 
    }
    if(length(Acuts)==5){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0("[",round(Acuts[3],2),"-",round(Acuts[4],2),")"), 
                paste0(">=",round(Acuts[4],2))) 
    }
    if(length(Acuts)==6){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0("[",round(Acuts[3],2),"-",round(Acuts[4],2),")"),
                paste0("[",round(Acuts[4],2),"-",round(Acuts[5],2),")"), 
                paste0(">=",round(Acuts[5],2))) 
    }    
    
    
    if(!is.null(labs)){
      Alevels=labs
    }
    if(!is.null(units)){
      Alevels=paste0(Alevels, " ", units)
    }
    
    if(length(unique(Acuts))==length((Acuts))){
      A <- cut(A, include.lowest = T, right = FALSE, breaks=Acuts,labels=Alevels)
    }else{
      A <- cut(A, include.lowest = T, right = FALSE, breaks=4,labels=c("Q1","Q2","Q3","Q4","Q5")[1:(length(Acuts)-1)])
    }
    A <- factor(A)
    
    printdf <- data.frame(id=paste0(data$studyid," ", data$country), A)
    printdf <- printdf %>% filter(!is.na(A))
    printdf <- droplevels(printdf) 
    print(table(printdf$id, printdf$A))
    
    print(table( printdf$A))
    return(A)
  }
}


#A-priori categorical levels
#gestational age at birth
#<37 weeks = preterm
#37-38 weeks = early term
#39-40 weeks = full term (baseline)
#>=41 weeks = late/post term

#maternal BMI
#<18.5 = underweight
#>=18.5 and <25 = normal weight (baseline)
#>=25 and <30 = overweight
#>=30 = obese

colnames(d)
# #Save continious variables as seperate variables to use as adjustment covariates
# d$W_gagebrth <- d$gagebrth
# d$W_birthwt <- d$birthwt
# d$W_birthlen <- d$birthlen
# d$W_mage <- d$mage
# d$W_mhtcm <- d$mhtcm
# d$W_mwtkg <- d$mwtkg
# d$W_mbmi <- d$mbmi
# d$W_fage <- d$fage
# d$W_fhtcm <- d$fhtcm
# d$W_meducyrs <- d$meducyrs
# d$W_feducyrs <- d$feducyrs
# 
# d$W_nrooms <- d$nrooms
# d$W_nhh <- d$nhh
# d$W_nchldlt5 <- d$nchldlt5
# d$W_parity <- d$parity




# #Overall a-priori quantiles
# d$gagebrth <- quantile_rf(d, d$W_gagebrth, Acuts=c(0,260,274,max(d$W_gagebrth, na.rm=T)), labs=c("Preterm", "Early term", "Full or late term"))
# d$birthwt <- quantile_rf(d, d$W_birthwt, Acuts=c(0,2500,max(d$W_birthwt, na.rm=T)), labs=c("Low birthweight", "Normal or high birthweight"))
# d$birthlen <- quantile_rf(d, d$W_birthlen, Acuts=c(0,48, 50, max(d$W_birthlen, na.rm=T)), units="cm")
# 
# # Fix Ages in Burkino Faso Zinc so they are categorized correctly by the function, which are based on these categories:
# #Categories: <20 years old; 20-29 years old; 30-39 years old; 40-49 years old; 50+ years old
# d$W_mage[d$studyid=="Burkina Faso Zn" & d$W_mage==20] <- 18
# d$mage <- quantile_rf(d, d$W_mage, Acuts=c(0,20,30,max(d$W_mage, na.rm=T)))
# 
# d$mhtcm <- quantile_rf(d, d$W_mhtcm, Acuts=c(0,151,155,max(d$W_mhtcm, na.rm=T)), units="cm")
# d$mwtkg <- quantile_rf(d, d$W_mwtkg, Acuts=c(0,52,58,max(d$W_mwtkg, na.rm=T)), units="kg")
# d$mbmi <- quantile_rf(d, d$W_mbmi, Acuts=c(0,18.5,max(d$W_mbmi, na.rm=T)), labs=c("Underweight", "Normal weight"))
# #d$fage <- quantile_rf(d, d$W_fage, Acuts=c(0,32,38,max(d$W_fage, na.rm=T)))
# d$fhtcm <- quantile_rf(d, d$W_fhtcm, Acuts=c(0,162,167,max(d$W_fhtcm, na.rm=T)), units="cm")
# 
# #d$fhtcm_rf <- quantile_rf(d, d$W_fhtcm, Acuts=c(0,165,175,max(d$W_fhtcm, na.rm=T)), units="cm")
# d$fage <- quantile_rf(d, d$W_fage, Acuts=c(0,30,35,max(d$W_fage, na.rm=T)))


#Make education categorizing function that handles the irregular distribution across studies.
# (As years of education is more country-specific, categorize within studies)
# Function groups subjects into even categories from irregular distributions of education 
# (just tertiling leads to sparsity/R errors)

# quantile_rf_edu <- function(d, Avar="meducyrs", to.character=F){
#   dfull <-d
#   
#   print(d$studyid[1])
#   print(d$country[1])
#   
#   A0 <- NULL
#   d <- data.frame(id=1:nrow(dfull), A=as.data.frame(dfull[,Avar])[,1])
#   
#   if(sum(is.na(d$A))!=nrow(d)){
#     
#     Acuts=c(0, as.numeric(quantile(d$A, probs = c(1/3, 2/3), na.rm=T)), max(d$A, na.rm=T))
#     if(length(Acuts)==length(unique(Acuts))){
#       
#       Alevels=c("Low","Medium","High")
#       A1 <- cut(d$A, include.lowest = T, right = T, breaks=Acuts,labels=Alevels)
#       A2 <- cut(d$A, include.lowest = T, right = F, breaks=Acuts,labels=Alevels)
#       rght=F
#       if(min(table(A1)) >= min(table(A2))) rght=T
#       A <- cut(d$A, include.lowest = T, right = rght, breaks=Acuts,labels=Alevels)
#       
#     }else{
#       if(Acuts[2]==Acuts[3] & Acuts[2]!=0){
#         A<-rep(NA, nrow(d))
#         A[d$A < Acuts[2] & !is.na(d$A)] <- "Low"
#         A[d$A == Acuts[2] & !is.na(d$A)] <- "Medium"
#         A[d$A > Acuts[2] & !is.na(d$A)] <- "High"
#         A <- factor(A, levels = c("Low","Medium","High"))
#       }else{
#         
#         if(sum(d$A==0, na.rm=T)>0){
#           A0 <- d[d$A==0,]
#           A0 <- A0[!is.na(A0$A),]
#           A0$A <- "Low"
#         }
#         
#         A <- d[d$A!=0 | is.na(d$A),]
#         Acuts=c(0, as.numeric(quantile(A$A, probs = 0.5, na.rm=T)), max(A$A, na.rm=T))
#         Alevels=c("Medium","High")    
#         A$A <- cut(A$A, include.lowest = T, right = T, breaks=Acuts,labels=Alevels)
#         if(!is.null(A0)){
#           df<-rbind(A,A0)
#         }else{
#           df<-A
#         }
#         
#         df <- df %>% arrange(id)
#         A <- factor(df$A, levels = c("Low","Medium","High"))
#       }
#     }
#     dfull[,Avar] <- A
#     
#   }
#   dfull <-as.data.frame(dfull)
#   if(to.character){
#     dfull[,Avar] <- as.character(dfull[,Avar])
#   }
#   print(class(dfull[,Avar]))
#   return(dfull)
# }
# 
# d$meducyrs <- as.numeric(as.character(d$meducyrs))
# d <- d %>% group_by(studyid, country) %>%
#   do(quantile_rf_edu(., Avar="meducyrs", to.character=T))
# #d <- d %>% arrange(feducyrs, studyid, country, subjid)
# d <- d %>% group_by(studyid, country) %>%
#   do(quantile_rf_edu(., Avar="feducyrs", to.character=T))
# d$meducyrs <- factor(d$meducyrs, levels = c("Low","Medium","High"))
# d$feducyrs <- factor(d$feducyrs, levels = c("Low","Medium","High"))
# 
# table(d$meducyrs)
# table(d$feducyrs)
# 
# table(paste0(d$studyid," ", d$country), d$meducyrs)
# table(paste0(d$studyid," ", d$country), d$feducyrs)




# #Categorize nrooms, nhh, nchild5
# table(d$nrooms)
# table(paste0(d$studyid," ", d$country), d$nrooms)
# nroom<-NA
# nroom[d$nrooms<2] <- "1"
# nroom[d$nrooms==2] <- "2"
# nroom[d$nrooms==3] <- "3"
# nroom[d$nrooms>3] <- "4+"
# d$nrooms <- as.factor(nroom)
# table(d$nrooms)
# table(paste0(d$studyid," ", d$country), d$nrooms)
# 
# 
# table(d$nhh)  
# table(paste0(d$studyid," ", d$country), d$nhh)
# 
# nhh<-NA
# nhh[d$nhh<4] <- "3 or less"
# nhh[d$nhh>3 & d$nhh<6] <- "4-5"
# nhh[d$nhh>5 & d$nhh<8] <- "6-7"
# nhh[d$nhh>7] <- "8+"
# d$nhh <- as.factor(nhh)
# table(d$nhh)
# table(paste0(d$studyid," ", d$country), d$nhh)
# 
# 
# table(d$nchldlt5)
# table(paste0(d$studyid," ", d$country), d$nchldlt5)
# 
# nchild5<-NA
# nchild5[d$nchldlt5==1] <- "1"
# nchild5[d$nchldlt5>=2] <- "2+"
# d$nchldlt5 <- as.factor(nchild5)
# table(d$nchldlt5)
# table(paste0(d$studyid," ", d$country), d$nchldlt5)
# 
# d$nchldlt5 <- relevel(d$nchldlt5, ref="1")
# 
# table(d$parity)
# table(paste0(d$studyid," ", d$country), d$parity)
# 
# parity<-NA
# parity[d$parity==1] <- "1"
# parity[d$parity==2] <- "2"
# parity[d$parity>2] <- "3+"
# d$parity <- as.factor(parity)
# table(d$parity)
# table(paste0(d$studyid," ", d$country), d$parity)
# 
# 
# 
# d$parity <- relevel(d$parity, ref="1")




# #---------------------------------------
# # Set reference levels
# #---------------------------------------
# 
# #birthweight
# # Low birth weight: < 2500
# # healthy birth weight 2500-4200
# 
# d$birthwt <- relevel(d$birthwt, ref="Normal or high birthweight")
# 
# 
# #birth length: 
# #No WHO categories:
# #Based on quantiles
# 
# d$birthlen <- relevel(d$birthlen, ref=">=50 cm")
# 
# #wealth index: 
# #wealthiest quartile - Q4 is baseline
# table(paste0(d$studyid," ", d$country), d$hhwealth_quart)
# d$hhwealth_quart <- relevel(d$hhwealth_quart, ref="WealthQ4")
# 
# # children < 5 in HH
# #not sure how this could be zero - can you double check this? 
# #baseline should be smallest number
# 
# d$nchldlt5 <- relevel(d$nchldlt5, ref="1")
# 
# #gestational age at birth
# #<37 weeks = preterm
# #37-38 weeks = early term
# #39-40 weeks = full term (baseline)
# #>=41 weeks = late/post term
# 
# d$gagebrth <- relevel(d$gagebrth, ref="Full or late term")
# 
# #maternal BMI (is this measured when pregnant or not? if pregnant, then we may need to change these categories)
# #<18.5 = underweight
# #>=18.5 and <25 = normal weight (baseline)
# #>=25 and <30 = overweight
# #>=30 = obese
# 
# d$mbmi <- relevel(d$mbmi, ref="Normal weight")
# 
# #maternal height (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3095774/)
# #less than 145 cm
# #145-149.9 cm
# #150-154.9 cm
# #155-159.9 cm
# #160.0 cm or greater. (baseline)
# 
# d$mhtcm <- relevel(d$mhtcm, ref=">=155 cm")
# 
# #maternal weight?
# d$mwtkg <- relevel(d$mwtkg, ref=">=58 kg")
# 
# #mother's/father's education
# #lowest education level = baseline
# d$meducyrs <- relevel(factor(d$meducyrs), ref="Low")
# d$feducyrs <- relevel(factor(d$feducyrs), ref="Low")
# 
# #mother's age
# #middle = baseline
# d$mage <- relevel(d$mage, ref="[20-30)")
# 
# #father age
# #oldest = baseline
# #d$fage <- relevel(d$fage, ref=">=38")
# d$fage <- relevel(d$fage, ref=">=35")
# 
# #father height
# d$fhtcm <- relevel(d$fhtcm, ref=">=167 cm")
# 
# #parental education
# d$meducyrs <- relevel(d$meducyrs, ref="High")
# d$feducyrs <- relevel(d$feducyrs, ref="High")
# 
# #number of rooms
# d$nrooms <- relevel(d$nrooms, ref="4+")
# 
# 
# 
# #Set remaining risk factors to factors
# d$brthmon <- factor(d$brthmon)
# d$month <- factor(d$month)
# d$single <- factor(d$single)
# d$vagbrth <- factor(d$vagbrth)
# d$hdlvry <- factor(d$hdlvry)
# d$hfoodsec <- factor(d$hfoodsec)
# d$enstunt <- factor(d$enstunt)
# d$sex <- factor(d$sex)
# d$meducyrs <- factor(d$meducyrs)
# 
# #Check that all risk factor variables are set as factors
# d<-as.data.frame(d)
# for(i in 1:ncol(d)){
#   cat(colnames(d)[i], ": ", class(d[,i]), "\n")
# }
# 
# 
# 
# #Tabulate missingness
# for(i in 1:ncol(d)){
#   print(colnames(d)[i])
#   print(table(is.na(d[,i])))
#   print(levels(d[,i]))
# }
# 
# 
# 
# #--------------------------------------------
# # Check for sparsity across RF levels
# #--------------------------------------------
# 
# tabRF <- function(d, Avar){
#   tab <- table(paste0(d$studyid, " ",d$country), d[,Avar])
#   tab <- tab[rowSums(tab)!=0, ]
#   print(tab)
# }
# 
# 
# 
# 
# 
# tabRF(d, "gagebrth")
# tabRF(d, "birthwt")
# tabRF(d, "birthlen")
# tabRF(d, "parity") 
# tabRF(d, "mage")
# tabRF(d, "mhtcm") 
# tabRF(d, "mwtkg") 
# tabRF(d, "mbmi") 
# tabRF(d, "fage")
# tabRF(d, "fhtcm")
# tabRF(d, "feducyrs")
# tabRF(d, "nrooms")
# tabRF(d, "nhh")
# tabRF(d, "nchldlt5")




#--------------------------------------------
# Save dataset
#--------------------------------------------



saveRDS(d, clean_covariates_path)


