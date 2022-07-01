
#-------------------------------------------------------------------------------
# Create wide datasets from the long version of the ELICIT and VITAL datasets.
#
# Output: Two descriptive statistics tables in imic/results.
#.        Two datasets with one row per child and all baseline and
#         Time-varying covariates also in imic/results.
#         Outcome plots
#
# Authors: Sajia Darwish (sajdarwish@berkeley.edu)
#-------------------------------------------------------------------------------

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(growthstandards)
#install.packages(naniar)
library(naniar) # For missingness
library(table1)
BV_dir = "/data/KI/imic/"
#------------------------------------------------------------------------------#
#           High level clean up of the joint dataset + subset [DONE]           #  
#------------------------------------------------------------------------------#

d <- readRDS("/data/KI/imic/data/combined_raw_data.rds")

head(d)

table(is.na(d$exbfdu_r))
table((d$h2osrcp))
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
#               Reshape the ELICIT data from long to wide [DONE]               # 
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

elicit $ h2osrcp <- ifelse(elicit $ h2osrcp == "Surface water(river/dam/lake/pond/stream",
                           "Surface water", elicit $ h2osrcp)

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

# Save the dataset
#saveRDS(combinedWideElicit, file = paste0(BV_dir, "/results/wideElicit.RDS"))

#------------------------------------------------------------------------------#
#          Make Summary Table and missingness plot - ELICIT [DONE]             #
#------------------------------------------------------------------------------#
#table1E <- table1(~ . | arm_base, data = combinedWideElicit)

# Save the table
#saveRDS(table1E, file = paste0(BV_dir, "/results/elicitTable.RDS"))
combinedWideElicit $ studyid <- "ELICIT"

# Overall missingness
fifty <- gg_miss_var(combinedWideElicit[, 1:50], show_pct = T)
fiftyPlus <- gg_miss_var(combinedWideElicit[, 51:106], show_pct = T)

ggsave(fifty, filename = paste0(BV_dir, "/results/figures/missing/fiftyE.png"))
ggsave(fiftyPlus, filename = paste0(BV_dir, "/results/figures/missing/fiftyPE.png"))

# By arm missingness
byArm <- gg_miss_case(combinedWideElicit, facet = arm_base)
ggsave(byArm, filename = paste0(BV_dir, "/results/figures/missing/armE.png"))
#------------------------------------------------------------------------------#
#               Reshape the VITAL data from long to wide [DONE]                #
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

# Anonymized data
combinedWideVital <- combinedWideV %>%
  select(-id)

# Save the anonymized dataset
#saveRDS(combinedWideVital, file = paste0(BV_dir, "/results/wideVital.RDS"))

#------------------------------------------------------------------------------#
#             Make Summary Table and missingness plots - VITAL [DONE]          #      
#------------------------------------------------------------------------------#
#table1V <- table1(~ . | arm_base, data = combinedWideVital)

# Save the table
#saveRDS(table1V, file = paste0(BV_dir, "/results/vitalTable.RDS"))
combinedWideVital $ studyid <- "VITAL-Lactation"

fifty <- gg_miss_var(combinedWideVital[, 1:50], show_pct = T)
fiftyPlus <- gg_miss_var(combinedWideVital[, 51:100], show_pct = T)
hundPlus <- gg_miss_var(combinedWideVital[, 101:140], show_pct = T)

ggsave(fifty, filename = paste0(BV_dir, "/results/figures/missing/fiftyV.png"))
ggsave(fiftyPlus, filename = paste0(BV_dir, "/results/figures/missing/fiftyPV.png"))
ggsave(hundPlus, filename = paste0(BV_dir, "/results/figures/missing/hundPV.png"))

# Recode the arm variable
combinedWideVital $ arm_base <- case_when(combinedWideVital $ arm_base == "Nutrient supplement+Ex.BreastFeed"
                         ~ "Nutrient+Ex",
                         combinedWideVital $ arm_base == "Nutrient supplement+Ex.BreastFeed+AZT"
                         ~ "Nutrient+Ex+AZT",
                         combinedWideVital $ arm_base == "Control" ~ "Control")

byArm <- gg_miss_case(combinedWideVital, facet = arm_base)
ggsave(byArm, filename = paste0(BV_dir, "/results/figures/missing/armV.png"))

#------------------------------------------------------------------------------#
#                       Plot Outcome Variables: [DONE]                         #                                                
#------------------------------------------------------------------------------#

# Filter out unwanted time points
elicit <- elicit %>%
  filter(visit2 == "base" | visit2 == "3m" | visit2 == "6m" | visit2 == "9m" |
           visit2 == "12m" | visit2 == "15m" | visit2 == "18m")

# Re order levels
elicit $ visit2 <- factor(elicit $ visit2, 
                           level = c("base", "1m", "3m", "5m", "6m", "9m",
                                     "12m", "15m", "18m"))

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

# Make a function for showing number of observations in plot
stat_box_data <- function (y) {
  return( 
    data.frame(
      y = 0.5 + 1.1 * max(y),  #may need to modify this depending on your data
      label = paste('',length(y), '\n')
      #'mean =', round(median(y), 1), '\n')
    )
  )
}

# Make a plot function
plot <- function (d, outcome) {
  d %>%
    filter(!is.na(visit2)) %>%
    group_by("subjid") %>%
    ggplot(aes(x = visit2, y = outcome)) +
    geom_boxplot(outlier.shape = NA) +
    #geom_violin()+
    #stat_summary(
     # fun.data = stat_box_data, 
      #geom = "text", 
      #hjust = 0.5,
     # vjust = 0.9) + 
    #geom_jitter(width = 0.05, alpha = 0.05) +
    xlab("") +
    theme(strip.text.x = element_text(size = 10))
}

# Make plots
# By intervention: ELICIT
bazE4 <- plot(d = elicit, outcome = elicit $ baz) +
  ylab("BMI-for-Age Z-Score") +
  facet_wrap(~ arm)
ggsave(bazE4, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/bazE-4.png"))

hazE4 <- plot(d = elicit, outcome = elicit $ haz) +
  ylab("Height-for-Age Z-Score") +
  facet_wrap(~ arm)
ggsave(hazE4, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/hazE-4.png"))

wazE4 <- plot(d = elicit, outcome = elicit $ waz) +
  ylab("Weight-for-Age Z-Score") +
  facet_wrap(~ arm)
ggsave(wazE4, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/wazE-4.png"))

whzE4 <- plot(d = elicit, outcome = elicit $ whz) +
  ylab("Weight-for-Height Z-Score") +
  facet_wrap(~ arm)
ggsave(whzE4, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/whzE-4.png"))

# Overall: ELICIT
bazE <- plot(d = elicit, outcome = elicit $ baz) +
  ylab("BMI-for-Age Z-Score")
ggsave(bazE, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/bazE.png"))

hazE <- plot(d = elicit, outcome = elicit $ haz) +
  ylab("Height-for-Age Z-Score")
ggsave(hazE, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/hazE.png"))

wazE <- plot(d = elicit, outcome = elicit $ waz) +
  ylab("Weight-for-Age Z-Score")
ggsave(wazE, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/wazE.png"))

whzE <- plot(d = elicit, outcome = elicit $ whz) +
  ylab("Weight-for-Height Z-Score")
ggsave(whzE, filename = paste0(BV_dir, "/results/figures/outcomes/elicit/whzE.png"))

# Make a plot function
plot <- function (d, outcome) {
  d %>%
    #filter(!is.na(c(outcome))) %>%
    group_by("subjid") %>%
    ggplot(aes(x = visit2, y = outcome)) +
    geom_boxplot(outlier.shape = NA) +
    #stat_summary(
    # fun.data = stat_box_data, 
    #geom = "text", 
    #hjust = 0.5,
    # vjust = 0.9) + 
    #geom_jitter(width = 0.05, alpha = 0.05) +
    xlab("") +
    theme(strip.text.x = element_text(size = 10))
}

vital $ haz

# By intervention: VITAL
bazE4 <- plot(d = vital, outcome = vital $ baz) +
  ylab("BMI-for-Age Z-Score") +
  facet_wrap(~ arm)
ggsave(bazE4, filename = paste0(BV_dir, "/results/figures/outcomes/vital/bazV-3.png"))

hazE4 <- plot(d = vital, outcome = vital $ haz) +
  ylab("Height-for-Age Z-Score") +
  facet_wrap(~ arm)
ggsave(hazE4, filename = paste0(BV_dir, "/results/figures/outcomes/vital/hazV-3.png"))

wazE4 <- plot(d = vital, outcome = vital $ waz) +
  ylab("Weight-for-Age Z-Score") +
  facet_wrap(~ arm)
ggsave(wazE4, filename = paste0(BV_dir, "/results/figures/outcomes/vital/wazV-3.png"))

whzE4 <- plot(d = vital, outcome = vital $ whz) +
  ylab("Weight-for-Height Z-Score") +
  facet_wrap(~ arm)
ggsave(whzE4, filename = paste0(BV_dir, "/results/figures/outcomes/vital/whzV-3.png"))

# Overall: VITAL
bazE <- plot(d = vital, outcome = vital $ baz) +
  ylab("BMI-for-Age Z-Score")
ggsave(bazE, filename = paste0(BV_dir, "/results/figures/outcomes/vital/bazV.png"))

hazE <- plot(d = vital, outcome = vital $ haz) +
  ylab("Height-for-Age Z-Score")
ggsave(hazE, filename = paste0(BV_dir, "/results/figures/outcomes/vital/hazV.png"))

wazE <- plot(d = vital, outcome = vital $ waz) +
  ylab("Weight-for-Age Z-Score")
ggsave(wazE, filename = paste0(BV_dir, "/results/figures/outcomes/vital/wazV.png"))

whzE <- plot(d = vital, outcome = vital $ whz) +
  ylab("Weight-for-Height Z-Score")
ggsave(whzE, filename = paste0(BV_dir, "/results/figures/outcomes/vital/whzV.png"))

