# Merge wide and biomarker datasets using the BMID variable for both VITAL and ELICIT.
# Author: Sajia Darwish

## Load libraries

BV_dir = "/data/KI/imic/"

library(DataExplorer)
library(tidyverse)
library(table1)
library(data.table)
library(SmartEDA)


# ELICIT

## Load datasets: wide + biomarker

wideElicit <- readRDS("/data/KI/imic/results/wideElicit.RDS")

hmo <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/HMO_ELICIT.csv")
hmo <- rename("bmid_base" = "X", hmo)

sapient <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Sapient_ELICIT.csv")
sapient <- rename("bmid_base" = "X", sapient)

bvit <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Allen_Bvit_ELICIT.csv")
bvit <- rename("bmid_base" = "X", bvit)

biocratesNorm <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Biocrates_ELICIT_Normalized_toALL_NS.csv")
biocratesNorm <- rename("bmid_base" = "X", biocratesNorm)

biocrates <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Biocrates_ELICIT_NOT_Normalized.csv")
biocrates <- rename("bmid_base" = "X", biocrates)

metabolInd <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/MetaboIndicators/Biocrates_ELICIT_Normalized_MetaboINDICATOR.csv")
metabolInd <- rename("bmid_base" = "X", metabolInd)


## Merge

hmoMerged <- merge(wideElicit, hmo, by = "bmid_base", all = TRUE)
saveRDS(hmoMerged, file = paste0(BV_dir, "/data/raw_lab_data/elicit/merged/hmo.RDS"))

sapientMerged <- merge(wideElicit, sapient, by = "bmid_base", all = TRUE)
saveRDS(sapientMerged, file = paste0(BV_dir, "/data/raw_lab_data/elicit/merged/sapient.RDS"))

bvitMerged <- merge(wideElicit, bvit, by = "bmid_base", all = TRUE)
saveRDS(bvitMerged, file = paste0(BV_dir, "/data/raw_lab_data/elicit/merged/bvit.RDS"))

biocratesNormMerged <- merge(wideElicit, biocratesNorm, by = "bmid_base", all = TRUE)
saveRDS(biocratesNormMerged, file = paste0(BV_dir, "/data/raw_lab_data/elicit/merged/biocratesNorm.RDS"))

biocratesMerged <- merge(wideElicit, biocrates, by = "bmid_base", all = TRUE)
saveRDS(biocratesMerged, file = paste0(BV_dir, "/data/raw_lab_data/elicit/merged/biocrates.RDS"))

metabolIndMerged <- merge(wideElicit, metabolInd, by = "bmid_base", all = TRUE)
saveRDS(metabolIndMerged, file = paste0(BV_dir, "/data/raw_lab_data/elicit/merged/metabolInd.RDS"))


# VITAL
wideVital <- readRDS("/data/KI/imic/results/wideVital.RDS")

hmo <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/HMO_VITAL.csv")
hmo <- rename("bmid_base" = "X", hmo)

sapient <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Sapient_VITAL.csv")
sapient <- rename("bmid_base" = "X", sapient)

bvit <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Allen_Bvit_VITAL.csv")
bvit <- rename("bmid_base" = "X", bvit) # This one does not have matching ID.

biocrates <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Biocrates_VITAL_NOT_Normalized.csv")
biocrates <- rename("bmid_base" = "X", biocrates)

biocratesNorm <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Biocrates_VITAL_Normalized_toALL_NS.csv")
biocratesNorm <- rename("bmid_base" = "X", biocratesNorm)

pbl <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/PBL_VITAL_NOT_Normalized.csv")
pbl <- rename("bmid_base" = "X", pbl)

# Substring the variable so that we only have the exact ID left.
# Keep everything before and up to "_P": 
pbl $ bmid_base <- gsub("_P.*", "", pbl $ bmid_base)

# Remove all before and up to "_A".
pbl $ bmid_base <- gsub(".*_A", "", pbl $ bmid_base)

# Subset the data to those with ID's ending with W and without.
withW <- pbl %>%
  filter(str_detect(bmid_base, "W"))

woW <- pbl %>%
  filter(!str_detect(bmid_base, "W"))

# Add A in front of all strings ending with W.
withW $ bmid_base <- paste0("A", withW $ bmid_base)

# Append the rest to the withW dataset.
pbl <- rbind(withW, woW)

pblNorm <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/PBL_VITAL_Normalized.csv")
pblNorm <- rename("bmid_base" = "X", pblNorm)

# Substring the variable so that we only have the exact ID left.
# Keep everything before and up to "_P": 
pblNorm $ bmid_base <- gsub("_P.*", "", pblNorm $ bmid_base)

# Remove all before and up to "_A".
pblNorm $ bmid_base <- gsub(".*_A", "", pblNorm $ bmid_base)

# Subset the data to those with ID's ending with W and without.
withW <- pblNorm %>%
  filter(str_detect(bmid_base, "W"))

woW <- pblNorm %>%
  filter(!str_detect(bmid_base, "W"))

# Add A in front of all strings ending with W.
withW $ bmid_base <- paste0("A", withW $ bmid_base)

# Append the rest to the withW dataset.
pblNorm <- rbind(withW, woW)

# Save this cleaned dataset
saveRDS(pblNorm, file = paste0(BV_dir, "/data/raw_lab_data/vital/milk_analytes/pblNormCleaned.RDS"))

metabolInd <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/MetaboIndicators/Biocrates_VITAL_Normalized_MetaboINDICATOR.csv")
metabolInd <- rename("bmid_base" = "X", metabolInd)


## Merge

hmoMerged <- merge(wideVital, hmo, by = "bmid_base", all = TRUE)
saveRDS(hmoMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/hmo.RDS"))

sapientMerged <- merge(wideVital, sapient, by = "bmid_base", all = TRUE)
saveRDS(sapientMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/sapient.RDS"))

# This one's IDs do not match so we cannot merge.
# bvitMerged <- merge(wideVital, bvit, by = "bmid_base", all = TRUE)
# saveRDS(bvitMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/bvit.RDS"))

biocratesNormMerged <- merge(wideVital, biocratesNorm, by = "bmid_base", all = TRUE)
saveRDS(biocratesNormMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/biocratesNorm.RDS"))

biocratesMerged <- merge(wideVital, biocrates, by = "bmid_base", all = TRUE)
saveRDS(biocratesMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/biocrates.RDS"))

pblNormMerged <- merge(wideVital, pblNorm, by = "bmid_base", all = TRUE)
saveRDS(pblNormMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/pblNorm.RDS"))

pblMerged <- merge(wideVital, pbl, by = "bmid_base", all = TRUE)
saveRDS(pblMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/pbl.RDS"))

metabolIndMerged <- merge(wideVital, metabolInd, by = "bmid_base", all = TRUE)
saveRDS(metabolIndMerged, file = paste0(BV_dir, "/data/raw_lab_data/vital/merged/metabolInd.RDS"))







