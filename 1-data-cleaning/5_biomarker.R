#-------------------------------------------------------------------------------
# Explore the biomarker datasets.
# Inputs:
#   Allen_Bvit_CHILD.csv
#   Allen_Bvit_VITAL.csv
#   PBL_VITAL_Normalized.csv
#-------------------------------------------------------------------------------

# Load data
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(table1)

child <- read.table("/data/KI/imic/data/raw_lab_data/Allen_Bvit_CHILD.csv",
                    sep = ",", header = TRUE)

vital <- read.table("/data/KI/imic/data/raw_lab_data/Allen_Bvit_VITAL.csv",
                    sep = ",", header = TRUE)

pblv <- read.table("/data/KI/imic/data/raw_lab_data/PBL_VITAL_Normalized.csv",
                    sep = ",", header = TRUE)

# Check if these datasets are mergable with the wide datasets
wideE <- readRDS("/data/KI/imic/results/wideElicit.RDS")
wideV <- readRDS("/data/KI/imic/results/wideVital.RDS")

head(wideE)
head(wideV)

# The IDs do not match. We therefore cannot merge these datasets.

# Make descriptive statistics tables
child2 <- child %>% select(-X)
childTable <- table1(~., data = child2)

vital2 <- vital %>% select(-X)
vitalTable <- table1(~., data = vital2)

# Check if column names are the same in child and vital
colNames <- function(x, y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }  
    else if(i == tail(names(y), n = 1)) {
      print('Names are identical')
    }
  }
}

colNames(child2, vital2) # Names are identical.








