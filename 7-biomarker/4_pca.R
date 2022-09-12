#-------------------------------------------------------------------------------
# PCA script calling functions from 0-project-functions.
#
# Inputs:
#   Biomarker datasets.
# Outputs:
#   A dataframe of results + graphs in 4_pca_files.
# Author:
#   Sajia Darwish
#-------------------------------------------------------------------------------


#install.packages("rlang", type = "source")
library(tidymodels)
library(corrplot)
library(ggrepel)
library(tidytext)
library(dotwhisker) # for coefficient plots.
library(tidyverse)

rm(list=ls())
source('/home/sajdarwish/iMiC/0-config.R')


# ELICIT data

# hmo <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/HMO_ELICIT.csv")
# hmo <- rename("bmid" = "X", hmo)
# 
# bvit <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Allen_Bvit_ELICIT.csv")
# bvit <- rename("bmid_base" = "X", bvit)
# 
biocratesNormE <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Biocrates_ELICIT_Normalized_toALL_NS.csv")
biocratesNormE <- rename("bmid_base" = "X", biocratesNormE)
#
# sapient <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Sapient_ELICIT.csv")
# sapient <- rename("bmid_base" = "X", sapient)
#
# biocrates <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/Biocrates_ELICIT_NOT_Normalized.csv")
# biocrates <- rename("bmid_base" = "X", biocrates)

metabolInd <- read.csv("/data/KI/imic/data/raw_lab_data/elicit/milk_analytes/MetaboIndicators/Biocrates_ELICIT_Normalized_MetaboINDICATOR.csv")
metabolInd <- rename("bmid_base" = "X", metabolInd)

# VITAL data
biocratesNormV <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/Biocrates_VITAL_Normalized_toALL_NS.csv")
biocratesNormV <- rename("bmid_base" = "X", biocratesNormV)

metabolIndV <- read.csv("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/MetaboIndicators/Biocrates_VITAL_Normalized_MetaboINDICATOR.csv")
metabolIndV <- rename("bmid_base" = "X", metabolIndV)

pblNormV <- readRDS("/data/KI/imic/data/raw_lab_data/vital/milk_analytes/pblNormCleaned.RDS")

# ELICIT

## Biocrates Normalizes: targeted metabolomics data-----------------------------

# Scree plot
cleanData <- cleanFunc(biocratesNormE) # Save plots as 800x500 png.
#ggsave(screePlot(cleanData), filename = paste0(BV_dir, "/results/pcaData/elicit/biocratesNormEsPlot.png"))
#ggsave(screePlot2(cleanData), filename = paste0(BV_dir, "/results/pcaData/elicit/biocratesNormEsPlot2.png"))

### Run PCA and display sample output
prepData <- prepData(data = cleanData, numC = 30) #num of components to extract.
pcaLoadings <- pcaEstimates(pca_estimates_prep = prepData)
head(pcaLoadings)
pcaEstimatesPlot <- pcaEstimatesPlot(pcaEstimates = pcaLoadings, PC = 1:30, n = 10) # n = number of top contributors.

### Top ten contributors to each component based on importance
ggsave(top10cI(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/elicit/biocratesNormEtop10cI.png"))

### Top 10 contributors to each component by value to see direction
ggsave(top10cV(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/elicit/biocratesNormEtop10cV.png"))

# Merge the PC data with ID's and save dataset.
biocratesNormPC <- juice(prepData)
bmid_base = biocratesNormE[, 1]
biocratesNormPC <- cbind(bmid_base, biocratesNormPC)
#saveRDS(biocratesNormPC, file = paste0("/data/KI/imic/results/pcaData/elicit/biocratesNormPC.RDS"))
#-------------------------------------------------------------------------------

## Metabolite Indicators

# Scree plot
cleanData <- cleanFunc(metabolInd)
ggsave(screePlot(cleanData), filename = paste0(BV_dir, "/results/pcaData/elicit/meIndEsPlot.png"))
ggsave(screePlot2(cleanData), filename = paste0(BV_dir, "/results/pcaData/elicit/meIndEsPlot2.png"))

### Run PCA and display sample output
prepData <- prepData(data = cleanData, numC = 8)
pcaLoadings <- pcaEstimates(pca_estimates_prep = prepData)
head(pcaLoadings)
pcaEstimatesPlot <- pcaEstimatesPlot(pcaEstimates = pcaLoadings, PC = 1:8, n = 10)

### Top ten contributors: importance
ggsave(top10cI(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/elicit/metabolIndtop10cI.png"))

### Top 10 contributors: value
ggsave(top10cV(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/elicit/metabolIndtop10cV.png"))

# Merge the PC data with ID's and save dataset.
metabolIndPC <- juice(prepData)
bmid_base = metabolInd[, 1]
metabolIndPC <- cbind(bmid_base, metabolIndPC)
saveRDS(metabolIndPC, file = paste0("/data/KI/imic/results/pcaData/elicit/metabolIndPC.RDS"))
#-------------------------------------------------------------------------------

# VITAL

## Biocrates Normalizes: targeted metabolomics data-----------------------------

# Scree plot
cleanData <- cleanFunc(biocratesNormV)
ggsave(screePlot(cleanData), filename = paste0(BV_dir, "/results/pcaData/vital/biocratesNormVsPlot.png"))
ggsave(screePlot2(cleanData), filename = paste0(BV_dir, "/results/pcaData/vital/biocratesNormVsPlot2.png"))

### Run PCA and display sample output
prepData <- prepData(data = cleanData, numC = 10)
pcaLoadings <- pcaEstimates(pca_estimates_prep = prepData)
head(pcaLoadings)
pcaEstimatesPlot <- pcaEstimatesPlot(pcaEstimates = pcaLoadings, PC = 1:10, n = 10)

### Top ten contributors to each component based on importance
ggsave(top10cI(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/vital/biocratesNormVtop10cI.png"))

### Top 10 contributors to each component by value to see direction
ggsave(top10cV(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/vital/biocratesNormVtop10cV.png"))

# Merge the PC data with ID's and save dataset.
biocratesNormPC <- juice(prepData)
bmid_base = biocratesNormV[, 1]
biocratesNormPC <- cbind(bmid_base, biocratesNormPC)
saveRDS(biocratesNormPC, file = paste0("/data/KI/imic/results/pcaData/vital/biocratesNormPC.RDS"))
#-------------------------------------------------------------------------------

## Metabolite Indicators

# Scree plot
cleanData <- cleanFunc(metabolIndV)
ggsave(screePlot(cleanData), filename = paste0(BV_dir, "/results/pcaData/vital/metabolIndsPlot.png"))
ggsave(screePlot2(cleanData), filename = paste0(BV_dir, "/results/pcaData/vital/metabolIndsPlot2.png"))

### Run PCA and display sample output
prepData <- prepData(data = cleanData, numC = 8)
pcaLoadings <- pcaEstimates(pca_estimates_prep = prepData)
head(pcaLoadings)
pcaEstimatesPlot <- pcaEstimatesPlot(pcaEstimates = pcaLoadings, PC = 1:8, n = 10)

### Top ten contributors: importance
ggsave(top10cI(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/vital/metabolIndtop10cI.png"))

### Top 10 contributors: value
ggsave(top10cV(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/vital/metabolIndtop10cV.png"))

# Merge the PC data with ID's and save dataset.
metabolIndPC <- juice(prepData)
bmid_base = metabolIndV[, 1]
metabolIndPC <- cbind(bmid_base, metabolIndPC)
saveRDS(metabolIndPC, file = paste0("/data/KI/imic/results/pcaData/vital/metabolIndPC.RDS"))
#-------------------------------------------------------------------------------

## Proteomics

# Scree plot
cleanData <- cleanFunc(pblNormV)
ggsave(screePlot(cleanData), filename = paste0(BV_dir, "/results/pcaData/vital/pblNormVsPlot.png"))
ggsave(screePlot2(cleanData), filename = paste0(BV_dir, "/results/pcaData/vital/pblNormVsPlot2.png"))

### Run PCA and display sample output
prepData <- prepData(data = cleanData, numC = 8)
pcaLoadings <- pcaEstimates(pca_estimates_prep = prepData)
head(pcaLoadings)
pcaEstimatesPlot <- pcaEstimatesPlot(pcaEstimates = pcaLoadings, PC = 1:8, n = 10)

### Top ten contributors: importance
ggsave(top10cI(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/vital/pblNormVtop10cI.png"))

### Top 10 contributors: value
ggsave(top10cV(estimates = pcaEstimatesPlot, data = cleanData), width = 1500,
       height = 2000, units = "px",
       filename = paste0(BV_dir, "/results/pcaData/vital/pblNormVtop10cV.png"))

# Merge the PC data with ID's and save dataset.
pblNormVPC <- juice(prepData)
bmid_base = pblNormV[, 1]
pblNormVPC <- cbind(bmid_base, pblNormVPC)
saveRDS(pblNormVPC, file = paste0("/data/KI/imic/results/pcaData/vital/pblNormVPC.RDS"))
#-------------------------------------------------------------------------------















