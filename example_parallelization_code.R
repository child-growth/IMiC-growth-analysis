

# Example parallelization
library(sl3)
library(tmle3)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
registerDoParallel(cores=50)

#load data
d<-readRDS()
# Y= HAZ at 6 months
# A = vector of different biomarkers

#Avar is a vector of biomarkers
#note we need 
Avar = c(...)

#set library as glm for speed
SLlibrary = "glm"

#write a wrapper function called tmle_wrapper_function (or something more clever) that runs a TMLE3 analysis and outputs a 1-row data.frame with all the results we want
#this is important to help loop through a bunch of A variables
#https://tlverse.org/tlverse-handbook/tmle3.html
#Need res to be a row of a dataframe with all information we'd want out of a tmle analysis. The name of Y and A, the ATE, 95% CI, pvalue, size of the dataset

#start by checking it works in a normal for loop

res_df <- NULL
for(i in 1:3){
  
  res <- NULL
  try(res <- tmle_wrapper_function(data=d, A=Avar[i], W=Wvar...))
  bind_rows(res_df,res)
  
}

res_df



#Vignette
#https://cran.r-project.org/web/packages/foreach/vignettes/foreach.html

#Note you may need to figure out which data/parameters/packages you need to pass in
res_df <- foreach(i = 1:length(Avar), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  

  try(res <- tmle_wrapper_function(data=d, A=Avar[i], ...))
  return(res)
}



#After running the for loop for all exposures, then apply P-value correction:|

