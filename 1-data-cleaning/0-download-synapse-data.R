#Install packages (run once)
# install.packages("synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))
# install.packages("synapserutils", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))


#set up profile and packages
library(synapser)
kiPath <- c("/data/KI/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths())
.libPaths(kiPath)
library(synapserutils)
synLogin('amertens','!1a2n3m4!')

#Download final.csv dataset
# Obtain a pointer and download the data
#imic_harmonized <- synGet(entity='syn27054241', downloadLocation = "/home/andrew.mertens/data/imic/")

vital <- synGet(entity='syn30279096', downloadLocation = "/home/andrew.mertens/data/imic/")
elicit <- synGet(entity='syn30277820', downloadLocation = "/home/andrew.mertens/data/imic/")


#Get milk data-full folder with meta-data and different lab data
vital_milkdata <- synapserutils::syncFromSynapse('syn27634422', path = "/data/KI/imic/data/raw_lab_data/vital") 
elicit_milkdata <- synapserutils::syncFromSynapse('syn27634377', path = "/data/KI/imic/data/raw_lab_data/elicit") 

