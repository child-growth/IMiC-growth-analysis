#Install packages (run once)
# install.packages("synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))
# install.packages("synapserutils", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))


#set up profile and packages
library(synapser)
library(synapserutils)
kiPath <- c("/data/KI/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths())
.libPaths(kiPath)
synLogin('amertens','!1a2n3m4!')

#Download final.csv dataset
# Obtain a pointer and download the data
#imic_harmonized <- synGet(entity='syn27054241', downloadLocation = "/home/andrew.mertens/data/imic/")

vital <- synGet(entity='syn30279096', downloadLocation = "/home/andrew.mertens/data/imic/")
elicit <- synGet(entity='syn30277820', downloadLocation = "/home/andrew.mertens/data/imic/")



class(vital)

as.data.frame(vital)


#download UCB-SuperLearner repo
# files <- synapserutils::syncFromSynapse('syn30279096', path = "/home/andrew.mertens/data/imic/cohort-datasets") 
# files <- synapserutils::syncFromSynapse('syn30277820', path = "/home/andrew.mertens/data/imic/cohort-datasets") 


# Below are links to the main datasets available in Synapse (Note however that the Harmonized datasets are not completely finalized yet). If you don’t have access to Synapse/links don’t work, but have a DAA, I believe Vishak will be able to help you or any data analysts within your group that needs access.
#
#
#
# ELICIT data:
#
#   Milk data: https://www.synapse.org/#!Synapse:syn27634377
#   Harmonized data/metadata: https://www.synapse.org/#!Synapse:syn27054251
#   Data dictionary: https://www.synapse.org/#!Synapse:syn27054249
#   See other details/versions in this adam folder
#
#
# VITAL data:
#
#   Milk data: https://www.synapse.org/#!Synapse:syn27634422
#   Harmonized data/metadata: https://www.synapse.org/#!Synapse:syn27306223
#   Data dictionary: https://www.synapse.org/#!Synapse:syn27306210
#   However also see the Harmonization spec for a dictionary more specific to this harmonized version.
# See other details/versions in this adam folder
#
#
# CHILD Milk data: https://www.synapse.org/#!Synapse:syn27634108
#
#
#
#   Harmonization spec: https://www.synapse.org/#!Synapse:syn27054241
#
#   Though each site has its own dictionary, this may be used as an overall dictionary for the harmonized data (note that the availability of variables for each site is also shown in this spec).
#
#
# Other notes:
#
#   For the Milk data, see 1_Data_Layout.rtf first for file descriptions and the “milk_analytes” subfolders for the main datasets.
# Rownames of milk datasets are the “BMID” (breastmilk sample ID) in the harmonized datasets (if the row doesn’t correspond to a breastmilk sample, then it is a pooled QC milk replicates or a negative control).
# VITAL flag variables (to be explained more in the meeting)
# VISIT_R_FL: Recall data =1
# VISITIMPCM: Visits having different agedays due to differences in dates (Where a participant had two records in the same visit but with differing dates)
# AGEIMPFL: agedays derived = 1 (see AGEIMPCM for comment where AGEIMPFL = 1)
