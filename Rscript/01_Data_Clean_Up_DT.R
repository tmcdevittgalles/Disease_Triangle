# The goal of this script is to merger the numerous raw data files together
# from the California database. Each of the resulting files will include the
# following common variables to allow for easy merging
# 
#   "Assmt_Code" == SITECODE_YYYYMMDD
#   "SiteCode == SITECODE
#   "Date" == YYYYMMMDD
#   
# There will be 5 differnt datasets that will be merged at the end.
# 
# 1) Infection patterns in Amphibian host
#   1a) Overall infection levels and locations  (Amphib_Parasite.csv)
#   1b) Host data such as body size and data collected (Amphib_Dissect.csv)
#   
# 2) Malformation Patterns
#   2a) Summary of transect information (Transect_SppSum.csv)
#   2b) Breaking down the specific types of malformations (Malform_transect.csv)
#   
# 3) Snail infection patterns
#   3a) Indiviudal snail infection data (Snail_Disect.csv)
#   
# 4) Dipnet survey data
#   4a) Summary of species found in each sweep and count (Survey_Spp.csv)
#   4b) number of sweeps and distance for each siene (Netting_Info.csv)
#   
# 5) Site survery data
#   5a) Site information relating to location and elevation (Site.Info.csv)
#   5b) Site survey data including vegetation and size (Wetland_Infor.csv)
#   5c) water chemestry data (Water_Quality.csv)

## Setting the working directory

library(tidyverse)
library(dplyr)

setwd("~/Desktop/Current_Projects/Disease_Triangle")


