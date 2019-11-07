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
# 1) Site survery data
#   1a) Site information relating to location and elevation (Site.Info.csv)
#   1b) Site survey data including vegetation and size (Wetland_Infor.csv)
#   1c) water chemestry data (Water_Quality.csv)
# 
# 2) Infection patterns in Amphibian host
#   2a) Overall infection levels and locations  (Amphib_Parasite.csv)
#   2b) Host data such as body size and data collected (Amphib_Dissect.csv)
#   
# 3) Malformation Patterns
#   3a) Summary of transect information (Transect_SppSum.csv)
#   3b) Breaking down the specific types of malformations (Malform_transect.csv)
#   
# 4) Snail infection patterns
#   4a) Indiviudal snail infection data (Snail_Disect.csv)
#   
# 5) Dipnet survey data
#   5a) Summary of species found in each sweep and count (Survey_Spp.csv)
#   5b) number of sweeps and distance for each siene (Netting_Info.csv)
#   
# 

## Setting the working directory

library(tidyverse)
library(dplyr)

setwd("~/Desktop/Current_Projects/Disease_Triangle")

############## 1) Site Survey Data  #############

# At the end of this section I will have produced 2 files,
# 
#    A) A simple list of the sites that will be used for data analysis
#       This should help eliminate sites that are not in the area of interest
#       for the study . I.E. non Bay Area Sites, each row is a unique Site
#     
#    B) One that contains all the site level information for each visit,
#       this includes area, depth and water chemeistry data
#       I.E. each row will be a Site X Visit

# Entering the two data sets

Site.Info.df <- read.csv( "./Data/Raw_Data/Site_Info.csv" )

Site.Wet.df <- read.csv( "./Data/Raw_Data/Wetland_Info.csv" )

Site.Water.df <- read.csv( "./Data/Raw_Data/Water_Quality.csv" )


## First thing is to modify the site info data frame to subset down to the sites
## I am interested in


dim(Site.Info.df) # 1482 x 19 # seems like way to many sites but ok

str(Site.Info.df) # also a lot of columns I think i can get away with 
                  # SiteCode, PropertyName, County, State, Latitude,
                  # Longitude, Elevation, Longevity so 8 columns
            

Site.Info.df <- select( Site.Info.df, c("SiteCode", "PropertyName", "County",
                                        "State","Latitude", "Longitude",
                                        "Elevation", "Longevity"))
# Renaming some of the columns

colnames(Site.Info.df) <- c("SiteCode", "PropName", "County", "State","Lat",
                            "Long", "Elev", "Longevity")

# Ok lets capitalize everything so there is no weird issue with 

Site.Info.df$SiteCode <- as.factor(toupper(Site.Info.df$SiteCode))
Site.Info.df$PropName <- as.factor(toupper(Site.Info.df$PropName))
Site.Info.df$County <- as.factor(toupper(Site.Info.df$County))
Site.Info.df$State <- as.factor(toupper(Site.Info.df$State ))


# Ok lets remove all sites that are not from California

Site.Info.df <- filter( Site.Info.df, State == "CA" )

dim(Site.Info.df) # 548 X 8  whoa cut 933 sites 

## ok now lerts cut all counties that are not apart of the tri county area
## Contra Costa, Alameda, Santa Clara

Site.Info.df <- filter( Site.Info.df, County == "CONTRA COSTA" |
                                      County == "ALAMEDA" |
                                      County == "SANTA CLARA" )

dim(Site.Info.df) ## 395 X 8 cute 153 more sites

## lets check out the sites remaining sites

unique(Site.Info.df$SiteCode)


## still a lot to cut, lets focus on the focal properties

Site.Info.df <- filter( Site.Info.df, PropName == "5 CANYONS REGIONAL PARK" |
                          PropName == "SILVER OAKS OSTRICH FARM" |
                          PropName == "BLUE OAKS RANCH RESERVE"|
                          PropName == "JOSEPH GRANT COUNTY PARK"|
                          PropName == "BRIONES REGIONAL PARK"|
                          PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR"|
                          PropName == "SAN FELIPE RANCH"|
                          PropName == "PLEASANTON RIDGE REGIONAL PARK"|
                          PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK"|
                          PropName == "BLUE OAK RANCH RESERVE"|
                          PropName == "VARGAS PLATEAU REGIONAL PRESERVE"
                          )

dim(Site.Info.df) ## 169 X 8 we removed 226 sites


############## 2) Amphibian  host dissection data  #############

# At the end of this section I will have produced 3 files,
# 
#    A) A more summerized version with unique Assmt_Code, total dissection
#     and total parasite for each taxa by site by visit I.E each row
#     will be at the site x visit x taxa x parasite level
#     
#    B) One that is looking at the individual amphibian host including size and 
#     age data  and what parasites and levels it has.
#     I.E. each row will be a host x taxa x site x visit x parasite
#     
#    C) One that is looking at where in the individual host do we see the 
#     infection, thus using host infection location data to better understand
#     virulence. 
#     I.E. each row will be a host x location x site x visit x parasite
#

# Entering the two data sets

host.info.df <- read.csv( "./Data/Raw_Data/Amphib_Dissect.csv" )

host.para.df <- read.csv( "./Data/Raw_Data/Amphib_Parasite.csv" )


# host.info.df only has host information, no infection, should be easier
# to work with. First step is to select the columns i need

str(host.info.df) 

dim(host.info.df) # 33633 x 42

### Lots of columns , after a quick scan i think i only need to work with the 
### following columns, DissectCode, SiteCode, CollectCode,Collect_Date, Spp_Code
### Stage, and SV_Length..mm.  So 7 columns

host.info.df <- select( host.info.df, c("DissectCode", "SiteCode",
                                        "CollectCode", "Collect_Date",
                                        "Spp_Code", "Stage", "SV_Length..mm."))

dim( host.info.df ) # 33633 x 7

# Simplifying the column names

colnames( host.info.df ) <- c("DissectCode", "SiteCode",
                              "CollectCode", "Date",
                              "SppCode", "Stage", "SVL")

## checking out the unique values for both SiteCode and SppCode

unique( host.info.df$SppCode )

## 58 total levels, I know the Bay Area should only 5 amphibian host so lets
## subset

host.info.df <- filter( host.info.df, SppCode == "PSRE" |
                             SppCode =="TATO" |
                             SppCode == "TAGR" | 
                             SppCode == "BUBO" |
                             SppCode == "RACA" )

dim( host.info.df )   # 25591 X 7 , looks like we los 8042 animals

host.info.df$SiteCode <- as.factor(toupper( host.info.df$SiteCode ))

unique( host.info.df$SiteCode )

  
