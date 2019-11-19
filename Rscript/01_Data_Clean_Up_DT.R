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

# Entering the three data sets

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

## Ok sweet I have the list of sites that I will use for the analysis this may
## change as I am sure some of these sites have 1 visit or so but we can deal
## with that later, this is a good place to start


##### Wetland information data frame

str( Site.Wet.df )

dim( Site.Wet.df ) # 3287 X 20

## So based on the wetland data set I need to seperate the assmt code to get 
## a sitte code and date into the data but first lets clean up the dataset


Site.Wet.df <- select( Site.Wet.df, c("AssmtCode", "PondArea..m.", 
                                      "Perimeter..m.", "MaxDepth..m.",
                                      "ShoreVeg_Pct", "Juncus_Pct", "Typha_Pct",
                                      "Bullrush_Pct","Other_Pct", 
                                      "OpenWater_Pct", "CanopyCov_Pct",
                                      "Tree.Measure", "Dry") )

# simplifying column names

colnames( Site.Wet.df ) <- c("AssmtCode", "Area", 
                             "Perim", "Depth",
                             "ShoreVeg", "Juncus", "Typha",
                             "Bullrush","Other", 
                             "OpenWate", "Canopy",
                             "Trees", "Dry")

## Capitalizing all the AssmtCode

Site.Wet.df$AssmtCode <- as.factor( toupper( Site.Wet.df$AssmtCode ) )

## creating a dummy assmtcode to seperate the SiteCode and Date

Site.Wet.df$dumAssmtCode <- Site.Wet.df$AssmtCode 


Site.Wet.df <- Site.Wet.df %>%
    separate( dumAssmtCode, c("SiteCode", "Date"), sep =-9) %>% 
    separate( Date, c("Throw1", "Date"), sep =-8) %>%
    select( -"Throw1")

Site.Wet.df$SiteCode <- as.factor(Site.Wet.df$SiteCode)


## Seperating by month year and date

Site.Wet.df$dumDate <- as.numeric(Site.Wet.df$Date)

Site.Wet.df <- Site.Wet.df %>%
  separate( dumDate, c("Year", "DumDate"), sep = -4) %>% 
  separate( DumDate, c("Month", "Day"), sep = 2) 

## ok lets filter out the unused SiteCodes

dim(Site.Wet.df) # 3287 x 15

Site.Wet.df <- Site.Wet.df %>%
               filter( SiteCode %in% Site.Info.df$SiteCode )
 

dim(Site.Wet.df) # 1872 X 15 wow, we cut out 1415 site assments

unique( Site.Wet.df$SiteCode ) #165 levels how does that compare to the Sitelist

unique( Site.Info.df$SiteCode ) # 169 levels, 4 SiteCodes don't have SiteAssmt


##### data clean up site wetland info ###

#Area
hist( log10( Site.Wet.df$Area + 1 ))
max( Site.Wet.df$Area, na.rm = T )## very large
Site.Wet.df[which.max( Site.Wet.df$Area),] # Ca-Glake is the large site just
                                           # going to put NA
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "CA-GLAKE_20150804"] <-NA


# Perim
hist( log10( Site.Wet.df$Perim + 1 ))
max( Site.Wet.df$Perim, na.rm = T )## very large
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-GLAKE_20150804"] <-NA



plot( x= log10(Site.Wet.df$Area+1), y= log10(Site.Wet.df$Perim + 1))

## Several points are not lining up so lets idenify and adjust
identify(x= log10(Site.Wet.df$Area+1), y= log10(Site.Wet.df$Perim + 1))

## Clearly Ca-Mccry should be 10414 not 10.414

Site.Wet.df$Area[Site.Wet.df$AssmtCode == "CA-MCCRY_20170525" ] <- 10414 
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-MCCRY_20170525" ] <- 550 

# Glake is just too weird i am goin to pu NA for all measurements
Site.Wet.df$Area[Site.Wet.df$SiteCode == "CA-GLAKE" ] <- NA
Site.Wet.df$Perim[Site.Wet.df$SiteCode == "CA-GLAKE" ] <- NA

## PRKING does not have an area of 2.875, should probably be 2875
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "PRKING_20150713" ] <- 2875

## Ca-BN016 perimeter is off at 1390, should be 139
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-BN016_20150519" ] <-  139

## BNPND011 area is off at 9087.98, should be 987.98
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "BNPND011_20160802" ]  <- 987.98

## Ca-Sf31 Perimeter is wrong changing it to 
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-SF31_20130722" ] <- 22.7

# Changing Mud65 area to 100 instead of 10
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "CA-MUD65_20160712" ] <- 100

# Changing area to 210.67 instead of just 21 
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "PRNTH2_20160726" ] <- 210.67 

# Changing area to 1260 instead of just 126
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "NBOR_20180518" ] <- 1260 

# Changing area from 140 to 1400
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "BNPND002_20190710" ] <- 1400

# Changing area from 69.8 to 698
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "MUD46_20140730" ] <- 698

# Changing area from 45.6 to 457
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "POGCP_20100716" ] <- 457

# Changing Perimter 
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-BN016_20100712" ] <- 157

# Changing Perim from 405 to 145
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRNTHMIT_20160804" ] <- 145

## changing area from 101.1 to 1011
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "RLSNKGCP_20180523" ] <- 1011

# Changing area from 37.3 to 373
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "VPPND004_20140724" ] <- 373

# Changing area from 38.8 to 138.8
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "WDMLGCP_20160606" ] <- 138.8

# Changing perimeter from 209 to 109
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "GDPND004_20160630" ]  <- 109

# Changing both Perim and area
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "HIDDEN_20130701" ]  <- 154

Site.Wet.df$Area[Site.Wet.df$AssmtCode == "HIDDEN_20130701" ] <- 838.3

## Changing area from 166 to 1660
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "PRPND003_20110519" ] <- 1660

# Changing Hidden area
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "HIDDEN_20190715" ] <- 1420

#Changing perimeter from 211 to 111
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRPND004_20180710" ] <- 111

# Changing perimter from 232 to 132

Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRNTH1_20100727" ] <- 132

## Changing perimeter of EDWD to 230 instead of 130

Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-EDWD_20100524" ] <- 230

## Changing perimeter of PRNTH2 form 55 to 155
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRNTH2_20190611" ] <- 155 


plot( x= log10(Site.Wet.df$Area+1), y= log10(Site.Wet.df$Perim + 1))
## ok i think fixing the perimeter was the most important step, feeling good
## about that now lets improve the rest, i think most things remaining is simply
## adding zeros or just deleting columns,

colSums(is.na(Site.Wet.df))

# lets add 0s to all the NAs in the vegetation data
Site.Wet.df$Juncus[is.na(Site.Wet.df$Juncus)] <- 0
Site.Wet.df$Typha[is.na(Site.Wet.df$Typha)] <- 0
Site.Wet.df$Other[is.na(Site.Wet.df$Other)] <- 0

## Combinding the two data frames
Site.df <- left_join(Site.Wet.df, Site.Info.df, by = "SiteCode")

## Ok lets check to make sure there are no outliers in perimeter or area
## for sites that only have one of these measurements

filter( Site.df, PropName == "5 CANYONS REGIONAL PARK")  %>%
      ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
      facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## 5 canyon looks good

## Silver oaks

filter( Site.df, PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## Early Barn perim and area seem high but probably a measurement issue not a 
## data issue

## Blue Oaks

filter( Site.df, PropName == "BLUE OAKS RANCH RESERVE")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "BLUE OAKS RANCH RESERVE")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## Blue Oaks looks good

## Grant
                                                   
filter( Site.df, PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## Grant is looking good, lots of variation , kind of loving it 

## Briones RP

filter( Site.df, PropName == "BRIONES REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "BRIONES REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


# Brinoes 003 early perimenter is way off change from 1212 to 121

Site.df$Perim[Site.df$AssmtCode == "CA-BN003_20100713" ]  <- 121


## EBMUD

filter( Site.df, PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## MUD is looking good

## San Felipe 

filter( Site.df, PropName == "SAN FELIPE RANCH")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "SAN FELIPE RANCH")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## looking good San Felipe

## Pleasanton RP

filter( Site.df, PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## PRPDN005 early is wrong, lets fix that should be 83 in steado f 283

Site.df$Perim[Site.df$AssmtCode == "PRPND005_20100729" ]  <- 83
Site.df$Area[Site.df$AssmtCode == "PRPND005_20100729" ]  <- 473

## Garin Dry Creek RP

filter( Site.df, PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## Garin looks good , man do i hate GDPND010

## Vargus RP

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

##### data clean up site water chemistry info info ###

str( Site.Water.df )

dim( Site.Water.df ) # 3174 X 24

## So based on the wetland data set I need to seperate the assmt code to get 
## a sitte code and date into the data but first lets clean up the dataset


Site.Water.df <- select( Site.Water.df, c("AssmtCode", "Conductivity..uS.cm.", 
                                      "TDS..g.l.", "Salinity",
                                      "pH", "Turbidity", "SecchiDepth..cm.",
                                      "NutrientSamples","TotalN..uMOLES.L.", 
                                      "TotalP..uMOLES.L.", "DOC.mg.C.L",
                                      "NH4...uEQ.L.", "DON..uM.L.",
                                      "PO4.3...uEQ.L.","IP..uM.L.",
                                      "DOP..uM.L.", "NO3..uEQ.L.",
                                      "IN..uM.L.") )

# simplifying column names

colnames( Site.Water.df ) <- c("AssmtCode", "Conduct", 
                               "TDS", "Salinity",
                               "pH", "Turbid", "Secchi",
                               "NutrientSamples","TotalN", 
                               "TotalP", "DOC",
                               "NH4", "DON",
                               "PO4","IP",
                               "DOP", "NO3",
                               "IN")

## Capitalizing all the AssmtCode

Site.Water.df $AssmtCode <- as.factor( toupper( Site.Water.df$AssmtCode ) )

## creating a dummy assmtcode to seperate the SiteCode and Date

Site.Water.df$dumAssmtCode <- Site.Water.df$AssmtCode 


Site.Water.df <- Site.Water.df %>%
  separate( dumAssmtCode, c("SiteCode", "Date"), sep =-9) %>% 
  separate( Date, c("Throw1", "Date"), sep =-8) %>%
  select( -"Throw1")

Site.Water.df$SiteCode <- as.factor(Site.Water.df$SiteCode)


## Seperating by month year and date

Site.Water.df$dumDate <- as.numeric(Site.Water.df$Date)

Site.Water.df <- Site.Water.df %>%
  separate( dumDate, c("Year", "DumDate"), sep = -4) %>% 
  separate( DumDate, c("Month", "Day"), sep = 2) 

## ok lets filter out the unused SiteCodes

dim(Site.Water.df) # 3174 x 20

Site.Water.df <-Site.Water.df %>%
  filter( SiteCode %in% Site.Info.df$SiteCode )


dim(Site.Water.df) # 1836 X 20 wow, we cut out 1338 site assments

unique( Site.Water.df$SiteCode ) #165 levels how does that compare to the Sitelist

unique( Site.Info.df$SiteCode ) # 169 levels, 4 SiteCodes don't have SiteAssmt

## cool lets clean up so data

##first lets look at the relationship betweem conductivity and salinity

hist( log10(Site.Water.df$TotalP+1 ))

plot( x = log10(Site.Water.df$Conduct+1), log10(Site.Water.df$TDS+1))

ggplot( Site.Water.df, aes(x = log10(Conduct+1), log10(TDS+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
  legend.text=element_text(size=18), 
  legend.title=element_text( size=18)) + facet_wrap(~Year)

ggplot( Site.Water.df, aes(x = log10(Conduct+1), log10(Salinity+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

ggplot( Site.Water.df, aes(x = log10(TDS+1), log10(Salinity+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

ggplot( Site.Water.df, aes(x = log10(TotalP+1), log10(TotalN+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

## Ok lets beging to look at density plots

## Turbidity ##
ggplot( Site.Water.df, aes( log10(Turbid +1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

## Turbidity looking pretty good, some potentialyl interesting results

## PH ##
ggplot( Site.Water.df, aes( pH ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## ph looking good

## Conductivity ##
ggplot( Site.Water.df, aes( log10(Conduct+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## Conductivity looking constant
 
## Salinity ##

ggplot( Site.Water.df, aes( log10(Salinity+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## hmm not just a a potneital shift in unit but also distributuion, more
## broad in later years

## TDS ##

ggplot( Site.Water.df, aes( log10(TDS+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## Similare patterns with TDS as with Salinity , probably connected or at least
## similare issue, hopefully one fix for all 

## TotalN

ggplot( Site.Water.df, aes( log10(TotalN+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## looking normal

## TotalP

ggplot( Site.Water.df, aes( log10(TotalP+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## looking normal expter for 2012 i wonder how much these daynamics are based
## on when the data is collected 

ggplot( Site.Water.df, aes( log10(DOC+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")


## Ok i am going to assume most of the nutrient data is consitent since we 
## dont run it so happy to move on

## lets combind all the data shall we and first lets simplify the data set we
## want



Site.df <- Site.Water.df %>% 
      select( c("AssmtCode","Conduct","TDS" ,"Salinity","pH" ,            
                 "Turbid"  ,"Secchi", "NutrientSamples" ,"TotalN" , "TotalP"  ,      
                 "DOC" , "NH4", "DON","PO4" , "IP"    ,         
                "DOP","NO3" , "IN" ) ) %>%
          right_join(Site.df, by = "AssmtCode")

at1 <- Site.df

at1$TDS[at1$Year == 2009 ] <-  at1$TDS[at1$Year == 2009 ] * 1000
at1$TDS[at1$Year == 2010 ] <-  at1$TDS[at1$Year == 2010 ] * 1000
at1$TDS[at1$Year == 2011 ] <-  at1$TDS[at1$Year == 2011 ] * 1000
at1$TDS[at1$Year == 2012 ] <-  at1$TDS[at1$Year == 2012 ] * 1000
at1$TDS[at1$Year == 2013 ] <-  at1$TDS[at1$Year == 2013 ] * 1000
at1$TDS[at1$Year == 2014 ] <-  at1$TDS[at1$Year == 2014 ] * 1000

ggplot( at1, aes(x = log10(Conduct+1), log10(TDS+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

ggplot( Site.df, aes(x = log10( Conduct+1 ), log10( TDS + 1 ), color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() + 
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

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


