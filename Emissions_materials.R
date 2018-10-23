###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
################################### Joan Meiners 2018 #############################
############################ MATERIALS and PARTY RESPONSIBLE ######################

### This script is for combining all years of emissions data. See separate scripts for specific years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)


# run other yearly scripts first to generate objects to combine
all_materials = full_join(CY08[ ,-53], CY09[ , -53])
all_materials = full_join(all_materials, CY10[ ,-53])
all_materials = full_join(all_materials, CY11[ ,-53])
all_materials = full_join(all_materials, CY12[ ,-53])
all_materials = full_join(all_materials, CY13[ ,-53])
all_materials = full_join(all_materials, CY14[ ,-53])
all_materials = full_join(all_materials, CY15[ ,-53])
all_materials = full_join(all_materials, CY16[ ,-53])
all_materials = full_join(all_materials, CY17[ ,-53])
dim(all_materials)


# separate combined dataset that only includes records of spills in Louisiana
unique(all_materials$LOCATION_STATE)
LA_materials = subset(all_materials, LOCATION_STATE == "LA")
dim(LA_materials)
names(LA_materials)
LA_MISS_materials = subset(LA_materials, BODY_OF_WATER == "MISSISSIPPI RIVER")
dim(LA_MISS_materials)
names(LA_MISS_materials)
LA_MISS_materials = LA_MISS_materials[ -c(69:119)]
LA_MISS_materials = LA_MISS_materials[ -c(27:65)]

# count spill incidences of certain materials
Materials = LA_MISS_materials %>%
  group_by(NAME_OF_MATERIAL) %>%
  summarise(
    Spill_Count = n(), 
    Quantity = sum(AMOUNT_IN_WATER))
View(Materials)

# count spill incidences of certain responsible company
Companies = LA_MISS_materials %>%
  filter(RESPONSIBLE_ORG_TYPE == "PRIVATE ENTERPRISE") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    Count_by_company = n())
View(Companies)

# total spill quantities of certain responsible company
Org_Type = LA_MISS_materials %>%
  group_by(RESPONSIBLE_ORG_TYPE) %>%
  summarise(
    Count_by_type = n())
View(Org_Type)
