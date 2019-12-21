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
all_materials = full_join(CY04[ ,-53], CY05[ , -53])
all_materials = full_join(all_materials, CY06[ ,-53])
all_materials = full_join(all_materials, CY07[ ,-53])
all_materials = full_join(all_materials, CY08[ ,-53])
all_materials = full_join(all_materials, CY09[ ,-53])
all_materials = full_join(all_materials, CY10[ ,-53])
all_materials = full_join(all_materials, CY11[ ,-53])
all_materials = full_join(all_materials, CY12[ ,-53])
all_materials = full_join(all_materials, CY13[ ,-53])
all_materials = full_join(all_materials, CY14[ ,-53])
all_materials = full_join(all_materials, CY15[ ,-53])
all_materials = full_join(all_materials, CY16[ ,-53])
all_materials = full_join(all_materials, CY17[ ,-53])
all_materials = full_join(all_materials, CY18[ ,-53])
all_materials = full_join(all_materials, CY19[ ,-53])
dim(all_materials)
head(all_materials)

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

LA_materials = LA_materials[ -c(69:119)]
LA_materials = LA_materials[ -c(27:65)]

# count spill incidences of certain materials
Materials = LA_MISS_materials %>%
  group_by(NAME_OF_MATERIAL) %>%
  summarise(
    Spill_Count_by_material = n(), 
    Quantity = sum(AMOUNT_IN_WATER))
View(Materials)
write.csv(Materials, "Materials.csv", row.names = FALSE)

all_Materials = LA_materials %>%
  group_by(NAME_OF_MATERIAL) %>%
  summarise(
    Spill_Count_by_material = n(), 
    Quantity = sum(AMOUNT_IN_WATER))
View(all_Materials)

# count spill incidences of certain responsible company
Companies = LA_MISS_materials %>%
  filter(RESPONSIBLE_ORG_TYPE == "PRIVATE ENTERPRISE") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    Spill_Count_by_company = n())
View(Companies)
write.csv(Companies, "Companies.csv", row.names = FALSE)

all_Companies = LA_materials %>%
  filter(RESPONSIBLE_ORG_TYPE == "PRIVATE ENTERPRISE") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    Spill_Count_by_company = n())
View(all_Companies)

# total spill quantities of certain responsible company
Org_Type = LA_MISS_materials %>%
  group_by(RESPONSIBLE_ORG_TYPE) %>%
  summarise(
    Count_by_type = n())
View(Org_Type)
write.csv(Org_Type, "Org_type.csv", row.names = FALSE)
