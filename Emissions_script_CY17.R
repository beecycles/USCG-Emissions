###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2017 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY17_calls = read.csv("CY17_calls.csv", header = TRUE)
CY17_incident_details = read.csv("CY17_incident_details.csv", header = TRUE)
CY17_incident_commons = read.csv("CY17_incident_commons.csv", header = TRUE)
CY17_material_involved = read.csv("CY17_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY17 = full_join(CY17_calls, CY17_incident_commons)
CY17 = full_join(CY17, CY17_incident_details)
CY17 = full_join(CY17, CY17_material_involved)
View(CY17)
dim(CY17)

# explore LA records
sort(table(CY17$RESPONSIBLE_STATE))
sort(table(CY17$LOCATION_STATE))
head(sort(table(CY17$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY17 = CY17 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY17_Spills = n())
View(SpillsCY17)

# look at how many were evacuated per state
EvacuationsCY17 = CY17 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    CY17_Spills = n(),
    CY17_TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
View(EvacuationsCY17)

# Number injured
InjuredCY17 = CY17 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    CY17_Spills = n(),
    CY17_TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
View(InjuredCY17)

# Fatalities by state
FatalitiesCY17 = CY17 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    CY17_Spills = n(),
    CY17_TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
View(FatalitiesCY17)

# Road closure time by state
Road_closureCY17 = CY17 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    CY17_Spills = n(),
    CY17_TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
View(Road_closureCY17)

# Medium description by state
medium_descCY17 = filter(CY17, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY17_Spills = n())
View(medium_descCY17)

# sources of spills into MISSISSIPPI description by state
mississippiCY17 = filter(CY17, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY17_Spills = n())
View(mississippiCY17)

# sources of spills into GULF OF MEXICO description by state
gulfCY17 = filter(CY17, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY17_Spills = n())
View(gulfCY17)

# companies responsible for of spills in Louisiana
companiesCY17 = filter(CY17, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY17_Spills = n())
View(companiesCY17)
