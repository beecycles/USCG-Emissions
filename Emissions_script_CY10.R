###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2010 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY10_calls = read.csv("CY10_calls.csv", header = TRUE)
CY10_incident_details = read.csv("CY10_incident_details.csv", header = TRUE)
CY10_incident_commons = read.csv("CY10_incident_commons.csv", header = TRUE)
CY10_material_involved = read.csv("CY10_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY10 = full_join(CY10_calls, CY10_incident_commons)
CY10 = full_join(CY10, CY10_incident_details)
CY10 = full_join(CY10, CY10_material_involved)
View(CY10)
dim(CY10)

# explore LA records
sort(table(CY10$RESPONSIBLE_STATE))
sort(table(CY10$LOCATION_STATE))
head(sort(table(CY10$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY10 = CY10 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY10_Spills = n())
View(SpillsCY10)

# look at how many were evacuated per state
EvacuationsCY10 = CY10 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    CY10_Spills = n(),
    CY10_TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
View(EvacuationsCY10)

# Number injured
InjuredCY10 = CY10 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    CY10_Spills = n(),
    CY10_TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
View(InjuredCY10)

# Fatalities by state
FatalitiesCY10 = CY10 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    CY10_Spills = n(),
    CY10_TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
View(FatalitiesCY10)

# Road closure time by state
Road_closureCY10 = CY10 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    CY10_Spills = n(),
    CY10_TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
View(Road_closureCY10)

# Medium description by state
medium_descCY10 = filter(CY10, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY10_Spills = n())
View(medium_descCY10)

# sources of spills into MISSISSIPPI description by state
mississippiCY10 = filter(CY10, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY10_Spills = n())
View(mississippiCY10)

# sources of spills into GULF OF MEXICO description by state
gulfCY10 = filter(CY10, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    CY10_Spills = n())
View(gulfCY10)

# companies responsible for of spills in Louisiana
companiesCY10 = filter(CY10, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY10_Spills = n())
View(companiesCY10)
