###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2005 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY05_calls = read.csv("CY05_calls.csv", header = TRUE)
CY05_incident_details = read.csv("CY05_incident_details.csv", header = TRUE)
CY05_incident_commons = read.csv("CY05_incident_commons.csv", header = TRUE)
CY05_material_involved = read.csv("CY05_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY05 = full_join(CY05_calls, CY05_incident_commons)
CY05 = full_join(CY05, CY05_incident_details)
CY05 = full_join(CY05, CY05_material_involved)
View(CY05)
dim(CY05)

# explore LA records
sort(table(CY05$RESPONSIBLE_STATE))
sort(table(CY05$LOCATION_STATE))
head(sort(table(CY05$BODY_OF_WATER), decreasing = TRUE), 20)

# look at how many were evacuated per state
EvacuationsCY05 = CY05 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
View(EvacuationsCY05)

# Number injured
InjuredCY05 = CY05 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
View(InjuredCY05)

# Fatalities by state
FatalitiesCY05 = CY05 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
View(FatalitiesCY05)

# Road closure time by state
Road_closureCY05 = CY05 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
View(Road_closureCY05)

# Medium description by state
medium_descCY05 = filter(CY05, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Spills = n())
View(medium_descCY05)

# sources of spills into MISSISSIPPI description by state
mississippiCY05 = filter(CY05, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Spills = n())
View(mississippiCY05)

# sources of spills into GULF OF MEXICO description by state
gulfCY05 = filter(CY05, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Spills = n())
View(gulfCY05)

# companies responsible for of spills in Louisiana
companiesCY05 = filter(CY05, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    Spills = n())
View(companiesCY05)
