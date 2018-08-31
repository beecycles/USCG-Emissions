###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for Current YEAR (2018) emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
Current_calls = read.csv("Current_calls.csv", header = TRUE)
Current_incident_details = read.csv("Current_incident_details.csv", header = TRUE)
Current_incident_commons = read.csv("Current_incident_commons.csv", header = TRUE)
Current_material_involved = read.csv("Current_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
Current = full_join(Current_calls, Current_incident_commons)
Current = full_join(Current, Current_incident_details)
Current = full_join(Current, Current_material_involved)
View(Current)
dim(Current)

# explore LA records
sort(table(Current$RESPONSIBLE_STATE))
sort(table(Current$LOCATION_STATE))
head(sort(table(Current$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCurrent = Current %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Current_Spills = n())
View(SpillsCurrent)

# look at how many were evacuated per state
EvacuationsCurrent = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    Current_Spills = n(),
    Current_TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
View(EvacuationsCurrent)

# Number injured
InjuredCurrent = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    Current_Spills = n(),
    Current_TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
View(InjuredCurrent)

# Fatalities by state
FatalitiesCurrent = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    Current_Spills = n(),
    Current_TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
View(FatalitiesCurrent)

# Road closure time by state
Road_closureCurrent = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    Current_Spills = n(),
    Current_TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
View(Road_closureCurrent)

# Medium description by state
medium_descCurrent = filter(Current, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Current_Spills = n())
View(medium_descCurrent)

# sources of spills into MISSISSIPPI description by state
mississippiCurrent = filter(Current, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Current_Spills = n())
View(mississippiCurrent)

# sources of spills into GULF OF MEXICO description by state
gulfCurrent = filter(Current, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Current_Spills = n())
View(gulfCurrent)

# companies responsible for of spills in Louisiana
companiesCurrent = filter(Current, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    Current_Spills = n())
View(companiesCurrent)
