###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2016 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY16_calls = read.csv("CY16_calls.csv", header = TRUE)
CY16_incident_details = read.csv("CY16_incident_details.csv", header = TRUE)
CY16_incident_commons = read.csv("CY16_incident_commons.csv", header = TRUE)
CY16_material_involved = read.csv("CY16_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY16 = full_join(CY16_calls, CY16_incident_commons)
CY16 = full_join(CY16, CY16_incident_details)
CY16 = full_join(CY16, CY16_material_involved)
#View(CY16)
dim(CY16)

# explore LA records
sort(table(CY16$RESPONSIBLE_STATE))
sort(table(CY16$LOCATION_STATE))
head(sort(table(CY16$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY16 = CY16 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY16)

# look at how many were evacuated per state
EvacuationsCY16 = CY16 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY16)

# Number injured
InjuredCY16 = CY16 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY16)

# Fatalities by state
FatalitiesCY16 = CY16 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY16)

# Road closure time by state
Road_closureCY16 = CY16 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY16)

# Medium description by state
medium_descCY16 = filter(CY16, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY16)

# sources of spills into MISSISSIPPI description by state
mississippiCY16 = filter(CY16, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY16)

# sources of spills into GULF OF MEXICO description by state
gulfCY16 = filter(CY16, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY16)

# join all state columns
sumCY16 = full_join(SpillsCY16, EvacuationsCY16)
sumCY16 = full_join(sumCY16, InjuredCY16)
sumCY16 = full_join(sumCY16, FatalitiesCY16)
sumCY16 = full_join(sumCY16, Road_closureCY16)
sumCY16 = full_join(sumCY16, medium_descCY16)
sumCY16 = full_join(sumCY16, mississippiCY16)
sumCY16 = full_join(sumCY16, gulfCY16)
sumCY16["Year"]="2016"
View(sumCY16)

# companies responsible for of spills in Louisiana
companiesCY16 = filter(CY16, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY16_Spills = n())
#View(companiesCY16)
