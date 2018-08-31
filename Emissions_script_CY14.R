###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2014 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY14_calls = read.csv("CY14_calls.csv", header = TRUE)
CY14_incident_details = read.csv("CY14_incident_details.csv", header = TRUE)
CY14_incident_commons = read.csv("CY14_incident_commons.csv", header = TRUE)
CY14_material_involved = read.csv("CY14_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY14 = full_join(CY14_calls, CY14_incident_commons)
CY14 = full_join(CY14, CY14_incident_details)
CY14 = full_join(CY14, CY14_material_involved)
#View(CY14)
dim(CY14)

# explore LA records
sort(table(CY14$RESPONSIBLE_STATE))
sort(table(CY14$LOCATION_STATE))
head(sort(table(CY14$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY14 = CY14 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY14)

# look at how many were evacuated per state
EvacuationsCY14 = CY14 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY14)

# Number injured
InjuredCY14 = CY14 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY14)

# Fatalities by state
FatalitiesCY14 = CY14 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY14)

# Road closure time by state
Road_closureCY14 = CY14 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY14)

# Medium description by state
medium_descCY14 = filter(CY14, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY14)

# sources of spills into MISSISSIPPI description by state
mississippiCY14 = filter(CY14, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY14)

# sources of spills into GULF OF MEXICO description by state
gulfCY14 = filter(CY14, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY14)

# join all state columns
sumCY14 = full_join(SpillsCY14, EvacuationsCY14)
sumCY14 = full_join(sumCY14, InjuredCY14)
sumCY14 = full_join(sumCY14, FatalitiesCY14)
sumCY14 = full_join(sumCY14, Road_closureCY14)
sumCY14 = full_join(sumCY14, medium_descCY14)
sumCY14 = full_join(sumCY14, mississippiCY14)
sumCY14 = full_join(sumCY14, gulfCY14)
sumCY14["Year"]="2014"
View(sumCY14)

# companies responsible for of spills in Louisiana
companiesCY14 = filter(CY14, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY14_Spills = n())
#View(companiesCY14)
