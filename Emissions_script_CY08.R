###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2008 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY08_calls = read.csv("CY08_calls.csv", header = TRUE)
CY08_incident_details = read.csv("CY08_incident_details.csv", header = TRUE)
CY08_incident_commons = read.csv("CY08_incident_commons.csv", header = TRUE)
CY08_material_involved = read.csv("CY08_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY08 = full_join(CY08_calls, CY08_incident_commons)
CY08 = full_join(CY08, CY08_incident_details)
CY08 = full_join(CY08, CY08_material_involved)
#View(CY08)
dim(CY08)

# explore LA records
sort(table(CY08$RESPONSIBLE_STATE))
sort(table(CY08$LOCATION_STATE))
head(sort(table(CY08$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY08 = CY08 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY08)

# look at how many were evacuated per state
EvacuationsCY08 = CY08 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY08)

# Number injured
InjuredCY08 = CY08 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY08)

# Fatalities by state
FatalitiesCY08 = CY08 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY08)

# Road closure time by state
Road_closureCY08 = CY08 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY08)

# Medium description by state
medium_descCY08 = filter(CY08, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY08)

# sources of spills into MISSISSIPPI description by state
mississippiCY08 = filter(CY08, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY08)

# sources of spills into GULF OF MEXICO description by state
gulfCY08 = filter(CY08, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY08)

# join all state columns
sumCY08 = full_join(SpillsCY08, EvacuationsCY08)
sumCY08 = full_join(sumCY08, InjuredCY08)
sumCY08 = full_join(sumCY08, FatalitiesCY08)
sumCY08 = full_join(sumCY08, Road_closureCY08)
sumCY08 = full_join(sumCY08, medium_descCY08)
sumCY08 = full_join(sumCY08, mississippiCY08)
sumCY08 = full_join(sumCY08, gulfCY08)
sumCY08["Year"]="2008"
View(sumCY08)

# companies responsible for of spills in Louisiana
companiesCY08 = filter(CY08, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY08_Spills = n())
#View(companiesCY08)
