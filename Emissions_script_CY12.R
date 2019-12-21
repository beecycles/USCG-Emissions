###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2012 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY12_calls = read.csv("CY12_calls.csv", header = TRUE)
CY12_incident_details = read.csv("CY12_incident_details.csv", header = TRUE)
CY12_incident_commons = read.csv("CY12_incident_commons.csv", header = TRUE)
CY12_material_involved = read.csv("CY12_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY12 = full_join(CY12_calls, CY12_incident_commons)
CY12 = full_join(CY12, CY12_incident_details)
CY12 = full_join(CY12, CY12_material_involved)
#View(CY12)
dim(CY12)

# explore LA records
sort(table(CY12$RESPONSIBLE_STATE))
sort(table(CY12$LOCATION_STATE))
head(sort(table(CY12$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY12 = CY12 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY12)

# look at how many were evacuated per state
EvacuationsCY12 = CY12 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY12)

# Number injured
InjuredCY12 = CY12 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY12)

# Fatalities by state
FatalitiesCY12 = CY12 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY12)

# Road closure time by state
Road_closureCY12 = CY12 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY12)

# Medium description by state
medium_descCY12 = filter(CY12, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY12)

# sources of spills into MISSISSIPPI description by state
mississippiCY12 = filter(CY12, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY12)

# sources of spills into GULF OF MEXICO description by state
gulfCY12 = filter(CY12, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY12)

# material spilled == OIL: CRUDE
library(stringr)
crudeCY12 = filter(CY12, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY12)

# join all state columns
sumCY12 = full_join(SpillsCY12, EvacuationsCY12)
sumCY12 = full_join(sumCY12, InjuredCY12)
sumCY12 = full_join(sumCY12, FatalitiesCY12)
sumCY12 = full_join(sumCY12, Road_closureCY12)
sumCY12 = full_join(sumCY12, medium_descCY12)
sumCY12 = full_join(sumCY12, mississippiCY12)
sumCY12 = full_join(sumCY12, gulfCY12)
sumCY12 = full_join(sumCY12, crudeCY12)
sumCY12["Year"]="2012"
View(sumCY12)

# companies responsible for of spills in Louisiana
companiesCY12 = filter(CY12, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY12_Spills = n())
#View(companiesCY12)
