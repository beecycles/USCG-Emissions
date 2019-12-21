###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for Current YEAR (2018) emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY19_calls = read.csv("CY19_calls.csv", header = TRUE)
CY19_incident_details = read.csv("CY19_incident_details.csv", header = TRUE)
CY19_incident_commons = read.csv("CY19_incident_commons.csv", header = TRUE)
CY19_material_involved = read.csv("CY19_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY19 = full_join(CY19_calls, CY19_incident_commons)
CY19 = full_join(CY19, CY19_incident_details)
CY19 = full_join(CY19, CY19_material_involved)
#View(CY19)
dim(CY19)

# explore LA records
sort(table(CY19$RESPONSIBLE_STATE))
sort(table(CY19$LOCATION_STATE))
head(sort(table(CY19$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY19 = CY19 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY19)

# look at how many were evacuated per state
EvacuationsCY19 = CY19 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY19)

# Number injured
InjuredCY19 = CY19 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY19)

# Fatalities by state
FatalitiesCY19 = CY19 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY19)

# Road closure time by state
Road_closureCY19 = CY19 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY19)

# Medium description by state
medium_descCY19 = filter(CY19, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
View(medium_descCY19)

# sources of spills into MISSISSIPPI description by state
mississippiCY19 = filter(CY19, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY19)

# sources of spills into GULF OF MEXICO description by state
gulfCY19 = filter(CY19, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCurrent)

# material spilled == OIL: CRUDE
library(stringr)
crudeCY19 = filter(CY19, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY19)

# join all state columns
sumCY19 = full_join(SpillsCY19, EvacuationsCY19)
sumCY19 = full_join(sumCY19, InjuredCY19)
sumCY19 = full_join(sumCY19, FatalitiesCY19)
sumCY19 = full_join(sumCY19, Road_closureCY19)
sumCY19 = full_join(sumCY19, medium_descCY19)
sumCY19 = full_join(sumCY19, mississippiCY19)
sumCY19 = full_join(sumCY19, gulfCY19)
sumCY19 = full_join(sumCY19, crudeCY19)
sumCY19["Year"]="2019"
View(sumCY19)

# companies responsible for of spills in Louisiana
companiesCY19 = filter(CY19, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY19_Spills = n())
#View(companiesCY19)
