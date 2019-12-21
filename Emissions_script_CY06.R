###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2005 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY06_calls = read.csv("CY06_calls.csv", header = TRUE)
CY06_incident_details = read.csv("CY06_incident_details.csv", header = TRUE)
CY06_incident_commons = read.csv("CY06_incident_commons.csv", header = TRUE)
CY06_material_involved = read.csv("CY06_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY06 = full_join(CY06_calls, CY06_incident_commons)
CY06 = full_join(CY06, CY06_incident_details)
CY06 = full_join(CY06, CY06_material_involved)
#View(CY06)
dim(CY06)

# explore LA records
sort(table(CY06$RESPONSIBLE_STATE))
sort(table(CY06$LOCATION_STATE))
head(sort(table(CY06$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY06 = CY06 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY06)

# look at how many were evacuated per state
EvacuationsCY06 = CY06 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY06)

# Number injured
InjuredCY06 = CY06 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY06)

# Fatalities by state
FatalitiesCY06 = CY06 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY06)

# Road closure time by state
Road_closureCY06 = CY06 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY06)

# Medium description by state
medium_descCY06 = filter(CY06, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY06)

# sources of spills into MISSISSIPPI description by state
mississippiCY06 = filter(CY06, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY06)

# sources of spills into GULF OF MEXICO description by state
gulfCY06 = filter(CY06, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY06)

# oil spilled into water
library(stringr)
crudeCY06 = filter(CY06, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY06)

# join all state columns
sumCY06 = full_join(SpillsCY06, EvacuationsCY06)
sumCY06 = full_join(sumCY06, InjuredCY06)
sumCY06 = full_join(sumCY06, FatalitiesCY06)
sumCY06 = full_join(sumCY06, Road_closureCY06)
sumCY06 = full_join(sumCY06, medium_descCY06)
sumCY06 = full_join(sumCY06, mississippiCY06)
sumCY06 = full_join(sumCY06, gulfCY06)
sumCY06 = full_join(sumCY06, crudeCY06)
sumCY06["Year"]="2006"
View(sumCY06)

# companies responsible for of spills in Louisiana
companiesCY06 = filter(CY06, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY06_Spills = n())
#View(companiesCY06)
