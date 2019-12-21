###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for Current YEAR (2018) emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY18_calls = read.csv("CY18_calls.csv", header = TRUE)
CY18_incident_details = read.csv("CY18_incident_details.csv", header = TRUE)
CY18_incident_commons = read.csv("CY18_incident_commons.csv", header = TRUE)
CY18_material_involved = read.csv("CY18_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY18 = full_join(CY18_calls, CY18_incident_commons)
CY18 = full_join(CY18, CY18_incident_details)
CY18 = full_join(CY18, CY18_material_involved)
#View(CY18)
dim(CY18)

# explore LA records
sort(table(CY18$RESPONSIBLE_STATE))
sort(table(CY18$LOCATION_STATE))
head(sort(table(CY18$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY18 = CY18 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY18)

# look at how many were evacuated per state
EvacuationsCY18 = CY18 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY18)

# Number injured
InjuredCY18 = CY18 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY18)

# Fatalities by state
FatalitiesCY18 = CY18 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY18)

# Road closure time by state
Road_closureCY18 = CY18 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY18)

# Medium description by state
medium_descCY18 = filter(CY18, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
View(medium_descCY18)

# sources of spills into MISSISSIPPI description by state
mississippiCY18 = filter(CY18, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY18)

# sources of spills into GULF OF MEXICO description by state
gulfCY18 = filter(CY18, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCurrent)

# material spilled == OIL: CRUDE
library(stringr)
crudeCY18 = filter(CY18, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY18)

# join all state columns
sumCY18 = full_join(SpillsCY18, EvacuationsCY18)
sumCY18 = full_join(sumCY18, InjuredCY18)
sumCY18 = full_join(sumCY18, FatalitiesCY18)
sumCY18 = full_join(sumCY18, Road_closureCY18)
sumCY18 = full_join(sumCY18, medium_descCY18)
sumCY18 = full_join(sumCY18, mississippiCY18)
sumCY18 = full_join(sumCY18, gulfCY18)
sumCY18 = full_join(sumCY18, crudeCY18)
sumCY18["Year"]="2018"
View(sumCY18)

# companies responsible for of spills in Louisiana
companiesCY18 = filter(CY18, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY18_Spills = n())
#View(companiesCY18)
