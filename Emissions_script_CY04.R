###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for 2004 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY04_calls = read.csv("CY04_calls.csv", header = TRUE)
CY04_incident_details = read.csv("CY04_incident_details.csv", header = TRUE)
CY04_incident_commons = read.csv("CY04_incident_commons.csv", header = TRUE)
CY04_material_involved = read.csv("CY04_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY04 = full_join(CY04_calls, CY04_incident_commons)
CY04 = full_join(CY04, CY04_incident_details)
CY04 = full_join(CY04, CY04_material_involved)
#View(CY04)
dim(CY04)

# explore LA records
sort(table(CY04$RESPONSIBLE_STATE))
sort(table(CY04$LOCATION_STATE))
head(sort(table(CY04$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY04 = CY04 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY04)

# look at how many were evacuated per state
EvacuationsCY04 = CY04 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY04)

# Number injured
InjuredCY04 = CY04 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY04)

# Fatalities by state
FatalitiesCY04 = CY04 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY04)

# Road closure time by state
Road_closureCY04 = CY04 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY04)

# Medium description by state
medium_descCY04 = filter(CY04, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
View(medium_descCY04)

# sources of spills into MISSISSIPPI description by state
mississippiCY04 = filter(CY04, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY04)

# sources of spills into GULF OF MEXICO description by state
gulfCY04 = filter(CY04, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCurrent)

# material spilled == OIL: CRUDE
library(stringr)
crudeCY04 = filter(CY04, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY04)

# join all state columns
sumCY04 = full_join(SpillsCY04, EvacuationsCY04)
sumCY04 = full_join(sumCY04, InjuredCY04)
sumCY04 = full_join(sumCY04, FatalitiesCY04)
sumCY04 = full_join(sumCY04, Road_closureCY04)
sumCY04 = full_join(sumCY04, medium_descCY04)
sumCY04 = full_join(sumCY04, mississippiCY04)
sumCY04 = full_join(sumCY04, gulfCY04)
sumCY04 = full_join(sumCY04, crudeCY04)
sumCY04["Year"]="2004"
View(sumCY04)

# companies responsible for of spills in Louisiana
companiesCY04 = filter(CY04, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY04_Spills = n())
#View(companiesCY04)
