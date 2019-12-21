###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2007 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY07_calls = read.csv("CY07_calls.csv", header = TRUE)
CY07_incident_details = read.csv("CY07_incident_details.csv", header = TRUE)
CY07_incident_commons = read.csv("CY07_incident_commons.csv", header = TRUE)
CY07_material_involved = read.csv("CY07_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY07 = full_join(CY07_calls, CY07_incident_commons)
CY07 = full_join(CY07, CY07_incident_details)
CY07 = full_join(CY07, CY07_material_involved)
#View(CY07)
dim(CY07)

# explore LA records
sort(table(CY07$RESPONSIBLE_STATE))
sort(table(CY07$LOCATION_STATE))
head(sort(table(CY07$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY07 = CY07 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY07)

# look at how many were evacuated per state
EvacuationsCY07 = CY07 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY07)

# Number injured
InjuredCY07 = CY07 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY07)

# Fatalities by state
FatalitiesCY07 = CY07 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY07)

# Road closure time by state
Road_closureCY07 = CY07 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY07)

# Medium description by state
medium_descCY07 = filter(CY07, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY07)

# sources of spills into MISSISSIPPI description by state
mississippiCY07 = filter(CY07, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY07)

# sources of spills into GULF OF MEXICO description by state
gulfCY07 = filter(CY07, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY07)

# oil spilled into water
library(stringr)
crudeCY07 = filter(CY07, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY07)

# join all state columns
sumCY07 = full_join(SpillsCY07, EvacuationsCY07)
sumCY07 = full_join(sumCY07, InjuredCY07)
sumCY07 = full_join(sumCY07, FatalitiesCY07)
sumCY07 = full_join(sumCY07, Road_closureCY07)
sumCY07 = full_join(sumCY07, medium_descCY07)
sumCY07 = full_join(sumCY07, mississippiCY07)
sumCY07 = full_join(sumCY07, gulfCY07)
sumCY07 = full_join(sumCY07, crudeCY07)
sumCY07["Year"]="2007"
View(sumCY07)

# companies responsible for of spills in Louisiana
companiesCY07 = filter(CY07, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY07_Spills = n())
#View(companiesCY07)
