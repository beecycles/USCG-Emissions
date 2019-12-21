###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2015 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY15_calls = read.csv("CY15_calls.csv", header = TRUE)
CY15_incident_details = read.csv("CY15_incident_details.csv", header = TRUE)
CY15_incident_commons = read.csv("CY15_incident_commons.csv", header = TRUE)
CY15_material_involved = read.csv("CY15_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY15 = full_join(CY15_calls, CY15_incident_commons)
CY15 = full_join(CY15, CY15_incident_details)
CY15 = full_join(CY15, CY15_material_involved)
#View(CY15)
dim(CY15)

# explore LA records
sort(table(CY15$RESPONSIBLE_STATE))
sort(table(CY15$LOCATION_STATE))
head(sort(table(CY15$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY15 = CY15 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY15)

# look at how many were evacuated per state
EvacuationsCY15 = CY15 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY15)

# Number injured
InjuredCY15 = CY15 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY15)

# Fatalities by state
FatalitiesCY15 = CY15 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY15)

# Road closure time by state
Road_closureCY15 = CY15 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY15)

# Medium description by state
medium_descCY15 = filter(CY15, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY15)

# sources of spills into MISSISSIPPI description by state
mississippiCY15 = filter(CY15, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY15)

# sources of spills into GULF OF MEXICO description by state
gulfCY15 = filter(CY15, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY15)

# material spilled == OIL: CRUDE
library(stringr)
crudeCY15 = filter(CY15, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY15)

# join all state columns
sumCY15 = full_join(SpillsCY15, EvacuationsCY15)
sumCY15 = full_join(sumCY15, InjuredCY15)
sumCY15 = full_join(sumCY15, FatalitiesCY15)
sumCY15 = full_join(sumCY15, Road_closureCY15)
sumCY15 = full_join(sumCY15, medium_descCY15)
sumCY15 = full_join(sumCY15, mississippiCY15)
sumCY15 = full_join(sumCY15, gulfCY15)
sumCY15 = full_join(sumCY15, crudeCY15)
sumCY15["Year"]="2015"
View(sumCY15)

# companies responsible for of spills in Louisiana
companiesCY15 = filter(CY15, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY15_Spills = n())
#View(companiesCY15)
