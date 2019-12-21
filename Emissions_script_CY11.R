###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2011 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY11_calls = read.csv("CY11_calls.csv", header = TRUE)
CY11_incident_details = read.csv("CY11_incident_details.csv", header = TRUE)
CY11_incident_commons = read.csv("CY11_incident_commons.csv", header = TRUE)
CY11_material_involved = read.csv("CY11_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY11 = full_join(CY11_calls, CY11_incident_commons)
CY11 = full_join(CY11, CY11_incident_details)
CY11 = full_join(CY11, CY11_material_involved)
#View(CY11)
dim(CY11)

# explore LA records
sort(table(CY11$RESPONSIBLE_STATE))
sort(table(CY11$LOCATION_STATE))
head(sort(table(CY11$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY11 = CY11 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY11)

# look at how many were evacuated per state
EvacuationsCY11 = CY11 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY11)

# Number injured
InjuredCY11 = CY11 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY11)

# Fatalities by state
FatalitiesCY11 = CY11 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY11)

# Road closure time by state
Road_closureCY11 = CY11 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY11)

# Medium description by state
medium_descCY11 = filter(CY11, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY11)

# sources of spills into MISSISSIPPI description by state
mississippiCY11 = filter(CY11, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY11)

# sources of spills into GULF OF MEXICO description by state
gulfCY11 = filter(CY11, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY11)

# oil spilled into water
library(stringr)
crudeCY11 = filter(CY11, str_detect(NAME_OF_MATERIAL, "CRUDE") & IF_REACHED_WATER == "YES") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Crude = n())
View(crudeCY11)

# join all state columns
sumCY11 = full_join(SpillsCY11, EvacuationsCY11)
sumCY11 = full_join(sumCY11, InjuredCY11)
sumCY11 = full_join(sumCY11, FatalitiesCY11)
sumCY11 = full_join(sumCY11, Road_closureCY11)
sumCY11 = full_join(sumCY11, medium_descCY11)
sumCY11 = full_join(sumCY11, mississippiCY11)
sumCY11 = full_join(sumCY11, gulfCY11)
sumCY11 = full_join(sumCY11, crudeCY11)
sumCY11["Year"]="2011"
View(sumCY11)

# companies responsible for of spills in Louisiana
companiesCY11 = filter(CY11, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY11_Spills = n())
#View(companiesCY11)
