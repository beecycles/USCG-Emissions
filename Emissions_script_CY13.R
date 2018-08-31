###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2013 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY13_calls = read.csv("CY13_calls.csv", header = TRUE)
CY13_incident_details = read.csv("CY13_incident_details.csv", header = TRUE)
CY13_incident_commons = read.csv("CY13_incident_commons.csv", header = TRUE)
CY13_material_involved = read.csv("CY13_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY13 = full_join(CY13_calls, CY13_incident_commons)
CY13 = full_join(CY13, CY13_incident_details)
CY13 = full_join(CY13, CY13_material_involved)
#View(CY13)
dim(CY13)

# explore LA records
sort(table(CY13$RESPONSIBLE_STATE))
sort(table(CY13$LOCATION_STATE))
head(sort(table(CY13$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY13 = CY13 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY13)

# look at how many were evacuated per state
EvacuationsCY13 = CY13 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY13)

# Number injured
InjuredCY13 = CY13 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY13)

# Fatalities by state
FatalitiesCY13 = CY13 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY13)

# Road closure time by state
Road_closureCY13 = CY13 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY13)

# Medium description by state
medium_descCY13 = filter(CY13, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY13)

# sources of spills into MISSISSIPPI description by state
mississippiCY13 = filter(CY13, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY13)

# sources of spills into GULF OF MEXICO description by state
gulfCY13 = filter(CY13, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY13)

# join all state columns
sumCY13 = full_join(SpillsCY13, EvacuationsCY13)
sumCY13 = full_join(sumCY13, InjuredCY13)
sumCY13 = full_join(sumCY13, FatalitiesCY13)
sumCY13 = full_join(sumCY13, Road_closureCY13)
sumCY13 = full_join(sumCY13, medium_descCY13)
sumCY13 = full_join(sumCY13, mississippiCY13)
sumCY13 = full_join(sumCY13, gulfCY13)
sumCY13["Year"]="2013"
View(sumCY13)

# companies responsible for of spills in Louisiana
companiesCY13 = filter(CY13, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY13_Spills = n())
#View(companiesCY13)
