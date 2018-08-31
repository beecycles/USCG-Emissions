###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for YEAR 2009 emissions data. See separate similar scripts for other years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
CY09_calls = read.csv("CY09_calls.csv", header = TRUE)
CY09_incident_details = read.csv("CY09_incident_details.csv", header = TRUE)
CY09_incident_commons = read.csv("CY09_incident_commons.csv", header = TRUE)
CY09_material_involved = read.csv("CY09_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
CY09 = full_join(CY09_calls, CY09_incident_commons)
CY09 = full_join(CY09, CY09_incident_details)
CY09 = full_join(CY09, CY09_material_involved)
#View(CY09)
dim(CY09)

# explore LA records
sort(table(CY09$RESPONSIBLE_STATE))
sort(table(CY09$LOCATION_STATE))
head(sort(table(CY09$BODY_OF_WATER), decreasing = TRUE), 20)

# how many total spills reported per state
SpillsCY09 = CY09 %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    TOTAL_Spills = n())
#View(SpillsCY09)

# look at how many were evacuated per state
EvacuationsCY09 = CY09 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    EVAC_Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
#View(EvacuationsCY09)

# Number injured
InjuredCY09 = CY09 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    INJ_Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
#View(InjuredCY09)

# Fatalities by state
FatalitiesCY09 = CY09 %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    FATAL_Spills = n(),
    TOTAL_FATALITIES = sum(NUMBER_FATALITIES, na.rm = TRUE))
#View(FatalitiesCY09)

# Road closure time by state
Road_closureCY09 = CY09 %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    ROADCLOSE_Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
#View(Road_closureCY09)

# Medium description by state
medium_descCY09 = filter(CY09, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    WATER_Spills = n())
#View(medium_descCY09)

# sources of spills into MISSISSIPPI description by state
mississippiCY09 = filter(CY09, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    MISS_Spills = n())
#View(mississippiCY09)

# sources of spills into GULF OF MEXICO description by state
gulfCY09 = filter(CY09, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    GULF_Spills = n())
#View(gulfCY09)

# join all state columns
sumCY09 = full_join(SpillsCY09, EvacuationsCY09)
sumCY09 = full_join(sumCY09, InjuredCY09)
sumCY09 = full_join(sumCY09, FatalitiesCY09)
sumCY09 = full_join(sumCY09, Road_closureCY09)
sumCY09 = full_join(sumCY09, medium_descCY09)
sumCY09 = full_join(sumCY09, mississippiCY09)
sumCY09 = full_join(sumCY09, gulfCY09)
sumCY09["Year"]="2009"
View(sumCY09)

# companies responsible for of spills in Louisiana
companiesCY09 = filter(CY09, LOCATION_STATE == "LA") %>%
  group_by(RESPONSIBLE_COMPANY) %>%
  summarise(
    CY09_Spills = n())
#View(companiesCY09)
