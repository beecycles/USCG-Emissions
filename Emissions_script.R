###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2017 #######################################

setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)

# load data
Current_calls = read.csv("Current_calls.csv", header = TRUE)
Current_incident_details = read.csv("Current_incident_details.csv", header = TRUE)
Current_incident_commons = read.csv("Current_incident_commons.csv", header = TRUE)
Current_material_involved = read.csv("Current_material_involved.csv", header = TRUE)

# join datasets by SEQNOS
Current = full_join(Current_calls, Current_incident_commons)
Current = full_join(Current, Current_incident_details)
Current = full_join(Current, Current_material_involved)
View(Current)
dim(Current)

# explore LA records
sort(table(Current$RESPONSIBLE_STATE))
sort(table(Current$LOCATION_STATE))
head(sort(table(Current$BODY_OF_WATER), decreasing = TRUE), 20)

# look at how many were evacuated per state
Evacuations = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_EVACUATED > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_EVACUATED, na.rm = TRUE))
View(Evacuations)

# Number injured
Injured = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_INJURED > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE))
View(Injured)

# Fatalities by state
Fatalities = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(NUMBER_FATALITIES > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_EVACUATED = sum(NUMBER_FATALITIES, na.rm = TRUE))
View(Fatalities)

# Road closure time by state -- La. not on current 2018 list (wonder if it would be now)
Road_closure = Current %>%
  group_by(LOCATION_STATE) %>%
  filter(ROAD_CLOSURE_TIME > 0) %>%
  summarise(
    Spills = n(),
    TOTAL_ROAD_CLOSURE_TIME = sum(ROAD_CLOSURE_TIME, na.rm = TRUE))
View(Road_closure)

# Medium description by state -- La has the most spills into water
medium_desc = filter(Current, MEDIUM_DESC == "WATER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Spills = n())
View(medium_desc)

# sources of spills into MISSISSIPPI description by state -- La the highest
mississippi = filter(Current, BODY_OF_WATER == "MISSISSIPPI RIVER") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Spills = n())
View(mississippi)

# sources of spills into MISSISSIPPI description by state -- La BY FAR the highest!
gulf = filter(Current, BODY_OF_WATER == "GULF OF MEXICO") %>%
  group_by(LOCATION_STATE) %>%
  summarise(
    Spills = n())
View(gulf)
