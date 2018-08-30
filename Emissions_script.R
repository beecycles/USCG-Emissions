###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2017 #######################################

setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)

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
