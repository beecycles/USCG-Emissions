###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
################################### Joan Meiners 2018 #############################
############################ calls and PARTY RESPONSIBLE ######################

### This script is for combining all years of emissions data. See separate scripts for specific years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)


# run other yearly scripts first to generate objects to combine
all = full_join(CY04, CY05)
all = full_join(all, CY06)
all = full_join(all, CY07)
all = full_join(all, CY08)
all = full_join(all, CY09)
all = full_join(all, CY10)
all = full_join(all, CY11)
all = full_join(all, CY12)
all = full_join(all, CY13)
all = full_join(all, CY14)
all = full_join(all, CY15)
all = full_join(all, CY16)
all = full_join(all, CY17)
all = full_join(all, CY18)
all = full_join(all, CY19)
dim(all)
View(all)

# subset data to only those incidents in LA and caused by TAYLOR ENERGY CO. LLC or TAYLOR ENERGY
taylor = subset(all, RESPONSIBLE_STATE == "LA")
View(taylor)
taylor = subset(taylor, RESPONSIBLE_COMPANY == "TAYLOR ENERGY" | RESPONSIBLE_COMPANY == "TAYLOR ENERGY COMPANY" | RESPONSIBLE_COMPANY == "TAYLOR ENERGY CO." | RESPONSIBLE_COMPANY == "TAYLOR ENERGY CO. LLC")
View(taylor)
write.csv(taylor, "taylor.csv")
