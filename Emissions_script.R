###################################################################################
################## Exploration of USCG Emissions Data in Louisiana ################
######################### Joan Meiners 2018 #######################################

### This script is for combining all years of emissions data. See separate scripts for specific years ####
setwd("/Users/joanmeiners/Dropbox/NOLA.com/USCG-Emissions/")

#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# run other yearly scripts first to generate objects to combine
all_emissions = full_join(sumCY05, sumCY10)
all_emissions = full_join(all_emissions, sumCY14)
all_emissions = full_join(all_emissions, sumCY15)
all_emissions = full_join(all_emissions, sumCY16)
all_emissions = full_join(all_emissions, sumCY17)
all_emissions = full_join(all_emissions, sumCY18)
dim(all_emissions)
all_emissions = all_emissions[,c(1,14,2:13)]
unique(all_emissions$Year)
unique(all_emissions$LOCATION_STATE)
View(all_emissions)

# prep data for plotting
all_emissions$Year = as.numeric(all_emissions$Year)
cols = c("grey","grey","grey","grey","grey","grey","grey","grey","lightblue","grey","grey","grey","grey","grey","grey","darkblue","grey","grey","grey","grey","grey","grey","grey","grey","grey","red","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","darkorange","grey","grey","grey","grey","grey","grey","grey","grey","grey")

# plot trend in spills in LA over time compared to state average
quartz(height = 6, width = 10)
ggplot(all_emissions, aes(x = Year, y = TOTAL_Spills, colour = LOCATION_STATE)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "grey") +
  xlab("Year") + ylab("Number of Spills") +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15)) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=10)) +
  scale_x_continuous(breaks=c(2005, 2010, 2014, 2015, 2016, 2017, 2018)) +
  scale_y_continuous(labels = function(x) paste0(scales::comma(x))) +
  scale_color_manual(values=cols)
