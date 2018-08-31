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
all_emissions = full_join(sumCY08, sumCY09)
all_emissions = full_join(all_emissions, sumCY10)
all_emissions = full_join(all_emissions, sumCY11)
all_emissions = full_join(all_emissions, sumCY12)
all_emissions = full_join(all_emissions, sumCY13)
all_emissions = full_join(all_emissions, sumCY14)
all_emissions = full_join(all_emissions, sumCY15)
all_emissions = full_join(all_emissions, sumCY16)
all_emissions = full_join(all_emissions, sumCY17)
dim(all_emissions)
all_emissions = all_emissions[,c(1,14,2:13)]
unique(all_emissions$Year)
unique(all_emissions$LOCATION_STATE)
View(all_emissions)

# remove states that aren't actually states from dataset
nonstates = c("AS","BF","CN","GU","MX","NI","XX","PI","PN","CB","BV","BE")
dim(all_emissions)
all_emissions = all_emissions[!(all_emissions$LOCATION_STATE == "AS") & !(all_emissions$LOCATION_STATE == "BF") & !(all_emissions$LOCATION_STATE == "CN") & !(all_emissions$LOCATION_STATE == "GU") & !(all_emissions$LOCATION_STATE == "MX") & !(all_emissions$LOCATION_STATE == "NI") & !(all_emissions$LOCATION_STATE == "XX") & !(all_emissions$LOCATION_STATE == "PI") & !(all_emissions$LOCATION_STATE == "PN") & !(all_emissions$LOCATION_STATE == "CB") & !(all_emissions$LOCATION_STATE == "BV") & !(all_emissions$LOCATION_STATE == "BE"), ]
dim(all_emissions)
unique(all_emissions$LOCATION_STATE)

# prep data for plotting
all_emissions$Year = as.numeric(all_emissions$Year)
cols = c("grey","grey","grey","grey","lightblue","grey","grey","grey","grey","darkblue","grey","grey","grey","grey","grey","grey","grey","grey","red","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","grey","darkorange","grey","grey","grey","grey","grey","grey","grey","grey","grey")

# plot trend in spills in LA over time compared to state average
quartz(height = 6, width = 10)
ggplot(all_emissions, aes(x = Year, y = GULF_Spills, colour = LOCATION_STATE)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "grey") +
  xlab("Year") + ylab("Number of Spills into Gulf of Mexico") +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15)) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=10)) +
  scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  scale_y_continuous(labels = function(x) paste0(scales::comma(x))) +
  scale_color_manual(values=cols)

# code to save graphs
tiff(filename = "Gulf_Spills", units = "in", compression = "lzw", res = 300, width = 10, height = 6)
dev.off()
