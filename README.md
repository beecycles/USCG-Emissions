# USCG-Emissions
**Project overview**  
Data journalism project exploring patterns of toxic substance spills using public data from the United States Coast Guard [National Response Center](www.nrc.uscg.mil/). Focusing on Louisiana issues for the [Polluter's Paradise series](https://www.propublica.org/series/polluters-paradise), a collaboration between ProPublica, _The Times-Picayune_ and _The Advocate_.

**File overview**  
**CY--.xlsx** files : were downloaded from the National Response Center database. The number represents the year, so CY01.xlsx is the NRC data file for spills reported to the Coast Guard in 2001.  
**CY--.csv** files : were saved from corresponding .xslx files into separate sheets containing info on the calls received, incident basics (commons), incident details, and materials involved for each year. The number represents the last two digits of a year, so CY01_calls.csv is a record of all calls received by the USCG about toxic spills during the year 2001.  
**Emissions_script_CY--.R** files : code to combine and sort the .csv sheets for each year, then group by variables of interest, in this case **crude oil** spills into **water** in **Louisiana**.  
**Emissions_script.R** : combines all year datasets and plots the number of spills of crude oil into water in different states between 2004 - 2017.  

**Analysis methods**  
Download data from NRC for 2004 - 2019. Save sheets separately and join into a larger dataset, one per year, including all fields of interest (mainly, spill number (SEQNOS), state, material involved, if the spill reached water). Group number of spills per state and type of material (crude oil) and whether or not it reached water (yes/no). Then join all year datasets into a master dataset and plot results to show the number of spills of crude oil into water in different states between 2004 - 2017.  

**Joan Meiners** is an Environmental/Data Journalist and a Ph.D. ecologist.  
More information here: www.joanmeiners.com  
Questions may be directed to: joan.meiners@gmail.com.
