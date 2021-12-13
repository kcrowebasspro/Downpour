# Author: Kevin Crowe, USA TODAY, kcrowe@usatoday.com

# Analysis of weather station data from Brian Brettschneider, a climatologist
# who used to work for the University of Alaska Fairbanks and is now with the
# National Weather Service. 

# He examined the change in frequency of certain kinds of events at weather stations
# across the U.S. He compared the base period 1951-1990 to 1991-2020 and calculated the 
# change in events that used to happen 3 days, 5 days, 10 days and 25 days per year. 

# More on his methodology here: http://us-climate.blogspot.com/2021/05/

# I've analyzed this data in Excel, QGIS and R, and am creating this file
# to have a central place to fact check the main findings. This is not a
# long file. 


library(readxl)
library(tidyverse)
library(tmap)
library(sf)

# Read in the summary and detail data
stations.summary <- read_excel("/data/Station_Data_USA_Today.xlsx",
                               sheet = "Summary")

# We're not going to deal much with the 
# detail data
stations.detail <- read_excel("/data/Station_Data_USA_Today.xlsx",
                              sheet = "each_year")

# Check the structures
glimpse(stations.detail)
glimpse(stations.summary)

# Create a state field for the summary data
stations.summary <- stations.summary %>%
  mutate(state_abbr = str_squish(substr(Station_Name,5,6))) 

# Create an SF object from the station data
stations.sf <- st_as_sf(stations.summary, coords = c("Lon", "Lat"), crs = st_crs(4269))

# Plot the stations to give a visual check
stations.map <- tm_shape(stations.sf) +
  tm_dots(col = "darkgoldenrod2")

# Look at the map in leaflet to check out individual points
tmap_leaflet(stations.map)
# All good

# We're not going to include the stations from Alaska or Hawaii
# in the main analysis
stations.sf <- stations.sf %>%
  filter(state_abbr != "AK", state_abbr != "HI")
# This brings us down to 285 stations

# Plot the stations again to give a visual check
stations.map <- tm_shape(stations.sf) +
  tm_dots(col = "darkgoldenrod2")

# Look at the map in leaflet to check out individual points
tmap_leaflet(stations.map)
# Only the stations from the lower 48 states are
# included here. 

#----------------------------
#----------------------------
# Analysis
#----------------------------
#----------------------------

# We are primarily interested in the changes in frequency
# of the precipitation events that used to happen 3 days
# per year on average from 1951-1990.

# What does the layout look like for which
# stations increased vs. decreased:
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI") %>%
  mutate(increased = if_else(ChangePct_Days3 > 0, 1, 0),
         decreased = if_else(ChangePct_Days3 < 0, 1, 0)) %>%
  summarise(increased = sum(increased),
            decreased = sum(decreased))
# 213 increased -- 75%
# 71 decreased -- 25%
# The station in Lubbock, TX had no change at all
213/285
71/285

# How many of those increases were of at least 33%?
# Since what we're talking about are 3-day events, a 
# 33% increased would mean 1 more of these events per year. 
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 >= 33) %>%
  count()
# 124 stations -- 44%
124/285

# Where are the 33% increase stations distributed?
stations.33 <- stations.sf %>%
  filter(ChangePct_Days3>=33) %>%
  tm_shape() +
  tm_dots(col = "darkgoldenrod2")

tmap_leaflet(stations.33)
# They are almost all east of the Rockies

# Of the stations with 33% increases, how many were
# statistically significant?
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 >= 33) %>%
  group_by(Analysis_Days3) %>%
  count()
# 81/124 were significant - 65.3%
81/124

# Take a look at the distribution of all stations by 
# percent change in 3 day events
stations.map2 <- tm_shape(stations.sf) +
  tm_dots(col = "ChangePct_Days3", size = 0.05, 
          style = "fixed", breaks = c(-60, -25, 0, 33, 66, 100, 160)) +
  tm_layout(legend.outside = TRUE)

tmap_leaflet(stations.map2)
# This pattern very much lines up with the areas of increasing
# average annual precipitation in the climate divisions. 


#----------------------------
#----------------------------
# Fact Checking nuggets
# from the main bar
#----------------------------
#----------------------------

# Graf: "Forty-four percent of 285 weather stations had at least one 
# additional day of precipitation so heavy it used to fall only three days 
# a year, based on data compiled by Brettschneider. Most of these increases 
# went beyond what scientists would attribute to chance."

# This means 44% of weather stations had an increase of at least 33% in the 
# frequency of 3-day events
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 >= 33) %>%
  count()
# 124/285 (43.5%) stations had at least a 33% increase in these events
124/285

# How many had positive significance?
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 >= 33) %>%
  group_by(Analysis_Days3) %>%
  count()
# 81/124 (65.3%) had a change considered to be significant at at least the p < 0.1 level

# Graf: "Nineteen places doubled their previous number of days of extreme 
# precipitation – from three a year to six."
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 >= 100) %>%
  group_by(Analysis_Days3) %>%
  count()
# 19 looks good

# Graf: "Three cities – Roanoke, Virginia; Madison, Wisconsin; and Akron, Ohio
# – jumped from three days to seven."
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 >= 133) %>%
  select(Station_Name, Days3_51_90, ChangePct_Days3)
# Looks good

# Graf: "In comparison, only 3% of weather stations had a one-day decline 
# in the frequency of the most extreme storms."
stations.summary %>%
  filter(state_abbr != "AK", state_abbr != "HI", ChangePct_Days3 <= -33) %>%
  select(Station_Name, Days3_51_90, ChangePct_Days3)
# 8/285 (2.8%) stations saw a decline of at least 33%
8/285

# Graf: "Tennessee is a state where rainfall totals and the frequency of 
# intense storms have climbed, fueled in part by this same devastating 
# combination of moisture from warming oceans and slower-moving storms."

# The rainfall totals portion of this comes from the climate division data.
# I'm testing the "frequency of intense storms" portion. 
stations.summary %>%
  filter(state_abbr == "TN") %>%
  select(Station_Name, Days3_51_90, ChangePct_Days3) %>%
  arrange(desc(ChangePct_Days3))
# This looks good. 5/8 cities are seeing at least one more
# day of heavy rainfall per year

