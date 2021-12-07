# Author: Kevin Crowe, USA TODAY, kcrowe@usatoday.com
#
# A look at the changes in average annual precipitation by NOAA climate division
# Data source: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
# This file: climdiv-pcpndv-v1.0.0-YYYYMMDD -- it gets updated at least monthly
# About the division codes from Scott Stevens at NOAA
# Example:
# 0101011895   7.37   1.41   7.17   2.72   3.06   4.04   4.58   4.00   3.41   2.28   1.83   5.83   
# 01 = state is Alabama
# 01 = climate division #2
# 01 = element code is precip
# 1895 = year
# 7.37 = January 1895 precip in inches
# 1.41 = February 1895 precip in inches
# ... etc thru DEC.

# The objective:
# - Analyze the average precipitation changes for different periods of time.
# - Scientists generally look at precipitation in 30-year chunks. It's the
#   time frame NOAA uses for it's Normals, for example. 
# - The most recent time periods for analysis will be 1961-1990 and 1991-2020.
# - We'll still compare the most recent period to other, non-overlapping periods
#   to see if the most recent has seen the most changes. 

# load libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(zoo)
library(openintro)

# Read in the precipitation data
precip.monthly <- read.table("/data/climdiv-pcpndv-v1.0.0-20211006.txt", header = FALSE,
                             colClasses = c("character","numeric","numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","numeric","numeric"))

# Check the structure
str(precip.monthly)

# assign the column names
colnames(precip.monthly) <- c("state_div_element","january","february","march","april","may","june",
                                    "july","august","september","october","november","december")


# If there are no measurements for a month, the value is
# -9.99. I need to replace those with NAs. 
precip.monthly <- precip.monthly %>%
  na_if(-9.99)

# Parse the division code to get state and division codes,
# as well as years. The breakdown goes:
# 0101011895    
# 01 = state is Alabama
# 01 = climate division #2
# 01 = element code is precip
# 1895 = year
precip.monthly <- precip.monthly %>%
  mutate(state_code = substr(state_div_element, 1, 2),
         division_code = substr(state_div_element, 3, 4),
         element_code = substr(state_div_element, 5, 6),
         year = as.numeric(substr(state_div_element, 7, 10)))

# slice out just the 1895-2020 data
precip.monthly <- precip.monthly %>%
  filter(year < 2021)

# Get the total annual precipitation
precip.monthly <- precip.monthly %>%
  rowwise(state_div_element) %>%
  mutate(annual_total = sum(c_across(january:december), na.rm = TRUE))

# Add in the state code data
state.codes <- read_tsv("/data/state_codes_climate_divisions.txt",
                        col_types = "cc")

# Add the state name to the divisions data 
precip.monthly <- precip.monthly %>%
  inner_join(state.codes, by = "state_code")

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Create the averages:
# - rolling 30 year average by division
# - the 30-year periods that match up with normals:
#     - 1991-2020
#     - 1961-1990
#     - 1931-1960
#     - 1901-1930
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# We're not working with data for Alaska, and Hawaii is not included.
# We're analyzing the trends for the lower 48 states.
precip.monthly <- precip.monthly %>%
  filter(state_code != "50")

# Create the rolling, 30-year average by division
precip.monthly <- precip.monthly %>%
  arrange(state_code, division_code, year) %>%
  group_by(state_code, division_code) %>%
  mutate(mean30yr = rollmean(annual_total, k = 30, fill = NA, align = "right")) %>%
  ungroup()

# Calculate the mean precipitation figures for each climate division
# for the different 30 year periods
# 1901-1930
avg.1901.1930 <- precip.monthly %>%
  filter(year >= 1901, year <= 1930) %>%
  group_by(state_code, division_code, state_name) %>%
  summarise(avg_1901_1930 = mean(annual_total)) %>%
  ungroup()

# 1931-1960
avg.1931.1960 <- precip.monthly %>%
  filter(year >= 1931, year <= 1960) %>%
  group_by(state_code, division_code, state_name) %>%
  summarise(avg_1931_1960 = mean(annual_total)) %>%
  ungroup()

# 1961-1990
avg.1961.1990 <- precip.monthly %>%
  filter(year >= 1961, year <= 1990) %>%
  group_by(state_code, division_code, state_name) %>%
  summarise(avg_1961_1990 = mean(annual_total)) %>%
  ungroup()

# 1991-2020
avg.1991.2020 <- precip.monthly %>%
  filter(year >= 1991, year <= 2020) %>%
  group_by(state_code, division_code, state_name) %>%
  summarise(avg_1991_2020 = mean(annual_total)) %>%
  ungroup()

# Join the averages together and calculate the changes
precip.averages <- avg.1901.1930 %>%
  inner_join(avg.1931.1960, by = c("state_code" = "state_code", "division_code" = "division_code")) %>%
  inner_join(avg.1961.1990, by = c("state_code" = "state_code", "division_code" = "division_code")) %>%
  inner_join(avg.1991.2020, by = c("state_code" = "state_code", "division_code" = "division_code")) %>%
  select(state_code, division_code, state_name = state_name.x, avg_1901_1930, avg_1931_1960, avg_1961_1990, avg_1991_2020)

# Calculate the differences
precip.averages <- precip.averages %>%
  mutate(diff_1901_1960 = avg_1931_1960 - avg_1901_1930,
         pct_diff_1901_1960 = (avg_1931_1960 - avg_1901_1930)/avg_1901_1930,
         diff_1931_1990 = avg_1961_1990 - avg_1931_1960,
         pct_diff_1931_1990 = (avg_1961_1990 - avg_1931_1960)/avg_1931_1960,
         diff_1961_2020 = avg_1991_2020 - avg_1961_1990,
         pct_diff_1961_2020 = (avg_1991_2020 - avg_1961_1990)/avg_1961_1990)

# How many divisions had increases from 1961-2020:
precip.averages %>%
  filter(diff_1961_2020 > 0) %>%
  count()
# 264 (77%) had increases
264/344

# how many had more than 3 inch increases?
precip.averages %>%
  filter(diff_1961_2020 >= 3) %>%
  count()
# 59/344 - 17% overall, 22% of the divisions with increases
59/344

# How many divisions saw declines, and where are they?
precip.averages %>%
  filter(diff_1961_2020 < 0) %>%
  group_by(state_code, state_name) %>%
  summarise(divisions = n_distinct(division_code)) %>%
  arrange(desc(divisions))
  
# How many divisions saw at least 0.5 inch declines, and where are they?
precip.averages %>%
  filter(diff_1961_2020 < -0.5, pct_diff_1961_2020 < -.02) %>%
  group_by(state_code, state_name) %>%
  summarise(divisions = n_distinct(division_code)) %>%
  arrange(desc(divisions))
# Arizona, Idaho and New Mexico have had some 
# of the worst drops. The Georgia results are interesting. 

# In how many divisions was the most recent period the most compared with
# the previous three?
precip.averages %>%
  filter(avg_1991_2020 > avg_1901_1930, avg_1991_2020 > avg_1931_1960, avg_1991_2020 > avg_1961_1990) %>%
  count()
# In 240 of the 344 divisions, this was 
# the wettest 30-year period of the the 3

# Create a field for the division code that I can
# use to join to the climate divisions shapefile
precip.averages <- precip.averages %>%
  mutate(DivJoin = paste0('CD',state_code,division_code)) %>%
  select(state_code:state_name, DivJoin, avg_1901_1930:pct_diff_1961_2020)

# Write out a file with different headers for QGIS.
# QGIS stores data in DBFs after joins, so headers
# can be a max of 10 characters
colnames(precip.averages)

precip.averages.qgis <- precip.averages %>%
  rename(stateCode = state_code,
         divCode = division_code,
         stateName = state_name,
         avg0130 = avg_1901_1930,
         avg3160 = avg_1931_1960,
         avg6190 = avg_1961_1990,
         avg9120 = avg_1991_2020,
         diff0160 = diff_1901_1960,
         pct0160 = pct_diff_1901_1960,
         diff3190 = diff_1931_1990,
         pct3190 = pct_diff_1931_1990,
         diff6120 = diff_1961_2020,
         pct6120 = pct_diff_1961_2020)

# Create a join field for that file. 
# I need state abbreviations
precip.averages.qgis <- precip.averages.qgis %>%
  mutate(stateAbbr = state2abbr(stateName),
         divJoin = paste0(stateAbbr, "C0", divCode)) %>%
  select(stateCode, divCode, stateName, stateAbbr, divJoin, everything())

# Write out the long file
write_csv(precip.averages.qgis, "/data/clim_div_avg_ann_precip_changes_QGIS.csv")
write_rds(precip.averages.qgis, "/data/clim_div_avg_ann_precip_changes_QGIS.RDS")


#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Analysis questions:
# - When have climate divisions seen their highest
#   and lowest 30-year averages?
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# What are the max 30-year averages?
max.30yr.means <- precip.monthly %>%
  group_by(state_code, division_code) %>%
  summarise(max_30yr_avg = max(mean30yr, na.rm = TRUE))

# Join those back to the divisions
# If a division has two years tied for the max,
# that will show up in this join
max.30yr.means <- max.30yr.means %>%
  inner_join(precip.monthly, by = c("state_code" = "state_code", 
                                    "division_code" = "division_code",
                                    "max_30yr_avg" = "mean30yr")) %>%
  select(state_code, division_code, max_30yr_avg, year)
# 344 divisions come out. Looks good. 

# plot that to see how many maxes we're
# seeing by year
max.30yr.means %>%
  group_by(year) %>%
  summarise(records = n()) %>%
  ggplot(aes(x = year, y = records)) +
  geom_bar(stat = "identity")

# counted by year
maxes.by.year <- max.30yr.means %>%
  group_by(year) %>%
  summarise(records = n())

# Which places had their highest 30-year mean
# in the past few years?

# Get the places that have had maxes
# in the past 5 years
recent.maxes <- max.30yr.means %>%
  filter(year >= 2018) %>%
  inner_join(precip.averages, by = c("state_code" = "state_code",
                                     "division_code" = "division_code")) %>%
  select(state_code, division_code, stateName = state_name, max30yAvg = max_30yr_avg, maxYear = year)

# 181 climate divisions have had their max 30-year rolling
# average in the past 3 years

# add another join field to the recent maxes
recent.maxes <- recent.maxes %>%
  inner_join(precip.averages, by = c("state_code" = "state_code",
                                     "division_code" = "division_code")) %>%
  select(stateCode = state_code, division_code, stateName, max30yAvg, maxYear)

# Join the max 30 year data to the updated QGIS file
precip.averages.qgis <- precip.averages.qgis %>%
  inner_join(max.30yr.means, by = c("stateCode" = "state_code", "divCode" = "division_code")) %>%
  rename(max30avg = max_30yr_avg,
         max30avgYr = year)

# Check to see what then year counts on the newly joined file
precip.averages.qgis %>%
  filter(max30avgYr >= 2018) %>%
  count()

# Write out the QGIS file
write_csv(precip.averages.qgis, "/data/climate_divisions_ann_precip_compare_w_30yr_maxes.csv")


# Write that out for QGIS join
write_csv(recent.maxes, "/data/recent_30yr_ann_precip_maxes_NOAA.csv")


#----------------------
#----------------------
# What were the max years for each climate division in
# terms of annual precip?
max.ann.totals <- precip.monthly %>%
  group_by(state_code, division_code, state_name) %>%
  summarise(max_ann_total = max(annual_total))

# Join that back to the main table
max.ann.totals <- max.ann.totals %>%
  inner_join(precip.monthly, by = c("state_code" = "state_code", 
                                    "division_code" = "division_code",
                                    "max_ann_total" = "annual_total")) %>%
  group_by(state_code, division_code, state_name.x, max_ann_total) %>%
  summarise(max_year = max(year))

# Get the histogram:
# There are many more max precip years from 2000-2020
# than in any other period, it appears. 
max.ann.totals %>%
  group_by(max_year) %>%
  summarise(records = n()) %>%
  ggplot(aes(x = max_year, y = records)) +
  geom_bar(stat = "identity") +
  labs(title = "Max precipitation year for each climate division")

# Sum the records by year:
max.ann.totals %>%
  group_by(max_year) %>%
  summarise(records = n()) %>%
  arrange(desc(max_year))

# How many since 2018?
max.ann.totals %>%
  ungroup() %>%
  filter(max_year >= 2018) %>%
  count()
# 72 -- 21%
72/344

# How many since 2010?
max.ann.totals %>%
  ungroup() %>%
  filter(max_year >= 2010) %>%
  count()
# 131 -- 38%
131/344

# How many since 2000?
max.ann.totals %>%
  ungroup() %>%
  filter(max_year >= 2000) %>%
  count()
# 150 -- 44%
150/344

#----------------------
#----------------------
# Which places had their lowest 30-year mean
# in the past few years?

# What are the max 30-year averages?
min.30yr.means <- precip.monthly %>%
  group_by(state_code, division_code) %>%
  summarise(min_30yr_avg = min(mean30yr, na.rm = TRUE))

# Join those back to the divisions
min.30yr.means <- min.30yr.means %>%
  inner_join(precip.monthly, by = c("state_code" = "state_code", 
                                    "division_code" = "division_code",
                                    "min_30yr_avg" = "mean30yr")) %>%
  select(state_code, division_code, min_30yr_avg, year)

# plot that to see how many mins we're
# seeing by year
min.30yr.means %>%
  group_by(year) %>%
  summarise(records = n()) %>%
  ggplot(aes(x = year, y = records)) +
  geom_bar(stat = "identity")
# This points to the late 1930s through 1970 as
# the driest periods on record. That squares with 
# what scientists have been telling us. 

# counted by year
min.by.year <- min.30yr.means %>%
  group_by(year) %>%
  summarise(records = n())





