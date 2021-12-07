# Author: Kevin Crowe, USA TODAY, kcrowe@usatoday.com
# This file contains an analysis of monthly precipitation values
# from NOAA
# The FTP site: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
# The file on the FTP site is "climdiv-pcpnst-v1.0.0-YYYYMMDD" where the date is numeric.
# Documentation: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/state-readme.txt
# Explanation of climate divisions: https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-divisions.php#grdd

# Load the libraries
library(janitor)
library(stringr)
library(lubridate)
library(tidyverse)
library(openintro)
library(zoo)


# Read in the data
state.precip.monthly <- read.table("/data/climdiv-pcpnst-v1.0_080521.txt", header = FALSE,
                             colClasses = c("character","numeric","numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","numeric","numeric"))

# Check the structure
str(state.precip.monthly)

# assign the column names
colnames(state.precip.monthly) <- c("stateDivElemYearCode","january","february","march","april","may","june",
                              "july","august","september","october","november","december")

# split up the code field into state and year
state.precip.monthly <- state.precip.monthly %>%
  mutate(stateCode = substr(stateDivElemYearCode,1,3),
         year = as.numeric((substr(stateDivElemYearCode,7,10))))

# Check out the state codes
state.precip.monthly %>%
  group_by(stateCode) %>%
  count() -> state.code.counts

# Read in the state codes from the documentation
state.codes <- read_tsv("/data/state_codes_state_NOAA_data.txt")

# attach state names to the data
state.precip.monthly <- state.precip.monthly %>%
  left_join(state.codes, by = "stateCode")

# Create a figure for annual total precip for
# all years but 2021, which is not complete
state.precip.monthly <- state.precip.monthly %>%
  rowwise(stateDivElemYearCode) %>%
  mutate(precipTotal = if_else(year == 2021,0,sum(c_across(january:december)))) %>%
  select(stateDivElemYearCode, stateCode, year, stateName, january:december, precipTotal) %>%
  ungroup()

#----------------------------
#----------------------------
# Ranking years by state by
# amount of precipitation
#----------------------------
#----------------------------
# The objective here is to rank the precipitation level for
# each state and see when each state has had their top 10 or top
# 20 wettest or driest years. There has been a lot of talk of 
# record precipitation years recently and we want to see 
# how much of that is true across the country.

# Analysis to do:
# - rank the years by station from driest to wettest
# - find out how many of the extremes have happened in the past 20 years
# - Make plots for each state showing the extremes:
#     - use different kinds of smoothing to show the extremes
#     - color the dots based on 10/20 wettest, 10/20 driest, then the rest of the years

# Check the counts by state to make sure
# I create a set with the same number of years for 
# all of them
years.per.state <- state.precip.monthly %>%
  group_by(stateName, stateCode) %>%
  summarise(min_year = min(year),
            max_year = max(year),
            total_years = n())

# I used min_rank here instead of dense rank
# because dense rank has ties, but then does
# not move the following rank down enough. So, if
# two values are 10.1 and are tied for 10th place,
# they'll both be 10th but the next value after those
# will be 11th, not 12th. 
# We excluded Alaska -- there are only 97 years of
# data for AK and we haven't focused any reporting on it. 
# We excluded all 2021 data.
state.precip.monthly.ranked <- state.precip.monthly %>%
  filter(year <= 2020, stateCode != "050") %>%
  group_by(stateName, stateCode) %>%
  mutate(rankYear = min_rank(precipTotal))

# Check to make sure each state  has the
# same number of years
state.precip.monthly.ranked %>%
  group_by(stateCode, stateName) %>%
  summarise(n = n(),
            distinct_rankings = n_distinct(rankYear),
            min_rank_year = min(rankYear),
            max_rank_year = max(rankYear)) -> state.counts
# These all have 126 years in them

# Create another field in the rankings data
# for the 10 wettest and 10 driest years
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  mutate(recordYear10 = case_when(rankYear <= 10 ~ "Top 10 driest",
                                rankYear >= 117 ~ "Top 10 wettest",
                                TRUE ~ "Not a record year"))

# Create another field in the rankings data
# for the 20 wettest and 20 driest years
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  mutate(recordYear20 = case_when(rankYear <= 20 ~ "Top 20 driest",
                                  rankYear >= 107 ~ "Top 20 wettest",
                                  TRUE ~ "Not a record year"))

# how many of the top 10 extreme years happened
# for each state since 2001?
states.last.20.years.ranked <- state.precip.monthly.ranked %>%
  mutate(leadNum = substr(stateCode,1,1)) %>%                 # This filters to states, not regions
  filter(year >= 2001, leadNum == "0") %>%
  group_by(stateName, stateCode, recordYear10) %>%
  summarise(counts = n()) %>%
  pivot_wider(names_from = recordYear10, values_from = counts) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(records = sum(top_10_wettest, top_10_driest, na.rm = TRUE),
         period = "2001-2020") %>%
  ungroup()

# How many states had at least 5 record years since 2001?
states.last.20.years.ranked %>%
  filter(records >= 5) %>%
  count()
# 25 states

# Replace the NAs with zeros
states.last.20.years.ranked <- states.last.20.years.ranked %>%
  replace(is.na(.),0)

#----------------------------
#----------------------------
# Summing records over 20-year
# periods. 
#----------------------------
#----------------------------
# We want to see which periods in time had the most
# record years in terms of wettest and driest years. 
# We are looking at 20 year periods because we want to
# be able to talk about what has happened since 2000-ish. 

# create a new numeric value for a record year
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  mutate(recordYear10Int = if_else(recordYear10 == "Not a record year", 0, 1),
         recordYear20Int = if_else(recordYear20 == "Not a record year", 0, 1))

# Create the coded variables for top 10 wettest, top 10 driest,
# top 20 wettest and top 20 driest
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  mutate(recordYear10IntDry = if_else(recordYear10 == "Top 10 driest", 1, 0),
         recordYear10IntWet = if_else(recordYear10 == "Top 10 wettest", 1, 0),
         recordYear20IntDry = if_else(recordYear20 == "Top 20 driest", 1, 0),
         recordYear20IntWet = if_else(recordYear20 == "Top 20 wettest", 1, 0))

# Create the rolling, 20-year sum for each set of records
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  group_by(stateCode, stateName) %>%
  mutate(rollingSumTop10 = rollsum(recordYear10Int, k = 20, fill = NA, align = "right"),
         rollingSumTop20 = rollsum(recordYear20Int, k = 20, fill = NA, align = "right"),
         rollingSumTop10Wet = rollsum(recordYear10IntWet, k = 20, fill = NA, align = "right"),
         rollingSumTop10Dry = rollsum(recordYear10IntDry, k = 20, fill = NA, align = "right"),
         rollingSumTop20Wet = rollsum(recordYear20IntWet, k = 20, fill = NA, align = "right"),
         rollingSumTop20Dry = rollsum(recordYear20IntDry, k = 20, fill = NA, align = "right"),)

# parse out the states since there are a lot of
# other geographies here, like the Ohio River Basin
# and Mississippi River basins, etc. All states have 
# 0 as the first digit of their code.
rolling.sum.states <- state.precip.monthly.ranked %>%
  filter(substr(stateCode,1,1) == 0)

#----------------------------
#----------------------------
# Create the rolling 30-year
# means for precipitation
#----------------------------
#----------------------------
# We want to see how many states are seeing their 
# wettest periods on record now, or within the 
# past few years. 30 years is a commonly used 
# time period to look at climate data like precipitation,
# temperature or drought. 

# Use rollmean() to create the rolling, 30-year average precipitation
# for each state
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  arrange(stateCode, year) %>%
  group_by(stateCode) %>%
  mutate(mean30yr = rollmean(precipTotal, k = 30, fill = NA, align = "right")) %>%
  ungroup()

# Analysis questions:
# - When have states seen their highest
#   and lowest 30-year averages?

# What are the max 30-year averages? Filter out
# non-state geographies
max.30yr.means <- state.precip.monthly.ranked %>%
  filter(substr(stateCode,1,1) == 0) %>%
  group_by(stateCode) %>%
  summarise(max_30yr_avg = max(mean30yr, na.rm = TRUE)) %>%
  ungroup()

# Join those back to the states using inner join
# in case there are any states where there were
# two years with the same amount of precip and
# those figures were somehow each the max:
max.30yr.means <- max.30yr.means %>%
  inner_join(state.precip.monthly.ranked, by = c("stateCode" = "stateCode",
                                    "max_30yr_avg" = "mean30yr")) %>%
  select(stateCode,stateName, max_30yr_avg, year)

# plot that to see how many maxes we're
# seeing by year
max.30yr.means %>%
  group_by(year) %>%
  summarise(records = n()) %>%
  ggplot(aes(x = year, y = records)) +
  geom_bar(stat = "identity")

# counted by year:
# 27 states have seen their max 30-year average
# precipitation since 2018
maxes.by.year <- max.30yr.means %>%
  group_by(year) %>%
  summarise(records = n())

# Which states had their highest 30-year mean
# in the past few years?
# - 27 states have had their highest 30-year mean precipitation
#   levels since 2018. 
max.30yr.means %>%
  filter(substr(stateCode,1,1) == "0") %>%
  group_by(year) %>%
  summarise(records = n()) %>%
  ggplot(aes(x = year, y = records)) +
  geom_bar(stat = "identity")

max.30yr.means %>%
  filter(substr(stateCode,1,1) == "0") %>%
  group_by(year) %>%
  summarise(records = n()) %>%
  arrange(desc(year))

# Get the list of states with max 30-year
# means since 2018:
recent.max.states <- max.30yr.means %>%
  filter(substr(stateCode,1,1) == "0", year >= 2018) 

#------------------------------------------------
#------------------------------------------------
# Analysis of the record years
#------------------------------------------------
#------------------------------------------------
# Overall analysis questions here:
# - What would be an expected number of records during
#   a 20-year period? The data go from 1895-2020.
# - If you're doing top 10 records, you'd
#   expect to have 1.6 top 10 records per 20-year period.
# - We'd expect to see 3.2 top 20 records in any 20-year period. 

# To do:
# - In which ways can we tell if the past 20 years really have
#   been the most chaotic in terms of precipitation since 1895?
#       - The most total top 10 or top 20 records
#       - The most records to top precip -- 30 year mean
#       - The most records for dry years

# Calculate a field for how much more or fewer than expected
# records happened in each 20-year period. 
rolling.sum.states <- rolling.sum.states %>%
  mutate(VsExpRecordTotal10 = if_else(!is.na(rollingSumTop10), rollingSumTop10/1.6, 0),
         VsExpRecordWet10 = if_else(!is.na(rollingSumTop10Wet), rollingSumTop10Wet/1.6, 0),
         VsExpRecordDry10 = if_else(!is.na(rollingSumTop10Dry), rollingSumTop10Dry/1.6, 0),
         VsExpRecordTotal20 = if_else(!is.na(rollingSumTop20), rollingSumTop20/3.2, 0),
         VsExpRecordWet20 = if_else(!is.na(rollingSumTop20Wet), rollingSumTop20Wet/3.2, 0),
         VsExpRecordDry20 = if_else(!is.na(rollingSumTop20Dry), rollingSumTop20Dry/3.2, 0))

# Parse out the 2020 records
rolling.sum.states.2020 <- rolling.sum.states %>%
  filter(year == 2020)

# Count the states that had at
# least 5 records years since 2001
rolling.sum.states %>%
  filter(year == 2020, rollingSumTop10 >= 5)
# 25 states

# Check that 2020 had the most record years
# -- It did - 209, next was 2019 with 194,
# then 1966 with 180
# - This is largely driven by the wet years
# - 2001-2020 had the most record wet years 
#   than any period on record
records.by.year <- rolling.sum.states %>%
  group_by(year) %>%
  summarise(totalTop10 = sum(rollingSumTop10, na.rm = TRUE),
            totalTop10Wet = sum(rollingSumTop10Wet, na.rm = TRUE),
            totalTop10Dry = sum(rollingSumTop10Dry, na.rm = TRUE),
            totalTop20 = sum(rollingSumTop20, na.rm = TRUE),
            totalTop20Wet = sum(rollingSumTop20Wet, na.rm = TRUE),
            totalTop20Dry = sum(rollingSumTop20Dry, na.rm = TRUE))

# How many states had more records than expected in 2020?
# Wet years
rolling.sum.states.2020 %>%
  filter(VsExpRecordWet10 >= 1) %>%
  group_by(rollingSumTop10Wet, VsExpRecordWet10) %>%
  count()

# Dry years:
rolling.sum.states.2020 %>%
  filter(VsExpRecordDry10 >= 1) %>%
  group_by(rollingSumTop10Dry, VsExpRecordDry10) %>%
  count()

# Whiplash states:
rolling.sum.states.2020 %>%
  filter(VsExpRecordTotal10 > 1, rollingSumTop10Wet >= 2, rollingSumTop10Dry >= 2) %>%
  select(stateName, rollingSumTop10, rollingSumTop10Wet, rollingSumTop10Dry) %>%
  arrange(desc(rollingSumTop10))
# 10 states have had more than expected record wet and dry years
# since 2001. Arkansas, Georgia and North Dakota saw the most extreme
# swings, with each state posting numerous years with record precipitation
# and numerous record dry years. 



#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Everything below here is plotting work. 
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------


#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Create a 100-year mean precipitation level for
# each state then measure the deviation each year. 
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# Create the 100-year mean and join it to the
# main state precip table
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  filter(year >= 1901, year <= 2000) %>%
  group_by(stateCode) %>%
  summarise(mean100yrPrecip = mean(precipTotal)) %>%
  inner_join(state.precip.monthly.ranked, by = "stateCode")

# Create a field for each year's deviation from the mean
state.precip.monthly.ranked <- state.precip.monthly.ranked %>%
  mutate(deviation100yrMean = precipTotal - mean100yrPrecip)

# Start plotting the deviation
state.precip.monthly.ranked %>%
  filter(stateCode == "020") %>%
  ggplot(aes(x = year, y = deviation100yrMean)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Michigan: Deviation from 100-year mean precipitation in inches",
       x = "", y = "",
       caption = "Source: NOAA", color = "", fill = "") +
  scale_x_continuous(limits = c(1894, 2022), breaks = seq(1900, 2020, by = 20)) +
  scale_fill_manual(values = c("gray80", "darkorange3", "steelblue4")) +
  theme_gray() +
  theme(legend.position = "bottom")

# Write a loop to create and export these 
# plots for each state and region

# Create a df of state/region codes and names
state.codes <- state.precip.monthly.ranked %>%
  distinct(stateCode,stateName)

i = 1

for(i in 1:nrow(state.codes)){
  state_name = state.codes$stateName[i]
  state_name_path = str_replace_all(state_name, " ", "_")
  path_name = paste0("/deviation_from_mean_plots/") 
  
  temp_plot <- state.precip.monthly.ranked %>%
    filter(stateCode == state.codes$stateCode[i]) %>%
    ggplot(aes(x = year, y = deviation100yrMean)) +
    geom_bar(stat = "identity", aes(fill = recordYear10)) +
    labs(title = paste0(state_name,": Deviation from 100-year mean precipitation"),
         x = "", y = "Inches",
         caption = "Source: NOAA", color = "", fill = "") +
    scale_x_continuous(limits = c(1894, 2022), breaks = seq(1900, 2020, by = 20)) +
    scale_fill_manual(values = c("gray80", "darkorange3", "steelblue4")) +
    theme_gray(base_size = 40) +
    theme(legend.position = "bottom")
  
  ggsave(filename = paste0(state_name_path,"_deviation_mean_1895_2020.png"), plot = temp_plot,path = path_name,
         width = 2000, height = 1300, units = "px", scale = 4)
  
}

#----------------------------
#----------------------------
# Plotting the rankings
#----------------------------
#----------------------------

# Create a scatterplot for a climate division AR
state.precip.monthly.ranked %>%
  filter(stateCode == "003") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_point(aes(color = recordYear10)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_color_manual(values = c("gray80", "darkorange3", "steelblue3")) +
  labs(title = "Arkansas: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  theme_dark()

state.precip.monthly.ranked %>%
  filter(stateCode == "003") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_point(aes(color = recordYear10)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_color_manual(values = c("gray80", "darkorange3", "steelblue3")) +
  labs(title = "Arkansas: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  theme_light()

# Create a barchart for a AR
state.precip.monthly.ranked %>%
  filter(stateCode == "003") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Arkansas: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) 

#-----------------
# Michigan
#----------------
state.precip.monthly.ranked %>%
  filter(stateCode == "020") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_point(aes(color = recordYear20)) +
  ylim(0,50) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  labs(title = "Michigan: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  theme_light()

# Create a barchart for a MI
state.precip.monthly.ranked %>%
  filter(stateCode == "020") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear20)) +
  labs(title = "Michigan: Annual precipitation in inches",
       x = "", y = "",
       caption = "Source: NOAA", color = "", fill = "") +
  scale_x_continuous(limits = c(1894, 2022), breaks = seq(1900, 2020, by = 20)) +
  scale_fill_manual(values = c("gray80", "darkorange3", "steelblue4")) +
  theme_gray() +
  theme(legend.position = "bottom")


#-----------------
# Georgia
#----------------
state.precip.monthly.ranked %>%
  filter(stateCode == "009") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_point(aes(color = recordYear10)) +
  ylim(0,80) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  labs(title = "Georgia: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  theme_light()

# Create a barchart for a GA
state.precip.monthly.ranked %>%
  filter(stateCode == "009") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Georgia: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) 

#-----------------
# Arkansas
#----------------

# Create a barchart for AR
state.precip.monthly.ranked %>%
  filter(stateCode == "003") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Arkansas: Annual precipitation 1895 - 2020",
       x = "", y = "",
       caption = "Source: NOAA", color = "", fill = "") +
  scale_x_continuous(limits = c(1895, 2022), breaks = seq(1900, 2022, by = 20)) +
  scale_fill_manual(values = c("gray80", "darkorange3", "steelblue4")) +
  theme_gray() +
  theme(legend.position = "none")


#-----------------
# Ohio
#----------------
# Create a barchart for OH
state.precip.monthly.ranked %>%
  filter(stateCode == "033") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Ohio: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  scale_fill_manual(values = c("gray80", "darkorange3", "steelblue4")) 

#-----------------------
# Plots for MI, OH, IN, PA
#-----------------------

# Create a barchart for MI, OH, IN, PA
state.precip.monthly.ranked %>%
  filter(stateName == "Michigan" | stateName == "Ohio" |
           stateName == "Indiana" | stateName == "Pennsylvania") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", fill = "") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  facet_wrap(~stateName) +
  theme_minimal()

# Do scatterplots for the same
state.precip.monthly.ranked %>%
  mutate(pointSize = if_else(recordYear20 == "Not a record year", 1, 2)) %>%
  filter(stateName == "Michigan" | stateName == "Ohio" |
           stateName == "Indiana" | stateName == "Pennsylvania") %>%
  ggplot(aes(x = year, y = precipTotal, size = pointSize)) +
  geom_point(aes(fill = recordYear20), colour="gray42",pch=21) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_fill_manual(values = c("gray80", "darkorange3", "steelblue3")) +
  labs(title = "Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", fill = "") +
  guides(size = FALSE) +
  geom_vline(xintercept = 2001, linetype="dotted", 
             color = "gray42", size= 0.5) +
  facet_wrap(~stateName) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 20),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.text=element_text(size = 15)) 


#-----------------------
# Plots for GA, AK, NC
#-----------------------
state.precip.monthly.ranked %>%
  mutate(pointSize = if_else(recordYear10 == "Not a record year", 1, 2)) %>%
  filter(stateName == "Georgia" | stateName == "Arkansas") %>%
  ggplot(aes(x = year, y = precipTotal, size = pointSize)) +
  geom_point(aes(fill = recordYear10), colour="gray42",pch=21) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  labs(title = "Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", fill = "") +
  guides(size = FALSE) +
  geom_vline(xintercept = 2000, linetype="dotted", 
             color = "gray42", size= 1.5) +
  facet_wrap(~stateName, ncol = 1) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text=element_text(size = 12))  

# Bar plot
state.precip.monthly.ranked %>%
  filter(stateName == "Georgia" | stateName == "Arkansas") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", fill = "") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  geom_vline(xintercept = 2000, linetype="dotted", 
             color = "gray42", size= 1.5) +
  facet_wrap(~stateName, ncol = 1) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text=element_text(size = 10))  


#-----------------------
# Plots for TN
#-----------------------

# Create a scatterplot for a climate division TN
state.precip.monthly.ranked %>%
  filter(stateCode == "040") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_point(aes(color = recordYear10)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_color_manual(values = c("gray80", "darkorange3", "steelblue3")) +
  labs(title = "Tennessee: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  theme_dark()

state.precip.monthly.ranked %>%
  filter(stateCode == "040") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_point(aes(color = recordYear10)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray38") +
  scale_color_manual(values = c("gray80", "darkorange3", "steelblue3")) +
  labs(title = "Tennessee: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  theme_light()

# Create a barchart for a TN
state.precip.monthly.ranked %>%
  filter(stateCode == "040") %>%
  ggplot(aes(x = year, y = precipTotal)) +
  geom_bar(stat = "identity", aes(fill = recordYear10)) +
  labs(title = "Tennessee: Annual precipitation by year 1895 - 2020",
       x = "Year", y = "Total precipitation (inches)",
       caption = "Source: NOAA", color = "") +
  scale_fill_manual(values = c("gray80", "darkorange3", "steelblue4")) 

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Compare 30-year periods for average annual 
# rainfall. 
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# We want to see which states are seeing more rainfall
# than ever before, so we need to compare the 4 30-year
# periods we have in the data:
# 1991 - 2020
# 1961 - 1990
# 1931 - 1960
# 1901 - 1930

# Create a DF for each state's 30-year average

# 1991-2020
avg.precip.1991.2020 <- state.precip.monthly.ranked %>%
  filter(year >= 1991, year <=2020) %>%
  group_by(stateCode, stateName) %>%
  summarise(n = n(),
            avg_precip_1991_2020 = mean(precipTotal))

# 1961-1990
avg.precip.1961.1990 <- state.precip.monthly.ranked %>%
  filter(year >= 1961, year <= 1990) %>%
  group_by(stateCode, stateName) %>%
  summarise(n = n(),
            avg_precip_1961_1990 = mean(precipTotal))

# 1931-1960
avg.precip.1931.1960 <- state.precip.monthly.ranked %>%
  filter(year >= 1931, year <= 1960) %>%
  group_by(stateCode, stateName) %>%
  summarise(n = n(),
            avg_precip_1931_1960 = mean(precipTotal))

# 1901-1930
avg.precip.1901.1930 <- state.precip.monthly.ranked %>%
  filter(year >= 1901, year <= 1930) %>%
  group_by(stateCode, stateName) %>%
  summarise(n = n(),
            avg_precip_1901_1930 = mean(precipTotal))

# Join those 4 sets of averages together
avg.precip.30.yr.compare <- avg.precip.1901.1930 %>%
  inner_join(avg.precip.1931.1960, by = "stateCode") %>%
  inner_join(avg.precip.1961.1990, by = "stateCode") %>%
  inner_join(avg.precip.1991.2020, by = "stateCode") %>%
  select(stateCode, stateName = stateName.x, avg_precip_1901_1930, avg_precip_1931_1960, 
         avg_precip_1961_1990, avg_precip_1991_2020)

# select the states where the last 30
# years of precip were higher than any
# other 30-year period
avg.precip.30.yr.compare.states <-avg.precip.30.yr.compare %>%
  filter(substr(stateCode,1,1) == 0)

avg.precip.30.yr.compare.states <- avg.precip.30.yr.compare.states %>%
  mutate(diff_0130_9120 = avg_precip_1991_2020 - avg_precip_1901_1930,
         pct_diff_0130_9120 = (avg_precip_1991_2020 - avg_precip_1901_1930)/avg_precip_1901_1930,
         diff_3160_9120 = avg_precip_1991_2020 - avg_precip_1931_1960,
         pct_diff_3160_9120 = (avg_precip_1991_2020 - avg_precip_1931_1960)/avg_precip_1931_1960,
         diff_6190_9120 = avg_precip_1991_2020 - avg_precip_1961_1990,
         pct_diff_6190_9120 = (avg_precip_1991_2020 - avg_precip_1961_1990)/avg_precip_1961_1990)

# Select every state where the period 1991-2020
# had an average at least an inch greater than
# every previous period
states.with.precip.increases <- avg.precip.30.yr.compare.states %>%
  filter(pct_diff_0130_9120 >= 0.05, pct_diff_3160_9120 >= 0.05, pct_diff_6190_9120 >= 0.05)
# This shows 19 states with at least a 5%
# increase over all time periods


