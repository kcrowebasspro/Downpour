# Author: Kevin Crowe, USA TODAY, kcrowe@usatoday.com

# This is an analysis of the locations of cities with combined 
# sewer systems and the trends in precipitation in those areas. 

# Cleaning the EPA data:
# - Some of the Lat/Lons of cities with combined systems are off.
# - Some of the cities, especially on the coasts, had coordinates that
#   placed them in the middle of bodies of water.
# - I'm going to join these cities to the internal point Lat/Lon from
#   the census. 

# Spatial analysis objectives:
# - Join each of the cities with a combined sewer system to its
#   U.S. Census geography. Some of these are "Places", some are 
#   other kinds of geographies. 
# - Join the combined sewers city file with the climate divisions
#   layer -- this will be a point-in-polygon spatial join. 
# - Determine how many cities with combined systems are in climate
#   divisions that are seeing increased annual precipitation. 
# - How many of these cities are in areas where extreme precipitation
#   has increased in the past few decades?

# Demographic analysis objectives:
# - The cities with combined systems file now has a NHGIS identifier.
# - Join the Census demographic data (race, eth, income, poverty) to the 
#   cities with combined systems table. 
# - Run an analysis of the demographic attributes of the cities with 
#   combined systems and compare that to the nation as a whole. 
#     - Are combined systems located in lower-income areas?
#     - Areas with higher poverty rates?
#     - We're testing to see if these kinds of systems are in areas
#       with populations that are ill-equipped to handle the expense
#       of updating them. 

# Data sources:
# - Cities with combined systems: https://www.google.com/url?q=https://echo.epa.gov/files/POTWs%2520with%2520Combined%2520Sewer%2520Overflows%2520Facility%2520List.xlsx&sa=D&source=editors&ust=1631034704940000&usg=AOvVaw0cWXkfi6XRSlPK-rILl_14
# - Census data all come from NHGIS

# Load the libraries
library(tidyverse)
library(usmap)
library(stringr)
library(tmap)
library(sf)
library(readxl)

# read in the CSV that has cities with combined sewer systems.
# This list comes from the EPA.
cso.cities <- read_csv("/data/cities_with_CSOs_082521.csv")
str(cso.cities)

# read in demographic data from the census
# for places
cities.census <- read_csv("/data/census_data/nhgis0087_ds244_20195_2019_place.csv",
                          col_types = "cccccccccccccccccccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")

# read in the census places CSV with centroid
cities.centroids <- read_csv("/data/census_data/places_w_centroids.csv")

# Add a state fips to the cso.cities DF
# using the usmap() fips function
cso.cities <- cso.cities %>%
  mutate(state_fips = fips(state))

# Create fields in CSO and centroids file for
# lower names to make the joins easier
cso.cities <- cso.cities %>%
  mutate(city_name_lower = str_to_lower(city))

cities.centroids <- cities.centroids %>%
  mutate(city_name_lower = str_to_lower(NAME))

# Attempt to join the CSO and centroids file
cso.cities %>%
  inner_join(cities.centroids, by = c("city_name_lower" = "city_name_lower", "state_fips" = "STATEFP"))
# Result: 694 records -- 35 short

# Run the anti-join to see what's not working
cities.not.joined <- cso.cities %>%
  anti_join(cities.centroids, by = c("city_name_lower" = "city_name_lower", "state_fips" = "STATEFP"))

# Steps to clean this up:
# - Run the join and create a new table that has the CSO 
#   data plus the GISJOIN from centroids dataframe
# - Check which records did not join
# - Check the few records that might have joined w a wrong place
#   by joining the results with the cities.not.joined table
# - Check the records and manually update the GISJOIN field
#   for those records that did not join -- maybe use NPDES_ID 
#   in the case_when statement

# Run the join -- I will grab the lat/lon for the centroid
# and the GISJOIN field from the cities shapefile
cso.centroids.join <- cso.cities %>%
  left_join(cities.centroids, by = c("city_name_lower" = "city_name_lower", "state_fips" = "STATEFP")) %>%
  select(npdes_id:city_name_lower, GISJOIN) 

# Which records did not join
cso.centroids.join %>%
  filter(is.na(GISJOIN))

# See which records had more than one record in join
cso.centroids.join %>%
  group_by(npdes_id) %>%
  count(sort = TRUE)
# 4 records joined twice

# Figure those out
# - IL0022322 -- should be this GISJOIN: G17028963
# - PA0024686 -- should be this GISJOIN: G42000988
# - PA0026174 -- should be this GISJOIN: G42027456
# - PA0026476 -- should be this GISJOIN: G42014600

# Change the GISJOINs for those records, then get the unique
# records from that joined table
cso.centroids.join <- cso.centroids.join %>%
  mutate(GISJOIN = case_when(npdes_id == "IL0022322" ~ "G17028963",
                             npdes_id == "PA0024686" ~ "G42000988",
                             npdes_id == "PA0026174" ~ "G42027456",
                             npdes_id == "PA0026476" ~ "G42014600",
                             TRUE ~ GISJOIN)) %>%
  unique()

# Which cities don't have a match? 39 total
cso.centroids.join %>%
  filter(is.na(GISJOIN)) %>%
  select(npdes_id) %>%
  print(n=40)

# Use case_when to fill in the blanks manually for CSO cities
# that are in the census places data:
cso.centroids.join <- cso.centroids.join %>%
  mutate(GISJOIN = case_when(npdes_id == "IL0033472" ~ "G17022255", npdes_id == "ILM580009" ~ "G17040767",
                             npdes_id == "IN0032191" ~ "G18025000", npdes_id == "IN0023183" ~ "G18036003",
                             npdes_id == "MA0101168" ~ "G25052144", npdes_id == "MD0067547" ~ "G24046075", 
                             npdes_id == "MI0025585" ~ "G26070760", npdes_id == "WV0023175" ~ "G54071212",
                             npdes_id == "MI0026735" ~ "G26070960", npdes_id == "MI0024058" ~ "G26071740",
                             npdes_id == "MI0020591" ~ "G26070680", npdes_id == "MO0023043" ~ "G29064550",
                             npdes_id == "MO0025178" ~ "G29065000", npdes_id == "NJ0020591" ~ "G34020020",
                             npdes_id == "NJ0108847" ~ "G34026820", npdes_id == "OH0025160" ~ "G39027902",
                             npdes_id == "PA0043885" ~ "G42062432",
                             npdes_id == "PA0025224" ~ "G42067224", npdes_id == "PA0026107" ~ "G42085152",
                             npdes_id == "RI0100072" ~ "G44022960", npdes_id == "TN0020575" ~ "G47052006",
                             TRUE ~ GISJOIN))

# Which are still missing? 18 cities
cso.centroids.join %>%
  filter(is.na(GISJOIN)) %>%
  select(npdes_id) %>%
  print(n=40)
# Some of these are sub units, and some might be PUMAs

# Import the county units for the remaining cities
county.subunits.centroids <- read_csv("/data/census_data/county_subunits_centroids.csv")

# Missing cities -- these are county subunits.
# I checked them individually in on the Census website:
# "ME0100153" ~ "G230019014310", 
# "ME0100307" ~ "G230001040035", 
# "MI0051829" ~ "G260163067625", 
# "MA0101338" ~ "G250013037175", 
# "MA0100137" ~ "G250011042285",
# "MA0100447" ~ "G250009046365", 
# "ME0102121" ~ "G230005010180",
# "NJ0108898" ~ "G340017052470",
# "NY0026191" ~ "G360005008510", 
# "NY0026212" ~ "G360047010022", 
# "NY0026174" ~ "G360085070915", 
# "NY0024414" ~ "G360007077255",
# "PAG064802" ~ "G420097014536" 

# Update those:
cso.centroids.join <- cso.centroids.join %>%
  mutate(GISJOIN = case_when(npdes_id == "ME0100153" ~ "G230019014310", npdes_id == "ME0100307" ~ "G230001040035",
                             npdes_id == "MI0051829" ~ "G260163067625", npdes_id == "MA0101338" ~ "G250013037175",
                             npdes_id == "MA0100137" ~ "G250011042285", npdes_id == "MA0100447" ~ "G250009046365",
                             npdes_id == "ME0102121" ~ "G230005010180", npdes_id == "NJ0108898" ~ "G340017052470",
                             npdes_id == "NY0026191" ~ "G360005008510", npdes_id == "NY0026212" ~ "G360047010022",
                             npdes_id == "NY0026174" ~ "G360085070915", npdes_id == "NY0024414" ~ "G360007077255",
                             npdes_id == "PAG064802" ~ "G420097014536", TRUE ~ GISJOIN))
# Which are still missing
cso.centroids.join %>%
  filter(is.na(GISJOIN)) %>%
  select(npdes_id)
# Still missing 5 -- check PUMAs

# Load the PUMA data
puma.centroids <- read_csv("/data/census_data/puma_centroids.csv")

# These look like Public Use Micro Areas in New York: 
# "NY0026158" ~ "G36004101", 
# "NY0026239" ~ "G36004103", -- used the flushing-whitestone-murray PUMA
# "NY0026115" ~ "G36004112", 
# "NY0026221" ~ "G36004114", -- used the far rockaway-breezy point-broad channel PUMA
# "NY0026131" ~ "G36003804", -- used the east harlem PUMA

# Update those using PUMA data:
cso.centroids.join <- cso.centroids.join %>%
  mutate(GISJOIN = case_when(npdes_id == "NY0026158" ~ "G36004101", npdes_id == "NY0026239" ~ "G36004103",
                             npdes_id == "NY0026115" ~ "G36004112", npdes_id == "NY0026221" ~ "G36004114",
                             npdes_id == "NY0026131" ~ "G36003804", TRUE ~ GISJOIN))

# Which are still missing?
cso.centroids.join %>%
  filter(is.na(GISJOIN)) %>%
  select(npdes_id)
# None!

# Time to add the centroid coordinates from the census data
# to the CSO stuff. 
str(cities.centroids)
str(county.subunits.centroids)
str(puma.centroids)
# All of the lat/longs are double, so it should be fine to 
# join them

# Run the first join on the places/cities dataframe
cso.centroids.join <- cso.centroids.join %>%
  left_join(cities.centroids, by = "GISJOIN") %>%
  select(npdes_id:GISJOIN, INTPTLAT, INTPTLON)

# Count the records where the INTPTLAT/INTPTLON are null
cso.centroids.join %>%
  filter(is.na(INTPTLAT))
# 18 records, which tracks with what I've done so far

# Run another left join and see if I can mutate and select
# to just update the 13 records I want to update using the
# county subunits lat/lon from the census:
cso.centroids.join <- cso.centroids.join %>%
  left_join(county.subunits.centroids, by = "GISJOIN") %>%
  mutate(INTPTLAT.x = case_when(is.na(INTPTLAT.x) & !is.na(INTPTLAT.y) ~ INTPTLAT.y,
                                TRUE ~ INTPTLAT.x),
         INTPTLON.x = case_when(is.na(INTPTLON.x) & !is.na(INTPTLON.y) ~ INTPTLON.y,
                                TRUE ~ INTPTLON.x)) %>%
  select(npdes_id:GISJOIN, INTPTLAT = INTPTLAT.x, INTPTLON = INTPTLON.x)

# Count the records where the INTPTLAT/INTPTLON are null
cso.centroids.join %>%
  filter(is.na(INTPTLAT))
# 5 records still do not have lat/lon
# these are the PUMAS

# Do the same thing for the last 5 records -- update
# only the null lat/lons using the PUMA centroids:
cso.centroids.join <- cso.centroids.join %>%
  left_join(puma.centroids, by = "GISJOIN") %>%
  mutate(INTPTLAT = case_when(is.na(INTPTLAT) & !is.na(INTPTLAT10) ~ INTPTLAT10,
                              TRUE ~ INTPTLAT),
         INTPTLON = case_when(is.na(INTPTLON) & !is.na(INTPTLON10) ~ INTPTLON10,
                              TRUE ~ INTPTLON)) %>%
  select(npdes_id:GISJOIN, INTPTLAT, INTPTLON) 

# Count the records where the INTPTLAT/INTPTLON are null
cso.centroids.join %>%
  filter(is.na(INTPTLAT))
# None -- all have lat/lon


#----------------------------
#----------------------------
# Check the work so far visually
#----------------------------
#----------------------------

# Read in the US states shapefile
us_shape <- st_read("/data/shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
                    stringsAsFactors = FALSE)

# Check out the states
tm_shape(us_shape) +
  tm_polygons()

# I'm going to cut out everything that is not
# in the lower 48:
us_shape <- us_shape %>%
  filter(GEOID != "72", GEOID != "02", GEOID != "15")

# Check out the states
tm_shape(us_shape) +
  tm_polygons()

# check the structure of cso.centriods.join to make sure
# the lat/lon fields we want are numeric
str(cso.centroids.join) # IN
# INTPTLAT/INTPTLON are numeric

# Check the CRS of the us_shape
st_crs(us_shape)
# CRS = 4269, NAD83

# Convert the centroids dataframe to an SF object using the 
# centroid coordinates as lat/long for points
cso.centroids.sf <- st_as_sf(cso.centroids.join, coords = c("INTPTLON", "INTPTLAT"), crs = st_crs(4269))

# Plot the cso centroids with the us_shape
cso.map <- tm_shape(cso.centroids.sf) +
  tm_dots(col = "blue")

# Look at the map in leaflet to check out individual points
tmap_leaflet(cso.map)
# Looks good overall, but a few points are out
# in the water, or in places they shouldn't be. 
# All but 3 cities look much better. 
# Here are the three that need to go back
# to using the original coordinates from the EPA:
# - San Francisco, CA: CA0037664
# - Portland, ME: ME0102075
# - Rockaway Park, NY: NY0026221

# I'll create two new fields for Lat/Long, as 
# well as a field indicating which data set the
# lat/long came from:
cso.centroids.join <- cso.centroids.join %>%
  mutate(good_LAT = case_when(npdes_id == "CA0037664" ~ latitude,
                              npdes_id == "ME0102075" ~ latitude,
                              npdes_id == "NY0026221" ~ latitude,
                              TRUE ~ INTPTLAT),
         good_LON = case_when(npdes_id == "CA0037664" ~ longitude,
                              npdes_id == "ME0102075" ~ longitude,
                              npdes_id == "NY0026221" ~ longitude,
                              TRUE ~ INTPTLON),
         lat_long_source =  case_when(npdes_id == "CA0037664" ~ "EPA",
                                      npdes_id == "ME0102075" ~ "EPA",
                                      npdes_id == "NY0026221" ~ "EPA",
                                      TRUE ~ "NHGIS"))

# Run the conversion to SF again
cso.centroids.sf <- st_as_sf(cso.centroids.join, coords = c("good_LON", "good_LAT"), crs = st_crs(4269))

# Plot the points with the new coordinates
cso.map2 <- tm_shape(cso.centroids.sf) +
  tm_dots(col = "darkgoldenrod2")

# Look at the map in leaflet to check out individual points
tmap_leaflet(cso.map2)
# It all looks good. 

# get rid of the other lat/lon fields for now
cso.centroids.sf <- cso.centroids.sf %>%
  select(-latitude, -longitude, -INTPTLAT, -INTPTLON)


#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Analyze the average annual precipitation 
# patterns for cities with combined systems
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# A few things to do:
# - read in the climate divisions shapefile
# - validate the polygons -- there's one messy one
# - join the cities w combined systems to the climate 
#   divisions, a point-in-polygon join
# - join the combined systems cities dataframe to the 
#   annual precipitation compare for the climate divisions
# - run the analysis of which cities are in climate divs with
#   increasing/decreasing precipitation
# - There is still one city with a combined system in Alaska
#   and I need to take that out because we're not writing
#   about Alaska or Hawaii (or the territories)


# read in climate divisions shapefile
climate.divs.sf <- st_read("/data/shapefiles/CONUS_CLIMATE_DIVISIONS.shp/GIS.OFFICIAL_CLIM_DIVISIONS.shp",
                           stringsAsFactors = FALSE)

# Check the CRS and make sure it's the same as the other files
st_crs(climate.divs.sf)
# It says it's in NAD83, but it doesn't have the ESPG number. 

# Have a look at the divisions
tm_shape(climate.divs.sf) +
  tm_polygons()

# From previous work, I know there is an invalid geometry.
# One climate division in Arizona is messed up. But, running
# the make valid function cleans it up. 
climate.divs.sf <- st_make_valid(climate.divs.sf)

# Have a look at the divisions
map3 <- tm_shape(climate.divs.sf) +
  tm_polygons()

tmap_leaflet(map3)
# they look good

# read in the 30-year period changes for climate divisions
climate.divs.precip.compare <- read_rds("/data/clim_div_avg_ann_precip_changes_QGIS_091521.RDS")

# Remove Alaska from the cities with combined systems
cso.centroids.sf <- cso.centroids.sf %>%
  filter(state != "AK")

# Check the projections before joining
st_crs(cso.centroids.sf) # CRS = 4269
st_crs(climate.divs.sf) # CRS is NAD83, but not 4269

# Set the climate divisions CRS to 4269 (NAD 83)
climate.divs.sf <- st_transform(climate.divs.sf, crs = 4269)

# Join the centroids to the climate divisions
cso.centroids.climate.divs.join <- st_join(cso.centroids.sf, left = FALSE, climate.divs.sf)

# Check a few things with the join.
# Are all of the states the same from the cities
# file and the climate divisions file
diff.state.cso.clim.div <- cso.centroids.climate.divs.join %>%
  filter(state != ST_ABBRV)
# One is not the same: Washington, D.C.
# It got placed in a Maryland climate division. That's
# fine because D.C. does not have it's own climate division.

# the DivJoin fields are in different formats in the
# precip compare and cso join data frames. I need to make
# them the same
cso.centroids.climate.divs.join <- cso.centroids.climate.divs.join %>%
  mutate(divJoin = if_else(nchar(as.character(STATE_CODE)) == 1, paste0("CD0", STATE_CODE, CD_2DIG), paste0("CD", STATE_CODE, CD_2DIG)))

# Join the centroids to the precip.compare data
cso.centroids.climate.divs.join <- cso.centroids.climate.divs.join %>%
  inner_join(climate.divs.precip.compare, by = c("divJoin" = "DivJoin"))

# Do some point-level checking:
cso.join.map <- map3 +
  tm_shape(cso.centroids.climate.divs.join) +
  tm_dots(col = "darkgoldenrod2")

# Point-level check looks good. 
tmap_leaflet(cso.join.map)

#----------------------------
#----------------------------
# Analysis of precipitation changes
# and cities with combined systems
#----------------------------
#----------------------------

# How many cities with combined systems are in areas
# of the country where precipitation is increasing?
cso.centroids.climate.divs.join %>%
  filter(diff6120 > 0)
# 723 of 728 (99%) of cities with combined systems
# are in areas where average annual precipitation
# increased over the past 60 years. 
723/728

# Histogram of counts of cities by inches
# increase in annual precipitation
ggplot(data = cso.centroids.climate.divs.join, aes(diff6120)) +
  geom_histogram()
# There is a pretty large grouping around 2.5 inches, meaning
# that a lot of cities with combined systems are in places
# where average annual precipitation increased by around that amount. 

# What was the average change?
cso.centroids.climate.divs.join %>%
  st_drop_geometry() %>%
  summarise(mean(diff6120))
# 2.61 inches. 

# run that with just the unique climate divisions
cso.centroids.climate.divs.join %>%
  st_drop_geometry() %>%
  select(divJoin, diff6120) %>%
  unique() %>%
  summarise(avg_change = mean(diff6120),
            median_change = median(diff6120))
# This way it's and average of 2.38 inches
# median change of 2.54 inches

#----------------------------
#----------------------------
# Bring in the station data to
# check on which cities are in
# areas with increasing extreme 
# precipitation
#----------------------------
#----------------------------

# load the station data
stations <- read_excel("/data/Station_Data_USA_Today.xlsx",
                       sheet = "Summary")

# add state to the station
stations <- stations %>%
  mutate(state_abbr = str_squish(substr(Station_Name, 5,6)))

# Filter out AK and HI
stations <- stations %>%
  filter(state_abbr != "AK", state_abbr != "HI")

# Convert the stations to an SF object
stations.sf <- st_as_sf(stations, coords = c("Lon", "Lat"), crs = st_crs(4269))

# Plot the stations to give a visual check
stations.map <- tm_shape(stations.sf) +
  tm_dots(col = "darkgoldenrod2")

# Look at the map in leaflet to check out individual points
tmap_leaflet(stations.map)
# All good

# Find each city's nearest station, which is a multi-step process:
# Use st_nearest_feature() to get the index of the station
# that is nearest to each city with a combined system.
# Then, get the distance to that station.
cso.clim.divs.nearest.stations <- st_join(cso.centroids.climate.divs.join, stations.sf, join = st_nearest_feature)

nearest.stations.index <- st_nearest_feature(cso.centroids.climate.divs.join, stations.sf)

# Use st_distance to compute the distance between the two points
# Units = meters
nearest.stations.distance <- st_distance(cso.centroids.climate.divs.join, stations.sf[nearest.stations.index,], by_element = TRUE)

# Convert the distance list to a dataframe
nearest.stations.distance.df <- as.data.frame(nearest.stations.distance)

str(nearest.stations.distance.df)

# Join the nearest stations distance DF to the 
# nearest stations join
cso.clim.divs.nearest.stations <- cso.clim.divs.nearest.stations %>%
  bind_cols(nearest.stations.distance.df) %>%
  mutate(nearest_station_distance_miles = as.numeric(nearest.stations.distance)/1609.344)

# Check the change in the 3-day precipitation events
# at the weather station closest to each city:
cso.clim.divs.nearest.stations %>%
  st_drop_geometry() %>%
  filter(ChangePct_Days3 > 0) %>%
  count()
# It increased at 673/728 (92%)
673/728

# Where it increased by at least 10%
cso.clim.divs.nearest.stations %>%
  st_drop_geometry() %>%
  filter(ChangePct_Days3 >= 10) %>%
  count()
# 594/728 (82%) increased by at least 10%
594/728

# I also used inverse distance weighting in QGIS to create a 
# gridded layer of the station data. I joined the cities with
# combined systems to that IDW layer in QGIS. 
# Reading in that file here: 
cso.cities.stations.idw <- read.csv("/data/cities_w_combined_systems_join_IDW_station_data_3day_pct.csv")

# Check how many were in places with increasing
# frequency of extreme events. 
cso.cities.stations.idw %>%
  filter(state != "AK", pctChg3day > 0) %>%
  count()
# 709/728 (97%) are in areas with increasing extreme precipitation
709/728

# How many are in areas where those events
# increased by at least 10%?
cso.cities.stations.idw %>%
  filter(state != "AK", pctChg3day >= 10) %>%
  count()
# 686/728 (94%) are in areas where the events increased
# by at least 10%
686/728

# How many are in areas where those events
# increased by at least 33%?
cso.cities.stations.idw %>%
  filter(state != "AK", pctChg3day >= 33) %>%
  count()
# 546/728 (75%) are in areas where the events increased
# by at least 33%, meaning they would see at least one
# additional day per year of extreme rainfall. 
546/728

#----------------------------
#----------------------------
# Prep data for demographic
# analysis
#----------------------------
#----------------------------
# We want to understand the demographic characteristics of
# cities that have combined sewer systems. 
# I got race, ethnicity, poverty rates and median household
# income for Census places, PUMAs and county subunits from 
# NHGIS. I'm using the 2019 5-year ACS figures.
# There is a GISJOIN field for each city with a combined 
# system so I can use that to run the joins on the data. 

# Loading the three census files:
places.census <- read_csv("//data/census_data/nhgis0087_ds244_20195_2019_place.csv",
                          col_types = "cccccccccccccccccccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnn")

county.subs.census <- read_csv("/data/census_data/nhgis0093_ds244_20195_cty_sub.csv",
                               col_types = "ccccccccccccccccccccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")

pumas.census <- read_csv("/data/census_data/nhgis0092_ds244_20195_puma.csv",
                         col_types = "ccccccccccccccccccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")

# Bind the three census files together to then do some
# column renaming, math, etc. 
census.combined <- places.census %>%
  bind_rows(county.subs.census, pumas.census)

# Check the row counts: 68,573 looks good
29573+2378+36622

# Rename some of the variables:
census.combined <- census.combined %>%
  rename(tot_pop = ALUKE001,
         not_hisp = ALUKE002,
         white_not_hisp = ALUKE003,
         black_not_hisp = ALUKE004,
         am_ind_not_hisp = ALUKE005,
         asian_not_hisp = ALUKE006,
         nat_haw_not_hisp = ALUKE007,
         other_race_not_hisp = ALUKE008,
         two_more_not_hisp = ALUKE009,
         two_more2_not_hisp = ALUKE010,
         two_more3_not_hisp = ALUKE011,
         hispanic = ALUKE012,
         white_hisp = ALUKE013,
         black_hisp = ALUKE014,
         am_ind_hisp = ALUKE015,
         asian_hisp = ALUKE016,
         nat_haw_hisp = ALUKE017,
         other_race_hisp = ALUKE018,
         two_more_hisp = ALUKE019,
         two_more2_hisp = ALUKE020,
         two_more3_hisp = ALUKE021,
         tot_pop_pov = ALWVE001,
         pct_50_pov = ALWVE002,
         pct_50_99_pov = ALWVE003,
         pct_100_124_pov = ALWVE004,
         pct_125_149_pov = ALWVE005,
         pct_150_184_pov = ALWVE006,
         pct_185_199_pov = ALWVE007,
         pct_200_pov = ALWVE008,
         median_hh_income = ALW1E001)

# Create some new columns with poverty and race/eth stats
census.combined <- census.combined %>%
  mutate(pct_white = (white_not_hisp/tot_pop),
         pct_non_white = ((tot_pop - white_not_hisp)/tot_pop),
         pct_black = (black_not_hisp/tot_pop),
         pct_hisp = (hispanic/tot_pop),
         pct_poverty = ((pct_50_pov + pct_50_99_pov)/tot_pop_pov))

# Join the census data to the cities with combined systems file:
cso.clim.divs.nearest.stations %>%
  inner_join(census.combined, by = "GISJOIN")
# I got 730 results, which means a few are in there
# more than once

# Which are joining more than once?
cso.clim.divs.nearest.stations %>%
  st_drop_geometry() %>%
  inner_join(census.combined, by = "GISJOIN") %>%
  group_by(GISJOIN) %>%
  count(sort = TRUE)
# 3 places are joining more than once, and one not at all
# G13004000 - Should be Atlanta, GA, not PUMA w GEOID = 79500US1304000
# G36001000 - should be Albany, NY, not PUMA w GEOID = 79500US3601000
# G39001000 - Should be Akron, OH, not PUMA w GEOID = 79500US3901000

# Which GISJOIN is missing?
cso.clim.divs.nearest.stations %>%
  st_drop_geometry() %>%
  anti_join(census.combined, by = "GISJOIN")
# Louisville, KY
# For this, we'll go with: Louisville/Jefferson County metro government (balance), GISJOIN: G21048006

# Reset the Louisville GISJOIN field
cso.clim.divs.nearest.stations <- cso.clim.divs.nearest.stations %>%
  mutate(GISJOIN = if_else(npdes_id == "KY0022411", "G21048006", GISJOIN))

# Test the join again:
cso.clim.divs.nearest.stations %>%
  st_drop_geometry() %>%
  inner_join(census.combined, by = "GISJOIN") %>%
  filter(GEOID != "79500US1304000", GEOID != "79500US3601000", GEOID != "79500US3901000") %>% # Filters out the double joined records
  group_by(GISJOIN) %>%
  count(sort = TRUE)
# 728 records -- looking good

# Run the join
cso.clim.divs.nearest.stations <- cso.clim.divs.nearest.stations %>%
  inner_join(census.combined, by = "GISJOIN") %>%
  filter(GEOID != "79500US1304000", GEOID != "79500US3601000", GEOID != "79500US3901000") %>%
  select(npdes_id:nearest_station_distance_miles, GEOID, tot_pop, pct_white, pct_non_white, pct_hisp, pct_black, pct_poverty, median_hh_income)


#----------------------------
#----------------------------
# Demographic Analysis
#----------------------------
#----------------------------

# We're running a basic analysis of household income,
# poverty rates and percentage of white vs non-white population.
# The questions:
# - Are combined sewer/storm systems in places with lower than
#   average household incomes?
# - Are they in places with higher than average poverty rates?
# - Are non-white populations more affected by the problem?

# Get the basic stats:
cso.clim.divs.nearest.stations %>%
  st_drop_geometry() %>%
  summarise(avg_hh_income = mean(median_hh_income),
            med_hh_income = median(median_hh_income),
            avg_pct_poverty = mean(pct_poverty),
            median_pct_poverty = median(pct_poverty),
            avg_pct_non_white = mean(pct_non_white),
            median_pct_non_white = median(pct_non_white),
            avg_pct_white_alone = mean(pct_white))
# Avg HH income: 49,872 compared to 62,843 national figure
# Avg pct poverty: 17.9% compared to 13.4% national rate
# Avg non-white pop: 23.8% 
# Avg white alone pop: 76.2% compared to 60.1%
