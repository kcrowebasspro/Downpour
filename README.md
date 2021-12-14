<h1>Analysis of NOAA Monthly Precipitation Totals 1895-2020</h1>

This repository contains the data, analytic code and the findings used in the USA TODAY project <a href = "https://www.usatoday.com/in-depth/news/investigations/2021/11/30/climate-change-extreme-rainfall/8550366002/">Downpour</a>, published in December 2021. 

<h2>Data</h2>

The two main datasets used for the visual story and main article of this project came from NOAA's National Centers for Environmental Information. They consist of monthly precipitation totals from 1895 to 2020 for NOAA's 344 <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/data/climdiv-pcpndv-v1.0.0-20211006.txt">climate divisions</a> as well as <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/data/climdiv-pcpnst-v1.0_080521.txt">states</a> in the continental U.S.

Link: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
	<br>- Climate divisions file convention: climdiv-pcpndv-v1.0.0-YYYYMMDD
	<br>- State file convention: climdiv-pcpnst-v1.0.0-YYYYMMDD.txt

<h2>Methodology and Analysis</h2>

We looked for the smallest geography we could to examine trends in annual precipitation. Several experts pointed us to NOAA's climate divisions as a good compromise between the state level, which can obscure trends within a state, and the county level, which can be inaccurate due to lack of weather stations present in the county.

In order to compare periods of time, we <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/scripts/NOAA_climate_division_avg_ann_precip_analysis.R">calculated average annual precipitation</a> figures per division for 30-year time periods ranging from 1901 to 2020. Some of the graphics and findings come from comparing the two most recent 30-year periods, 1961-1990 and 1991-2020. During that time, three quarters of the climate divisions in the U.S. experienced an increase in average annual precipitation. 

To determine when a majority of climate divisions saw their wettest and driest periods on record, we calculated rolling 30-year averages and compared those over time. More than half of the climate divisions have seen their wettest 30-year periods on record in the years ending in 2018, 2019 and 2020. Most divisions had their driest periods end during the 1930s and 1970s. 

We used the state-level data for two purposes: to determine when each state hit record precipitaiton levels, and to measure the changes over longer periods of time. <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/scripts/NOAA_climate_division_state_level_data_analysis.R">We measured</a> the top 10 wettest and top 10 driest years to get an idea of which states have seen the most extremes in terms of overall precipitation (wet years), drought (dry years), or whiplash (a mix of wet and dry extremes). We found that in the past 20 years, states have experienced more record wet and/or dry years than any other 20-year period on record. Twenty-five states had at least 5 record years during a period they could expect to see 3 on average. 

To create rolling 30-year averages for each state to measure which have seen their wettest periods on record in the past few years.


<h1>Analyzing changes in heavy precipitation events</h1>

<h2>Data</h2>

As our climate grows warmer, the frequency of intense precipitation events has increased. Our goal with this <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/scripts/Weather_station_analysis.R">analysis</a> was to give readers an idea of what has been happening in their own backyards in terms of heavy storms. 

Brian Brettschneider, a climatologist based in Alaska, <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/data/Station_Data_USA_Today.xlsx">built a dataset</a> from daily precipitation readings at weather stations across the U.S. Station had to meet certain reporting thresholds to be included in the analysis. He compared the two periods 1951-1990 and 1991-2020 to measure the change in frequency of heavy precipitation events. More on Brettschneider's methodology <a href = "http://us-climate.blogspot.com/2021/05/">here.</a>

<h1>Analysis of Cities with Combined Sewer Systems</h1>

<h2>Data</h2>

In order to determine how many cities with combined sewer systems are in areas with increasing average annual precipitation and/or in areas where heavy precipiation events are happening with increasing frequency, USA TODAY analyzed data from the Environmental Protection Agency, NOAA, U.S. Census data from IPUMS NHGIS at the University of Minnesota, and the weather station data compiled by climatologist Brian Brettschneider. 

<h2>Methodology and Analysis</h2>

For <a href = "https://github.com/kcrowebasspro/Downpour/blob/main/scripts/Cities_with_combined_sewer_systems_analysis.R">this analysis</a>, we ran a spatial join on the cities with combined sewer systems and NOAA's climate divisions. We had already computed the changes in average annual precipitation by climate division by comparing two 30-year periods 1960-1990 and 1991-2020. 

We found an overwhelming majority of cities with combined systems are in places that saw annual precipitation increase during the past 60 years. 

We used weather station data compiled by Brettschneider to determine which cities with combined systems are in areas that saw an increasing number of heavy precipitation events. 

<h2>Feedback or questions?</h2>
Contact Kevin Crowe at kcrowe@usatoday.com