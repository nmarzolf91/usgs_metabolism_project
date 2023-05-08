## Header ----
## Script name: get_usgs_streampulse_data.R
##
## Purpose of script: pull the old (2007-2016) USGS data from streampulse and format for streamMetabolizer
##
## Author: Nick Marzolf
## Date Created: 
## Date Modified: 2022-03-15
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dataRetrieval)
library(glue)
library(StreamPULSE)
library(lubridate)
library(streamMetabolizer)
library(padr)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())

# read in function from streamPULSE to pull nearby barometric pressure
# Might be redundant, as streampulse is being loaded in this script
source("code/functions/FindandCollect_airpres.R")

# usgs sites to pull old data
# same 63 sites for the recent data (2017-2021)
site_codes <- readr::read_csv('data/site_locs.csv') %>% 
  dplyr::mutate(site_code = paste('nwis-',site_no, sep = ''))

# pull all site data from streampulse
sp_all_sites <- StreamPULSE::query_available_data(region='all')

# filter streampulse sites to the sites we need
old_usgs_sites <- sp_all_sites$sites %>% 
  dplyr::filter(site %in% site_codes$site_code) %>% 
  dplyr::mutate(sitecode = paste(region, site, sep = '_')) # mutate the sitecode used downstream


# get the data
for(i in 1:length(old_usgs_sites$sitecode)) {
  
  # extract streampulse code and site name for later
  sp_code <- old_usgs_sites$sitecode[i]
  
  site_name <- sub('-','_', sp_code) %>% 
    substr(4,20)
  
  # define longitude
  lon <- old_usgs_sites$longitude[i]
  
  # pulls data from streampulse; takes a few minutes per site
  streampulse_data <- try({
    StreamPULSE::request_data(sitecode = sp_code
                              #startdate = start_date,
                              #enddate = end_date
    )})
  
  # define date range for file naming
  start_date <- dplyr::first(streampulse_data$data$DateTime_UTC) %>% 
    substr(1,10)
  
  end_date <- dplyr::last(streampulse_data$data$DateTime_UTC) %>% 
    substr(1,10)
  
  readr::write_csv(streampulse_data$data,
                   glue::glue('data/usgs_from_streampulse_old/{s}_{start_date}_{end_date}.csv',
                              s = site_name))
}

# rearrange the output dataset to the input for streamMetabolizer
for(i in 1:length(old_usgs_sites$sitecode)) {
  
  # # extract streampulse code and site name for later
  # sp_code <- old_usgs_sites$sitecode[i]
  # 
  # site_name <- sub('-','_', sp_code) %>% 
  #   substr(4,20)
  # 
  # # define longitude
  # lon <- old_usgs_sites$longitude[i]
  # 
  
  # read in downloaded streampulse data
  df <- readr::read_csv(glue::glue('data/usgs_from_streampulse_old/{s}_{start_date}_{end_date}.csv',
                                   s = site_name))
  
  # format the data to input into streamMetabolizer
  out <- df %>% 
    # pivot from long to wide
    tidyr::pivot_wider(names_from = variable, 
                       values_from = value) %>% 
    # throw out no (and negative) discharge values
    dplyr::filter(Discharge_m3s > 0) %>% 
    # adjust the date-times
    dplyr::mutate(datetime = lubridate::ceiling_date(DateTime_UTC, '1 min'),
                  solar.time = streamMetabolizer::convert_UTC_to_solartime(datetime,
                                                                           longitude = lon, time.type = 'mean solar'),
                  site_id = sub('-','_', site)) %>% 
    dplyr::select(site_id,
                  solar.time,
                  temp.water = WaterTemp_C,
                  DO.obs = DO_mgL,
                  DO.sat = satDO_mgL,
                  depth = Depth_m,
                  discharge = Discharge_m3s,
                  light = Light_PAR)
  
  # write the file for the old csv to the drive
  readr::write_csv(out,
                   glue::glue('data/usgs_sm_ready_old/{s}_{start_date}_{end_date}.csv',
                              s = site_name))
  
  readr::write_csv(out,
                   glue::glue('data/data_citation/2_timeseries_raw/usgs_sm_ready_old/{s}_{start_date}_{end_date}.csv',
                              s = site_name))
}
