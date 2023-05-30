## Header ----
## Script name: prep_usgs_data.R
##
## Purpose of script: format downloaded USGS data (see usgs_data_retrivel.R and 
##                    usgs_data_compiler.R) into streamMetabolizer ready format
##
## Author: Nick Marzolf
## Date Created: 
## Date Modified: 2022-03-21
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(streamMetabolizer)
library(glue)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


# import site data ---
sites <- read_csv('trial/data/sites_with6yrs.csv')

length(unique(sites$x)) # 63
names <- pull(sites, x)

# import USGS data ----
# q <- read_csv('data/sites/sites_disch.csv')
# 
# unique(q$site)
# length(unique(q$site))
# q_names <- pull(q, site)
# 
# setdiff(names, q_names)
# # "nwis_02148315" "nwis_02156500" "nwis_02160105" "nwis_08180700"
# 
# 
# do <- read_csv('data/sites/sites_doobs.csv')
# length(unique(do$site))
# 
# do_names <- pull(do, site)
# setdiff(names, do_names)
# # "nwis_02148315" "nwis_08180700"
# 
# 
# temp <- read_csv('data/sites/sites_temp.csv')
# length(unique(temp$site))
# temp_names <- pull(temp,site)
# 
# setdiff(names,temp_names)
# # "nwis_02148315" "nwis_02156500" "nwis_02160105" "nwis_08180700"

# jump here, usgs_data.csv contains discharge, DO, and temperature in wide-format
all <- read_csv('data/usgs_data.csv')
length(unique(all$site))

all_names <- pull(all, site)

bad_sites <- setdiff(names, all_names)
# "nwis_02148315" "nwis_02156500" "nwis_02160105" "nwis_08180700"

# exlude sites that we thought had data but did not return data in dataRetriveal::readNWISuv()
all <- all %>%
  filter(!site %in% bad_sites) 


# for loop to prep input files ----
# get barometric pressure for each site using the functions in StreamPulse 
# calc light using streamMetabolizer function

# function to pull nearby barometric pressure
source('code/functions/FindandCollect_airpres.R')

# lat/long of the sites
usgs_site_locs <- read_csv('data/site_locs.csv') %>%
  mutate(site_no = paste0('nwis_', site_no)) %>%
  filter(!site_no %in% bad_sites)


usgs_sites_coefs <- read_csv('data/morphology/Appling et al. 2018/usgs_sites_coefs.csv')

# for loop to create a csv for each site in streamMetabolizer format
for(j in 1:length(usgs_site_locs$site_no)) {
  
  # ID the site
  site_id <- unique(usgs_site_locs$site_no)[j]
  
  # define lat and long
  lat <- usgs_site_locs %>%
    filter(site_no == site_id) %>%
    pull(dec_lat_va)
  
  long <- usgs_site_locs %>%
    filter(site_no == site_id) %>%
    pull(dec_long_va)
  
  # date range we are interested in
  start_datetime <- '2016-12-01'
  end_datetime <- '2021-12-31'
  
  # download barometric pressure based on coordinates and dates
  bp <- FindandCollect_airpres(lat = lat,
                               long = long,
                               start_datetime = start_datetime,
                               end_datetime = end_datetime) 
  bp <- rename(bp,
               'DateTime' = 'DateTime_UTC')
  
  
  c <- usgs_sites_coefs %>% 
    filter(site_name == site_id) %>% 
    pull(dvqcoefs.c)
  
  d <- usgs_sites_coefs %>% 
    filter(site_name == site_id) %>% 
    pull(dvqcoefs.d)
  
  # create output dataframe, starting from wide-fromat data
  out <- all %>%
    # match sites
    filter(site == site_id) %>%
    # join with barometric pressure data
    left_join(., bp,
              by.x = 'DateTime') %>%
    # select the columns and rename the ones needing reframing
    select(DateTime_UTC = DateTime,
           site,
           temp.water = wtr,
           disch,
           DO.obs = doobs,
           air_mb) %>%
    mutate(
      # calculate DO.sat from barometric pressure
      DO.sat = calc_DO_sat(temp.water = temp.water, 
                           pressure.air = air_mb,
                           salinity.water = 0,
                           model = 'garcia-benson'),
      # calculate solar.time from UTC
      solar.time = calc_solar_time(local.time = DateTime_UTC, 
                                   longitude = long),
      # estimate light using streamMetabolizer in-house function
      light = calc_light(solar.time = solar.time,
                         latitude = lat,
                         longitude = long),
      # convert discharge from cfs to m3/s
      discharge = disch*0.0283168,
      depth = c*discharge^d) %>% 
    # one more select of the needed columns
    select(site, solar.time, temp.water, discharge, DO.obs, DO.sat, light, depth)
  
  # write the csv file
  write_csv(out,
            glue('data/usgs_sm_ready/{s}.csv',
                 s = site_id))
  
  # make each output dataframe into a figure
  ggplot(out %>%
           filter(site == site_id) %>%
           pivot_longer(cols = c(3:8)),
         aes(x = solar.time, y = value))+
    geom_point()+
    facet_wrap(name ~ .,
               scales = 'free')+
    ggtitle(site_id)
  
  # save each site-timeseries
  ggsave(glue('figures/usgs_ts/{s}_ts.png',
              s = site_id),
         width = 5, height = 4)
}

# make time series plots into a gif ----
library(magick)
library(magrittr)

list.files(path='figures/usgs_ts', 
           pattern = '*.png', 
           full.names = TRUE) %>% 
  image_read() %>%                               # reads each path file
  image_join() %>%                               # joins image
  image_animate(fps = 4) %>%                     # animates, can opt for number of loops
  image_write("figures/usgs_ts/usgs_ts_all.gif") # write to current dir
