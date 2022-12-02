#modified for use on DCC.

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
# library(ggplot2)
library(streamMetabolizer)
library(glue)
library(padr)
library(imputeTS)
library(lubridate)
library(geosphere)
library(data.table)
##
## clear the environment if needed
# rm(list = ls())
##
## set the ggplot theme
# source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
# theme_set(theme_nick())
setwd('/hpc/group/bernhardtlab/usgs_metab_forMike_v2_test/')


# import site data ---
# sites <- read_csv('trial/data/sites_with6yrs.csv')
# names <- pull(sites, x)
names <- list.files('data/usgs_from_streampulse_old/') %>% 
  str_match('(nwis_[0-9]+).*') %>% 
  {.[, 2]}


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
all <- read_csv('data/usgs_data_2017-2021.csv')

all_names <- pull(all, site)

bad_sites <- dplyr::setdiff(names, all_names)
# "nwis_02148315" "nwis_02156500" "nwis_02160105" "nwis_08180700"

# exlude sites that we thought had data but did not return data in dataRetriveal::readNWISuv()
all <- all %>%
  filter(!site %in% bad_sites) 


# for loop to prep input files ----
# get barometric pressure for each site using the functions in StreamPulse 
# calc light using streamMetabolizer function

# function to pull nearby barometric pressure
source('code/functions/FindandCollect_airpres.R')
# source('code/functions/complete_days.R')
source('code/functions/Mode.R')
source('code/functions/determine_sample_interval.R')

# lat/long of the sites
usgs_site_locs <- read_csv('data/site_locs.csv') %>%
  mutate(site_no = paste0('nwis_', site_no)) %>%
  filter(!site_no %in% bad_sites)

# read in the hydraulic coefficients from Appling et al. 2018 (and on her Github)
usgs_sites_coefs <- read_csv('data/usgs_sites_coefs.csv')


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
  
  # define start and end dates
  start_datetime <- all %>%
    filter(site == site_id) %>% 
    arrange(DateTime) %>% 
    pull(DateTime) %>% 
    first()
  
  end_datetime<- all %>%
    filter(site == site_id) %>% 
    arrange(DateTime) %>% 
    pull(DateTime) %>% 
    last()
  
  
  # download barometric pressure based on coordinates and dates; available at 15-minute resolution
  bp <- FindandCollect_airpres(lat = lat,
                               long = long,
                               start_datetime = start_datetime,
                               end_datetime = end_datetime) 
  bp <- rename(bp,
               'DateTime' = 'DateTime_UTC')
  
  # read in hydraulic coefficients from Appling et al. (2018)
  c <- usgs_sites_coefs %>% 
    filter(site_name == site_id) %>% 
    pull(dvqcoefs.c)
  
  d <- usgs_sites_coefs %>% 
    filter(site_name == site_id) %>% 
    pull(dvqcoefs.d)
  
  # create output dataframe, starting from wide-fromat data
  filt <- all %>%
    
    # match sites
    filter(site == site_id)
  
          # remove days with 0 (or negative) discharge [don't do this yet]
           # disch > 0) %>%
  
  samp_int <- determine_sample_interval(filt, 'DateTime') #in minutes
  samps_per_3hr <- 180 / samp_int
  samps_per_day <-  1440/ samp_int
  
  dat <- filt %>% 
    arrange(DateTime) %>% 
    
    # correct date times that are off of regular intervals. if samp_int is inconsistent, this will unify it.
    mutate(DateTime = floor_date(DateTime, paste(samp_int, 'min'))) %>% 
    
    #create records for implicit missing values
    tidyr::complete(DateTime = seq(.$DateTime[1],
                                   .$DateTime[nrow(.)],
                                   by = paste(samp_int, 'min'))) %>% 
  
    # join with barometric pressure data
    left_join(., bp,
              by.x = 'DateTime') %>%
    
    # complete_days: removes days with not enough data and gap fills days that have gaps <3 hours
    # complete_days() %>%
    
    #interpolate gaps of no more than 3 hours
    mutate(across(any_of(c('disch', 'wtr', 'doobs', 'air_mb', 'air_temp')),
                  ~imputeTS::na_interpolation(., maxgap = samps_per_3hr))) %>% 
    
    # select the columns and rename the ones needing renaming
    select(DateTime_UTC = DateTime,
           site,
           temp.water = wtr,
           disch,
           DO.obs = doobs,
           air_mb) %>% 
    arrange(DateTime_UTC) #another arrange is required if samp_int is inconsistent
  
  dat$site[is.na(dat$site)] <- site_id
  
  out <- dat %>%  
    mutate(
      # calculate DO.sat from barometric pressure
      DO.sat = calc_DO_sat(temp.water = temp.water, 
                           pressure.air = air_mb,
                           salinity.water = 0,
                           model = 'garcia-benson'),
      # calculate solar.time from UTC; this may cut off a few days
      solar.time = convert_UTC_to_solartime(date.time = DateTime_UTC, 
                                            longitude = long, 
                                            time.type = 'mean solar'),
      # estimate light using streamMetabolizer in-house function
      light = calc_light(solar.time = solar.time,
                         latitude = lat,
                         longitude = long),
      # convert discharge from cfs to m3/s
      discharge = disch*0.0283168,
      depth = c*discharge^d) %>% 
    
    # one more select of the needed columns
    select(site, solar.time, temp.water, discharge, DO.obs, DO.sat, light, depth)
  
  if(any(duplicated(out$solar.time))){
    
    #average replicate (or duplicate) measurements
    # data_cols <- c('discharge', 'temp.water', 'DO.obs', 'DO.sat', 'light', 'depth')
    out <- as.data.table(out)
    out <- out[, lapply(.SD, mean, na.rm = TRUE), by = c('site', 'solar.time')]
    out <- as_tibble(out)
    # out <- group_by(out, solar.time) %>% 
    #   summarize(across(any_of(c('discharge', 'temp.water', 'DO.obs', 'DO.sat', 'light', 'depth')),
    #                    mean, na.rm = TRUE),
    #             .groups = 'drop')
  }
  
  # remove records with missing values
  out <- out[complete.cases(out), ]
  
  #remove days with any missing samples
  insufficient_days <- out %>% 
    group_by(solar.date = as.Date(solar.time)) %>%
    filter(n() < samps_per_day) %>% 
    ungroup() %>% 
    pull(solar.date) %>% 
    unique()
  
  out <- filter(out, ! date(solar.time) %in% insufficient_days)
  
  nn = out %>%
    group_by(date = as.Date(solar.time)) %>%
    summarize(n = n()) %>% 
    pull(n)
  
  if(any(nn != samps_per_day)) stop('address this')
  
  # nn = out %>%
  #   group_by(date = as.Date(solar.time)) %>%
  #   summarize(n = n())
  # dd = out %>%
  #   mutate(date = as.Date(solar.time)) %>%
  #   full_join(nn)
  # dd2 = filter(dd, n != 48)
  # filter(dd, n == 48) %>% head()
  # as.data.frame(distinct(dd2, date, n))
  # jj = filter(dd, solar.time >= as.POSIXct('2021-03-27', tz='UTC'),
  #        solar.time <= as.POSIXct('2021-03-28', tz='UTC'))
  #   View(jj)
  # jj2 = filter(dd, solar.time >= as.POSIXct('2021-01-01', tz='UTC'),
  #        solar.time <= as.POSIXct('2021-01-02', tz='UTC'))
  #   View(jj2)
  # qqr = filter(filt, DateTime >= as.POSIXct('2021-12-28', tz='UTC'),
  #              DateTime <= as.POSIXct('2021-12-29', tz='UTC'))
  # View(qqr)
  
  # # to come: interpolate if data can't be run at >15 min interval
  # out_15min <- out %>% 
  #   mutate(diffTime = c(NA, diff(solar.time))) %>% 
  #   filter(diffTime < 10801) %>% 
  #   pad('15 min') %>%
  #   fill(site) %>%
  #   fill_by_function(temp.water,discharge,depth,light,DO.obs, DO.sat,
  #                    fun = na_interpolation) %>% 
  #   dplyr::select(-diffTime)
  
  # write the csv file
  write_csv(out,
            glue('data/usgs_sm_ready_recent/{s}_{first(date(out$solar.time))}_{last(date(out$solar.time))}.csv',
                 s = site_id))
  
  # make each output dataframe into a figure
  # ggplot(out %>%
  #          filter(site == site_id) %>%
  #          pivot_longer(cols = c(3:8)),
  #        aes(x = solar.time, y = value))+
  #   geom_point()+
  #   facet_wrap(name ~ .,
  #              scales = 'free')+
  #   ggtitle(site_id)
  # 
  # # save each site-timeseries
  # ggsave(glue('figures/usgs_ts/{s}_ts.png',
  #             s = site_id),
  #        width = 5, height = 4)
}

# dat %>% group_by(date(DateTime_UTC)) %>% summarise(n()) %>% View
# out %>% group_by(date(solar.time)) %>% summarise(n()) %>% View


# # make time series plots into a gif ----
# library(magick)
# library(magrittr)
# 
# list.files(path='figures/usgs_ts', 
#            pattern = '*.png', 
#            full.names = TRUE) %>% 
#   image_read() %>%                               # reads each path file
#   image_join() %>%                               # joins image
#   image_animate(fps = 4) %>%                     # animates, can opt for number of loops
#   image_write("figures/usgs_ts/usgs_ts_all.gif") # write to current dir
