## Header ----
## Script name: 
##
## Purpose of script:
##
## Author: Nick Marzolf
## Date Created: 
## Date Modified: 2022-04-14
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(padr)
library(streamMetabolizer)
library(StreamPULSE)
library(glue)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())

source('code/functions/complete_days.R')

# append the old usgs data to the new data


#
old_sites <- list.files('data/usgs_sm_ready_old/') %>% 
  str_split('_', simplify = TRUE) %>% 
  data.frame() %>% 
  mutate(sitecode = paste(X1,X2, sep = '_'))


recent_sites <- list.files('data/usgs_sm_ready_recent') %>% 
  str_split('_', simplify = TRUE) %>% 
  data.frame() %>% 
  mutate(sitecode = paste(X1,X2, sep = '_')) 

long_sites <- left_join(recent_sites, old_sites,
                        by = 'sitecode') %>% 
  select(sitecode,
         startDate_recent = X3.x,
         endDate_recent = X4.x,
         startDate_old = X3.y,
         endDate_old = X4.y)

for(i in 1:length(long_sites$sitecode)) {
  
  sitecode <- long_sites$sitecode[i]
  
  old_date_range <- paste(long_sites$startDate_old[i], long_sites$endDate_old[i], sep = '_')
  recent_date_range <- paste(long_sites$startDate_recent[i], long_sites$endDate_recent[i], sep = '_')
  
  old <- read_csv(glue('data/usgs_sm_ready_old/{sitecode}_{old_date_range}')) %>% 
    select(site = site_id,
           solar.time,
           temp.water,
           DO.obs,
           DO.sat,
           depth,
           discharge,
           light)
  recent <- read_csv(glue('data/usgs_sm_ready_recent/{sitecode}_{recent_date_range}'))%>% 
    select(site,
           solar.time,
           temp.water,
           DO.obs,
           DO.sat,
           depth,
           discharge,
           light)
   
  all <- rbind(old,recent) %>%    
    distinct(solar.time, 
             .keep_all = TRUE) 
  
  write_csv(all,
            glue('data/usgs_sm_ready_all/{sitecode}_{substr(old_date_range, 1,10)}_{substr(recent_date_range, 12,25)}'))
} 


