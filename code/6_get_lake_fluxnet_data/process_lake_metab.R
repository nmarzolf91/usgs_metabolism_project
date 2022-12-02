## Header ----
## Script name: process_lake_gpp.R
##
## Purpose of script: read in lake GPP data from Solomon et al. (2013) for further analysis
##
## Author: Nick Marzolf
## Date Created: 2022-12-02
## Date Modified: 
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


library(tidyverse)
library(dplyr)
library(lubridate)

# data were shared by Chris Solomon in a Onedrive folder
# those data were downloaded as a zip file and moved to the directory here:
d <- unzip('data/lake_gpp_solomon.zip',
           exdir = 'data/lake_gpp_solomon')

# extract lake names
names <- vector()
for(i in 1:length(daily_ER_files$.)){
  file <- daily_ER_files$.[i]
  
  name <- str_split(file, '/')[[1]][3]
  names[i] <- name
}

# Ecosystem respiration (mg O2 L-1 d-1) are stored in these files
daily_ER_files <- d %>% 
  data.frame() %>% 
  filter(grepl('optimOut.txt',.)) %>% 
  mutate(names = names) %>% 
  rename(dir = '.')

# processing 
list_er <- as.list(c(daily_ER_files$dir))
names(list_er) <- names

# read in and append
daily_ER <- list %>%
  map_dfr(read_delim,
          .id = 'names')

# read out ER data
daily_ER_clean <- data.frame(daily_ER %>% 
                               mutate(date = as_date(solarDay)) %>% 
                               select(lake_name = names,
                                      date,
                                      ER = rhoEst)) 

# get GPP data ----
daily_GPP_files <- d %>% 
  data.frame() %>% 
  filter(grepl('GPPFitOut.txt',.)) %>% 
  mutate(names = names) %>% 
  rename(dir = '.')

list_gpp <- as.list(c(daily_GPP_files$dir))
names(list_gpp) <- names

# read in and append
daily_GPP <- list_gpp %>%
  map_dfr(read.table,
          .id = 'names')

daily_GPP_clean <- daily_GPP %>% 
  mutate(date = lubridate::date(solarDay)) %>% 
  select(lake_name = names,
         date,
         GPP = GPPFit) 



out <- full_join(daily_ER_clean, daily_GPP_clean,
                 by = c('date', 'lake_name'))

write_csv(out,
          'data/lake_gpp_solomon.csv')


