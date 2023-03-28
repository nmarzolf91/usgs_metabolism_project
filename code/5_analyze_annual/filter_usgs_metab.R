## Header ----
## Script name: 
##
## Purpose of script:
##
## Author: Nick Marzolf
## Date Created: 2022-12-05
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

# get USGS metabolism data: this will have to be adjusted to pull from figshare
## estimates 
## diagnostics
## light estimates
## site data w/NHD



# read-in usgs estimates
estimates <- read_csv('data/output_data/estimates_2007-2021.csv') %>% 
  dplyr::filter(lubridate::year(date) > 2007) 


# pair with diagnostics
diagnostic <- read_csv('data/model_runs/bayes_all/diagnostics.csv')

good_site_years <- diagnostic %>% 
  filter(ER_K_r2 < 0.6,
         K_median < 100,
         n_days >= (365*0.6)) %>% 
  mutate(site_year = paste(site, year, sep = '-')) %>% 
  pull(site_year)

# read-in streamlight data

daily_light <- read_csv('data/output_data/daily_light_metrics.csv')%>% 
  mutate(year = lubridate::year(date),
         site_year = paste(site, year, sep = '-'))

light_sites <- daily_light %>% 
  pull(site_year)


# filter the output data
usgs_metab_use <- estimates %>% 
  mutate(year = lubridate::year(date),
         site_year = paste(site, year, sep = '-')) %>% 
  filter(site_year %in% good_site_years) %>% 
  select(-site_year, -year)

readr::write_csv(usgs_metab_use,
                 'data/output_data/good_estimates_2007-2021.csv')

# gap-fill and normalize
source('code/functions/fill_and_normalize_metab.R')

usgs_metab_fill_norm <- fill_and_normalize_metab(usgs_metab_use)
write_csv(usgs_metab_fill_norm,
          'data/output_data/usgs_metab_fill_norm.csv')


lake_metab <- read_csv('data/output_data/lake_gpp_solomon.csv')
