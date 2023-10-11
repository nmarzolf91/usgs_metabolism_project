## Header ----
## Script name: estimate_metab.R
##
## Purpose of script: function to process stream metabolizer outputs to usable metabolism data
##
## Author: Nick Marzolf
## Date Created: 2022-09-27
## Date Modified: 
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(glue)
library(streamMetabolizer)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


# template from Appling et al. 2018
estimates <- unz('C:/Users/Nick Marzolf/Downloads/daily_predictions.zip',
                 'daily_predictions.tsv') %>% 
  read_tsv()

names(estimates)



estimate_metab <- function(model_dir = 'data/model_runs/bayes_all/') {
  n_mods <- length(list.files(glue(model_dir, 'daily')))
  
  # estimates <- data.frame(site = character(),
  #                         resolution = character(),
  #                         date = character(),
  #                         GPP = numeric(),
  #                         GPP.lower = numeric(),
  #                         GPP.upper = numeric(),
  #                         GPP.n_eff = numeric(),
  #                         GPP.Rhat = numeric(),
  #                         ER = numeric(),
  #                         ER.lower = numeric(),
  #                         ER.upper = numeric(),
  #                         ER.n_eff = numeric(),
  #                         ER.Rhat = numeric(),
  #                         K600 = numeric(),
  #                         K600.lower = numeric(),
  #                         K600.upper = numeric(),
  #                         K600.n_eff = numeric(),
  #                         K600.Rhat = numeric(),
  #                         DO.obs = numeric(),
  #                         DO.sat = numeric(),
  #                         DO.amp = numeric(),
  #                         DO.psat = numeric(),
  #                         depth = numeric(),
  #                         temp.water = numeric(),
  #                         day.length = numeric(),
  #                         discharge = numeric(),
  #                         shortwave = numeric()
  #                         )
  
  for(i in 1:n_mods) {
    
    dir_daily <- glue(model_dir,'daily')
    
    daily <- list.files(dir_daily, full.names = TRUE)[i]
    
    site <- paste0('nwis_', {str_split(daily, '_')[[1]][4]})
    start_date <- str_split(daily, '_')[[1]][5] 
    end_date <- str_split(daily, '_')[[1]][6] 
    year <- substr(start_date, 1,4)
    
    d <- read_csv(daily)
    
    if('GPP_50pct' %in% names(d)) {
      
      d_clean <- d %>% 
        filter(GPP_50pct > -0.5,
               ER_50pct < 0.5) %>% 
        select(date,
               GPP = GPP_50pct,
               GPP.lower = GPP_2.5pct,
               GPP.upper = GPP_97.5pct,
               GPP.n_eff = GPP_n_eff,
               GPP.Rhat = GPP_Rhat,
               ER = ER_50pct,
               ER.lower = ER_2.5pct,
               ER.upper = ER_97.5pct,
               ER.n_eff = ER_n_eff,
               ER.Rhat = ER_Rhat,
               K600 = K600_daily_50pct,
               K600.lower = K600_daily_2.5pct,
               K600.upper = K600_daily_97.5pct,
               K600.n_eff = K600_daily_n_eff,
               K600.Rhat = K600_daily_Rhat
        )
      }

    dir_DO <- glue(model_dir, 'mod_and_obs_DO')
    DO <- try(read_csv(glue(dir_DO, '/', site, '_', start_date, '_', end_date, '_mod_and_obs_DO.csv')))
    if(inherits(DO, 'try-error')){
      next
    }
    
    res <- glue(diff(DO$solar.time) %>% 
                  first(), 
                'min')
    
    DO_clean <- DO %>% 
      group_by(date) %>% 
      summarise(temp.water = mean(temp.water, na.rm = TRUE),
                DO.obs = mean(DO.obs, na.rm = TRUE),
                DO.sat = mean(DO.sat, na.rm = TRUE),
                DO.min = min(DO.obs, na.rm = TRUE),
                DO.max = max(DO.obs, na.rm = TRUE),
                depth = mean(depth, na.rm = TRUE),
                discharge = mean(discharge, na.rm = TRUE),
                shortwave = mean(light, na.rm = TRUE)) %>% 
      mutate(DO.amp = DO.max - DO.min,
             DO.psat = (DO.obs/DO.sat)*100)
    
    merged <- left_join(d_clean,
                        DO_clean, 'date') %>% 
      mutate(resolution = res, 
             site = site) %>% 
      select(site, resolution, date, GPP, GPP.lower, GPP.upper, GPP.n_eff, GPP.Rhat,
             ER, ER.lower, ER.upper, ER.n_eff, ER.Rhat, K600, K600.lower, K600.upper,
             K600.n_eff, K600.Rhat, DO.obs, DO.sat, DO.amp, DO.psat, depth, temp.water,
             discharge, shortwave) %>% 
      filter(K600.Rhat <= 1.2, 
             ER.Rhat <= 1.2, 
             GPP.Rhat <= 1.2)
    
    write_csv(merged,
              glue('data/model_runs/bayes_all/estimates/{site}_{year}.csv'))
    
  }
  
  estimates <- map_dfr(list.files(glue(model_dir, 'estimates'), 
                                  full.names = TRUE),
                       read_csv)
  
  write_csv(estimates,
          'data/model_runs/estimates_2007-2021.csv')
  
  return(estimates)
}













