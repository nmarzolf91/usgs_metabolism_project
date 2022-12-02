## Header ----
## Script name: 
##
## Purpose of script:
##
## Author: Nick Marzolf
## Date Created: 
## Date Modified: 2022-03-22
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dataRetrieval)
library(glue)
library(streamMetabolizer)
library(lubridate)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


# look here for help: https://rdrr.io/github/USGS-R/streamMetabolizer/

sites <- substr(list.files('data/usgs_sm_ready_all//'),
                1,13)

site_info <- sites %>%
  substr(6,16) %>%
  readNWISsite()

# create site_deets
site_deets = data.frame(
  site = rep(sites, each = length(2007:2021)),
  year = rep(2007:2021, times = length(sites))
  )

results = matrix('', ncol = 3, nrow = nrow(site_deets))
colnames(results) = c('Site', 'Year', 'Fit Error')

# initial model run ----
## use MLE to determine
## 1) K600 priors
## 2) days with no data
for(i in 757:nrow(site_deets)) {
  
  site_code <- site_deets$site[i]
  run_year <- site_deets$year[i]
  
  d <- try(read_csv(glue('data/usgs_sm_ready_all/{s}_all.csv',
                     s = site_code)) %>% 
             select(-site)
           )
  
  # check if there is an error in reading in data
  if(inherits(d, 'try-error')) {
    next
  }
  
  d <- d %>%
    filter(year(solar.time) == run_year) 
  
  start_date <- first(date(d$solar.time))
  end_date <- last(date(d$solar.time))
  
  # init model run
  # From Appling et al. 2018 (SD), they defined the sd hyperparameter of K~Q by running model
  # m_np_oi_tr_plrckm.stan 
  
  # mm_parse_name('m_np_oi_tr_plrckm.stan')
  
  init_name = streamMetabolizer::mm_name(
    type = 'mle',
    pool_K600 = 'none',
    err_obs_iid = TRUE,
    err_proc_iid = FALSE,
    ode_method = 'trapezoid',
    deficit_src = 'DO_mod',
    engine = 'nlm',
    GPP_fun = 'linlight',
    ER_fun = 'constant')
  
  
  #officially set all specs, including priors
  init_specs = specs(model_name = init_name)        

  #fit model
  fit_err = FALSE
  
  # fault tolerance; alternative to try; catch an errors and dictate the response to the errors
  tryCatch(                                           
    {dat_metab = streamMetabolizer::metab(init_specs, 
                                          data = d)
    dat_fit = get_fit(dat_metab)
    },
    error=function(e) {
      fit_err <<- TRUE
    },
    warning = function(w) {
      fit_err <<- TRUE
    }
  )
  
  results[i,1] = site_code
  results[i,2] = year(start_date)
  results[i,3] = ifelse(
    fit_err == TRUE,
    NA,
    get_fitting_time(dat_metab)[3]
  )
  
  if(fit_err) {
    results[i,3] = 'fit error'
    next
  }
  
  #save model output
  write_dir = 'data/model_runs/init_all'
  fn_prefix = paste0(write_dir, '/dat_fit','/', site_code, '_', start_date, '_', end_date, '_')
  write_csv(dat_fit, paste0(fn_prefix, 'dat_fit.csv'))
  
  specs_out = data.frame(unlist(get_specs(dat_metab)))
  fn_prefix = paste0(write_dir, '/specs','/', site_code, '_', start_date, '_', end_date, '_')
  write_csv(specs_out, paste0(fn_prefix, 'specs.csv'))
  
  data_out = get_data(dat_metab)
  fn_prefix = paste0(write_dir, '/mod_obs_DO','/', site_code, '_', start_date, '_', end_date, '_')
  write_csv(data_out, paste0(fn_prefix, 'mod_and_obs_DO.csv'))
}

write_csv(data.frame(results), 
          'data/model_runs/init_all/results_init.csv') #save run results if you want to

