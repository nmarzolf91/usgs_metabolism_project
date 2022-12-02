## Header ----
## Script name: 
##
## Purpose of script: MLE preliminary metab runs on DCC
##
## Author: Nick Marzolf
## Date Created: 
## Date Modified: 2022-05-16
## Email: nicholas.marzolf@duke.edu
##
.libPaths(c("/hpc/group/bernhardtlab/rlib", .libPaths()))
i = as.numeric(commandArgs(trailingOnly=TRUE)[1])

## load packages:
suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(rstan, lib.loc="/hpc/group/bernhardtlab/rlib")
  library(deSolve, lib.loc="/hpc/group/bernhardtlab/rlib")
  # remotes::install_github('appling/unitted')
  # remotes::install_github("USGS-R/streamMetabolizer")
  library(streamMetabolizer, lib.loc="/hpc/group/bernhardtlab/rlib")
  library(lubridate)
})

setwd('/hpc/group/bernhardtlab/usgs_metab_forMike_v2_test')

# look here for help: https://rdrr.io/github/USGS-R/streamMetabolizer/

mle_data_files <- list.files('data/usgs_sm_ready_all/')
sites <- str_match(mle_data_files, '(nwis_[0-9]+)')[, 2]

# site_info <- sites %>%
#   str_match('nwis_([0-9]+)') %>%
#   {.[, 2]} %>% 
#   readNWISsite()
# write_csv(site_info, 'data/nwis_site_info.csv')
site_info <- read_csv('data/nwis_site_info.csv')

# create site_deets
site_deets = data.frame(
  site = rep(sites, each = length(2007:2021)),
  year = rep(2007:2021, times = length(sites)))

results = matrix('', ncol = 3, nrow = nrow(site_deets))
colnames(results) = c('Site', 'Year', 'Fit Error')

# initial model run ----
## use MLE to determine
## 1) K600 priors
## 2) days with no data

site_code <- site_deets$site[i]
run_year <- site_deets$year[i]

fn <- grep(site_code, mle_data_files, value = TRUE)

d <- read_csv(paste0('data/usgs_sm_ready_all/', fn)) %>% select(-site)

d <- d %>%
  filter(year(solar.time) == run_year) 

if(nrow(d) == 0) stop('nothing to do here')

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

dat_metab = streamMetabolizer::metab(init_specs, data = d)
dat_fit = get_fit(dat_metab)

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

