library(tidyverse)
library(dplyr)
library(ggplot2)
library(dataRetrieval)
library(glue)
library(streamMetabolizer)
library(lubridate)

# run the Bayesian model ----


# changes from meeting with A. Appling on 4/13/22
## pull old data from StreamPULSE
## Quality filter for ER~K600
## Remove days with large changes in discharge a priori
## Interpolation- fix to sites with the same resolution or interpolate gaps <3 hr
## rstan options for re-starting the MCMC in the same place for sites that don't converge

bayes_sites <- read_csv('data/model_runs/run_2/bayes_sites.csv') %>% 
  pull(bayes_sites)


bayes_site_deets = data.frame(
  site_code = rep(bayes_sites, each = length(2007:2021)), 
  year = rep(2007:2021, times = length(bayes_sites))
  ) 

bayes_results = matrix('', ncol=4, nrow = nrow(bayes_site_deets))
colnames(bayes_results) = c('Site', 'Year', 'Fit Error', 'Fit Time')


# change the numbers to index for each workspace on BM2
for(i in 1:nrow(bayes_site_deets)){
  
  # define site and year from input df
  site_code <- bayes_site_deets$site_code[i]
  run_year <- bayes_site_deets$year[i]
  
  # read in the data; possibly change this to the 15-min interpolated version
  d <- try(read_csv(glue('data/usgs_sm_ready_all/{s}_all.csv',
                         s = site_code)))
  
  # check if there is an error in reading in data
  if(inherits(d, 'try-error')) {
    next
  }
  
  # filter to one year per model run
  d <- d %>%
    filter(year(solar.time) == run_year) %>%
    select(-site)
  
  d <- complete_days(d)
  
  # bring in the priors from the MLE run
  init <- read_csv('data/model_runs/init/eval.csv')
  
  # pull the years where priors exist
  median_k600d_from_mle_run <- init %>% 
    filter(site == site_code) %>% 
    select(year, K_median)
  
  # get model name, which establishes core hyperparams. these would only be
  # changed as a last resort if the model just refuses to converge. i.e., if
  # we have to settle for a simpler model
  bayes_name_new = streamMetabolizer::mm_name(
    type='bayes',
    pool_K600='binned',
    err_obs_iid=TRUE,
    err_proc_iid=TRUE,
    ode_method='trapezoid',
    deficit_src='DO_mod',
    engine='stan')
  
  ## now we set up the priors. you can see their defaults with `?specs`.
  ## we need priors for the centers of the Q bins in the Q-K600 relationship,
  ## and for the means and SDs of K600.
  Q_by_day = tapply(log(d$discharge),
                    substr(d$solar.time, 1, 10),
                    mean)
  
  #establish node centers as an even sequence from the min to the max daily log Q.
  #this can be refined so that the nodes better represent the bulk of your data.
  K600_lnQ_nodes_centers = seq(from=min(Q_by_day, na.rm=TRUE),
                               to=max(Q_by_day, na.rm=TRUE),
                               by = 0.2)
  n_nodes = length(K600_lnQ_nodes_centers)
  
  
  #officially set all specs, including priors
  bayes_specs_new = streamMetabolizer::specs(
    model_name=bayes_name_new,
    K600_lnQ_nodes_centers = K600_lnQ_nodes_centers,
    K600_lnQ_nodediffs_sdlog = 0.1,
    K600_lnQ_nodes_meanlog = rep(log(12), length(K600_lnQ_nodes_centers)),  # default; see ?specs
    K600_lnQ_nodes_sdlog = rep(0.1, n_nodes),
    #K600_daily_sigma_mean = 0,                                # dont think this is even a settable parameter?
    K600_daily_sigma_sigma = (filter(median_k600d_from_mle_run, year == run_year)
                              %>% pull(K_median))* 0.02,
    burnin_steps = 1000,
    saved_steps = 500)
  
  # fit model
  fit_err = FALSE
  
  # define the begininning and end of the time series
  start_date <- first(date(d$solar.time))
  end_date <- last(date(d$solar.time))
  
  # fault tolerance; alternative to try; catch an errors and dictate the response to the errors
  tryCatch(
    {
      # runs the model
      dat_metab = streamMetabolizer::metab(bayes_specs_new,
                                           data = d)
      # saves the model output
      dat_fit = get_fit(dat_metab)
    },
    error = function(e) {
      fit_err <<- TRUE
    },
    warning = function(w) {
      fit_err <<- TRUE
    }
  )
  
  # populate the output matrix to evaluate model fits
  bayes_results[i,1] = site_code   # which site
  bayes_results[i,2] = run_year    # which year
  bayes_results[i,3] = fit_err     # did the model work?
  bayes_results[i,4] = ifelse(fit_err == TRUE, # if the model did run, how long did it take?
                              NA,
                              (get_fitting_time(dat_metab)[3])/60)
  
  if(fit_err)
    next

  #save model outputs
  write_dir = 'data/model_runs/run_2/outputs/'
  fn_prefix = paste0(write_dir, '/', site_code, '_', start_date, '_', end_date, '_')
  
  specs_out = data.frame(unlist(get_specs(dat_metab)))
  daily_out = get_data_daily(dat_metab)
  data_out = get_data(dat_metab)
  
  write_csv(dat_fit$daily, paste0(fn_prefix, 'daily/daily.csv'))
  write_csv(dat_fit$overall, paste0(fn_prefix, 'overall/overall.csv'))
  write_csv(dat_fit$KQ_overall, paste0(fn_prefix, 'KQ_overall/KQ_overall.csv'))
  write_csv(specs_out, paste0(fn_prefix, 'specs/specs.csv'))
  write_csv(daily_out, paste0(fn_prefix, 'datadaily/datadaily.csv'))
  write_csv(data_out, paste0(fn_prefix, 'mod_and_obs_DO/mod_and_obs_DO.csv'))
}

# do this after the loop is run
write_csv(bayes_results,
          'data/model_runs/run_2/results_XXX-XXX.csv') # change XXX to index numbers in the loop


