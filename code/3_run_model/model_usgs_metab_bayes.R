library(tidyverse)
library(dplyr)
library(ggplot2)
library(dataRetrieval)
library(glue)
library(streamMetabolizer)
library(lubridate)

# run the Bayesian model ----

bayes_sites <- substr(list.files('data/usgs_sm_ready/'),
                      1,13)

bayes_site_deets = data.frame(
  site_code = bayes_sites, 
  start_date = '2017-01-01', 
  end_date = '2021-12-31', 
  int = '15 min'        #~any other specs you want to define this way
) 

bayes_results = matrix('', ncol = 4, 
                       nrow = nrow(bayes_site_deets))

colnames(bayes_results) = c('Site', 'Year', 'Fit Time', 'Result')


years <- c(2018:2021)
step <- 1:10
step_index <- 1:length(step)

all_files <- list.files('data/model_runs/bayes/')

step_index_split <- str_split_fixed(all_files, '_', n = Inf)[,2:3]
site_codes <- step_index_split[,1]
site_dates <- step_index_split[,2]
site_years <- str_split_fixed(site_dates, '-', n = Inf)[,1]
site_years_done <- paste0('nwis_', site_codes, '_', site_years)
site_years_done <- unique(site_years_done)


for(run_year in years) {
  
  for(i in step) {
    
    site_code = bayes_sites[i]
    
    # Skip site years that have already run
    if(paste0(site_code, '_', run_year) %in% site_years_done){
      next
    }
    
    d <- try(read_csv(glue('data/usgs_sm_ready/{s}.csv',
                       s = site_code))
             )
    if(inherits(d, 'try-error')) {
      next
    }
    
    # turn comments off to explore shorter periods; good for trials
    d <- d %>%
      filter(year(solar.time) == run_year) %>%
      select(-site)
    
    if(! nrow(d)){
      bayes_results[i,4] = 'no modelable days'
      next
    }
    
    # bring in the priors from the MLE run
    init <- read_csv('data/model_runs/init/eval.csv')
    
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
      K600_lnQ_nodes_meanlog = rep(log(12), length(K600_lnQ_nodes_centers)),  # default; see ?specs
      K600_lnQ_nodes_sdlog = rep(0.1, n_nodes),
      #K600_daily_sigma_mean = 0,   # dont think this is even a settable parameter?
      K600_daily_sigma_sigma = (filter(median_k600d_from_mle_run, year == run_year)
                                %>% pull(K_median))* 0.02,
      burnin_steps = 1000,
      saved_steps = 500)
    
    #fit model
    fit_err = FALSE
    
    start_date <- first(date(d$solar.time))
    end_date <- last(date(d$solar.time))
    
    bayes_results[i,1] = site_code
    bayes_results[i,2] = year(start_date)
    
    # fault tolerance; alternative to try; catch an errors and dictate the response to the errors
    tryCatch(
      {
        dat_metab = streamMetabolizer::metab(bayes_specs_new,
                                             data = d)
        dat_fit = get_fit(dat_metab)
      },
      error = function(e) {
        fit_err <<- TRUE
      },
      warning = function(w) {
        fit_err <<- TRUE
      }
    )
    
    if(fit_err) {
      bayes_results[i,4] = 'fit error'
      next
    }
    
    bayes_results[i,3] = get_fitting_time(dat_metab)[3]
    
    #save model output
    write_dir = 'data/model_runs/bayes'
    fn_prefix = paste0(write_dir, '/', site_code, '_', start_date, '_', end_date, '_')
    
    specs_out = data.frame(unlist(get_specs(dat_metab)))
    daily_out = get_data_daily(dat_metab)
    data_out = get_data(dat_metab)
    
    write_csv(dat_fit$daily, paste0(fn_prefix, 'daily.csv'))
    write_csv(dat_fit$overall, paste0(fn_prefix, 'overall.csv'))
    write_csv(dat_fit$KQ_overall, paste0(fn_prefix, 'KQ_overall.csv'))
    write_csv(specs_out, paste0(fn_prefix, 'specs.csv'))
    write_csv(daily_out, paste0(fn_prefix, 'datadaily.csv'))
    write_csv(data_out, paste0(fn_prefix, 'mod_and_obs_DO.csv'))
    
    bayes_results[i,4] = 'Run Finished'
  }
  step_index <- step_index + 10
}

write_csv(as.data.frame(bayes_results), 
          file = 'data/model_runs/results_bayes_1-10.csv') # save run results if you want to
