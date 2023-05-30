## Header ----
## Script name: 
##
## Purpose of script:
##
## Author: Nick Marzolf
## Date Created: 2022-02-03
## Date Modified: 
## Email: nmarzol@ncsu.edu
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
source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())

library(devtools)

# StreamPULSE-streamMetabolizer synergy

# You can run through this in example mode, using the site WI_BEC from StreamPULSE.
# that will clarify what the dataset looks at each phase of this process (data
# retrieval, preparation, and model fitting). i left __ in spots where you'll need
# to fill in the blanks with paths and such

# in case you need to install either of these packages:
devtools::install_github('streampulse/StreamPULSE',
                         ref='master', dependencies=TRUE,
                         force = TRUE)
# install.packages('streamMetabolizer',
#                  dependencies=TRUE,
#                  repos=c('https://owi.usgs.gov/R','https://cran.rstudio.com'))

devtools::install_github('appling/unitted')
devtools::install_github('https://github.com/USGS-R/streamMetabolizer')
library(StreamPULSE)       # v0.0.0.9043
library(streamMetabolizer) # v0.11.4
library(plyr)
library(tidyverse)
library(rstan)

# you could put all the NEON sites in a run config file like so. this one has just
# one row as an example
site_deets = tribble(
    ~site_code, ~start_date, ~end_date, ~int,        #~any other specs you want to define this way
    'WI_BEC', '2010-01-01', '2010-12-31', '15 min',
)
# add to this for each site year as a chunk for modeling

# this will track run successes or locations of errors
results = matrix('', ncol=4, nrow=nrow(site_deets))
colnames(results) = c('Region', 'Site', 'Year', 'Result')

for(i in 1:nrow(site_deets)){

    site_code = site_deets$site_code[i]
    # token = site_deets$tok[i]
    start_date = site_deets$start_date[i]
    end_date = site_deets$end_date[i]
    int = site_deets$int[i]

    write(paste(i, site_code, substr(start_date, 1, 4), Sys.time()), # creates a log for the for loop each time its run
          'model_run_log.txt', append=TRUE)

    results[i,1] = strsplit(site_code, '_')[[1]][1]
    results[i,2] = strsplit(site_code, '_')[[1]][2]
    results[i,3] = substr(start_date, 1, 4)

    # request data from StreamPULSE server. here are the site_codes for the neon pseudo-sites
    # Spencer used to manually QC oxygen data:
    # AZ_SYCAtemp PR_GUILtemp MA_HOPBtemp GA_FLNTtemp AK_CARItemp AK_OKSRtemp AK_TOOKtemp AL_BLWAtemp AL_MAYFtemp AL_TOMBtemp
    # CA_BIGCtemp CA_TECRtemp CO_ARIKtemp CO_COMOtemp CO_WLOUtemp FL_BARCtemp FL_SUGGtemp KS_KINGtemp KS_MCDItemp ND_PRLAtemp
    # ND_PRPOtemp OK_BLUEtemp OR_MCRAtemp PR_CUPEtemp TN_LECOtemp TN_WALKtemp TX_PRINtemp UT_REDBtemp VA_LEWItemp VA_POSEtemp
    # WA_MARTtemp WI_CRAMtemp WI_LIROtemp WY_BLDEtemp
    streampulse_data = try({
        StreamPULSE::request_data(sitecode = site_code,
                                  startdate = start_date,
                                  enddate = end_date)
                                  # token = token, #only needed for accessing embargoed data
                                  # variables='DO_mgL') #use this for the neon pseudosites; 

                                  # here's the full list of StreamPULSE variables, just for kicks
                                  # variables=c('DO_mgL','DOsat_pct','satDO_mgL','WaterPres_kPa',
                                  #     'Level_m','WaterTemp_C','Light_PAR','AirPres_kPa','Discharge_m3s')))
        })

    if(inherits(streampulse_data, 'try-error')){
        results[i, 4] = 'request error'
        next
    }

    # here's where you'll munge the various input series together if you want to use
    # StreamPULSE::prep_metabolism. use that if you want to estimate PAR from location
    # and time, or air pressure from the nearest NCDC gauge, or if you need to estimate
    # Q from a rating curve for some of these sites. See ?prep_metabolism. Otherwise, skip
    # to ***

    ## we can try the following, commented steps for sites where Q is unknowable,
    ## if any such sites are left months from now

    # if(rating curve required to estimate Q){
    #
    #     site = strsplit(site_code, '_')[[1]][2]
    #     Z = zq[zq$site == site, 'level_m']
    #     Q = zq[zq$site == site, 'discharge_cms']
    #     offset = offsets[offsets$site == site, 2] / 100
    #
    #     fitdata = try({
    #         StreamPULSE::prep_metabolism(d=streampulse_data,
    #                                      type='bayes',
    #                                      model='streamMetabolizer',
    #                                      interval=int,
    #                                      zq_curve=list(sensor_height=offset,
    #                                                    Z=Z,
    #                                                    Q=Q,
    #                                                    fit='power',
    #                                                    ignore_oob_Z=TRUE,
    #                                                    plot=TRUE),
    #                                      estimate_areal_depth=FALSE,
    #                                      estimate_PAR=TRUE,
    #                                      retrieve_air_pres=TRUE)
    #     })
    #
    # } else {

        fitdata = try({
            #this will pull air pressure and light data if necessary
            StreamPULSE::prep_metabolism(d=streampulse_data,
                                         type='bayes',
                                         model='streamMetabolizer',
                                         interval=int,
                                         estimate_areal_depth=FALSE, # causes the warning message re: depth vs level
                                         estimate_PAR=TRUE,
                                         retrieve_air_pres=TRUE)
        })
    # }

    if(inherits(fitdata, 'try-error')){
        results[i, 4] = 'prep error'
        next
    }

    d = fitdata$data

    #*** if you didn't use prep_metabolism, you'll want to format your input data
    #to look like `d`, after running the WI_BEC example

    ## we can try the following, commented steps if some models remain wonky after a first pass

    # remove "storm" days (days with Q range over some threshold--in this case 10% of median Q for the day
    # d$Date = as.Date(d$solar.time)
    # d_list = split(d, d$Date)
    # d_diff = ldply(lapply(d_list,
    #     function(x){
    #         range_diff = max(x$discharge) - min(x$discharge)
    #         return(range_diff)
    #     }), data.frame)
    # colnames(d_diff) = c("Date", "Diff")
    # low_diff_days = d_diff$Diff < (0.1 * median(d$discharge, na.rm=TRUE))
    # d_diff = d_diff[low_diff_days, ]
    # d = d[as.character(d$Date) %in% d_diff$Date, ]
    # d$Date = NULL
    #
    # exclude low DO observations
    # d = d[d$DO.obs > 2, ]

    # exclude NA rows, impossible water temperatures
    d = na.omit(d)
    d$temp.water[d$temp.water > 40] = NA

    if(! nrow(d)){
        results[i,4] = 'no modelable days'
        next
    }

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

    # n_KQ_nodes = 7 #the number of bins. not sure when you'd need more than 7. i guess for super wacky K600-Q relationships
    Q_by_day = tapply(log(d$discharge),
                      substr(d$solar.time, 1, 10),
                      mean)

    #establish node centers as an even sequence from the min to the max daily log Q.
    #this can be refined so that the nodes better represent the bulk of your data.
    K600_lnQ_nodes_centers = seq(from=min(Q_by_day, na.rm=TRUE),
                                 to=max(Q_by_day, na.rm=TRUE),
                                 by = 0.2)


    #officially set all specs, including priors
    bayes_specs_new = specs(model_name=bayes_name_new,
                            K600_lnQ_nodes_centers = K600_lnQ_nodes_centers,
                            # K600_lnQ_nodes_meanlog = 1, #vector; known or estimated mean log K600 corresponding to each Q node; keep out, the vector should be as long as the centers (n = 7); default = log(12) for all nodes
                            K600_lnQ_nodes_sdlog = 0.1)   #vector; known or estimated SD for each node. 1 is broad, 0.7 is informative, and 0.1 is tight
                            # K600_daily_sigma_sigma = __ #daily bounce; how much K600 is allowed to fluctuate day-by-day.


    #fit model
    fit_err = FALSE
    
    tryCatch(                                           # fault tolerance; alternative to try; catch an errors and dictate the response to the errors
        {
            dat_metab = metab(bayes_specs_new, data=d)
            dat_fit = get_fit(dat_metab)
        },
        error=function(e) fit_err <<- TRUE
    )

    if(fit_err){
        results[i,4] = 'fit error'
        next
    }

    #save model output
    write_dir = 'data/results'
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

    results[i,4] = 'Run Finished'
}

write.csv(results, row.names=FALSE, file = 'data/results.csv') #save run results if you want to

# check streamMetabolizer docs and/or talk to people about how to plot diagnostics.
# you can check http://data.streampulse.org:3838/streampulse_diagnostic_plots/
# for ideas of the types of plots you might want to check, but the sm ones will look
# different.

# check ER-K600 for correlation
ggplot(dat_fit$daily, aes(x = ER_mean, 
                          y = K600_daily_mean))+
  geom_point()+
  geom_smooth(method = 'lm')

# select convergance Rhat metrics for GPP, ER, DO, K600, and K600 pred
dat_fit$daily %>%
  dplyr::select(ends_with('Rhat')) %>%
  dplyr::filter_all(any_vars(. <1))


# do with data prepped elsewhere
como <- read_csv('data/sm_ready/COMO.csv')
source('code/functions/full_days.R')

como <- como %>%
  filter(year(solar.time) == '2021',
         month(solar.time) =='7') %>%
  dplyr::rename(discharge = disharge) %>%
  full_days()

name <- mm_name(type = 'bayes',
                pool_K600 = 'binned',
                err_obs_iid = TRUE,
                err_proc_iid = TRUE,
                ode_method = 'trapezoid')

specs <- specs(model_name = name,
               n_cores = 4, n_chains = 4,
               burnin_steps = 100,
               saved_steps = 1000)

metab_como <- metab(specs = specs,
                    data = como)

get_fit(metab_como)

daily_data <-metab_como@fit$daily

ggplot(daily_data,
       aes(x = ER_mean, y = K600_daily_mean))+
  geom_point()

summary(daily_data$ER_Rhat)
