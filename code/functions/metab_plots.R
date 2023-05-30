## Header ----
## Script name: metab_plots.R
##
## Purpose of script:
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
library(lubridate)
library(moments)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())



estimates <- read_csv('data/model_runs/estimates_2007-2021.csv') %>% 
  filter(year(date) > 2007) %>% 
  mutate(GPP_C = GPP*(12/32))


# create summary environmental driver tables

env_daily <- estimates %>% 
  select(site, date, temp.water, discharge) %>% 
  group_by(site, 
           julian_day = yday(date)) %>% 
  summarise(temp_mean = mean(temp.water, na.rm = TRUE),
            disch_mean = mean(discharge, na.rm = TRUE))

env_year <- estimates %>% 
  select(site, date, temp.water, discharge) %>% 
  group_by(site, 
           year = year(date)) %>% 
  summarise(temp_mean = mean(temp.water, na.rm = TRUE),
            disch_mean = mean(discharge, na.rm = TRUE),
            temp_var = var(temp.water, na.rm = TRUE),
            disch_var = var(discharge, na.rm = TRUE))


# plot all GPP estimates, as is
estimates %>% 
  ggplot(., aes(x = date, y = GPP_C))+
  geom_line(aes(color = site))


# Median (+IQR) GPP by year and site
GPP_year <- estimates %>% 
  group_by(site, 
           year = lubridate::year(date)) %>% 
  summarise(GPP_ann_median = median(GPP_C, na.rm = TRUE),
            GPP_ann_IQR = IQR(GPP_C, na.rm = TRUE),
            GPP_ann_tot = sum(GPP_C, na.rm = TRUE)) 


# mean annual GPP by site bar plot
estimates %>% 
  group_by(site, 
           Year = lubridate::year(date)) %>% 
  summarise(GPP_ann_tot = sum(GPP_C, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = GPP_ann_tot))+
  geom_bar(stat = 'identity')+
  facet_wrap(site ~ .)+
  ylab(expression(paste('Annual GPP (g C ', m^-2,' ', y^-1,')')))



# 'average' year of productivity from each site
estimates %>% 
  group_by(site, jday = yday(date)) %>% 
  summarise(GPP_daily = median(GPP_C, na.rm = TRUE),
            GPP_IQR = IQR (GPP_C, na.rm = TRUE)) %>% 
  ggplot(., aes(x = jday))+
  geom_line(aes(y = GPP_daily),
            color = 'darkgreen')+
  geom_ribbon(aes(ymin = GPP_daily - (GPP_IQR/2),
                  ymax = GPP_daily + (GPP_IQR/2)),
              fill = 'darkgreen', alpha = 0.5)+
  facet_wrap(site ~ .)+
  ylab(expression(paste('Median daily GPP (g C ', m^-2,' ', d^-1,')')))+
  xlab('Day of Year')


# date of max (or 95th percentile) GPP
estimates %>% 
  group_by(site, year = year(date)) %>% 
  filter(GPP_C >= quantile(GPP_C, probs = 0.95, 
                         na.rm = TRUE)) %>% 
  summarise(GPP_95 = quantile(GPP_C, probs = 0.95, 
                              na.rm = TRUE),
            day_95 = first(yday(date))) %>% 
  ggplot(., aes(x = year, y = day_95))+
  geom_point()+
  geom_line()+
  labs(x = 'Year', y = 'Julian Day of First Day of 95%-ile GPP')+
  facet_wrap(site ~ .)



# treat each site-year as a distribution and calculate some statistics
# use Savoy et al. 2019 as a guide for those statistics

summarise_GPP <- function(estimates) {
  
  data.frame <- estimates %>% 
    mutate(site_year = interaction(site, year(date))) 
  
  ar_est = data.frame(site_year = character(),
                      GPP_ar = numeric())
  for(id in 1:length(unique(data.frame$site_year))) {
    
    use <- unique(data.frame$site_year)[id]
    
    dat <- data.frame %>% 
      filter(site_year %in% use)
    
    mod <- arima(dat$GPP_C, order = c(1, 0,0))
    ar <- mod$coef[1]
    ar_est <- ar_est %>% 
      add_row(
        site_year = use,
        GPP_ar = ar
      )
  }
  
  out <- data.frame %>% 
    group_by(site, 
             year = year(date)) %>% 
    summarise(GPP_mean = mean(GPP_C, na.rm = TRUE),
              GPP_median = median(GPP_C, na.rm = TRUE),
              GPP_min = min(GPP_C, na.rm = TRUE),
              GPP_max = max(GPP_C, na.rm = TRUE),
              GPP_amp = GPP_max - GPP_min,
              GPP_sd = sd(GPP_C, na.rm = TRUE),
              GPP_total = sum(GPP_C, na.rm = TRUE),
              GPP_CV = (GPP_sd/GPP_mean)*100,
              GPP_skew = skewness(GPP_C),
              GPP_kurt = kurtosis(GPP_C)) 
  out$GPP_ar <- ar_est$GPP_ar
}



write_csv(out, 
          'data/model_runs/summarised_2007-2021.csv')
GPP_summary <- read_csv('data/model_runs/summarised_2007-2021.csv')

ggplot(GPP_summary,
       aes(x = year, y = GPP_skew))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 1, color = 'red')+
  facet_wrap(site ~ .,
             scales = 'free')

ggplot(GPP_summary,
       aes(x = year, y = GPP_kurt))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 3, color = 'red')+
  facet_wrap(site ~ .,
             scales = 'free')

ggplot(GPP_summary,
       aes(x = year, y = GPP_sd^2))+
  geom_point()+
  geom_line()+
  facet_wrap(site ~ .,
             scales = 'free')


estimates %>% 
  filter(site == 'nwis_01400500') %>% 
  select(GPP_C) %>% 
  decompose(., type = 'multiplicative', frequency = 365)



# histogram of watershed areas
site_dat <- readr::read_csv('data/site_info/nwis_site_info.csv')

ggplot(site_dat,
       aes(x = drain_area_va*2.58999))+ # conver to km2
  geom_histogram()+
  labs(x = expression(paste('Drainage area (', km^2,')')),
       y = 'Count')+
  scale_x_log10()
