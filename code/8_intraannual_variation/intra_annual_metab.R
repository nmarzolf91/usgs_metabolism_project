## Header ----
## Script name: intra_annual_metab.R
##
## Purpose of script:
##
## Author: Nick Marzolf
## Date Created: 2022-12-12
## Date Modified: 
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gghighlight)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


# read in data ----
river_metab <- readr::read_csv('data/output_data/good_estimates_2007-2021.csv') 


source('code/8_intraannual_variation/calc_intrayear_metrics.R')
river_intra_metrics <- calc_intrayear_metrics()

river_intra_metrics[,3:9] <- sapply(river_intra_metrics[,3:9],
                                    as.numeric)

# calculate magnificent 7 ----
temp_list <- list()
for(j in 3:9) {
  metric <- colnames(river_intra_metrics)[j]
  
  temp_list[[j]] <- river_intra_metrics %>% 
    select(site, year, col = metric) %>% 
    dplyr::filter(!is.na(col)) %>% 
    group_by(site) %>% 
    nest() %>% 
    mutate(sens = map(data, 
                      ~trend::sens.slope(x = .$col)),
           mk_test = map(data, 
                         ~trend::mk.test(x = .$col))) %>% 
    mutate(metric = metric,
           sens_p = map(sens, ~.$p.value), 
           sens_s = map_dbl(sens, ~.$estimates),
           mk_p = map_dbl(mk_test, ~.$p.value),
           mk_s = map_dbl(mk_test, ~.$estimates['S']),
           sens_sig = ifelse(sens_p <= 0.05,'significant', 'non-significant'),
           sens_slope = ifelse(sens_s > 0, 'increasing', 'decreasing'),
           mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
           mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing'))
  
  river_intra_metrics_trend <- do.call(rbind.data.frame,
                                       temp_list)
}




# significance plots ----
river_intra_metrics_sigs <- river_intra_metrics_trend %>% 
  ungroup() %>% 
  mutate(sens_p = unlist(sens_p)) %>% 
  select(site, metric, sens_sig, sens_slope, sens_s, sens_p)


river_ann_metrics_summary <- river_intra_metrics %>% 
  left_join(river_intra_metrics_sigs) %>% 
  group_by(site, year) %>% 
  slice_head() %>% 
  rename(mean = L1,
         CV = tau2,
         skewness = tau3,
         kurtosis = tau4)

write_csv(river_ann_metrics_summary,
          'data/output_data/river_ann_metrics_summary.csv')

# plot magnificent 7 ----
## by year ----
river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = mean,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = expression(paste('Mean Daily GPP (g C  ',m^-2,' ',y^-1,')')))+
  facet_grid(sens_sig ~ sens_slope)


river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = CV,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = expression(paste(CV[GPP],' (%)')))+
  facet_grid(sens_sig ~ sens_slope)

river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = skewness,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = 'Skewness')+
  facet_grid(sens_sig ~ sens_slope)

river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = kurtosis,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = 'Kurtosis')+
  facet_grid(sens_sig ~ sens_slope)


river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = amplitude,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = 'Amplitude')+
  facet_grid(sens_sig ~ sens_slope)

river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = phase,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = 'Phase')+
  facet_grid(sens_sig ~ sens_slope)

river_ann_metrics_summary %>% 
  ggplot(.,
         aes(x = year, 
             y = ar1,
             color = interaction(sens_slope,
                                 sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  labs(y = 'AR(1) Coefficient')+
  facet_grid(sens_sig ~ sens_slope)

## by site ----
# by site plots ----
river_ann_metrics_summary %>% names()

ggplot(river_ann_metrics_summary,
       aes(x = site, y = mean))+
  geom_point()

ggplot(river_ann_metrics_summary,
       aes(x = site, y = CV))+
  geom_point()

ggplot(river_ann_metrics_summary,
       aes(x = site, y = skewness))+
  geom_point()

ggplot(river_ann_metrics_summary,
       aes(x = site, y = kurtosis))+
  geom_point()

ggplot(river_ann_metrics_summary,
       aes(x = site, y = amplitude))+
  geom_point()

ggplot(river_ann_metrics_summary,
       aes(x = site, y = phase))+
  geom_point()

# ----

# What is the DOY when each site-year reaches 50% of annual GPP
river_metab_50iles <- river_metab %>% 
  select(site, date, GPP_filled) %>% 
  mutate(doy = lubridate::yday(date)) %>% 
  group_by(site, 
           year = lubridate::year(date)) %>% 
  mutate(gpp_cdf = ecdf(GPP_filled)(GPP_filled)) %>% arrange(gpp_cdf) %>% 
  slice(which.min(abs(gpp_cdf - 0.5))) 

doy_model <- river_metab_50iles %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(lm = map(data,
                  ~lm(doy ~ year, data = .)),
         coefs = map(lm, coefficients),
         anova = map(lm, anova))

doy_model_pvals <- unnest(doy_model, anova) %>% 
  filter(Df == 1) %>% 
  select(pval = `Pr(>F)`)

doy_model_merged <- left_join(river_metab_50iles, 
                              doy_model_pvals, 
                              'site')

ggplot(doy_model_merged,
       aes(x = site, 
           y = doy))+
  geom_point()+
  #geom_line()+
  #gghighlight(pval <= 0.05)+
  labs(y = 'DOY of 50th-ile GPP')+
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# When is the most productive week?
river_metab_week <- river_metab %>% 
  select(site, date, GPP_filled) %>% 
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) %>% 
  group_by(site, week, year) %>% 
  summarise(gpp_week = sum(GPP_filled, na.rm = TRUE)) %>% 
  group_by(site, year) %>% 
  slice_max(gpp_week) 

week_model <- river_metab_week %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(lm = map(data,
                  ~lm(gpp_week ~ year, data = .)),
         coefs = map(lm, coefficients),
         anova = map(lm, anova))

week_model_pvals <- unnest(week_model, anova) %>% 
  filter(Df == 1) %>% 
  select(pval = `Pr(>F)`)

ggplot(left_join(river_metab_week,
                 week_model_pvals,
                 'site'),
       aes(x = year, 
           y = week,
           color = site))+
  geom_point()+
  geom_line()+
  gghighlight(pval <= 0.05)+
  ylab('Week of highest GPP')
  


# How many days does each S-Y have in the upper 75%-ile
river_metab_75 <- river_metab %>% 
  select(site, date, GPP_filled) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(site,year) %>% 
  mutate(quant_75 = quantile(GPP_filled, probs = 0.75)) %>% 
  filter(GPP_filled > quant_75) %>% 
  group_by(site, year) %>% 
  summarise(n_75_days = n())

n_75_model <- river_metab_75 %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(lm = map(data,
                  ~lm(n_75_days ~ year, data = .)),
         coefs = map(lm, coefficients),
         anova = map(lm, anova))

n_75_model_pvals <- unnest(n_75_model, anova) %>% 
  filter(Df == 1) %>% 
  select(pval = `Pr(>F)`)

ggplot(left_join(river_metab_75,
                 n_75_model_pvals,
                 'site') %>% summary(),
       aes(x = site, 
           y = n_75_days))+
  geom_point()+
  #geom_line()+
  gghighlight(pval <= 0.05)+
  ylab('Number of Days >75th-ile Annual GPP')


# trends in GPP variance
annual_C_cv_trends <- river_metab_ann %>% 
  dplyr::select(site, year, GPP_daily_cv) %>% 
  dplyr::group_by(site) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(sens = purrr::map(data, ~trend::sens.slope(x = .$GPP_daily_cv)),
                mk_test = purrr::map(data, ~trend::mk.test(x = .$GPP_daily_cv))) %>% 
  dplyr::mutate(sens_p = purrr::map(sens, ~.$p.value), 
                sens_s = purrr::map_dbl(sens, ~.$estimates),
                mk_p = purrr::map_dbl(mk_test, ~.$p.value),
                mk_s = purrr::map_dbl(mk_test, ~.$estimates['S']),
                sens_sig = ifelse(sens_p <= 0.05,'significant', 'non-significant'),
                sens_slope = ifelse(sens_s > 0, 'increasing', 'decreasing'),
                mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
                mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing')) 


annual_C_cv_sigs <- annual_C_cv_trends %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(sens_p = unlist(sens_p)) %>% 
  dplyr::select(site, sens_sig, sens_slope, sens_s, sens_p)


river_metab_ann %>% 
  dplyr::left_join(annual_C_cv_sigs) %>% 
  ggplot2::ggplot(.,
                  ggplot2::aes(x = year, 
                               y = GPP_daily_cv,
                               color = interaction(sens_slope,sens_sig)))+
  ggplot2::geom_point(aes(group = site))+
  ggplot2::geom_line(aes(group = site))+
  #geom_smooth(method = 'lm', se = FALSE)+
  ggplot2::labs(y = expression(paste(CV[GPP], ' (%)')))+
  # scale_color_viridis_d(direction = -1)+
  ggplot2::facet_grid(sens_sig ~ sens_slope)

