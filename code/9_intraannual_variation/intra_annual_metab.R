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
{library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(gghighlight)
  library(BernhardtMetabolism)
}
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())

# source additional functions
source('code/functions/fill_and_normalize_metab.R')
source('code/8_intraannual_variation/calc_intrayear_metrics.R')


# read in data ----
river_metab <- readr::read_csv('data/output_data/good_estimates_2007-2021.csv') 

river_metab_filled <- fill_and_normalize_metab(river_metab)


# calculate magnificent 7 ----

# Archfield et al. 2014 (10.1002/rra.2710):

# The moments of the distribution of daily streamflow (mean, CV, skewness, kurtosis) also describe the 
# magnitude of the streamflows.

# The autoregressive lag-one (AR(1)) correlation coefficient describes the persistence of 
# streamflow from 1 day to the next and, therefore, could be considered a proxy for the duration and 
# rate of change in streamflow

# The seasonal signal (amplitude and phase) could be related to the timing of streamflow events.


# river_intra_metrics <- calc_intrayear_metrics(data = river_metab_filled)
# 
# river_intra_metrics[,3:9] <- sapply(river_intra_metrics[,3:9],
#                                     as.numeric)
# 
# readr::write_csv(river_intra_metrics,
#                  'data/output_data/river_ann_metrics.csv')

river_intra_metrics <- readr::read_csv('data/output_data/river_ann_metrics.csv')

# calculate trends of each metric for each site
temp_list <- list()
for(j in 3:9) {
  
  metric <- colnames(river_intra_metrics)[j]
  
  temp_list[[j]] <- river_intra_metrics %>%
    select(site, year, col = metric) %>%
    dplyr::filter(!is.na(col)) %>%
    group_by(site) %>%
    nest() %>%
    mutate(sens = map(data,
                      ~trend::sens.slope(x = as.numeric(.$col))),
           mk_test = map(data,
                         ~trend::mk.test(x = as.numeric(.$col)))) %>%
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


readr::write_csv(river_intra_metrics_trend,
                 'data/output_data/river_intra_metrics_trend.csv')

river_intra_metrics_sigs <- river_intra_metrics_trend %>%
  ungroup() %>%
  mutate(sens_p = unlist(sens_p)) %>%
  select(-data, -sens, -mk_test)
# 
# 
# readr::write_csv(river_intra_metrics_sigs,
#                  'data/output_data/river_intra_metrics_sig.csv')

river_intra_metrics_sigs <- readr::read_csv('data/output_data/river_intra_metrics_sig.csv')
river_intra_metrics_trend <- readr::read_csv('data/output_data/river_intra_metrics_trend.csv')

# plot magnificent 7 ----

for(i in 1:length(unique(river_intra_metrics_sigs$metric))){
  
  metric <- unique(river_intra_metrics_sigs$metric)[i]
  
  d <- river_intra_metrics_sigs %>% 
    dplyr::filter(metric == !!metric) %>% 
    left_join(river_intra_metrics_trend %>% 
                unnest(data) %>% 
                dplyr::filter(metric == !!metric),
              by = 'site') 
  
  n_sig <- d %>% 
    dplyr::filter(sens_p.x <= 0.05) %>% 
    group_by(site, sens_slope.x) %>% 
    summarise(n = n()) %>%
    dplyr::group_by(sens_slope.x) %>% 
    summarise(n = n()) %>% 
    data.frame()
  
  
  ts <- ggplot(d,
               aes(x = year, 
                   y = col,
                   group = site,
                   color = sens_slope.x))+
    geom_line(linewidth = 1.25)+
    gghighlight::gghighlight(sens_sig.x == 'significant',
                             use_direct_label = FALSE,
                             keep_scales = TRUE,
                             unhighlighted_params = list(alpha("grey", 0.4),
                                                         linewidth = 0.1))+
    scale_color_manual(name = element_blank(),
                       values = c('#855E09','#40E304'),
                       labels = c(glue::glue('Decreasing (n = ', n_sig[1,2],')'),
                                  glue::glue('Increasing (n = ', n_sig[2,2],')')))+
    labs(y = metric)
  
  site_dist <- ggplot(d,
                      aes(x = col,
                          y = site,
                          fill = sens_slope.x))+
    geom_density_ridges(rel_min_height = 0.01)+
    labs(x = metric)+
    scale_fill_manual(name = element_blank(),
                      values = c('#855E09','#40E304'),
                      labels = c(glue::glue('Decreasing (n = ', n_sig[1,2],')'),
                                 glue::glue('Increasing (n = ', n_sig[2,2],')')))+
    theme(legend.position = 'none')
  
  plot <- ggpubr::ggarrange(ts, site_dist, ncol = 2, nrow = 1)
  
  ggsave(plot = plot,
         glue::glue('figures/intra_ms/plot_{metric}.png'),
         dpi = 600, height = 10, width = 11)
}

# ----

# What is the DOY when each site-year reaches 50% of annual GPP
quantiles <- c(0.5, 0.75, 0.95)
for(i in 1:length(quantiles)){
  
  quantile <- quantiles[i]
  
  river_metab_tiles <- river_metab_filled %>% 
    dplyr::select(site, date, GPP_filled) %>% 
    dplyr::mutate(doy = lubridate::yday(date)) %>% 
    dplyr::group_by(site, 
                    year = lubridate::year(date)) %>% 
    dplyr::mutate(gpp_cdf = ecdf(GPP_filled)(GPP_filled)) %>% 
    dplyr::arrange(gpp_cdf) %>% 
    dplyr::slice(which.min(abs(gpp_cdf - quantile))) 
  
  doy_model <- river_metab_tiles %>% 
    dplyr::group_by(site) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(lm = map(data,
                           ~lm(doy ~ year, data = .)),
                  coefs = map(lm, coefficients),
                  anova = map(lm, anova))
  
  doy_model_pvals <- tidyr::unnest(doy_model, 
                                   anova,
                                   coefs) %>% 
    dplyr::filter(Df == 1) %>% 
    dplyr::select(pval = `Pr(>F)`,
                  coefs) %>% 
    dplyr::mutate(sig = ifelse(pval <= 0.05,
                               'significant',
                               'non-significant'),
                  dir = ifelse(coefs > 0,
                               'increasing',
                               'decreasing'))
  
  doy_model_merged <- dplyr::left_join(river_metab_tiles, 
                                       doy_model_pvals, 
                                       'site') %>% 
    dplyr::mutate(year = lubridate::year(date))
  
  changers <- ggplot(doy_model_merged,
                     aes(x = year, 
                         y = doy,
                         group = site,
                         color = dir))+
    geom_point()+
    geom_line(linewidth = 1.25)+
    gghighlight::gghighlight(pval <= 0.05,
                             use_direct_label = FALSE,
                             keep_scales = TRUE,
                             unhighlighted_params = list(alpha("grey", 0.4),
                                                         linewidth = 0.1))+
    labs(y = glue::glue('DOY of {quantile*100}th-ile GPP'))+
    theme(axis.title.x = element_blank(),
          legend.title = element_blank())
  
  dist <- ggplot(doy_model_merged,
                 aes(x = doy, 
                     y = site,
                     group = site))+
    geom_boxplot()+
    #geom_density_ridges(jittered_points = TRUE)+
    labs(x = glue::glue('DOY of {quantile*100}th-ile GPP'))+
    lims(x = c(0, 367))
  
  plot <- ggpubr::ggarrange(changers, dist,
                            ncol = 2)
  
  ggsave(plot = plot,
         glue::glue('figures/intra_ms/plot_{quantile*100}_ile.png'),
         dpi = 600,
         height = 10, width = 11)
}



# When is the most productive 7 day period? ----
river_metab_week <- river_metab_filled %>% 
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



# How many days does each S-Y have in the upper 75%-ile ----

for(i in 1:length(quantiles)) {
  
  quantile <- quantiles[i]
  
  river_metab_quantile <- river_metab_filled %>% 
    dplyr::select(site, date, GPP_filled) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>% 
    dplyr::group_by(site,year) %>% 
    dplyr::mutate(quant = quantile(GPP_filled, probs = quantile)) %>% 
    dplyr::filter(GPP_filled > quant) %>% 
    dplyr::group_by(site, year) %>% 
    dplyr::summarise(n_days = n())
  
  quantile_model <- river_metab_quantile %>% 
    dplyr::group_by(site) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(lm = map(data,
                    ~lm(n_days ~ year, data = .)),
           coefs = map(lm, coefficients),
           anova = map(lm, anova))
  
  quantile_model_pvals <- tidyr::unnest(quantile_model, 
                                        anova) %>% 
    dplyr::filter(Df == 1) %>% 
    dplyr::select(pval = `Pr(>F)`)
  
  plot <- ggplot(dplyr::left_join(river_metab_quantile,
                   quantile_model_pvals,
                   'site'),
         aes(x = year, 
             y = n_days,
             group = site))+
    geom_point()+
    geom_line()+
    gghighlight::gghighlight(pval <= 0.05)+
    ylab(glue::glue('Number of Days >={quantile*100}th-ile Daily GPP'))
  
  ggsave(plot = plot,
         glue::glue('figures/intra_ms/plot_days_{quantile*100}_greater.png'),
         dpi = 600,
         width = 7, height = 6)
}

# trends in GPP variance ----
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

