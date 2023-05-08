## Header ----
## Script name: usgs_annual_metabolism.R
##
## Purpose of script: compile and analyze annual GPP data from USGS sites (2007-2021)
##
## Author: Nick Marzolf
## Date Created: 2022-10-07
## Date Modified: 
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
{library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(lme4)
  library(nlme)
  library(ggpubr)
  library(performance)
  library(EnvStats)
  library(ggExtra)
}
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


# 1) read in data ----
river_metab <- readr::read_csv('data/output_data/good_estimates_2007-2021.csv') 

fluxnet_metab <- readr::read_csv('data/output_data/fluxnet_daily_wBiome.csv')


metab_units_area <- (expression(paste('GPP (g C ', m^-2, ' ',y^-1,')', sep = ' ')))

# # lake_metab <- read_csv('data/output_data/lake_metab_fill_norm.csv')
# 

# 2) calculate long-term total GPP and CV for FNet and USGS ----
metab_all_daily <- rbind(river_metab %>% 
                           dplyr::select(site, date, GPP = GPP) %>% 
                           dplyr::mutate(source = 'USGS',
                                         biome = NA),
                         fluxnet_metab %>% 
                           dplyr::select(site, date, GPP, biome) %>% 
                           dplyr::mutate(source = 'Fluxnet'))


metab_all_annual <- metab_all_daily %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::group_by(source, site, year, biome) %>% 
  dplyr::summarise(GPP_ann_C = dplyr::if_else('USGS' %in% source,
                                              sum(GPP/1.25, na.rm = TRUE),
                                              sum(GPP, na.rm = TRUE)),
                   GPP_daily_cv = EnvStats::cv(GPP, 
                                               na.rm = TRUE, 
                                               method = 'l.moments')*100)



plot_GPP_ts <- ggplot2::ggplot(metab_all_annual,
                               aes(x = year,
                                   y = GPP_ann_C,
                                   group = site,
                                   color = GPP_ann_C))+
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::scale_color_gradient(name = metab_units_area,
                                low = '#855E09',
                                high = '#40E304')+
  ggplot2::facet_grid(source ~ .)+
  ggplot2::ylab(metab_units_area)+
  ggplot2::theme(axis.title.x = element_blank())
plot_GPP_ts



plot_GPP_ann <- ggplot2::ggplot(data = metab_all_annual,
                                ggplot2::aes(x = reorder(site, desc(GPP_ann_C)), 
                                             y = GPP_ann_C,
                                             color = GPP_ann_C))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::ylab(metab_units_area)+
  ggplot2::facet_wrap(. ~ source,
                      scales = 'free')+
  ggplot2::scale_color_gradient(name = metab_units_area,
                                low = '#855E09',
                                high = '#40E304')+
  ggplot2::scale_y_log10(limits = c(10, 10000))+
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 #legend.position = 'top',
                 panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank())
plot_GPP_ann

# calculate long term CV
lt_cv_df <- metab_all_annual %>% 
  dplyr::group_by(site, source, biome) %>% 
  dplyr::summarise(lt_mean_GPP = mean(GPP_ann_C, na.rm = TRUE),
                   n_obs = length(GPP_ann_C),
                   lt_se_GPP = sd(GPP_ann_C, na.rm = TRUE)/n_obs,
                   lt_cv = lt_mean_GPP/sd(GPP_ann_C, na.rm = TRUE),
                   yr_range = paste(min(year), max(year), sep = '-'))  

lt_cv_df_sub_flux <- data.frame(lt_cv_df) %>% 
  dplyr::arrange(desc(lt_cv)) %>% 
  dplyr::filter(source == 'Fluxnet',
                between(row_number(), 1, 5))
lt_cv_df_sub_flux

river_names <- readr::read_csv('data/site_info/nwis_site_info.csv') %>% 
  mutate(site = paste0('nwis_',site_no)) %>% 
  select(site, station_nm)

lt_cv_df_sub_flux_usgs <- data.frame(lt_cv_df) %>% 
  dplyr::arrange(desc(lt_cv)) %>% 
  dplyr::filter(source == 'USGS') %>% 
  dplyr::filter(between(row_number(), 1, 5)) %>% 
  dplyr::left_join(river_names, 'site') %>% 
  dplyr::mutate(clean_names = stringr::str_to_title(station_nm, locale = 'en'))
lt_cv_df_sub_flux_usgs

# and plot
plot_lt_cv <- ggplot2::ggplot()+
  ggplot2::geom_dotplot(data = lt_cv_df,
                        aes(x = source, y = lt_cv,
                            fill = lt_mean_GPP,
                            group = lt_mean_GPP),
                        dotsize = 0.8,
                        binaxis='y', 
                        stackdir='center',
                        stackgroups=TRUE, 
                        method="histodot",
                        binwidth = 3)+
  ggplot2::scale_fill_gradient(name = metab_units_area,
                               low = '#855E09',
                               high = '#40E304')+
  ggplot2::ylab(expression(paste(CV[GPP],' (%)')))+
  ggrepel::geom_label_repel(data = lt_cv_df_sub_flux,
                            aes(x = 1, 
                                y = lt_cv,
                                label = biome),
                            nudge_x = c(0.3,-0.3,0.3,-0.3,-0.3),
                            nudge_y = c(1,1,-5,0,-1),
                            size = 2)+
  ggrepel::geom_label_repel(data = lt_cv_df_sub_flux_usgs,
                            aes(x = 2, 
                                y = lt_cv,
                                label = clean_names),
                            size = 2,
                            nudge_x = 0.5,
                            nudge_y = c(50,40,30,20,10))+
  ggplot2::theme(axis.title.x = element_blank(),
                 legend.position = 'none')
plot_lt_cv


fig_1 <- cowplot::plot_grid(plot_GPP_ann,
                            plot_lt_cv,
                            nrow = 2, 
                            ncol = 1,
                            common.legend = TRUE,
                            align = 'hv',
                            axis = 'l')

ggsave(plot = fig_1,
       'manuscript/long-term_GPP/figures/fig_1.png',
       dpi = 600,
       width = 6.5, height = 5)


fig_2_main <- ggplot(lt_cv_df,
                     aes(x = lt_mean_GPP,
                         y = lt_cv,
                         color = source))+
  geom_point(size = 1.5)+
  geom_errorbarh(aes(xmin = lt_mean_GPP - lt_se_GPP,
                     xmax = lt_mean_GPP + lt_se_GPP))+
  ggplot2::scale_color_manual(name = element_blank(),
                              values = c('#855E09','darkblue'))+
  labs(x = expression(paste("Mean Annual GPP (g C ", m^-2, " ", y^-1, ")", sep = " ")),
       y = expression(paste(CV[GPP],' (%)')),
       color = NULL)+
  geom_smooth(method = 'lm')+
  theme(legend.position = c(0.15,0.85),
        legend.background = element_rect(fill = 'white',
                                         color = 'black'),
        legend.margin = margin(-5, 1, 1, 1),
        legend.spacing = unit(0, 'pt'))
fig_2_main

fig_2 <- ggExtra::ggMarginal(fig_2_main,
                             type = 'density',
                             margins = 'both',
                             groupFill = TRUE,
                             groupColour = TRUE,
                             size = 10)
ggsave(plot = fig_2,
       'manuscript/long-term_GPP/figures/fig_2.png',
       dpi = 300,
       width = 6, height = 5)

extreme_prod_sites <- lt_cv_df %>%
  data.frame() %>% 
  dplyr::filter(source == 'USGS') %>% 
  dplyr::arrange(lt_mean_GPP) %>% 
  dplyr::slice(c(which.min(lt_mean_GPP), which.max(lt_mean_GPP))) %>% 
  dplyr::pull(site)


catch_area <- table_s1 %>% 
  dplyr::mutate(site = paste0('nwis_', `Site Number`))

area_cor_df <-lt_cv_df %>% 
  dplyr::filter(source == 'USGS') %>% 
  dplyr::left_join(catch_area, 
                   by = 'site') 

area_cor <- lm(data = area_cor_df, 
               lt_cv ~ log10(`Drainage Area (km2)`))

summary(area_cor)

river_metab %>% 
  filter(site %in% extreme_prod_sites)


# 3) calculate trends ----

# trends in total productivity
river_metab_ann <- metab_all_annual %>%
  dplyr::filter(source == 'USGS') 

annual_C_sum_trends <- river_metab_ann %>% 
  dplyr::select(site, year, GPP_ann_C) %>% 
  dplyr::group_by(site) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(sens = purrr::map(data, ~trend::sens.slope(x = .$GPP_ann_C)),
                mk_test = purrr::map(data, ~trend::mk.test(x = .$GPP_ann_C))) %>% 
  dplyr::mutate(sens_p = purrr::map(sens, ~.$p.value), 
                sens_s = purrr::map_dbl(sens, ~.$estimates),
                mk_p = purrr::map_dbl(mk_test, ~.$p.value),
                mk_s = purrr::map_dbl(mk_test, ~.$estimates['S']),
                sens_sig = ifelse(sens_p <= 0.05,'significant', 'non-significant'),
                sens_slope = ifelse(sens_s > 0, 'increasing', 'decreasing'),
                mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
                mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing')) 


annual_C_sum_sigs <- annual_C_sum_trends %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(sens_p = unlist(sens_p)) %>% 
  dplyr::select(site, sens_sig, sens_slope, sens_s, sens_p)

n_sig <- annual_C_sum_sigs %>% 
  dplyr::filter(sens_p <= 0.05) %>% 
  dplyr::group_by(sens_slope) %>% 
  summarise(n = n()) %>% 
  data.frame()


river_trends <- river_metab_ann %>% 
  dplyr::left_join(annual_C_sum_sigs) 

fig_3 <- ggplot2::ggplot(river_trends,
                         ggplot2::aes(x = year, 
                                      y = GPP_ann_C,
                                      color = sens_slope))+
  #ggplot2::geom_point(aes(group = site))+
  ggplot2::geom_line(aes(group = site),
                     linewidth = 1.25)+
  scale_y_log10()+
  scale_x_continuous(n.breaks = 10)+
  gghighlight::gghighlight(sens_sig == 'significant',
                           use_direct_label = FALSE,
                           keep_scales = TRUE,
                           unhighlighted_params = list(alpha("grey", 0.4),
                                                       linewidth = 0.1))+
  ggplot2::labs(y = metab_units_area)+
  scale_color_manual(name = 'Trend',
                     values = c('#855E09','#40E304'),
                     labels = c(glue::glue('Decreasing (n = ', n_sig[1,2],')'),
                                glue::glue('Increasing (n = ', n_sig[2,2],')')))+
  theme(axis.title.x = element_blank())
fig_3

ggsave(plot = fig_3,
       'manuscript/long-term_GPP/figures/fig_3.png',
       dpi = 600,
       width = 6, height = 4)

# 4) plot drivers: light, temperature, and discharge  ----
river_light <- readr::read_csv('data/output_data/daily_light_metrics.csv')

library(BernhardtMetabolism)
source('code/functions/fill_and_normalize_metab.R')
river_metab_filled <- fill_and_normalize_metab(river_metab)

river_metab_wLight <- river_metab_filled %>% 
  dplyr::left_join(.,
                   river_light, 
                   by = c('site', 'date'))


river_metab_ann_drivers <- river_metab_wLight %>% 
  dplyr::select(date, site, GPP_filled, discharge, daily_PAR, temp.water) %>% 
  dplyr::group_by(site,
                  year = lubridate::year(date)) %>% 
  dplyr::summarise(GPP_ann_C = sum(GPP_filled/1.25, na.rm = TRUE),         # g C m-2 y-1
                   Q_ann_mean = mean(discharge,na.rm = TRUE),
                   Q_ann_cv = EnvStats::cv(discharge, method = 'l.moments'),
                   light_ann_tot = sum(daily_PAR, na.rm = TRUE),
                   temp_ann_mean = mean(temp.water, na.rm = TRUE)) %>%      # mol m-2 y-1
  dplyr::filter(light_ann_tot != 0)  


# each point is a site-year
ggplot2::ggplot(river_metab_ann_drivers,
                ggplot2::aes(x = light_ann_tot,
                             y = Q_ann_cv,
                             color = GPP_ann_C))+
  ggplot2::geom_point(size = 2,
                      alpha = 0.8)+
  # ggplot2::labs(x = expression(paste('Stream PAR (mol ', m^-2,' ',y^-1,')')),
  #               y = expression(paste(CV[Q], ' (%)')))+
  ggplot2::scale_color_gradient(name = metab_units_area,
                                low = '#855703',
                                high = '#40E304')

# summarise by site
river_metab_ann_drivers_site <- river_metab_ann_drivers %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarise(GPP_mean_ann = mean(GPP_ann_C, na.rm = TRUE),
                   GPP_sd_ann = sd(GPP_ann_C, na.rm = TRUE),
                   n_obs = length(GPP_ann_C),
                   GPP_se_ann = GPP_sd_ann/n_obs,
                   GPP_cv = EnvStats::cv(GPP_ann_C)*100,
                   light_ann_mean = mean(light_ann_tot, na.rm = TRUE),
                   light_ann_cv = EnvStats::cv(light_ann_tot)*100,
                   discharge_mean = mean(Q_ann_mean, na.rm = TRUE),
                   discharge_cv = EnvStats::cv(Q_ann_mean)*100,
                   temp_mean = mean(temp_ann_mean, na.rm = TRUE),
                   temp_sd = sd(temp_ann_mean, na.rm = TRUE),
                   temp_se = temp_sd/length(temp_ann_mean),
                   temp_cv = EnvStats::cv(temp_ann_mean)*100) 

driver_sum <- river_metab_ann_drivers_site %>% 
  tidyr::pivot_longer(cols = c(light_ann_mean, light_ann_cv, 
                               discharge_mean, discharge_cv,
                               temp_mean,temp_cv),
                      names_to = 'driver',
                      values_to = 'value') 

driver_sum$driver <- factor(driver_sum$driver, 
                            levels = c('light_ann_mean','discharge_mean', 'temp_mean', 
                                       'light_ann_cv', 'discharge_cv','temp_cv'))

units_GPP_ann <- expression(paste('GPP (g C ', m^-2,' ', y^-1,')'))

# drive plots
## GPP vs. drivers
# ggarrange(
#   ggplot(driver_sum %>% 
#            filter(driver == 'light_ann_mean'),
#          aes(x = value,
#              y = GPP_mean_ann,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = units_GPP_ann,
#          x = expression(paste('Mean Annual Light (mol ', m^-2,' ', y^-1,')')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   
#   ggplot(driver_sum %>% 
#            filter(driver == 'discharge_mean'),
#          aes(x = value,
#              y = GPP_mean_ann,
#              color = GPP_mean_ann))+
#     geom_point()+
#     scale_x_log10()+
#     labs(y = element_blank(),
#          x = expression(paste('Mean Annual Discharge (', m^3, ' ',s^-1,')')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   ggplot(driver_sum %>% 
#            filter(driver == 'temp_mean'),
#          aes(x = value,
#              y = GPP_mean_ann,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = element_blank(),
#          x = 'Mean Annual Temperature (°C)')+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304'),
#   
#   
#   
#   # GPP vs. CV drivers
#   ggplot(driver_sum %>% 
#            filter(driver == 'light_ann_cv'),
#          aes(x = value,
#              y = GPP_mean_ann,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = units_GPP_ann,
#          x = expression(paste(CV[Light], ' (%)')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   ggplot(driver_sum %>% 
#            filter(driver == 'discharge_cv'),
#          aes(x = value,
#              y = GPP_mean_ann,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = element_blank(),
#          x = expression(paste(CV[Discharge], ' (%)')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   ggplot(driver_sum %>% 
#            filter(driver == 'temp_cv'),
#          aes(x = value,
#              y = GPP_mean_ann,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = element_blank(),
#          x = expression(paste(CV[Temperature], '(%)')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304'),
#   ncol = 3, nrow = 2,
#   align = 'hv',
#   legend = 'right',
#   common.legend = TRUE,
#   labels = 'auto',
#   label.x = 0.93)



# CV of GPP vs. same drivers
# ggarrange(
#   ggplot(driver_sum %>% 
#            filter(driver == 'light_ann_mean'),
#          aes(x = value,
#              y = GPP_cv,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = expression(paste(CV[GPP], ' (%)')),
#          x = expression(paste('Mean Annual Light (mol ', m^-2,' ', y^-1,')')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   
#   ggplot(driver_sum %>% 
#            filter(driver == 'discharge_mean'),
#          aes(x = value,
#              y = GPP_cv,
#              color = GPP_mean_ann))+
#     geom_point()+
#     scale_x_log10()+
#     labs(y = element_blank(),
#          x = expression(paste('Mean Annual Discharge (', m^3, ' ',s^-1,')')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   ggplot(driver_sum %>% 
#            filter(driver == 'temp_mean'),
#          aes(x = value,
#              y = GPP_cv,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = element_blank(),
#          x = 'Mean Annual Temperature (°C)')+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304'),
#   
#   
#   
#   # GPP vs. CV drivers
#   ggplot(driver_sum %>% 
#            filter(driver == 'light_ann_cv'),
#          aes(x = value,
#              y = GPP_cv,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = expression(paste(CV[GPP], ' (%)')),
#          x = expression(paste(CV[Light], ' (%)')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   ggplot(driver_sum %>% 
#            filter(driver == 'discharge_cv'),
#          aes(x = value,
#              y = GPP_cv,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = element_blank(),
#          x = expression(paste(CV[Discharge], ' (%)')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304')+
#     theme(legend.position = 'none'),
#   ggplot(driver_sum %>% 
#            filter(driver == 'temp_cv'),
#          aes(x = value,
#              y = GPP_cv,
#              color = GPP_mean_ann))+
#     geom_point()+
#     labs(y = element_blank(),
#          x = expression(paste(CV[Temperature], '(%)')))+
#     scale_color_gradient(name = metab_units_area,
#                          low = '#855703',
#                          high = '#40E304'),
#   ncol = 3, nrow = 2,
#   align = 'hv',
#   legend = 'right',
#   common.legend = TRUE,
#   labels = 'auto',
#   label.x = 0.93)



# 5) driver stats: for all site-years ----

# all 3 drivers
gpp_mult_reg <- lme4::lmer(data = river_metab_ann_drivers,
                           GPP_ann_C ~ Q_ann_cv + light_ann_tot + temp_ann_mean + (1|site))
summary(gpp_mult_reg)
car::Anova(gpp_mult_reg)
plot(gpp_mult_reg)
resid(gpp_mult_reg) %>% hist()

mult_ranef <- nlme::ranef(gpp_mult_reg)[[1]] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename(site = rowname,
         ranef = X.Intercept.) %>% 
  mutate(effect = 'interaction')
nlme::fixef(gpp_mult_reg)


# light only
gpp_reg_light <- lme4::lmer(data = river_metab_ann_drivers,
                            GPP_ann_C ~ light_ann_tot + (1|site))
summary(gpp_reg_light)
light_ranef <- nlme::ranef(gpp_reg_light)[[1]] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename(site = rowname,
         ranef = X.Intercept.) %>% 
  mutate(effect = 'light')


# cv Q only
gpp_reg_cvQ <- lme4::lmer(data = river_metab_ann_drivers,
                          GPP_ann_C ~ Q_ann_cv + (1|site))
summary(gpp_reg_cvQ)
cvQ_ranef <- nlme::ranef(gpp_reg_cvQ)[[1]] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename(site = rowname,
         ranef = X.Intercept.) %>% 
  mutate(effect = 'cv_Q')

# temperature
gpp_reg_temp <- lme4::lmer(data = river_metab_ann_drivers,
                           GPP_ann_C ~ temp_ann_mean + (1|site))

summary(gpp_reg_temp)
temp_ranef <- nlme::ranef(gpp_reg_temp)[[1]] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename(site = rowname,
         ranef = X.Intercept.) %>% 
  mutate(effect = 'temperature')


# compile and compare model outputs
ranefs_all <- list(mult_ranef,
                   light_ranef,
                   temp_ranef,
                   cvQ_ranef) %>% 
  reduce(rbind) %>% 
  pivot_wider(names_from = effect,
              values_from = ranef)
ranefs_all[,-1] <- round(ranefs_all[,-1], 1)


performance::compare_performance(gpp_reg_temp,
                                 gpp_reg_cvQ, 
                                 gpp_reg_light,
                                 gpp_mult_reg) 

pred_river_metab_ann_drivers <- river_metab_ann_drivers %>% 
  group_by(site, Q_ann_cv, light_ann_tot, temp_ann_mean) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(GPP_ann_C_pred = stats::predict(gpp_mult_reg, 
                                         river_metab_ann_drivers,
                                         re.form = ~(1|site)),
         intervals = merTools::predictInterval(gpp_mult_reg,
                                               level = 0.95,
                                               newdata = river_metab_ann_drivers,
                                               n.sims = 1000,
                                               stat = 'mean',
                                               type = 'linear.prediction'),
         GPP_ann_C = river_metab_ann_drivers$GPP_ann_C)

cond_r2 <- round(performance::performance(gpp_mult_reg)$R2_conditional, 2)

fig_4 <- ggplot(pred_river_metab_ann_drivers,
                aes(x = GPP_ann_C))+
  geom_ribbon(aes(ymax = intervals$upr,
                  ymin = intervals$lwr),
              alpha = 0.2)+
  geom_point(aes(y = intervals$fit))+
  geom_abline(slope = 1,
              intercept = 0,
              linetype = 'dashed',
              color = 'red')+
  labs(x = metab_units_area,
       y = expression(paste('Predicted GPP (g C ', m^-2, ' ',y^-1,')')))+
  lims(x = c(0, 4000),
       y = c(-500, 3000))+
  annotate('text',
           x = 3000, y = -400, 
           label = bquote(R[Conditional]^2 == .(cond_r2)))
fig_4

ggsave(plot = fig_4,
       'manuscript/long-term_GPP/figures/fig_4.png',
       dpi = 600,
       width = 5, height = 4)


# 6) driver stats for average site years ----
glimpse(river_metab_ann_drivers_site)

hist(river_metab_ann_drivers_site$GPP_mean_ann)      # not normal
hist(log(river_metab_ann_drivers_site$GPP_mean_ann)) # not normal


shapiro.test(driver_sum$GPP_mean_ann)       # not normal
shapiro.test(log(driver_sum$GPP_mean_ann))  # not normal
shapiro.test(sqrt(driver_sum$GPP_mean_ann)) # not normal 


gpp_glm_site_all <- glm(data = river_metab_ann_drivers_site,
                        GPP_mean_ann ~ discharge_cv + light_ann_mean + temp_mean)

summary(gpp_glm_site_all)
plot(gpp_glm_site_all)
hist(gpp_glm_site_all$residuals)
performance(gpp_glm_site_all)


gpp_glm_site_light <- glm(data = river_metab_ann_drivers_site,
                          GPP_mean_ann ~ light_ann_mean)
summary(gpp_glm_site_light)
gpp_glm_site_cvQ <- glm(data = river_metab_ann_drivers_site,
                        GPP_mean_ann ~ discharge_cv)
summary(gpp_glm_site_cvQ)
gpp_glm_site_temp <- glm(data = river_metab_ann_drivers_site,
                         GPP_mean_ann ~ temp_mean)
summary(gpp_glm_site_temp)

performance::compare_performance(gpp_glm_site_temp,
                                 gpp_glm_site_cvQ,
                                 gpp_glm_site_light,
                                 gpp_glm_site_all)


