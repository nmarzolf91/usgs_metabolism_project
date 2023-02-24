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
  library(nlme)}
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())

{
  library(dplyr)
  library(ggplot2)
  library(ggExtra)
  library(lme4)
  library(nlme)
}
# 1) read in data ----
river_light <- readr::read_csv('data/output_data/daily_light_metrics.csv')
river_metab <- readr::read_csv('data/output_data/usgs_metab_fill_norm.csv') %>% 
  dplyr::left_join(.,river_light, 
                   by = c('site', 'date'))

metab_units_area <- (expression(paste('GPP (g C ', m^-2, ' ',y^-1,')', sep = ' ')))

# lake_metab <- read_csv('data/output_data/lake_metab_fill_norm.csv')
fluxnet_metab <- readr::read_csv('data/output_data/fluxnet_daily_wBiome.csv')


# 2) calculate long-term total GPP and CV for FNet and USGS ----
metab_all_daily <- rbind(river_metab %>% 
                           dplyr::select(site, date, GPP = GPP_filled) %>% 
                           dplyr::mutate(source = 'USGS'),
                         fluxnet_metab %>% 
                           dplyr::select(site, date, GPP) %>% 
                           dplyr::mutate(source = 'Fluxnet'))


metab_all_annual <- metab_all_daily %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::group_by(source, site, year) %>% 
  dplyr::summarise(GPP_ann_C = dplyr::if_else('USGS' %in% source,
                                              sum(GPP/1.25, na.rm = TRUE),
                                              sum(GPP, na.rm = TRUE)),
                   GPP_daily_cv = EnvStats::cv(GPP, na.rm = TRUE)*100)



plot_GPP_ts <- ggplot(metab_all_annual,
                      aes(x = year,
                          y = GPP_ann_C,
                          group = site,
                          color = GPP_ann_C))+
  geom_line()+
  geom_point()+
  ggplot2::scale_color_gradient(name = metab_units_area,
                                low = '#855E09',
                                high = '#40E304')+
  facet_grid(source ~ .)+
  ylab(metab_units_area)+
  theme(axis.title.x = element_blank())
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
  ggplot2::scale_y_log10(limits = c(10, 5000))+
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 legend.position = 'none',
                 panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank())
plot_GPP_ann

# calculate long term CV
lt_cv_df <- metab_all_annual %>% 
  dplyr::group_by(site, source) %>% 
  dplyr::summarise(lt_mean_GPP = mean(GPP_ann_C, na.rm = TRUE),
                   n_obs = length(GPP_ann_C),
                   lt_se_GPP = sd(GPP_ann_C, na.rm = TRUE)/n_obs,
                   lt_cv = EnvStats::cv(GPP_ann_C, na.rm = TRUE)*100,
                   yr_range = paste(min(year), max(year), sep = '-'))  

# and plot
plot_lt_cv <- ggplot2::ggplot(lt_cv_df,
                              aes(x = source, y = lt_cv,
                                  fill = lt_mean_GPP,
                                  group = lt_mean_GPP))+
  #ggplot2::geom_boxplot()+
  ggplot2::geom_dotplot(dotsize = 0.75,
                        binaxis='y', 
                        stackdir='center',
                        stackgroups=TRUE, 
                        method="histodot")+
  ggplot2::scale_fill_gradient(name = metab_units_area,
                               low = '#855E09',
                               high = '#40E304')+
  ggplot2::ylab('CV (%)')+
  #ylim(0,1)+
  ggplot2::theme(axis.title.x = element_blank())

cowplot::plot_grid(plot_GPP_ann,
                   plot_lt_cv,
                   align = 'hv',
                   axis = 'b',
                   rel_widths = c(1.5,1))


plot <- ggplot(lt_cv_df,
               aes(x = lt_mean_GPP,
                   y = lt_cv,
                   color = source))+
  geom_point(size = 1.5)+
  geom_errorbarh(aes(xmin = lt_mean_GPP - lt_se_GPP,
                     xmax = lt_mean_GPP + lt_se_GPP))+
  ggplot2::scale_color_manual(name = element_blank(),
                              values = c('#855E09','darkblue'))+
  labs(x = expression(paste("Mean GPP (g C ", m^-2, " ", y^-1, ")", sep = " ")),
       y = 'Annual GPP CV (%)')+
  scale_y_log10()+
  theme(legend.position = c(0.9,0.9),
        legend.background = element_blank())

ggExtra::ggMarginal(plot,
                    type = 'boxplot',
                    groupFill = TRUE)


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


river_metab_ann %>% 
  dplyr::left_join(annual_C_sum_sigs) %>% 
  #dplyr::mutate(sig_cat = ifelse())
  ggplot2::ggplot(.,
                  ggplot2::aes(x = year, 
                               y = GPP_ann_C,
                               color = sens_slope))+
  ggplot2::geom_point(aes(group = site))+
  ggplot2::geom_line(aes(group = site))+
  scale_y_log10()+
  scale_x_continuous(n.breaks = 10)+
  gghighlight::gghighlight(sens_sig == 'significant',
                           use_direct_label = FALSE)+
  ggplot2::labs(y = metab_units_area)+
  scale_color_manual(name = 'Trend',
                     values = c('#855E09','#40E304'),
                     labels = c('Decreasing', 'Increasing'))+
  theme(axis.title.x = element_blank())



# 4) plot drivers: light, temperature, and discharge  ----

river_metab_ann_drivers <- river_metab %>% 
  dplyr::select(date, site, GPP_filled, discharge, daily_PAR, temp.water) %>% 
  dplyr::group_by(site,
                  year = lubridate::year(date)) %>% 
  dplyr::summarise(GPP_ann_C = sum(GPP_filled/1.25, na.rm = TRUE),         # g C m-2 y-1
                   Q_ann_mean = mean(discharge,na.rm = TRUE),
                   Q_ann_cv = EnvStats::cv(discharge),
                   light_ann_tot = sum(daily_PAR, na.rm = TRUE)/1e6,
                   temp_ann_mean = mean(temp.water, na.rm = TRUE))       # mol m-2 y-1


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
ggarrange(
  ggplot(driver_sum %>% 
           filter(driver == 'light_ann_mean'),
         aes(x = value,
             y = GPP_mean_ann,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = units_GPP_ann,
         x = expression(paste('Mean Annual Light (mol ', m^-2,' ', y^-1,')')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  
  ggplot(driver_sum %>% 
           filter(driver == 'discharge_mean'),
         aes(x = value,
             y = GPP_mean_ann,
             color = GPP_mean_ann))+
    geom_point()+
    scale_x_log10()+
    labs(y = element_blank(),
         x = expression(paste('Mean Annual Discharge (', m^3, ' ',s^-1,')')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  ggplot(driver_sum %>% 
           filter(driver == 'temp_mean'),
         aes(x = value,
             y = GPP_mean_ann,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = element_blank(),
         x = 'Mean Annual Temperature (°C)')+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304'),
  
  
  
  # GPP vs. CV drivers
  ggplot(driver_sum %>% 
           filter(driver == 'light_ann_cv'),
         aes(x = value,
             y = GPP_mean_ann,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = units_GPP_ann,
         x = expression(paste(CV[Light], ' (%)')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  ggplot(driver_sum %>% 
           filter(driver == 'discharge_cv'),
         aes(x = value,
             y = GPP_mean_ann,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = element_blank(),
         x = expression(paste(CV[Discharge], ' (%)')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  ggplot(driver_sum %>% 
           filter(driver == 'temp_cv'),
         aes(x = value,
             y = GPP_mean_ann,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = element_blank(),
         x = expression(paste(CV[Temperature], '(%)')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304'),
  ncol = 3, nrow = 2,
  align = 'hv',
  legend = 'right',
  common.legend = TRUE,
  labels = 'auto',
  label.x = 0.93)



# CV of GPP vs. same drivers
ggarrange(
  ggplot(driver_sum %>% 
           filter(driver == 'light_ann_mean'),
         aes(x = value,
             y = GPP_cv,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = expression(paste(CV[GPP], ' (%)')),
         x = expression(paste('Mean Annual Light (mol ', m^-2,' ', y^-1,')')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  
  ggplot(driver_sum %>% 
           filter(driver == 'discharge_mean'),
         aes(x = value,
             y = GPP_cv,
             color = GPP_mean_ann))+
    geom_point()+
    scale_x_log10()+
    labs(y = element_blank(),
         x = expression(paste('Mean Annual Discharge (', m^3, ' ',s^-1,')')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  ggplot(driver_sum %>% 
           filter(driver == 'temp_mean'),
         aes(x = value,
             y = GPP_cv,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = element_blank(),
         x = 'Mean Annual Temperature (°C)')+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304'),
  
  
  
  # GPP vs. CV drivers
  ggplot(driver_sum %>% 
           filter(driver == 'light_ann_cv'),
         aes(x = value,
             y = GPP_cv,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = expression(paste(CV[GPP], ' (%)')),
         x = expression(paste(CV[Light], ' (%)')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  ggplot(driver_sum %>% 
           filter(driver == 'discharge_cv'),
         aes(x = value,
             y = GPP_cv,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = element_blank(),
         x = expression(paste(CV[Discharge], ' (%)')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304')+
    theme(legend.position = 'none'),
  ggplot(driver_sum %>% 
           filter(driver == 'temp_cv'),
         aes(x = value,
             y = GPP_cv,
             color = GPP_mean_ann))+
    geom_point()+
    labs(y = element_blank(),
         x = expression(paste(CV[Temperature], '(%)')))+
    scale_color_gradient(name = metab_units_area,
                         low = '#855703',
                         high = '#40E304'),
  ncol = 3, nrow = 2,
  align = 'hv',
  legend = 'right',
  common.legend = TRUE,
  labels = 'auto',
  label.x = 0.93)



# 5) driver stats: for all site-years ----


# both drivers
gpp_mult_reg <- lme4::lmer(data = river_metab_ann_drivers,
                           GPP_ann_C ~ Q_ann_cv + light_ann_tot + temp_ann_mean + (1|site)
)
summary(gpp_mult_reg)
car::Anova(gpp_mult_reg)
plot(gpp_mult_reg)
resid(gpp_mult_reg) %>% hist()

nlme::ranef(gpp_mult_reg)
nlme::fixef(gpp_mult_reg)


# light only
gpp_mult_reg_light <- lme4::lmer(data = river_metab_ann_drivers,
                                 log10(GPP_ann_C) ~ light_ann_tot + (1|site))
summary(gpp_mult_reg_light)
light_model_eff <- nlme::ranef(gpp_mult_reg_light)


# cv Q only
gpp_mult_reg_Q <- lme4::lmer(data = river_metab_ann_drivers,
                             log10(GPP_ann_C) ~ Q_ann_cv + (1|site))
summary(gpp_mult_reg_Q)
q_model_eff <- nlme::ranef(gpp_mult_reg_Q)

eff <- cbind(q_model_eff$site,
             light_model_eff$site,
             mult_model_eff$site)
names(eff) <- c('CV_Q only', 'light only', 'CV_Q*light')


# extract coefficients from multivariate model
out <- data.frame(site = character(),
                  r2 = numeric(),
                  p_val = numeric())
for(i in 1:length(unique(river_metab_ann_drivers$site))){
  
  site_id <- river_metab_ann_drivers$site[i]
  
  d <- river_metab_ann_drivers %>% 
    dplyr::filter(site %in% site_id)
  
  mod <- lm(data = d,
            log(GPP_ann_C) ~ log10(Q_ann_cv) * light_ann_tot)
  
  r2 <- round(summary(mod)$r.squared, 3)
  p_val <- broom::glance(mod) %>% 
    dplyr::pull(p.value) %>% 
    round(.,3)
  
  out <- out %>% 
    dplyr::add_row(site = site_id,
                   r2 = r2,
                   p_val)
}

dplyr::arrange(out, desc(r2))

river_metab_ann_drivers %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarise(GPP_mean_C = mean(GPP_ann_C, na.rm = TRUE)) %>% 
  dplyr::right_join(out, 'site') %>% 
  ggplot2::ggplot(., 
                  ggplot2::aes(x = GPP_mean_C, y = r2))+
  ggplot2::geom_point()


mean_ann <- river_metab_ann_drivers %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarise(Q_Cv_mean = mean(Q_ann_cv, na.rm = TRUE),
                   light_mean = mean(light_ann_tot, na.rm = TRUE),
                   GPP_ann = mean(GPP_ann_C, na.rm = TRUE))

summary(lm(data = mean_ann,
           GPP_ann ~ Q_Cv_mean * light_mean))

# 6) driver stats for average site years
glimpse(river_metab_ann_drivers_site)

hist(river_metab_ann_drivers_site$GPP_mean_ann)      # not normal
hist(log(river_metab_ann_drivers_site$GPP_mean_ann)) # not normal


shapiro.test(driver_sum$GPP_mean_ann)       # not normal
shapiro.test(log(driver_sum$GPP_mean_ann))  # not normal
shapiro.test(sqrt(driver_sum$GPP_mean_ann)) # not normal 


gpp_glmer_site <- lm(data = river_metab_ann_drivers_site,
                     log10(GPP_mean_ann) ~ discharge_cv * light_ann_mean,
                     #family = gaussian
)
summary(gpp_glmer_site)
nlme::ranef(gpp_glmer_site)
nlme::fixef(gpp_glmer_site)
plot(gpp_glmer_site)






