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
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())

# read in data
river_light <- read_csv('data/output_data/daily_light_metrics.csv')
river_metab <- read_csv('data/output_data/usgs_metab_fill_norm.csv') %>% 
  left_join(.,river_light, by = c('site', 'date'))


lake_metab <- read_csv('data/output_data/lake_metab_fill_norm.csv')
fluxnet_metab <- read_csv('data/output_data/fluxnet_daily_wBiome.csv')


# calculate annual C fluxes

river_metab_ann <- river_metab %>% 
  select(site, date, GPP_filled, depth) %>% 
  group_by(site,
           year = lubridate::year(date)) %>% 
  summarise(GPP_ann_C = sum(GPP_filled*(44/32), na.rm = TRUE),          # g C m-2 y-1
            GPP_daily_cv = EnvStats::cv(GPP_filled, na.rm = TRUE),
            GPP_ann_C_vol = sum((GPP_filled*(44/32))/depth, na.rm = TRUE),
            GPP_daily_cv_vol = EnvStats::cv((GPP_filled*(44/32))/depth)) %>%  # g C m-3 y-1
  mutate(source = 'USGS')

river_metab_ann$site <- fct_relevel(river_metab_ann$site,
                                    river_metab_ann %>% 
                                      dplyr::group_by(site) %>% 
                                      dplyr::summarise(mean_ann_GPP = mean(GPP_ann_C, na.rm = TRUE)) %>% 
                                      arrange(desc(mean_ann_GPP)) %>% 
                                      pull(site))

lake_metab_ann <- lake_metab %>% 
  select(site, date, GPP_filled) %>% 
  group_by(site,
           year = lubridate::year(date)) %>% 
  summarise(GPP_ann_C_vol = sum(GPP_filled*(44/32), na.rm = TRUE), # g C m-3 y-1
            GPP_daily_cv_vol = EnvStats::cv(GPP_filled, na.rm = TRUE)) %>% 
  mutate(source = 'Solomon et al. (2013)')


fluxnet_metab_ann <- fluxnet_metab %>% 
  select(site, date, GPP) %>% 
  group_by(site,
           year = lubridate::year(date)) %>% 
  summarise(GPP_ann_C = sum(GPP, na.rm = TRUE),
            GPP_daily_cv = EnvStats::cv(GPP, na.rm = TRUE)) %>% # g C m-2 y-1
  mutate(source = 'Fluxnet')


metab_units_area <- (expression(paste('GPP (g C ', m^-2, ' ',y^-1,')', sep = ' ')))
ggplot(data = rbind(river_metab_ann, 
                    fluxnet_metab_ann),
       aes(x = reorder(site, GPP_ann_C), 
           y = GPP_ann_C,
           fill = source))+
  geom_boxplot()+
  ylim(0, 7000)+
  ylab(metab_units)+
  facet_wrap(. ~ source,
             scales = 'free')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggplot(data = rbind(river_metab_ann, 
                    fluxnet_metab_ann),
       aes(x = reorder(site, GPP_daily_cv), 
           y = GPP_daily_cv,
           fill = source))+
  geom_boxplot()+
  #ylim(0, 7000)+
  ylab(expression(paste(CV[GPP],' (%)')))+
  facet_wrap(. ~ source,
             scales = 'free')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



metab_units_vol <- (expression(paste('GPP (g C ', m^-3, ' ',y^-1,')', sep = ' ')))
ggplot(data = rbind(river_metab_ann, 
                    lake_metab_ann),
       aes(x = reorder(site, GPP_ann_C_vol), 
           y = GPP_ann_C_vol,
           fill = source))+
  geom_boxplot()+
  ylim(0, 7000)+
  ylab(metab_units_vol)+
  facet_wrap(. ~ source,
             scales = 'free')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggplot(data = rbind(river_metab_ann, 
                    lake_metab_ann),
       aes(x = reorder(site, GPP_daily_cv_vol), 
           y = GPP_daily_cv_vol,
           fill = source))+
  geom_boxplot()+
  #ylim(0, 7000)+
  ylab(expression(paste(CV[GPP],' (%)')))+
  facet_wrap(. ~ source,
             scales = 'free_x')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


annual_C_sum_trends <- river_metab_ann %>% 
  select(site, year, GPP_ann_C) %>% 
  dplyr::group_by(site) %>% 
  nest() %>% 
  mutate(sens = map(data, ~trend::sens.slope(x = .$GPP_ann_C)),
         mk_test = map(data, ~trend::mk.test(x = .$GPP_ann_C))) %>% 
  mutate(sens_p = map(sens, ~.$p.value), 
         sens_s = map_dbl(sens, ~.$estimates),
         mk_p = map_dbl(mk_test, ~.$p.value),
         mk_s = map_dbl(mk_test, ~.$estimates['S']),
         sens_sig = ifelse(sens_p <= 0.05,'significant', 'non-significant'),
         sens_slope = ifelse(sens_s > 0, 'increasing', 'decreasing'),
         mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
         mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing')) 


annual_C_sum_sigs <- annual_C_sum_trends %>% 
  ungroup() %>% 
  mutate(sens_p = unlist(sens_p)) %>% 
  select(site, sens_sig, sens_slope, sens_s, sens_p)



river_metab_ann %>% 
  left_join(annual_C_sum_sigs) %>% 
  ggplot(.,
         aes(x = year, 
             y = GPP_ann_C,
             color = interaction(sens_slope,sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  #geom_smooth(method = 'lm', se = FALSE)+
  labs(y = metab_units_area)+
  # scale_color_viridis_d(direction = -1)+
  facet_grid(sens_sig ~ sens_slope)



annual_C_cv_trends <- river_metab_ann %>% 
  select(site, year, GPP_daily_cv) %>% 
  dplyr::group_by(site) %>% 
  nest() %>% 
  mutate(sens = map(data, ~trend::sens.slope(x = .$GPP_daily_cv)),
         mk_test = map(data, ~trend::mk.test(x = .$GPP_daily_cv))) %>% 
  mutate(sens_p = map(sens, ~.$p.value), 
         sens_s = map_dbl(sens, ~.$estimates),
         mk_p = map_dbl(mk_test, ~.$p.value),
         mk_s = map_dbl(mk_test, ~.$estimates['S']),
         sens_sig = ifelse(sens_p <= 0.05,'significant', 'non-significant'),
         sens_slope = ifelse(sens_s > 0, 'increasing', 'decreasing'),
         mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
         mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing')) 


annual_C_cv_sigs <- annual_C_cv_trends %>% 
  ungroup() %>% 
  mutate(sens_p = unlist(sens_p)) %>% 
  select(site, sens_sig, sens_slope, sens_s, sens_p)


river_metab_ann %>% 
  left_join(annual_C_cv_sigs) %>% 
  ggplot(.,
         aes(x = year, 
             y = GPP_daily_cv,
             color = interaction(sens_slope,sens_sig)))+
  geom_point(aes(group = site))+
  geom_line(aes(group = site))+
  #geom_smooth(method = 'lm', se = FALSE)+
  labs(y = expression(paste(CV[GPP], ' (%)')))+
  # scale_color_viridis_d(direction = -1)+
  facet_grid(sens_sig ~ sens_slope)



ggplot()+
  geom_density(data = river_metab_ann,
               alpha = 0.4,
               aes(x = GPP_daily_cv,
                   fill = 'River'))+
  geom_density(data = fluxnet_metab_ann,
               alpha = 0.4,
               aes(x = GPP_daily_cv,
                   fill = 'Fluxnet'))+
  geom_density(data = lake_metab_ann,
               alpha = 0.4,
               aes(x = GPP_daily_cv,
                   fill = 'Lakes'))+
  labs(x = 'CV (%)',
       y = 'Density')



## both light and discharge together

river_metab_ann_drivers <- river_metab %>% 
  select(date, site, GPP_filled, discharge, daily_PAR) %>% 
  group_by(year = lubridate::year(date), site) %>% 
  summarise(GPP_ann_C = sum(GPP_filled*(44/32), na.rm = TRUE), # g C m-2 y-1
            Q_cv = EnvStats::cv(discharge, na.rm = TRUE), 
            light_ann = sum(daily_PAR, na.rm = TRUE)/1e6) # mol m-2 y-1



ggplot(river_metab_ann_drivers,
       aes(x = light_ann,
           y = 1/Q_cv))+
  geom_point(aes(size = GPP_ann_C))+
  labs(x = expression(paste('Stream PAR (mol ', m^-2,' ','y^-1)')),
       y = expression(paste(CV[Q], ' (%)')),
       size = metab_units_area)

ggplot(river_metab_ann_drivers,
       aes(x = light_ann,
           y = GPP_ann_C))+
  geom_point()+
  labs(x = expression(paste('Stream PAR (mol ', m^-2,' ',y^-1,')')),
       y = metab_units_area)

ggplot(river_metab_ann_drivers,
       aes(x = 1/Q_cv,
           y = GPP_ann_C))+
  geom_point()+
  labs(x = expression(paste(CV[Q[daily]], ' (%)')),
       y = metab_units_area)


# multiple regression

gpp_glmer <- glmer(data = river_metab_ann_drivers,
                   log10(GPP_ann_C) ~ Q_cv * light_ann + (1|site),
                   family = gaussian)
summary(gpp_glmer)
ranef(gpp_glmer)
fixef(gpp_glmer)
plot(gpp_glmer)


gpp_mult_reg <- lmer(data = river_metab_ann_drivers,
                     log10(GPP_ann_C) ~ Q_cv * light_ann + (1|site))
summary(gpp_mult_reg)

anova(gpp_glmer, gpp_mult_reg)



