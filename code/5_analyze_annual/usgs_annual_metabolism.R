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



estimates <- read_csv('data/model_runs/estimates_2007-2021.csv') %>% 
  dplyr::filter(lubridate::year(date) > 2007) %>% 
  mutate(GPP_C = GPP*(12/32))

nwis_site_info <- read_csv('data/site_info/nwis_site_info.csv') %>% 
  mutate(site = paste0('nwis_', site_no)) %>% 
  dplyr::filter(unique(estimates$site) %in% site)


annual_light <- read_csv('data/usgs_streamlight/annual_light.csv')


annual_data <- estimates %>% 
  dplyr::group_by(site, 
                  year = year(date)) %>% 
  dplyr::summarise(GPP_C_ann = sum(GPP_C),
                   CV_Q = EnvStats::cv(discharge)) %>% 
  inner_join(annual_light, by = c('site', 'year')) %>% 
  left_join(nwis_site_info, by = 'site')

# sort sites by decreasing GPP
annual_data$station_nm <- fct_relevel(annual_data$station_nm,
                                      annual_data %>% 
                                        dplyr::group_by(station_nm) %>% 
                                        dplyr::summarise(mean_ann_GPP = mean(GPP_C_ann, na.rm = TRUE)) %>% 
                                        arrange(desc(mean_ann_GPP)) %>% 
                                        pull(station_nm))

# create dataset that combines GPP from USGS and fluxnet sites
fluxnet_biome <- read_csv('data/fluxnet_annual_wBiome.csv') 

merged_gpp <- fluxnet_biome %>% 
  select(sitecode,
         biome, 
         gpp = GPP_site_mean) %>% 
  mutate(source = 'Fluxnet') %>% 
  bind_rows(annual_data %>% 
              select(site, 
                     year,
                     gpp = GPP_C_ann) %>% 
              mutate(source = 'USGS'))

merged_gpp$site <- fct_relevel(merged_gpp$site,
                               merged_gpp %>% 
                                 filter(!is.na(site)) %>% 
                                 dplyr::group_by(site) %>% 
                                 dplyr::summarise(mean_ann_GPP = mean(gpp, na.rm = TRUE)) %>% 
                                 arrange(desc(mean_ann_GPP)) %>% 
                                 pull(site))



fluxnet_biome_boxplot <- ggplot(merged_gpp %>% 
                                  filter(source == 'Fluxnet'),
                                aes(x = biome, y = gpp))+
  geom_boxplot(fill = 'red')+
  ylab(expression(paste('GPP (g C ',m^-2,' ',y^-1,')')))+
  ylim(0, 4600)+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1,
                                   size = 8),
        axis.text.y = element_blank())


usgs_site_boxplot <- ggplot(merged_gpp %>% 
                              filter(source == 'USGS'),
                            aes(x = site, y = gpp))+
  geom_boxplot(fill = 'lightblue')+
  ylim(0, 4600)+
  ylab(expression(paste('GPP (g C ',m^-2,' ',y^-1,')')))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 0,
                                   size = 8),
        plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(usgs_site_boxplot,
          NULL,
          fluxnet_biome_boxplot,
          rel_widths = c(1,0,1),
          ncol = 3, 
          align = 'hv')


# boxplots by site
ggplot()+
  geom_boxplot(data = annual_data,
               aes(x = station_nm,
                   y = GPP_C_ann))+
  # geom_hline(data = ameriflux_GPP_sum,
  #            aes(yintercept = min),
  #            color = 'red', linetype = 'dashed')+
  # geom_hline(data = ameriflux_GPP_sum,
  #            aes(yintercept = quant_25),
  #            color = 'red', linetype = 'dashed')+
  # geom_hline(data = ameriflux_GPP_sum,
  #            aes(yintercept = quant_50),
  #            color = 'red', linetype = 'dashed')+
  # geom_hline(data = ameriflux_GPP_sum,
  #            aes(yintercept = quant_75),
#            color = 'red', linetype = 'dashed')+
# geom_hline(data = ameriflux_GPP_sum,
#            aes(yintercept = max),
#            color = 'red', linetype = 'dashed')+
labs(x = element_blank(),
     y = expression(paste('GPP (g C ', m^-2,' ',y^-1,')')))+
  # annotate(geom = 'text',
  #          x = 45,
  #          y = c(200, 1050,1600,2000,4700),
  #          label = c('Min', '25pct', '50pct', '75pct', 'Max'),
  #          color = 'red')+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   size = 6))

# calculate CV by fluxnet biome and by USGS site
fluxnet_biome_cv <- merged_gpp %>% 
  group_by(biome) %>% 
  summarise(cv_gpp = EnvStats::cv(gpp, na.rm = TRUE)) %>% 
  filter(!is.na(biome))


usgs_site_cv <- merged_gpp %>% 
  group_by(site) %>% 
  summarise(cv_gpp = EnvStats::cv(gpp, na.rm = TRUE)) %>% 
  filter(!is.na(site))

plot_grid(ggplot(usgs_site_cv, 
                 aes(x = site, y = cv_gpp))+
            geom_point(color = 'lightblue')+
            ylim(0, 1),
          ggplot(fluxnet_biome_cv, 
                 aes(x = biome, y = cv_gpp))+
            geom_point(color = 'red')+
            ylim(0, 1),
          ncol = 2, align = 'hv'
)


# drivers 
## both light and discharge together

ggplot(annual_data,
       aes(x = stream_light/1e6,
           y = 1/CV_Q))+
  # geom_point(aes(size = GPP_C_ann,
  #                color = station_nm))+
  stat_ellipse(aes(size = GPP_C_ann,
                   color = station_nm))+
  scale_color_viridis_d(direction = -1)+
  scale_size(name = expression(paste('GPP (g C ', m^-2,' ', y^-1,')')),
             range = c(0.25,5))+
  labs(x = 'Light at Stream Surface (mol m-2 y-1)',
       y = 'Annual Hydrologic predicability (1/CV(discharge))')+
  geom_hline(yintercept = median(1/annual_data$CV_Q, na.rm = TRUE),
             linetype = 'dashed')+
  geom_vline(xintercept = median(annual_data$stream_light/1e6, na.rm = TRUE),
             linetype = 'dashed')+
  theme(legend.text = element_text(size = 4),
        legend.position = 'none')

# average by site
ggplot(data = annual_data %>% 
         group_by(site) %>% 
         summarise(mean_light = mean(stream_light, na.rm = TRUE),
                   sd_light = sd(stream_light, na.rm = TRUE),
                   mean_CV_Q = mean(CV_Q, na.rm = TRUE),
                   sd_CV_Q  =  sd(CV_Q, na.rm = TRUE),
                   mean_GPP = mean(GPP_C_ann, na.rm = TRUE),
                   sd_GPP = sd(GPP_C_ann, na.rm = TRUE)),
       aes(x = mean_light,
           y = mean_CV_Q))+
  geom_point(aes(size = mean_GPP))+
  geom_errorbarh(aes(xmin = mean_light - sd_light,
                    xmax = mean_light + sd_light))+
  geom_errorbar(aes(ymin = mean_CV_Q - sd_CV_Q,
                     ymax = mean_CV_Q + sd_CV_Q))




# light only
ggplot(annual_data,
       aes(x = stream_light/1e6,
           y = GPP_C_ann))+
  geom_point(aes(color = station_nm))+
  scale_color_viridis_d(direction = -1)+
  labs(x = 'Light at Stream Surface (mol m-2 y-1)',
       y = expression(paste('GPP (g C ', m^-2,' ', y^-1,')')))+
  theme(legend.text = element_text(size = 4))


# discharge
ggplot(annual_data,
       aes(x = 1/CV_Q,
           y = GPP_C_ann))+
  geom_point(aes(color = station_nm))+
  scale_color_viridis_d(direction = -1)+
  labs(x = 'Annual Hydrologic Stability (1/CV(Q))',
       y = expression(paste('GPP (g C ', m^-2,' ', y^-1,')')))+
  theme(legend.text = element_text(size = 4))


# multiple regression
annual_data_in <- annual_data %>% 
  select(site, station_nm,GPP_C_ann,CV_Q, stream_light) %>% 
  mutate(Q_stability = 1/CV_Q,
         stream_light_mol_yr = stream_light/1e6)

gpp_glmer <- glmer(data = annual_data_in,
                   GPP_C_ann ~ Q_stability * stream_light_mol_yr + (1|station_nm),
                   family = gaussian)
summary(gpp_glmer)
ranef(gpp_glmer)
fixef(gpp_glmer)
plot(gpp_glmer)


gpp_mult_reg <- lm(data = annual_data_in,
                   GPP_C_ann ~ Q_stability * stream_light_mol_yr)
summary(gpp_mult_reg)


anova(gpp_glmer, gpp_mult_reg)



annual_trends <- annual_data %>% 
  select(site, year, GPP_C_ann) %>% 
  dplyr::group_by(site) %>% 
  nest() %>% 
  mutate(sens = map(data, ~trend::sens.slope(x = .$GPP_C_ann)),
         mk_test = map(data, ~trend::mk.test(x = .$GPP_C_ann))) %>% 
  mutate(sens_p = map(sens, ~.$p.value), 
         sens_s = map_dbl(sens, ~.$estimates),
         mk_p = map_dbl(mk_test, ~.$p.value),
         mk_s = map_dbl(mk_test, ~.$estimates['S']),
         sens_sig = ifelse(sens_p <= 0.05,'significant', 'non-significant'),
         sens_slope = ifelse(sens_s > 0, 'increasing', 'decreasing'),
         mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
         mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing')) 


annual_sigs <- annual_trends %>% 
  ungroup() %>% 
  mutate(sens_p = unlist(sens_p)) %>% 
  select(site, sens_sig, sens_slope, sens_s, sens_p)


annual_data %>% 
  left_join(annual_sigs) %>% 
  ggplot(.,
         aes(x = year, y = GPP_C_ann,
             color = station_nm))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  scale_color_viridis_d(direction = -1)+
  facet_wrap(sens_sig ~ sens_slope, 
             nrow = 2)+
  theme(legend.text = element_text(size = 5))

