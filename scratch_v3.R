cv <- site_mean_ann_gpp %>% 
  dplyr::select(site, GPP_ann_cv) %>% 
  left_join(.,metab_all_annual, 'site')

cv_sum <- cv %>% 
  group_by(site) %>% 
  summarise(mean_ann_GPP_C = mean(GPP_ann_sum, na.rm = TRUE), 
            sd_ann_GPP_C = sd(GPP_ann_sum, na.rm = TRUE),
            median_daily_CV = median(GPP_daily_cv, na.rm = TRUE),
            min_daily_CV = min(GPP_daily_cv), 
            max_daily_CV = max(GPP_daily_cv),
            GPP_ann_cv = first(GPP_ann_cv),
            GPP_range = max(GPP_ann_sum) - min(GPP_ann_sum),
            CV_ratio = median_daily_CV/GPP_ann_cv)

ggplot(cv_sum,
       aes(x = mean_ann_GPP_C,
           y = GPP_ann_cv))+
  geom_point()


ggplot(cv,
       aes(x = GPP_ann_cv,
           y = GPP_daily_cv,
           color = GPP_ann_sum,
           group = site))+
  geom_point()+
  geom_line()+
  # geom_linerange(aes(ymin = min_daily_CV,
  #                    ymax = max_daily_CV))+
  geom_smooth(method = 'lm')+
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')+
  
  scale_color_gradient(name = metab_units_area,
                       low = low_p,
                       high = high_p)+
  lims(x = c(5,40),
       y = c(20,160))+
  labs(x = expression(paste(CV[GPP-Inter], ' (%)')),
       y = expression(paste(CV[GPP-Intra], ' (%)')))

cv_lm <- lm(data = cv,
            GPP_daily_cv ~ GPP_ann_cv)
summary(cv_lm)

cv_lm_site <- lm(data = cv,
                 GPP_daily_cv ~ GPP_ann_cv + site)
summary(cv_lm_site)

cv_lmer <- lme4::lmer(data = cv,
                      GPP_daily_cv ~ GPP_ann_cv + (1|site))
summary(cv_lmer)
car::Anova(cv_lmer)


ggplot(cv_sum,
       aes(x = GPP_range,
           y = CV_ratio))+
  geom_point()+
  labs(y = 'CV-intra/CV-inter')





# Figure 4
annual_cv_trends <- metab_all_annual %>% 
  # dplyr::select(site, year, GPP_ann_C) %>% 
  dplyr::group_by(site) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk_test = purrr::map(data, ~trend::mk.test(x = .$GPP_daily_cv))) %>% 
  dplyr::mutate(mk_p = purrr::map_dbl(mk_test, ~.$p.value),
                mk_s = purrr::map_dbl(mk_test, ~.$estimates['S']),
                mk_sig = ifelse(mk_p <= 0.05, 'significant', 'non-significant'),
                mk_slope = ifelse(mk_s > 0, 'increasing', 'decreasing')) 


annual_cv_sigs <- annual_cv_trends %>% 
  dplyr::ungroup() %>% 
  dplyr::select(site, 
                mk_p, mk_s, mk_sig, mk_slope)


n_sig_cv <- annual_cv_sigs %>% 
  dplyr::filter(mk_p <= 0.05) %>% 
  dplyr::group_by(mk_slope) %>% 
  summarise(n = n()) %>% 
  data.frame()


river_trends_cv <- metab_all_annual %>% 
  dplyr::left_join(annual_cv_sigs) 



fig_4_b <- river_trends %>% 
  ggplot(aes(x = year,
             y = GPP_daily_cv,
             group = site,
             color = mk_s))+
  geom_line()+
  gghighlight::gghighlight(site %in% lt_changers,
                           use_direct_label = FALSE,
                           keep_scales = TRUE,
                           unhighlighted_params = list(alpha("grey10", 0.4),
                                                       linewidth = 0.2))+
  scale_color_gradient(name = element_blank(),
                       low = low_p, high = high_p)+
  labs(y = expression(paste(CV[GPP-Intra])))+
  theme(legend.position = 'none',
        axis.title.x = element_blank())


# flow variance stats

cvQ_all <- lm(data = metab_all_annual, 
              log10(GPP_ann_sum) ~ Q_daily_cv)
summary(cvQ_all)

cvQ_by_site <- lm(data = metab_all_annual,
                  GPP_ann_sum ~ Q_daily_cv + site)
summary(cvQ_by_site)

cvQ_by_site_ranef <- lme4::lmer(data = metab_all_annual,
                                GPP_ann_sum ~ Q_daily_cv + (1|site))
summary(cvQ_by_site_ranef)
car::Anova(cvQ_by_site_ranef)


cv_Q_stats <- metab_all_annual %>% 
  group_by(site) %>% 
  summarise(out = list(tidy(lm(log10(GPP_ann_sum) ~ Q_daily_cv)))) %>% 
  unnest(out) %>% 
  filter(term == 'Q_daily_cv')


metab_all_cvQ_stats <- left_join(metab_all_annual, cv_Q_stats, 'site') %>% 
  mutate(is_sig = ifelse(p.value <= 0.05,'significant', 'non-significant'), 
         direction = ifelse(estimate >= 0, 'positive', 'negative'))

plot_driver_cvQ <- metab_all_cvQ_stats %>% 
  ggplot(.,
         aes(x = Q_daily_cv,
             y = GPP_ann_sum))+
  geom_point(#aes(color = estimate)
             )+
  geom_smooth(aes(group = site),
              method = 'lm', 
              se = FALSE, color = 'grey40')+
  # geom_abline(slope = cvQ_all$coefficients[2],
  #             intercept = cvQ_all$coefficients[1],
  #             linetype = 'dashed',
  #             color = 'black')+
  gghighlight::gghighlight(site %in% lt_changers,#is_sig == 'significant',
                           keep_scales = TRUE,
                           unhighlighted_params = list(alpha("grey10", 0.4),
                                                       linewidth = 0.2))+
  # facet_grid(. ~ is_sig)+
  scale_y_log10()+
  scale_color_gradient(low = low_p,
                       high = high_p)+
  labs(x = expression(paste(CV[Q-Intra], ' (%)')),
       y = metab_units_area)+
  lims(x = c(0,100))


scale_factor <- max(metab_all_annual$GPP_ann_sum) / max(metab_all_annual$Q_daily_cv)
metab_all_annual %>% 
  ggplot(.,
         aes(x = year,
             group = site))+
  geom_line(aes(y = GPP_ann_sum), color = 'green')+
  geom_line(aes(y = Q_daily_cv * scale_factor), color = 'blue')+
  scale_y_continuous(name = 'GPP',
                     sec.axis = sec_axis(~./scale_factor, name = 'CV_Q'))+
  facet_wrap(site ~ .,
             scales = 'free')



# temperature effects ----
temp_all <- lm(data = metab_all_annual %>% 
                 filter(temp_daily_mean > 5),
               log10(GPP_ann_sum) ~ temp_daily_mean)
summary(temp_all)

temp_by_site <- lm(data = metab_all_annual,
                   GPP_ann_sum ~ temp_daily_mean + site)
summary(temp_by_site)

temp_by_site_ranef <- lme4::lmer(data = metab_all_annual,
                                 GPP_ann_sum ~ temp_daily_mean + (1|site))
summary(temp_by_site_ranef)
car::Anova(temp_by_site_ranef)



temp_stats <- metab_all_annual %>% 
  group_by(site) %>% 
  summarise(out = list(tidy(lm(log10(GPP_ann_sum) ~ temp_daily_mean)))) %>% 
  unnest(out) %>% 
  filter(term == 'temp_daily_mean')


metab_all_temp_stats <- left_join(metab_all_annual, temp_stats, 'site') %>% 
  mutate(is_sig = ifelse(p.value <= 0.05,
                         'significant', 
                         'non-significant'), 
         direction = ifelse(estimate >= 0, 
                            'positive', 
                            'negative'))

plot_driver_temp <- metab_all_temp_stats %>% 
  filter(temp_daily_mean > 5) %>% 
  ggplot(.,
         aes(x = temp_daily_mean,
             y = GPP_ann_sum))+
  geom_point(aes(color = estimate))+
  geom_smooth(aes(group = site),
              method = 'lm', 
              se = FALSE, color = 'grey40')+
  # geom_abline(slope = temp_all$coefficients[2],
  #             intercept = temp_all$coefficients[1],
  #             linetype = 'dashed',
  #             color = 'black')+
  gghighlight::gghighlight(is_sig == 'significant',
                           keep_scales = TRUE,
                           unhighlighted_params = list(alpha("grey10", 0.4),
                                                       linewidth = 0.2))+
  # facet_grid(. ~ is_sig)+
  scale_y_log10()+
  scale_color_gradient(low = low_p,
                       high = high_p)+
  labs(x = 'Mean Daily Temperature (°C)',
       y = metab_units_area)


ggpubr::ggarrange(fig_4,fig_4_b,
                  plot_driver_temp,
                  plot_driver_cvQ,
                  ncol = 2, nrow = 2,
                  align = 'hv')



site_mean_ann_gpp %>% 
  ggplot(.,aes(x = cv_mean_temp, 
               y = median_ann_GPP , 
               color = site))+
  geom_point()+
  geom_errorbar(aes(ymin = median_ann_GPP - sd_ann_GPP,
                    ymax = median_ann_GPP + sd_ann_GPP))
# geom_errorbarh(aes(xmin = mean_ann_temp - mean_sd_temp,
#                    xmax = mean_ann_temp + mean_sd_temp))


site_mean_ann_gpp  %>% 
  ggplot(.,aes(x = cv_median_discharge, 
               y = median_ann_GPP , 
               color = site))+
  geom_point()+
  geom_errorbar(aes(ymin = median_ann_GPP - sd_ann_GPP,
                    ymax = median_ann_GPP + sd_ann_GPP))+
  # geom_errorbarh(aes(xmin = median_ann_discharge - median_sd_discharge,
  #                    xmax = median_ann_discharge + median_sd_discharge))+
  scale_x_log10()
