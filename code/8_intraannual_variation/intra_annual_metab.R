river_light <- read_csv('data/output_data/daily_light_metrics.csv')
river_metab <- read_csv('data/output_data/usgs_metab_fill_norm.csv') %>% 
  left_join(.,river_light, by = c('site', 'date'))


river_intra_metrics <- calc_intrayear_metrics()

river_intra_metrics[,3:9] <- sapply(river_intra_metrics[,3:9],
                                    as.numeric)

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
  labs(y = metab_units)+
  # scale_color_viridis_d(direction = -1)+
  facet_grid(sens_sig ~ sens_slope)