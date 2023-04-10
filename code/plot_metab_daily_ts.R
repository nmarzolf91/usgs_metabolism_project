# make the metabolism plots for each site-year

estimates <- readr::read_csv('data/output_data/usgs_metab_fill_norm.csv')

river_light <- readr::read_csv('data/output_data/daily_light_metrics.csv')


river_metab_wLight <- estimates %>% 
  dplyr::left_join(.,
                   river_light, 
                   by = c('site', 'date'))

pdf(file = 'usgs_metab_scroll.pdf')
for(i in 1:length(unique(river_metab_wLight$site))){
  site <- river_metab_wLight$site[i]
  
  years <- unique(lubridate::year(river_metab_wLight$date))
  
  for(j in 1:length(years)) {
    
    year <- years[j]
    
    dat <- river_metab_wLight %>% 
      filter(site == !!site,
             lubridate::year(date) %in% year) %>% 
      mutate(GPP_C = GPP_filled/1.25,
             ER_C = ER_filled/1.25)
    
    metab <- ggplot(dat,
                    aes(x = date))+
      geom_area(aes(y = daily_PAR), fill = 'grey')+
      geom_area(aes(y = GPP_C/1), fill = 'darkgreen')+
      geom_area(aes(y = ER_C/1), fill = 'goldenrod4')+
      scale_y_continuous(name = expression(paste("Stream Surface PAR (mol  ", m^-2, " ",d^-1,')')),
                         sec.axis = sec_axis(~.*1, 
                                             name = expression(paste("Metabolism (g C ",m^-2," " ,d^-1,")"))))+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())+
      ggtitle(paste(site, year, sep = ' : '))
    
    discharge <- ggplot(dat,
                        aes(x = date,
                            y = discharge))+
      geom_area()+
      ylab(expression(paste('Mean Daily Discharge (  ',m^3,'/s)')))+
      theme(axis.title.x = element_blank())
    
    final <- cowplot::plot_grid(metab, discharge,
                                ncol = 1, nrow = 2,
                                align = 'v',
                                rel_heights = c(2,1))

    print(final)
  }
}
dev.off()
