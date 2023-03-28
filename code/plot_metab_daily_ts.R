# make the metabolism plots for each site-year

estimates <- readr::read_csv('data/output_data/usgs_metab_fill_norm.csv')


pdf(file = 'usgs_metab_scroll.pdf')
for(i in 1:length(unique(estimates$site))){
  site <- estimates$site[i]
  
  years <- unique(lubridate::year(estimates$date))
  
  for(j in 1:length(years)) {
    
    year <- years[j]
    
    dat <- estimates %>% 
      filter(site == !!site,
             lubridate::year(date) %in% year) %>% 
      mutate(GPP_C = GPP_filled/1.25,
             ER_C = ER_filled/1.25)
    
    metab <- ggplot(dat,
                    aes(x = date))+
      geom_area(aes(y = shortwave), fill = 'grey')+
      geom_point(aes(y = GPP_C*10), color = 'darkgreen')+
      geom_point(aes(y = ER_C*10), color = 'goldenrod4')+
      scale_y_continuous(name = "Shortwave Radiation",
                         sec.axis = sec_axis(~./10, 
                                             name="Metabolism (g C m-2 d-1)"))+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())+
      ggtitle(paste(site, year, sep = '-'))
    
    discharge <- ggplot(dat,
                        aes(x = date,
                            y = discharge))+
      geom_line()+
      ylab('Mean Daily Q (m3/s)')+
      theme(axis.title.x = element_blank())
    
    final <- cowplot::plot_grid(metab, discharge,
                                ncol = 1, nrow = 2,
                                align = 'v',
                                rel_heights = c(2,1))

    print(final)
  }
}
dev.off()
