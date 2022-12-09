calc_intrayear_metrics <- function(data = river_metab) {
  sites <- unique(river_metab$site)
  years <- lubridate::year(river_metab$date) %>% 
    unique()
  
  site_years <- data.frame(sites = rep(sites, each = length(years)),
                           years = rep(years, times = length(sites)))
  
  metrics <- matrix('', ncol = 9, nrow = nrow(site_years))
  
  colnames(metrics) <- c('site', 'year', 'L1', 'tau2', 'tau3', 'tau4', 'amplitude', 'phase', 'ar1')
  
  for(i in 1:nrow(metrics)) {
    site_use <- site_years[i,1]
    year_use <- site_years[i,2]
    
    metrics[i, 'site'] = site_use
    metrics[i, 'year'] = year_use
    
    df <- river_metab %>% 
      filter(site == site_use,
             lubridate::year(date) %in% year_use) %>% 
      select(site, date, GPP_filled) %>% 
      data.frame()
    
    if(nrow(df) == 0) {
      metrics[i, 3:9] <- NA
      next}
    
    l_mom <- lmomco::lmom.ub(df$GPP_filled)
    
    seasonality <- get_seasonality(timeseries = df, var = 'GPP_filled', standardize = 'yes')
    
    ar1 <- ar_fun(df, 'GPP_filled')
    
    
    metrics[i, 'L1'] = round(l_mom$L1, 2)
    metrics[i, 'tau2'] = round(l_mom$LCV, 2)
    metrics[i, 'tau3'] = round(l_mom$TAU3, 2)
    metrics[i, 'tau4'] = round(l_mom$TAU4, 2)
    metrics[i, 'amplitude'] = round(seasonality[,'amplitude'], 2)
    metrics[i, 'phase'] = round(seasonality[,'phase'], 2)
    metrics[i, 'ar1'] = ar1
    
  } # end for loop
  
  return(data.frame(metrics))
} # end function
