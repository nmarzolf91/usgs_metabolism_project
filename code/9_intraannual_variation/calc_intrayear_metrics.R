calc_intrayear_metrics <- function(data = NULL) {
  
  river_metab = river_metab_filled
  
  source('code/8_intraannual_variation/get_seasonality.R')
  source('code/8_intraannual_variation/mag7_fun.R')
  source('code/8_intraannual_variation/ar_fun.R')
  
  sites <- unique(river_metab$site)
  years <- lubridate::year(river_metab$date) %>% 
    unique()
  
  site_years <- data.frame(sites = rep(sites, each = length(years)),
                           years = rep(years, times = length(sites)))
  
  metrics <- matrix('', ncol = 9, nrow = nrow(site_years))
  
  colnames(metrics) <- c('site', 'year', 'mean', 'cv', 'skewness', 'kurtosis', 'amplitude', 'phase', 'ar1')
  
  for(i in 1:nrow(metrics)) {
    site_use <- site_years[i,1]
    year_use <- site_years[i,2]
    
    metrics[i, 'site'] = site_use
    metrics[i, 'year'] = year_use
    
    df <- river_metab %>% 
      filter(site == site_use,
             lubridate::year(date) %in% year_use) %>% 
      mutate(GPP_filled_C = GPP_filled*0.75) %>% 
      select(site, date, GPP_filled_C) %>% 
      data.frame()
    
    if(nrow(df) == 0) {
      metrics[i, 3:9] <- NA
      next}
    
    l_mom <- lmomco::lmom.ub(df$GPP_filled)
    
    seasonality <- get_seasonality(timeseries = df, 
                                   var = 'GPP_filled_C', 
                                   standardize = 'yes')
    
    ar1 <- ar_fun(df, 'GPP_filled_C')
    
    
    metrics[i, 'mean'] = round(l_mom$L1, 4)
    metrics[i, 'cv'] = round(l_mom$LCV, 4)*100
    metrics[i, 'skewness'] = round(l_mom$TAU3, 4)
    metrics[i, 'kurtosis'] = round(l_mom$TAU4, 4)
    metrics[i, 'amplitude'] = round(seasonality[,'amplitude'], 4)
    metrics[i, 'phase'] = round(seasonality[,'phase'], 4)
    metrics[i, 'ar1'] = ar1
    
  } # end for loop
  
  return(data.frame(metrics))
} # end function
