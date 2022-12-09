get_seasonality <- function (timeseries, var, standardize) {
  
  if(!'Year' %in% colnames(timeseries)) {
    timeseries$Year = lubridate::year(timeseries$date) 
  }
  
  if(!'DOY' %in% colnames(timeseries)) {
    timeseries$DOY = lubridate::yday(timeseries$date) 
  }
  
  decimal_year <- as.numeric(timeseries[, "Year"]) + (timeseries[, "DOY"]/365.25)
  
  ifelse(standardize == "yes", 
         standard <- scale(timeseries[,var], center = TRUE, scale = TRUE), 
         standard <- timeseries[,var])
  
  x_mat <- cbind(1, 
                 sin(2 * pi * decimal_year), 
                 cos(2 * pi * decimal_year))
  
  seasonfit <- .lm.fit(x_mat, standard)
  b1 <- as.vector(coef(seasonfit)[2])
  b2 <- as.vector(coef(seasonfit)[3])
  amplitude <- round(sqrt((b2^2) + (b1^2)), digits = 2)
  
  MaxDay <- function(b1, b2) {
    version1 <- 365.25 * ((pi/2) - atan(b2/b1))/(2 * pi)
    version2 <- 365.25 * ((pi/2) - pi - atan(b2/b1))/(2 * 
                                                        pi)
    MaxDay <- if (b1 > 0) 
      version1
    else 365.25 + version2
    MaxDay <- if (b1 == 0 & b2 > 0) 
      365.25
    else MaxDay
    MaxDay <- if (b1 == 0 & b2 < 0) 
      365.25/2
    else MaxDay
    MaxDay <- if (b1 == 0 & b2 == 0) 
      NA
    else MaxDay
    return(MaxDay)
  }
  phase <- MaxDay(b1, b2)
  get_seasonalityv <- cbind(amplitude, phase)
  return(get_seasonalityv)
}
