ar_fun <- function (timeseries, var) {
  
  if(!'Year' %in% colnames(timeseries)) {
    timeseries$Year = lubridate::year(timeseries$date) 
  }
  
  if(!'DOY' %in% colnames(timeseries)) {
    timeseries$DOY = lubridate::yday(timeseries$date) 
  }
  
  timeseries$Month <- as.numeric(format(as.Date(timeseries[,  "DOY"] - 1, 
                                                origin = paste(timeseries[, "Year"], "-01-01", 
                                                               sep = "")), 
                                        format = "%m"))
  
  monmeans <- aggregate(timeseries[, var], 
                        list(timeseries[,"Month"]), FUN = mean, na.rm = TRUE)
  
  mon_merge <- merge(timeseries, monmeans, by.x = "Month", 
                     by.y = "Group.1")
  
  mon_merge$deseason <- mon_merge[, var] - mon_merge[, "x"]
  
  ordered <- mon_merge[order(mon_merge$Year, mon_merge$DOY), ]
  stand <- scale(ordered[, "deseason"], center = TRUE, scale = TRUE)
  ar_mod <- ar(stand, aic = FALSE, order.max = 1, method = "yule-walker")
  ar_cor <- round(ar_mod$ar, digits = 3)
  return(ar_cor)
}
