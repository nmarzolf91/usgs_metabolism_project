fillMiss3 <- function (dataset, var, block = 30, pmiss = 40, model = "trend", 
                       smooth = TRUE, plot = TRUE, ...) 
{
  
  # modified function from Bernhardt et al. 2022, which modifies code from USGS-R/waterData/fillMiss.R
  ## https://github.com/USGS-R/waterData/blob/main/fillMiss.R
  
  # convert the dataset to a data frame; the downstream functions struggle with tidyverse derived objects so we'll convert at the start
  dataset <- data.frame(dataset)
  
  # create a T/F for if the dataset contains NAs
  pck <- is.na(dataset[, var])
  
  # if there are missing data (or this statement is true)
  if (all(pck == TRUE) == FALSE) {
    
    # and there are more than 0 missing values
    if (sum(pck) > 0) {
      # calculate the percentage of missing data in the time-series
      percent <- round((sum(pck)/length(dataset[, var])) * 
                         100, digits = 2)
      # and figure out where those missing sequences are in the dataset
      rles <- rle(is.na(dataset[, var]))
      # determine what the longest string of missing data are
      max.mis <- max(rles$lengths[rles$values])
      
      # if there are no missing data, by default these values are 0
    } else {
      percent <- 0
      max.mis <- 0
    }
    
    # if (percent >= pmiss | max.mis >= block) {
    # }
    
    # if the percent missing and less than the missing threshold and the longest block of missing is less than the block threshold
    if (percent < pmiss & max.mis < block) {
      
      # create a time series object with NAs for missing data
      my.series <- window(ts(dataset[, var]))
      
      # if the first entry in the time series is NA, replace it with a LOCF gap fill
      if (is.na(my.series)[1]) {
        my.series[1] <- zoo::na.locf(my.series, option = "nocb", 
                                     na.remaining = "rev")[1]
      }
      
      # try to fit a structural time series model to the data
      struct_try <- try(StructTS(my.series, type = model), 
                        silent = TRUE)
      
      # if the try statement worked (or there is no try error), fit the model
      if (class(struct_try)[1] != "try-error") {
        my.struct <- StructTS(my.series, type = model)
      }
      
      # else use the level arguement in the time series model fit
      if (class(struct_try)[1] == "try-error") {
        message(paste0("type = level was used for ", 
                       var))
        my.struct <- StructTS(my.series, type = "level")
      }
      
      # if we want to smooth, apply the smoother function
      if (smooth){ 
        fit <- tsSmooth(my.struct)
      } else {
        fit <- fitted(my.struct)
      }
      
      # extract min and max from the dataset
      data_min <- min(dataset[, var], na.rm = TRUE)
      data_max <- max(dataset[, var], na.rm = TRUE)
      
      # change values that are less than the min to the min and greater than the max to the max
      fit[fit[, 1] < data_min, 1] <- data_min
      fit[fit[, 1] > data_max, 1] <- data_max
      
      # plot the fitted data over the raw data if we want to 
      if (plot == TRUE) {
        plot(my.series, typ = "l", lwd = 4, xlab = "Observation", 
             ylab = "Observed and estimated times series", 
             ...)
        lines(fit[, 1], col = "green")
        leg.txt <- c("Observed values", "New time series")
        legend("topleft", leg.txt, col = c("black", 
                                           "green"), lwd = c(4, 1), bty = "n", ncol = 2, 
               cex = 0.8)
      }
      
      # append the filled data to the dataset where gap filling was applied
      dataset$filled <- dataset[, var]
      dataset$filled[pck] <- fit[pck, 1]
    }
    
    # and return the data
    return(dataset[, "filled"])
  }
  
  # if there was nothing but missing data, return this error message
  if (all(pck == TRUE) == TRUE) {
    message("Input variable contained only NA values so only NA values were returned")
    return(rep(NA, nrow(dataset)))
  }
}