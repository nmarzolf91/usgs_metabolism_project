fillMiss3 <- function (dataset, var, block = 30, pmiss = 40, model = "trend", 
                       smooth = TRUE, plot = TRUE, ...) 
{
  pck <- is.na(dataset[, var])
  if (all(pck == TRUE) == FALSE) {
    if (sum(pck) > 0) {
      percent <- round((sum(pck)/length(dataset[, var])) * 
                         100, digits = 2)
      rles <- rle(is.na(dataset[, var]))
      max.mis <- max(rles$lengths[rles$values])
    }
    else {
      percent <- 0
      max.mis <- 0
    }
    if (percent >= pmiss | max.mis >= block) {
    }
    if (percent < pmiss & max.mis < block) {
      my.series <- window(dataset[, var])
      if (is.na(my.series)[1]) {
        my.series[1] <- zoo::na.locf(my.series, option = "nocb", 
                                     na.remaining = "rev")[1]
      }
      struct_try <- try(StructTS(my.series, type = model), 
                        silent = TRUE)
      if (class(struct_try)[1] != "try-error") {
        my.struct <- StructTS(my.series, type = model)
      }
      if (class(struct_try)[1] == "try-error") {
        message(paste0("type = level was used for ", 
                       var))
        my.struct <- StructTS(my.series, type = "level")
      }
      if (smooth) 
        fit <- tsSmooth(my.struct)
      else fit <- fitted(my.struct)
      data_min <- min(dataset[, var], na.rm = TRUE)
      data_max <- max(dataset[, var], na.rm = TRUE)
      fit[fit[, 1] < data_min, 1] <- data_min
      fit[fit[, 1] > data_max, 1] <- data_max
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
      dataset$filled <- dataset[, var]
      dataset$filled[pck] <- fit[pck, 1]
    }
    return(dataset[, "filled"])
  }
  if (all(pck == TRUE) == TRUE) {
    message("Input variable contained only NA values so only NA values were returned")
    return(rep(NA, nrow(dataset)))
  }
}