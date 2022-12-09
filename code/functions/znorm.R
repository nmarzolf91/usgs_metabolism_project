znorm <- function(timeseries, var) {
  ts.mean <- mean(timeseries[, var], na.rm = TRUE)
  ts.dev <- sd(timeseries[, var], na.rm = TRUE)
  (timeseries[, var] - ts.mean)/ts.dev
}
