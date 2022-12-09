mag7_fun <- function (timeseries, var, standardize) 
{
  l_mom <- lmomco::lmom.ub(timeseries[, var])
  lam1 <- round(l_mom$L1, digits = 2)
  tau2 <- round(l_mom$LCV, digits = 2)
  tau3 <- round(l_mom$TAU3, digits = 2)
  tau4 <- round(l_mom$TAU4, digits = 2)
  seasonality <- get_seasonality(timeseries, var, standardize)
  ar1 <- ar_fun(timeseries, var)
  mag7 <- data.frame(lam1, tau2, tau3, tau4, seasonality, 
                     ar1)
  return(mag7)
}
