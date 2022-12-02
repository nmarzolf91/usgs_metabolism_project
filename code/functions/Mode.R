Mode <- function(x, na.rm = TRUE){
  if(na.rm){
    x <- na.omit(x)
  }
  ux <- unique(x)
  mode_out <- ux[which.max(tabulate(match(x, ux)))]
  return(mode_out)
}
