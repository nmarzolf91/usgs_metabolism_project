calc_DO_sat <- calc_DO_at_sat <- function(temp.water, pressure.air, salinity.water = u(0,'PSU'), model='garcia-benson', ...){
  
  if(as.character(sys.call()[[1]]) == 'calc_DO_at_sat') {
    .Deprecated('calc_DO_sat')
  }
  
  with.units <- any(sapply(list(temp.water, pressure.air), is.unitted)) || (if(!missing(salinity.water)) is.unitted(salinity.water) else FALSE)
  
  if (with.units){
    # if any units are set, they all must be set and must be correct
    verify_units(temp.water, "degC")
    verify_units(pressure.air, "mb")
    verify_units(salinity.water, "PSU")
  }
  
  # units are stripped regardless
  temp.water <- v(temp.water)
  pressure.air <- v(pressure.air)
  salinity.water <- v(salinity.water)
  
  o2.at.sat <- LakeMetabolizer::o2.at.sat.base(temp = temp.water, baro = pressure.air, salinity = salinity.water, model = model, ...)
  
  if (with.units) {
    return(u(o2.at.sat, 'mgO2 L^-1'))
  } else {
    return(o2.at.sat)
  }
  
}
