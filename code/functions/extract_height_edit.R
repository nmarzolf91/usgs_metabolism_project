#' Get LiDAR derived tree heights based on Latitude and Longitude
#' @description Extract canopy height from the LiDAR derived map of
#' tree heights produced by Simard et al. 2011 (asdf)
#'
#' @param Site The site ID
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param site_crs The coordinate reference system of the points, preferably designated
#' as an EPSG code. For example, the most common geographic system is WGS84 and its EPSG 
#' code is 4326. 
#' 
#' @return Site ID, Latitude, Longitude, and tree height
#' @export

#===============================================================================
#Extracting canopy height from Simard et al. (2011) for our site
#Map downloaded from http://webmap.ornl.gov/wcsdown/dataset.jsp?ds_id=10023
#Simard, M., N. Pinto, J. B. Fisher, and A. Baccini (2011), Mapping forest canopy
#height globally with spaceborne lidar, J. Geophys. Res., 116, G04021,
#doi:10.1029/2011JG001708.
#===============================================================================
extract_height <- function(Site_ID, Lat, Lon, site_crs){
  #Import the Simard et al. (2011) dataset 
  simard2011 <- raster::raster(paste0(.libPaths()[1], "/StreamLightUtils/data/simard2011.tif"))
  
  # #Import the Simard et al. (2011) dataset if it is not already loaded
  #   if(!exists("simard2011")){data("simard2011", package = "StreamLightUtils")}
  
  #Defining the min and max values (by default these are not associated with the raster)
  map_ranges <- raster::setMinMax(simard2011)
  
  #Create a simple features object from the site location
  site_location <- data.frame(Lat, Lon)
  
  #Check if the data is in WGS84, if not reproject to WGS84
  if(site_crs == 4326){
    xy_sf <- sf::st_as_sf(site_location, coords = c('Lon', 'Lat'), crs = site_crs)
  } else{
    xy_native <- sf::st_as_sf(site_location, coords = c('Lon', 'Lat'), crs = site_crs)
    xy_sf <- sf::st_transform(xy_native, crs = 4326)
  } #End if else statement    
  
  #Extracting the canopy height at our site
  suppressWarnings(TH <- raster::extract(map_ranges, xy_sf))
  
  #Bind together the final information
  bound <- setNames(data.frame(Site_ID, Lat, Lon, TH), c("Site_ID", "Lat", "Lon", "TH"))
  
  return(bound)
} #End extract_height function
