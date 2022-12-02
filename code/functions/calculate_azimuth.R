library(nhdplusTools)
library(geosphere)


points <- site_locs %>% 
  dplyr::select(Lon, Lat) %>% 
  as.matrix() %>% 
  st_multipoint() %>% 
  st_sfc(crs = 4326) %>% 
  st_cast('POINT')


azimuth_df <- data.frame(Site_ID = character(),
                         azimuth = numeric())
for(k in 1:nrow(site_locs)){
  site <- site_locs[k,]
  
  point <- site %>% 
    dplyr::select(Lon, Lat) %>% 
    as.matrix() %>% 
    st_multipoint() %>% 
    st_sfc(crs = 4326) %>% 
    st_cast('POINT')
  
  comid <- discover_nhdplus_id(point)
  
  flowline <- navigate_nldi(list(featureSource = 'comid',
                                 featureID = comid),
                            mode = 'upstreamTributaries',
                            distance_km = 2)
  
  subset <- subset_nhdplus(comids = as.integer(flowline$UT_flowlines$nhdplus_comid[1]),
                           nhdplus_data = 'download',
                           flowline_only = TRUE,
                           return_data = TRUE,
                           overwrite = TRUE)
  
  flow_coords <- st_coordinates(subset$NHDFlowline_Network)
  
  azimuth <- bearing(p1 = flow_coords[1,1:2],
                     p2 = flow_coords[nrow(flow_coords), 1:2])
  
  if(azimuth < 0){azimuth = azimuth + 180}
  
  azimuth_df <- azimuth_df %>% 
    add_row(Site_ID = pull(site, Site_ID),
            azimuth = azimuth)
}
azimuth_df

write_csv(azimuth_df,
          'data/usgs_streamlight/usgs_azimuths.csv')

