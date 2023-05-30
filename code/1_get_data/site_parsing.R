sites <- readr::read_csv('data/site_info/nwis_site_info.csv')

names(sites)

table_s1 <- sites %>% 
  dplyr::select("Site Number" = site_no,
                Name = station_nm,
                Latitude = dec_lat_va,
                Longitude = dec_long_va,
                "Drainage Area (km2)" = drain_area_va)

readr::write_csv(table_s1,
                 'data/data_citation/1_site_info.csv')
