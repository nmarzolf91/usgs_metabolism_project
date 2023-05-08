## Header ----
## Script name: metab_streamlight.R
##
## Purpose of script: run streamlight package on USGS sites
##
## Author: Nick Marzolf
## Date Created: 2022-10-04
## Date Modified: 
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
{library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(glue)}
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/Nick Marzolf/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


library(devtools)
# devtools::install_github("psavoy/StreamLightUtils")
# devtools::install_github("psavoy/StreamLight")

library(StreamLightUtils)
library(StreamLight)

# assign the sub-directory for streamlight related things
dir <- 'data/usgs_streamlight/'

# create the dataframe with siteID, lat, long, and start date
sites <- read_csv('data/site_info/nwis_site_info.csv') %>% 
  dplyr::select(site_no, 
                Lon = dec_long_va, 
                Lat = dec_lat_va) %>% 
  mutate(Site_ID = paste0('nwis_',site_no),
         startDate = as.character('2007-01-01')) %>% 
  dplyr::select(Site_ID, Lat, Lon, startDate) %>% 
  as.data.frame()

# pull NLDAS data from NASA using StreamLightUtils function
NLDAS_DL_bulk(save_dir = glue(dir, '/NLDAS'),
              site_locs = sites,
              startDate = sites$startDate)

# completed on 10/5/2022

NLDAS_list <- stringr::str_sub(list.files(glue(dir, '/NLDAS')),
                               1, -11)
# process the outputs
# requires the full file-path directory
NLDAS_processed <- StreamLightUtils::NLDAS_proc(read_dir = "C:/Users/Nick Marzolf/Desktop/Research/Projects/USGS_metabolism/usgs_metabolism_project/data/usgs_streamlight/NLDAS",
                                                Site_IDs = NLDAS_list)
saveRDS(NLDAS_processed,
        'data/usgs_streamlight/NLDAS_proc.rds')


# create dataframe that is the input to the AppEEARS portal
AppEEARS_sites <- sites %>% 
  dplyr::select(Site_ID, Lat, Lon)

write_csv(AppEEARS_sites,
          glue(dir, '/usgs_appeears_sites.csv'))


# Unzips, processes, and saves the AppEEARS data
Mod_unpack <- AppEEARS_unpack_QC(zip_file = '/usgs_metabolism_sites.zip',
                                 zip_dir = dir,
                                 request_sites = sites$Site_ID)
saveRDS(Mod_unpack,
        paste0(dir, '/Mod_unpack.rds'))

Mod_processed <- AppEEARS_proc(unpacked_LAI = Mod_unpack,
                               fit_method = 'Gu',
                               plot = TRUE)

saveRDS(Mod_processed,
        paste0(dir, '/Mod_processed.rds'))


# use the streamlight function
## prepare the driver file

site_locs <- sites %>% 
  dplyr::select(-startDate) %>% 
  mutate(epsg_crs = 4326)

make_driver(site_locs = site_locs,
            NLDAS_processed = NLDAS_processed,
            MOD_processed = Mod_processed,
            write_output = TRUE, 
            save_dir = 'data/usgs_streamlight/driver')


# prepare the parameter file
source('code/functions/extract_height_edit.R')
widths <- read_csv('data/usgs_sites_coefs.csv') %>% 
  rename(Site_ID = site_name) %>% 
  dplyr::select(Site_ID)

azimuths <- read_csv('data/usgs_streamlight/usgs_azimuths.csv')




width_df <- data.frame(Site_ID = character(),
                       width = numeric(),
                       depth = numeric())
for(j in 1:nrow(widths)) {
  Site_ID <- widths[j,1] %>% pull()
  a <- widths[j, 'dvqcoefs.a'] %>% pull()
  b <- widths[j, 'dvqcoefs.b'] %>% pull()
  c <- widths[j, 'dvqcoefs.c'] %>% pull()
  d <- widths[j, 'dvqcoefs.d'] %>% pull()
  
  Q_position <- grep(Site_ID, 
                     list.files('data/usgs_sm_ready_all/'))
  
  if(is_empty(Q_position))
    next
  
  file <- try(read_csv(list.files('data/usgs_sm_ready_all/', 
                                  full.names = TRUE)[Q_position]))
  if(inherits(file, 'try-error'))
    next
  
  median_discharge <- file %>% 
    dplyr::select(discharge) %>% 
    summarise(median_discharge = median(discharge, na.rm = TRUE)) %>% 
    pull()
  
  median_width = a*median_discharge^b
  median_depth = c*median_discharge^d
  
  width_df <- width_df %>% 
    add_row(
      Site_ID = Site_ID,
      width = median_width,
      depth = median_depth
    )
}



# calculate tree height
TH <- list()
for(i in 1:nrow(site_locs)){
  Site_ID <- site_locs[i,1]
  Lat <- site_locs[i,2]
  Lon <- site_locs[i,3]
  site_crs <- 4326
  
  TH[[i]] <- extract_height(Site_ID, Lat, Lon, site_crs)
}
TH <- bind_rows(TH) %>% 
  dplyr::select(Site_ID, TH)


# compile parameter file
# usgs_params <- site_locs %>% 
#   left_join(TH, 'Site_ID') %>% 
#   left_join(width_df, 'Site_ID') %>% 
#   left_join(azimuths, 'Site_ID') %>% 
#   rename(bottom_width = width,
#          WL = depth) %>% 
#   mutate(BH = 0.1,
#          BS = 100,
#          overhang = TH/10,
#          overhang_height = 0.75*TH,
#          x_LAD = 1)

# write_csv(usgs_params,
#           'data/usgs_streamlight/usgs_params.csv')

usgs_params <- readr::read_csv('data/usgs_streamlight/usgs_params.csv')



for(m in 1:length(NLDAS_list)) {
  read_dir <- 'data/usgs_streamlight/driver'
  save_dir <- 'data/usgs_streamlight/predicted'
  
  Site <- NLDAS_list[m]
  
  driver_file <- try(readRDS(paste(read_dir,'/', Site, '_driver.rds', sep = "")))
  
  if(inherits(driver_file, 'try-error'))
    next
  
  site_p <- usgs_params %>% 
    dplyr::filter(Site_ID %in% Site) %>% 
    data.frame()
  
  modeled <- stream_light(
    driver_file,
    Lat = site_p[,'Lat'],
    Lon = site_p[,'Lon'],
    channel_azimuth = site_p[, 'channel_azimuth'],
    bottom_width = site_p[,'bottom_width'],
    BH = site_p[,'BH'],
    BS = site_p[,'BS'],
    WL = site_p[,'WL'],
    TH = site_p[,'TH'],
    overhang = site_p[,'overhang'],
    overhang_height = site_p[,'overhang_height'],
    x_LAD = site_p[,'x_LAD']
  )
  
  saveRDS(modeled, 
          paste(save_dir, '/', Site, '_predicted.rds', sep = ""))
  
}

# summarize annual light data
summarise_streamlight <- function(prediction_dir = 'data/usgs_streamlight/predicted/'){
  
  files <- list.files(prediction_dir, full.names = TRUE)
  
  files_short <- list.files(prediction_dir) %>% 
    stringr::str_split_fixed(., '_',3) %>% 
    data.frame() %>% 
    dplyr::mutate(site = paste(X1,X2,sep = '_')) %>% 
    dplyr::pull(site)
  
  all_preds <- files %>% 
    purrr::set_names(files_short) %>% 
    purrr::map_dfr(.,
                   readRDS,
                   .id = 'site') 
  
  daily_light_metrics <- all_preds %>% 
    dplyr::group_by(site,
                    date = lubridate::date(local_time)) %>% 
    dplyr::summarise(daily_SW_inc = sum(SW_inc, na.rm = TRUE),
                     daily_LAI = mean(LAI, na.rm = TRUE),
                     daily_PAR = (sum(PAR_surface, na.rm = TRUE)*3600)/10^6
    )
  
  readr::write_csv(daily_light_metrics,
                   'data/usgs_streamlight/daily_light_metrics.csv')
  readr::write_csv(daily_light_metrics,
                   'data/output_data/daily_light_metrics.csv')
  print(daily_light_metrics)  
  
  
  annual_light_metrics <- daily_light_metrics %>% 
    dplyr::group_by(site, 
                    year = lubridate::year(date)) %>% 
    dplyr::summarise(annual_SW_inc = sum(daily_SW_inc, na.rm = TRUE),
                     annual_LAI = mean(daily_LAI, na.rm = TRUE),
                     annual_PAR = sum(daily_PAR, na.rm = TRUE))
  
  readr::write_csv(annual_light_metrics,
                   'data/usgs_streamlight/annual_light_metrics.csv')

  readr::write_csv(annual_light_metrics,
                   'data/data_citation/4_river_light.csv')
  
  
  return(annual_light_metrics)
  
}
summarise_streamlight()







