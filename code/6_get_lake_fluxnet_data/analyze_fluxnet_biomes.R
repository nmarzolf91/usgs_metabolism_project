## Header ----
## Script name: analyze_fluxnet.R
##
## Purpose of script: Analyze USGS annual metabolism data
##
## Author: Nick Marzolf
## Date Created: 2022-10-18
## Date Modified: 
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
##
## clear the environment if needed
rm(list = ls())


analyze_fluxnet_biomes <- function() {
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  
  
  # read fluxnet data (taken from Bernhardt et al. 2022: https://figshare.com/articles/dataset/CSV_data_for_Bernhardt_et_al_2022_PNAS_/19074275?backTo=/collections/Data_and_code_for_Bernhardt_et_al_2022_PNAS_/5812160)
  fluxnet <- readRDS('data/metabolism_synthesis/output_data/fluxnet_filtered_metabolism.rds')
  fluxnet_names <- names(fluxnet)
  
  # read in biomes for each site; did this by hand on 10/18/2022
  fluxnet_biomes <- read_csv('data/site_info/fluxnet_site_info.csv') %>% 
    select(Site_ID = SITE_ID,
           IBGP_long)
  
  # add site column to each list entry
  for(i in 1:length(fluxnet)){
    fluxnet[[i]]$Site_ID = fluxnet_names[i]
  }
  
  # convert from list to dataframe of daily metabolic data
  fluxnet_full <- Reduce(bind_rows, fluxnet) %>%
    as_tibble() %>%
    filter(Site_ID != 'CZ-BK2') %>% 
    left_join(fluxnet_biomes, 'Site_ID')

  # read in summary site data
  fluxnet_site <- readRDS('data/metabolism_synthesis/output_data/fluxnet_site_info_filtered.rds') %>%
    as_tibble() %>% 
    left_join(fluxnet_biomes, 'Site_ID')
  
  # filter to metabolic data
  fluxnet_site_met <- fluxnet_site %>%
    select(sitecode = Site_ID,
           biome = IBGP_long,
           GPP_site_mean = ann_GPP,
           ER_site_mean = ann_ER) %>%
    filter(!is.na(GPP_site_mean),     # CZ-BK2 has NaN
           !is.na(ER_site_mean)) %>%
    mutate(NEP_site_mean = GPP_site_mean + ER_site_mean) %>%
    arrange(sitecode)
  
  # site summary data and filter to coverage data
  coverage_fluxnet <- readRDS('data/metabolism_synthesis/output_data/fluxnet_site_info_filtered.rds') %>%
    as_tibble() %>%
    select(sitecode = Site_ID,
           nyears, ndays, coverage) %>%
    filter(sitecode != 'CZ-BK2')
  
  # define high coverage sites
  fluxnet_high_cov_sites <- coverage_fluxnet %>%
    filter(coverage >= 0.8) %>%
    pull(sitecode)
  
  # create boolean for high coverage sites
  fnet_high_cov_bool <- fluxnet_site_met$sitecode %in% fluxnet_high_cov_sites
  
  # remove low coverage sites from daily dataset
  fluxnet_day_use <- fluxnet_full %>% 
    filter(Site_ID %in% fluxnet_high_cov_sites)
  
  # remove low coverage sites from annual dataset
  fluxnet_annual_use <- fluxnet_site_met %>% 
    filter(sitecode %in% fluxnet_high_cov_sites)
  
  
  write_csv(fluxnet_annual_use %>% 
              filter(!is.na(biome)),
            'data/fluxnet_annual_wBiome.csv')
} # end function


analyze_fluxnet_biomes()


ggplot(fluxnet_annual_use %>% 
         filter(! is.na(biome)),
       aes(x = biome,
           y = GPP_site_mean))+
  geom_boxplot()+
  labs(x = element_blank(),
       y = expression(paste('GPP (g C ', m^-2,' ', y^-1,')')))+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


fluxnet_GPP_sum <- fluxnet_annual_use %>% 
  summarise(min = min(GPP_site_mean, na.rm = TRUE),
            quant_25 = quantile(GPP_site_mean, probs = 0.25),
            quant_50 = quantile(GPP_site_mean, probs = 0.50),
            quant_75 = quantile(GPP_site_mean, probs = 0.75),
            max = max(GPP_site_mean, na.rm = TRUE))




