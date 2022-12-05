library(tidyverse)
library(dplyr)
library(glue)

# get USGS metabolism data: this will have to be adjusted to pull from figshare
## estimates 
## diagnostics
## light estimates
## site data w/NHD



# read-in usgs estimates
estimates <- read_csv('data/output_data/estimates_2007-2021.csv') %>% 
  dplyr::filter(lubridate::year(date) > 2007) %>% 
  mutate(GPP_C = GPP*(12/32))


# pair with diagnostics
diagnostic <- read_csv('data/model_runs/bayes_all/diagnostics.csv')

good_site_years <- diagnostic %>% 
  filter(ER_K_r2 < 0.6,
         K_median < 100,
         n_days >= (365*0.6))

# read-in streamlight data

light <- read_csv('data/usgs_streamlight/annual_light.csv')



# gap-fill
## see synthesis_gapfill()

# do for terrestrial and lake data too


# calculate metrics: calc_site_metrics() for each site-year
# GPP, water temperature, discharge, PAR