library(dataRetrieval)
library(tidyverse)
library(mda.streams)

# list of target sites
site_list <- read.csv("./data/sites/sites.csv")
site_list <- site_list[,2]

# names of target varibales for mda.streams function
var_codes <- c("disch", "wtr", "doobs")

# loop thru target variables
for (var_code in var_codes) {
  # initialize the counter (i) and  progress bar
  i <- 0
  pb <- txtProgressBar(min = 0, max = length(site_list), style = 3, width = 50, char = "=")

  # start list to append failed sites (if any)
  failed_sites <- c()

  # loop thru sites and pull data
  for(site_code in site_list) {
    i <- i + 1

    tryCatch(
      expr = {
        # actual data pull, for target site and variable
        # from stary of 2017 to end of 2021
        q_data <- stage_nwis_ts(site_code,
                          var_code,
                          times = c('2017-01-01','2021-12-31'),
                          folder="./data/tmp")

        setTxtProgressBar(pb, i)
        print(paste("pulled", var_code, site_code))
      },
      error = function(e) {
        print(paste("---- error:", site_code))
        failed_sites <- c(failed_sites, site_code)
        setTxtProgressBar(pb, i)
      }
    )
  }
  print(paste("failed sites:", failed_sites))
}


# USGS parameter codes       CrossReference URL: https://help.waterdata.usgs.gov/parameter_cd?group_cd=PHY
## p_codes <- c("temp"="00010", "Q"="00060", "DO"="00300", "mean_depth"="00064")
