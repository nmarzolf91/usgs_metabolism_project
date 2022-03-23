# Title: Site Filter
# this script will create the list of all USGS sites with temp data
# that have > 10 years, and, plausibly weekly data
library(dataRetrieval)
library(dplyr)
library(mda.streams)

tempCode <- "00010"
dischCode <- "00061"

# state by state for loop, to identify temp sites in USGS
for (state in state.abb) {
  tryCatch(
    expr = {
      print(paste("attempting pull, USGS temp data: ", state))

      # get all sites with temp data for state
      sitesTemp <- whatNWISdata(stateCd = state,
                            parameterCd = c(tempCode, dischCode))

      # > 10 years of data
      sitesTemp_filter <- sitesTemp %>%
          mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
          filter(period > 10*365)

      # keep only continuous data
      sitesTemp_uv <- sitesTemp_filter %>%
        filter(data_type_cd == "uv")

    },
    error = function(e) {
      print(paste("failed to load sites with temp data:", state))
    },
    finally = {
      fp <- paste0("data/sites/uv/", state, ".csv")
      write.csv(sitesTemp_uv, file = fp)
    }
  )
}

# names of target varibales for mda.streams function
var_codes <- c("wtr")

for (var_code in var_codes) {
  # loop thru target variables

  for (state in state.abb) {
    # start list to append failed sites (if any)
    failed_sites <- c()

    csv_fp <- paste0("./data/sites/uv/", state, ".csv")
    site_data <- read.csv(csv_fp)
    site_list <- unique(site_data$site_no)

    print(paste("__________", state, "__________"))

    # initialize the counter (i) and  progress bar
    if(length(site_list) > 0) {

      i <- 0
      pb <- txtProgressBar(min = 0, max = length(site_list), style = 3, width = 50, char = "=")

      # loop thru sites and pull data
      for(site_code in site_list) {
        i <- i + 1

        tryCatch(
          expr = {
            ## get dates for site
            begin <- site_data[site_data$site_no == site_code,]$begin_date
            end <- site_data[site_data$site_no == site_code,]$end_date

            # actual data pull, for target site and variable
            site_nwis <- paste0("nwis_", site_code)
            q_data <- stage_nwis_ts(site_nwis,
                              var_code,
                              times = c(begin, end),
                              folder="./data/temp")

            setTxtProgressBar(pb, i)
            print(paste("pulled", state, var_code, site_code))
          },
          error = function(e) {
            print(paste("---- error:", state, var_code, site_code))
            failed_sites <- c(failed_sites, site_code)
            setTxtProgressBar(pb, i)
          }
        )
      }
    }
    print(paste("failed sites:", failed_sites))
  }
}

  # USGS parameter codes       CrossReference URL: https://help.waterdata.usgs.gov/parameter_cd?group_cd=PHY
## p_codes <- c("temp"="00010", "Q"="00060", "DO"="00300", "mean_depth"="00064")
