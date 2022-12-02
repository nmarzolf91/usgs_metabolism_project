# this function removes days that don't have enough data for streamMetabolizer to generate estimates
# for days that gaps that are <3 hours long, this will interpolate those gaps and append back to the dataframe


complete_days <- function(df) {
  
  # how many measurements are in each data, use the obs column
  # the interval column will indicate if data are at 15, 30, or 60 minute resolution
  df_sum <- df %>%
    dplyr::group_by(date = lubridate::date(DateTime)) %>%
    dplyr::summarise(obs = dplyr::n(),
                     interval = Mode(diff(DateTime)),
                     min = first(DateTime),
                     max = last(DateTime)) 
  
  # define good days as days that have hourly data collection and 24 points, and so on for higher resolution data
  good_days <- df_sum %>%
    dplyr::filter(obs == 24 & interval == 60 |
                    obs == 48 & interval == 30 |
                    obs == 96 & interval == 15) %>%
    dplyr::pull(date)
  
  # define short days that will be interpolated
  short_days <- df_sum %>%
    dplyr::filter(obs %in% 21:23 & interval == 60 |
                    obs %in% 42:47 & interval == 30 |
                    obs %in% 84:95 & interval == 15) %>%
    dplyr::pull(date)
  
  # these days have 1 extra measurement; we will throw out the last measurement in each day to get the
  weird_days <- df_sum %>%
    dplyr::filter(obs == 25 & interval == 60 |
                    obs == 49 & interval == 30 |
                    obs == 97 & interval == 15) %>%
    dplyr::pull(date)
  
  # find the exact time in the weird days to throw out
  weird_times <- df_sum %>%
    dplyr::filter(obs == 25 & interval == 60 |
                    obs == 49 & interval == 30 |
                    obs == 97 & interval == 15) %>%
    dplyr::pull(max)
  
  # create an empty object to populate in the for loop
  all_days_fix <- tibble() 
  
  # if there are days that need interpolating, enter the for loop
  if(! length(short_days) == 0){
    for(z in 1:length(short_days)) {
      
      # filter to the short day that needs interpolating
      dat <- df %>%
        filter(date(DateTime) %in% short_days[z]) 
      # 
      # site_no <- unique(dat$site) %>% 
      #   str_split_fixed('_', n = Inf)
      # 
      # site_no <- site_no[,2]
      # 
      # long <- read_csv('data/site_locs.csv') %>% 
      #   filter(site_no == !!site_no) %>% 
      #   pull(dec_long_va)
      # 
      # )
      
      # define the interval for the day
      int <- df_sum %>% 
        filter(date == short_days[z]) %>% 
        pull(interval)
      
      # here we will test if there are missing temperature or DO data; if there are, no point in interpolating
      # what percent of NAs are in the temperature time-series
      tempNAs <- statsNA(dat$wtr, print_only = FALSE)$percentage_NAs 
      tempNAs <- substr(tempNAs, 1, nchar(tempNAs)-1) %>% 
        as.numeric() 
      
      # what percent of fNAs in the DO.obs time-series
      DONAs <- statsNA(dat$doobs, print_only = FALSE)$percentage_NAs 
      DONAs <- substr(DONAs, 1, nchar(DONAs)-1) %>% 
        as.numeric() 
      
      # 12.5% represents 3 hours worth of data
      # if there is more than 12.5% missing data in either of these time-series, we omit this day and go to the nex
      if(tempNAs > 12.5 || DONAs > 12.5)
        next
      
      # for days that can be interpolated
      df_short_fix <- tibble(
        # create a date-time column that is evenly spaced at the resolution of the data collection for this site
        DateTime = seq.POSIXt(ymd_hms(glue('{short_days[z]} 00:00:00')), ymd_hms(glue('{short_days[z]} {t}',
                                                                                          t = case_when(int == 30 ~ '23:30:00',
                                                                                                        int == 15 ~ '23:45:00',
                                                                                                        int == 60 ~ '23:00:00'))), 
                                  length.out = case_when(int == 30 ~ 48,
                                                         int == 15 ~ 96,
                                                         int == 60 ~ 24))) %>% 
        
        # full_join this to the data for this day
        # full_join will add NAs where there is no data
        full_join(dat, by = 'DateTime') %>% 
          # and we will linearlly gap-fill those NAs here
          mutate(wtr = imputeTS::na_interpolation(wtr),
                 doobs = imputeTS::na_interpolation(doobs),
                 disch = imputeTS::na_interpolation(disch),
                 air_mb = imputeTS::na_interpolation(air_mb)) %>% 
        fill_by_prevalent(site) 

        # populate the empty object outside the loop that contains all the short days in a single object
        all_days_fix <- rbind(all_days_fix, df_short_fix)
    }
  }
  
  # create a new output object, starting with the good and weird days
  df_out <- df %>%
    dplyr::filter(date(DateTime) %in% c(good_days, weird_days)) 
  
  # throw out the weird data point that defines weird days as weird
  df_out <- df_out %>%
    dplyr::filter(! DateTime %in% weird_times)
  
  # append the good data to the gap filled data, and re-arrange the time-series in chronological order
  df_out <- rbind(df_out, all_days_fix) %>% 
    arrange(DateTime)
  
  # remain_good_days <- df_out %>%
  #   dplyr::group_by(date = lubridate::date(solar.time)) %>%
  #   dplyr::summarise(obs = dplyr::n()) %>% 
  #   filter(obs %in% c(24,48,96)) %>% 
  #   pull(date)
  # 
  # df_final <- df_out %>% 
  #   filter(date(solar.time) %in% remain_good_days)
  
  # output the time-series
  return(df_out)
}


# complete.cases
# this will filter out missing DO.obs, temp, and discharge/depth 
# for days with few missing complete cases, we can interpolate as above
# append and kick back to the for loop
