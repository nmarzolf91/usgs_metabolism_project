fill_and_normalize_metab <- function(df) {
  
  # source in functions
  source('code/functions/fillMiss3.R')
  source('code/functions/znorm.R')
  
  # define the number of sites
  sites <- unique(df$site)
  
  # and years
  years <- unique(df %>% 
                    dplyr::mutate(year = lubridate::year(date)) %>% 
                    dplyr::pull(year))
  
  # create a data frame to loop around and to build out as a summary table
  df_NA <- data.frame(site = rep(sites, each = length(years)),
                      year = rep(years, times = length(sites)))
  
  # where to save output data
  out <- list()
  
  # build for loop around site-year (or rows in our tracker data frame)
  for(i in 1:nrow(df_NA)) {
    
    # extract the row of interest
    deets <- df_NA[i,]
    
    # and its site and year
    site <- deets['site']
    year <- deets['year']
    
    # filter the larger data frame to this site-year
    df_use <- df %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::filter(site %in% !!site,
                    year %in% !!year)
    
    # if this results in an empty data frame, we record that and go to the next step in the loop
    if(nrow(df_use) == 0){
      df_NA[i, 'start_date'] <- NA
      df_NA[i, 'per_NA'] <- NA
      df_NA[i, 'complete_year'] <- FALSE
      next
    }
    
    # pad out the number of days
    ## the cleaned and filtered model outputs (river_metab in the Rmd) takes away the 'bad' or missing days
    ## the padr::pad() function expands the time series back to Jan 1 of the given year by day
    df_pad <- df_use %>% 
      padr::pad(start_val = lubridate::date(glue::glue("{year}-01-01")),
                end_val = lubridate::date(glue::glue("{year}-12-31")),
                'day') %>% 
      tidyr::fill(site, resolution, year, 
                  .direction = 'downup')
    
    # with the padded dataset
    ## whats the start date (of GPP, not padded GPP)
    start_date <- df_pad %>% 
      dplyr::filter(!is.na(GPP)) %>% 
      dplyr::pull(date) %>% 
      dplyr::first()
    
    end_date <- df_pad %>% 
      dplyr::filter(!is.na(GPP)) %>% 
      dplyr::pull(date) %>% 
      dplyr::last()
    
    # how many NAs are there (# and percent)
    NAs <- sum(is.na(df_pad$GPP))
    per_NA <- round(NAs/length(df_pad$GPP)*100, digits = 2)
    
    # record these values in our tracker dataframe
    df_NA[i, 'start_date'] <- start_date
    df_NA[i, 'per_NA'] <- per_NA
    
    # now that we've padded the dataset, we should be able to gap fill for all time points
    # however, this is an issue if there is a large gap at the start or end of the time series (ie early or late in the year)
    ## the gap fill model below takes the first estimates of GPP and back calculates in time what it thinks. But we know rivers are highly seasonal and this can be a bad approach
    # this if statement checks if the first date of GPP measurements is within the first 90 days of the padded dataset
    ## if there is >90 missing days at the start, we don't gap fill
    ### this is a subjective call and maybe worth exploring the window here a bit. 90 days roughly corresponds to 25% of a year
    if(!start_date %in% seq(dplyr::pull(df_pad[1,'date']),dplyr::pull(df_pad[90,'date']), by = 1) ||
       !end_date %in% seq(dplyr::pull(df_pad[nrow(df_pad) - 90,'date']),dplyr::pull(df_pad[nrow(df_pad),'date']), by = 1)){
      df_NA[i, 'complete_year'] <- FALSE
      next
    } else {
      df_NA[i, 'complete_year'] <- TRUE
    }
    
    # here we gap fill
    ## we are catching if (somehow) a dataset with >40% missing data has passed through and ignore. 40% missing (or 60% data) is the thresold used in Bernhardt et al. (2022) and we use that here
    ## the block and pmiss parameters also address the size and amount of gaps, but are accounted for upstream in the code
    # the fillMiss3 function has roots in github.com/USGS-R/waterData/fillMiss (https://github.com/USGS-R/waterData), modified by Bernhardt et al. (2022, PNAS)
    ## the function looks for the number and sizes of gaps such that they meet the criteria defined and fit and smooth missing data through a structural state-space time series model (tsSmooth)
    # gap filling methods are many and come with pros and cons. We apply this method given use on similar data (Bernhardt et al. 2022, PNAS)
    if(per_NA < 40){
      df_pad$GPP_filled <- fillMiss3(dataset = data.frame(df_pad), 
                                     var = 'GPP', 
                                     block = 125,
                                     pmiss = 40, 
                                     plot = FALSE)
      
      out[[i]] <- df_pad
      
    } # end if statement
    
  } # end  for loop
  
  # print and save the tracker
  readr::write_csv(df_NA,
                   'data/summary_site-year_NAs.csv')
  
  # return the compiled object
  out <- do.call(rbind, out)

} # end function
