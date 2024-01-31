fill_and_normalize_metab <- function(df) {
  
  source('code/functions/fillMiss3.R')
  source('code/functions/znorm.R')
  out <- list()
  
  for(i in 1:length(unique(df$site))) {
    
    site_use <- unique(df$site)[i]
    
    df_fill <- try(df %>% 
                     dplyr::filter(site == site_use) %>% 
                     #select(date, GPP) %>% 
                     data.frame() %>% 
                     dplyr::mutate(GPP_filled = fillMiss3(., var = "GPP", plot = FALSE),
                                   ER_filled = fillMiss3(., var = 'ER', plot = FALSE))
                   )
    
    if(inherits(df_fill,'try-error'))
      next
    
    df_norm <- df_fill %>% 
      dplyr::mutate(GPP_filled_norm = znorm(., 'GPP_filled'),
                    ER_filled_norm = znorm(., 'ER_filled'),
                    NEP = GPP + ER,
                    NEP_filled = GPP_filled + ER_filled,
                    NEP_filled_norm = GPP_filled_norm + ER_filled_norm)
    
    out[[i]] <- df_norm
    
  } # end for loop
  
  do.call(dplyr::bind_rows, out)
  
} # end function
