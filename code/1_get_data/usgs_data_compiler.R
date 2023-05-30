library(tidyverse)
library(stringr)

# load sites and list target variables
site_files <- list.files(path = "./data/tmp/", pattern = ".rds", full.names = TRUE)
vars <- c("wtr", "disch", "doobs")

# loop through variables, making a dataframe of all data for all sites for each
# full loop reports progress for each variable, should be max runtime 5-30m
for(var in vars){
  # isolate variable types
  fp_var <- site_files[grep(var, site_files)]

  # load first file to inform dummy df
  ex_var <- readRDS(fp_var[1])
  df_var <- data.frame(matrix(ncol = ncol(ex_var) + 1, nrow = 0))
  colnames(df_var) <- c(colnames(ex_var), "site")

  # progress bar, and index
  i <- 0
  pb <- txtProgressBar(min = 0, max = length(fp_var), style = 3, width = 50, char = "=")

  # make variable df
  for(file in fp_var) {
    i <- i + 1

    # read data and site name
    var_file <- readRDS(file)
    site_name <- str_match(fp_var[i], "//(nwis_[0-9]+)-")[,2]
    var_file$site <- site_name

    # bind into variable df, print progress bar
    df_var <- rbind(df_var, var_file)
    setTxtProgressBar(pb, i)
  }

  # save to CSV
  filepath <- paste0(".data/sites/", var, ".csv")
  write.csv(df_var, filepath)
  print(paste("SUCCESS:", var))
  print(paste("file saved to", filepath))
}

# combine all three (dropping non-matching datetime stamps, eg 15m and 30m interval discrepencies)
usgs_data <- merge(df_disch, df_wtr, by = c("site", "DateTime"))
usgs_data <- merge(usgs_data, df_doobs, by = c("site", "DateTime"))

write.csv(usgs_data, "data/usgs_data.csv")
