library(dplyr)
library(dataRetrieval)

for (state in state.abb) {
  tryCatch(
    expr = {
      fp <- paste0("./data/sites/uv/", state, ".csv")
      site_data <- read.csv(fp)

      info <- readNWISsite(site_data$site_no)
      write.csv(info, paste0("./data/sites/info/", state, ".csv"))

      if(!exists("site_info")) {
        site_info <- info
      } else {
        site_info <- rbind(site_info, info)
      }

      print(paste("DONE:", state))
    },
    error = function(e) {
      print("ERROR", state)
    }
  )
}
