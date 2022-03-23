library(tidyverse)
library(stringr)
library(multidplyr)
library(data.table)

# multiprocessing
cluster <- new_cluster(8)

# import water temp and Q data
df_disch <- fread("./data/sites/disch.csv")
df_wtr <- fread("./data/sites/wtr.csv")

# filters to make sure the "years" are all at least 75% complete
df <- df_disch %>%
  mutate(year = format(as.Date(DateTime), "%Y")) %>%
  mutate(day = format(as.Date(DateTime), "%j")) %>%
  group_by(site, year) %>%
  summarise(days_n = n_distinct(day)) %>%
  filter(days_n > 273) %>%
  partition(cluster) %>%
  collect()

df_disch_filter <- df_disch %>%
  filter(site %in% df$site) %>%
  partition(cluster) %>%
  collect()



# combine all three (dropping non-matching datetime stamps, eg 15m and 30m interval discrepencies)
usgs_data <- merge(df_disch, df_wtr, by = c("site", "DateTime"))
usgs_data <- merge(usgs_data, df_doobs, by = c("site", "DateTime"))

write.csv(usgs_data, "data/usgs_data.csv")

# season stuff later
# label samples per seasons,
## getSeason <- function(DATES) {
##     WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
##     SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
##     SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
##     FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

##     # Convert dates from any year to 2012 dates
##     d <- as.Date(strftime(DATES, format="2012-%m-%d"))

##     ifelse (d >= WS | d < SE, "Winter",
##       ifelse (d >= SE & d < SS, "Spring",
##         ifelse (d >= SS & d < FE, "Summer", "Fall")))
## }
