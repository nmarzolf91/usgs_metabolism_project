## Header ----
## Script name: eval_model.R
##
## Purpose of script: evaluate model run_1 
##
## Author: Nick Marzolf
## Date Created: 
## Date Modified: 2022-04-12
## Email: nicholas.marzolf@duke.edu
##
## load packages:  
library(tidyverse)
library(dplyr)
library(ggplot2)
library(streamMetabolizer)
library(glue)
library(ggpubr)
library(cowplot)
##
## clear the environment if needed
rm(list = ls())
##
## set the ggplot theme
source("C:/Users/nmarz/Desktop/Research/R code/theme_nick.R")
theme_set(theme_nick())


run_files <- list.files('data/model_runs/run_1/outputs/daily/')

rating_tbl <- data.frame(
  site = character(),
  year = character(),
  K600_daily_sigma_Rhat = numeric(),
  err_proc_iid_sigma_Rhat = numeric()
)

for(i in 186:length(run_files)) {
  
  site_code <- substr(run_files[i],
                      1,13)
  date <- substr(run_files[i],
                 15,35)
  
  year <- substr(run_files[i],
                 15,18)
  
  daily <- read_csv(paste0('data/model_runs/run_1/outputs/daily/', site_code,'_',date,'_daily.csv'))
  
  GPP_neg <- daily %>% 
    filter(GPP_mean < -0.5) %>% 
    summarise(n()) %>% 
    pull
  
  ER_pos <- daily %>% 
    filter(ER_mean > 0.5) %>% 
    summarise(n()) %>% 
    pull
  
  K600_sum <- daily %>% 
    summarise(meanK = mean(K600_daily_mean, na.rm = TRUE),
              minK600 = min(K600_daily_mean, na.rm = TRUE),
              maxK600 = max(K600_daily_mean, na.rm = TRUE)) %>% 
    mutate(rangeK = maxK600 - minK600)
  
  
  KQ_overall <- try(
    read_csv(paste0('data/model_runs/run_1/outputs/KQ_overall/', site_code,'_',date,'_KQ_overall.csv'))) 
  if(inherits(KQ_overall, 'try-error')){
    next
  }
  
  overall <- try(read_csv(paste0('data/model_runs/run_1/outputs/overall/', site_code,'_',date,'_overall.csv'))) 
  if(inherits(overall, 'try-error')){
    next
  }
  
  rating_tbl <- rating_tbl %>% 
    add_row(
      site = site_code,
      year = year,
      K600_daily_sigma_Rhat = KQ_overall$K600_daily_sigma_Rhat,
      err_proc_iid_sigma_Rhat = overall$err_proc_iid_sigma_Rhat) %>% 
    mutate(
      RhatL = K600_daily_sigma_Rhat > 1.2 | err_proc_iid_sigma_Rhat > 1.2,
      RhatH = !RhatL,
      KrangeL = K600_sum$rangeK > 50,
      KrangeM = K600_sum$rangeK <= 50 & K600_sum$rangeK > 15,
      KrangeH = K600_sum$rangeK < 15,
      GPP_L = GPP_neg > 50,
      GPP_M = GPP_neg <= 50 & GPP_neg > 25,
      GPP_H = GPP_neg <= 25,
      ER_L = ER_pos > 50,
      ER_M = ER_pos <= 50 & ER_pos > 25,
      ER_H = ER_pos <= 25 
    )
  
  preds <- ggplot(daily, 
                  aes(x = date))+
    geom_line(aes(y = GPP_mean, color = 'GPP'))+
    geom_line(aes(y = ER_mean, color = 'ER'))+
    scale_color_manual(name = element_blank(),
                       values = c('tan', 'darkgreen'))+
    ylab(expression(paste('g ', O[2], ' ', m^-2, d^-1)))+
    ggtitle(paste(site_code, year, sep = ': '))
  
  equifin <- ggscatter(daily,
                       x = 'K600_daily_mean', y = 'ER_mean', add = 'reg.line')+
    stat_cor(label.y=3)
  
  plot_grid(preds, equifin,
            ncol = 2)
  
  ggsave(glue('figures/model_runs/run_1/{site_code}_{year}_eval.png'),
         width = 10, height = 4)
}

# print evaluation table
write_csv(rating_tbl,
          'data/model_runs/run_1/rating_run1.csv')


# turn images into GIF ----
library(magick)
library(magrittr)

list.files(path='figures/model_runs/run_1/', 
           pattern = '*.png', 
           full.names = TRUE) %>% 
  image_read() %>%                               # reads each path file
  image_join() %>%                               # joins image
  image_animate(fps = 1) %>%                     # animates, can opt for number of loops
  image_write("figures/model_runs/usgs_bayes_run1.gif") # write to current dir
