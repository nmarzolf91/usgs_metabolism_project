# evaluate initial model run ----
# for each site, determine the
## 1) number of GPP < -0.5
## 2) number of ER > 0.5
## 3) Mean and SD of K600
## 4) dates with no data

out <- data.frame(
  site = character(),
  year = numeric(),
  GPP_neg = numeric(),
  ER_pos = numeric(),
  K_median = numeric(),
  K_sd = numeric(),
  no_dat_num = numeric(),
  no_dat_dates = character()
)

fits <- list.files('data/model_runs/init_all/dat_fit/')

for(s in 1:length(fits)) {
  
  fit <- read_csv(paste0('data/model_runs/init_all/dat_fit/', 
                         fits[s]))
  
  site_code = substr(fits[s], 1,13)
  
  GPP_neg <- fit %>% 
    filter(GPP.daily < -0.5) %>% 
    summarise(n = n()) %>% 
    pull(n)
  
  ER_pos <- fit %>% 
    filter(ER.daily > 0.5) %>% 
    summarise(n = n()) %>% 
    pull(n)
  
  K_sum <- fit %>% 
    summarise(medianK = median(K600.daily, na.rm = TRUE),
              sdK = sd(K600.daily, na.rm = TRUE))
  
  no_dat_num <- sum(is.na(fit$GPP.daily))
  
  no_dat_dates <- fit %>% 
    filter(is.na(GPP.daily)) %>% 
    pull(date)
  
  out <- out %>% 
    add_row(
      site = site_code,
      year = lubridate::year(fit$date)[1],
      GPP_neg = GPP_neg,
      ER_pos = ER_pos,
      K_median = pull(K_sum,medianK),
      K_sd = pull(K_sum,sdK),
      no_dat_num = no_dat_num,
      no_dat_dates = str_c(as.character(no_dat_dates), collapse = ' ')
    )
  
  # preds <- ggplot(fit,
  #                 aes(x = date))+
  #   geom_line(aes(y = GPP.daily, color = 'GPP'))+
  #   geom_line(aes(y = ER.daily, color = 'ER'))+
  #   scale_color_manual(name = element_blank(),
  #                      values = c('tan', 'darkgreen'))+
  #   ylab(expression(paste('g ', O[2], ' ', m^-2, d^-1)))+
  #   ggtitle(paste(site_code, {year(fit$date)[1]}, sep = '-'))
  # 
  # equifin <- ggscatter(fit,
  #        x = 'K600.daily', y = 'ER.daily', add = 'reg.line')+
  #   stat_cor()
  # 
  # plot_grid(preds, equifin,
  #           ncol = 2)
  # 
  # ggsave(glue('figures/model_runs/init/{site_code}_{year(fit$date)[1]}_eval.png'),
  #        width = 10, height = 4)
}

write_csv(out,
          'data/model_runs/init_all/eval.csv')
