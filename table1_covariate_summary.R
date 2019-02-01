# read data----
perna_wai <-
  readRDS('data/spat_wai.RDS') %>% 
  drop_na(log_perna, SOI_lag1) 

table_covariate_wai <- 
perna_wai %>% 
  select(Depth,SOI_lag1,TauCS_lag1, tidal_range_lag0,temp_anom_lag1) %>% 
  gather(key,value) %>% 
  group_by(key) %>% 
  summarise_all(
    funs(
      mean = mean,
      median = median,
      min = min,
      max = max,
      q25 = quantile(., 0.25),
      q75 = quantile(., 0.75),
      sd = sd),
    na.rm = T
  ) %>% 
  mutate(Site = 'Wainui')


table_covariate_pel <- 
perna_pel %>% 
  select(Depth,SOI_lag1,TauCS_lag1, tidal_range_lag0,temp_anom_lag1) %>% 
  gather(key,value) %>% 
  group_by(key) %>% 
  summarise_all(
    funs(
      mean = mean,
      median = median,
      min = min,
      max = max,
      q25 = quantile(., 0.25),
      q75 = quantile(., 0.75),
      sd = sd),
    na.rm = T
  ) %>% 
  mutate(Site = 'Clova Bay')

table_covariates <- 
  bind_rows(table_covariate_wai, table_covariate_pel) %>% 
  select(key, Site, everything()) %>% arrange(key) %>% 
  write_csv('tables/table_covariates.csv')