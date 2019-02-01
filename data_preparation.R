library(lubridate)
library(tidyverse)

spat_raw <-
  readRDS('data/spat.RDS') %>% 
  drop_na(Perna)
dim(spat_raw)


# spat all---------------------------------------
spat_all <-
  readRDS('data/spat.RDS') %>%
  mutate(
    log_perna = log(Perna + 1),
    month = month(Date),
    year = year(Date)
  ) %>%
  mutate(SOI = if_else(YearMonth == "2018-05-01", 0.4, SOI), # add latest SOI data not available from website used
         SOI = if_else(YearMonth == "2018-06-01", -0.1, SOI),
         SOI = if_else(SOI <= -99, NA_real_, SOI)) %>%
  filter(!is.na(Depth)) %>%
  group_by(Area, month) %>% 
  mutate(area_month_mean_temp = mean(ET100, na.rm = T),
         area_month_mean_tdry = mean(Tdry, na.rm = T),
         tidal_range = Max_Tide-Min_Tide) %>% 
  ungroup() %>% 
  mutate(temp_anom = ET100 - area_month_mean_temp,
         Tdry_anom = Tdry - area_month_mean_tdry) %>% 
  group_by(Area, Region, Lat, Long, Depth, Bathymetry, PosFetch, NegFetch) %>%
  mutate_at(vars(-Date), funs(lag0 = identity(.))) %>%
  mutate_at(vars(-matches('_lag\\d*$'), -Date), funs(lag1 = lag(., 1, order_by = Date))) %>%
  select(
    -SOI,-RH,-Rain,-Tmax,-Tmin,-Pmsl,-Sun,
    -Rad,-TauCS,-TauAS,-Min_Tide,-Max_Tide,
    -matches('^ET\\d*$'),-Tdry,-Tgmin,-TWet
  ) %>%
  group_by(YearMonth, Area, Region) %>%
  summarise_all(mean, na.rm = T) %>%
  mutate(Site = fct_collapse(
    Area,
    Ringroad = c(
      "Golden Bay Ringroad" ,
      "Golden Bay Ringroad S",
      "Golden Bay Ringroad N"
    ),
    Wainui = c("Wainui SE", "Wainui NE")
  )) %>%
  drop_na(Perna) %>%
  dplyr::select(
    YearMonth, Date, year, month, Area, Region, Site, Perna, log_perna, everything(),
    -Mytilus,
    -PosFetch_lag1,
    -NegFetch_lag1,
    -PosFetch_lag0,
    -NegFetch_lag0,
    # -Mytilus_lag0,
    # -Mytilus_lag1,
    -Bathymetry_lag1,
    -Lat_lag0,
    -Long_lag0,
    -Area_lag0,
    -Region_lag0,
    -YearMonth_lag0,
    -YearMonth_lag1,
    -Depth_lag1,
    -Lat_lag1,
    -Bathymetry_lag0,
    -Long_lag1,
    -Area_lag1,
    -Region_lag1,
    -Depth_lag0,
    -Perna_lag0,
    -Perna_lag1,
    -area_month_mean_temp,
    -area_month_mean_temp_lag1
    ) %>% 
  ungroup()

ggplot(spat_all,aes(ET100_lag1, temp_anom_lag1)) + geom_point(aes(color = factor(month)))
ggplot(spat_all,aes(Tdry_lag1, Tdry_anom_lag1)) + geom_point(aes(color = factor(month)))


# Subset by Region/Sites -----
spat_gb <- 
  spat_all %>% 
  dplyr::filter(Region == "GB" & Area != "Patons Rock") 

spat_wai<- 
  spat_all %>% 
  dplyr::filter(Site == "Wainui")

spat_pel<- 
  spat_all %>% 
  dplyr::filter(Area == "Clova Bay" |
                  Area == "Clova NE"|
                  Area == "Clova SW"|
                  Area == "Manaroa"|
                  Area == "Crail Bay") 

spat_mana<- 
  spat_all %>% 
  dplyr::filter(Area == "Manaroa")


spat_skiddaw<- 
  spat_all %>% 
  dplyr::filter(Area == "Skiddaw")


#save RDS files ------------
saveRDS(spat_all, 'data/spat_all.RDS')
saveRDS(spat_pel, 'data/spat_pel.RDS')
saveRDS(spat_gb, 'data/spat_gb.RDS')
saveRDS(spat_wai, 'data/spat_wai.RDS')
saveRDS(spat_mana, 'data/spat_mana.RDS')
saveRDS(spat_skiddaw, 'data/spat_skiddaw.RDS')
