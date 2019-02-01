# devtools::install_github("bgreenwell/pdp", force = T)
library(pdp)
library(tidyverse)
library(ggpubr)
theme_set(theme_javier())


depth <- 
  bind_rows(
  Wainui = partial(model_wai, pred.var = "Depth", inv.link = exp),
  Clova = partial(model_pel, pred.var = "Depth", inv.link = exp),
  .id = 'Site'
) 


# month-------
month_pp <-
  bind_rows(
    Wainui = partial(model_wai, pred.var = "month", inv.link = exp),
    Clova = partial(model_pel, pred.var = "month", inv.link = exp),
    .id = 'Site'
  ) %>%
  ggplot(aes(month, yhat, color = Site)) +
  geom_smooth(se = F, span = .6, aes(lty = Site)) +
  labs(x = 'Month',
       y = italic(Perna) ~ m ^ -1,
       parse = T) +
  scale_x_continuous(limits = c(1, 12), breaks = seq(0, 12, 2)) +
  scale_y_continuous(limits = c(0, 2200))


# year------
year_pp <-  
  bind_rows(
    Wainui =partial(model_wai, pred.var = "year", inv.link = exp),
    Clova = partial(model_pel, pred.var = "year", inv.link = exp),
    .id = 'Site'
  ) %>% 
  ggplot(aes(year, yhat, color = Site)) +
  geom_smooth(se = F, span = .6, aes(lty = Site)) +
  xlab('Year') +  
  ylab('') +
  scale_x_continuous(limits = c(1992,2018), breaks = seq(1992,2018,6)) +
  scale_y_continuous(limits = c(0,2200))

# wind
wind_pp <- 
bind_rows(
  Wainui =partial(model_wai, pred.var = "TauCS_lag1", inv.link = exp),
  Clova = partial(model_pel, pred.var = "TauCS_lag1", inv.link = exp),
    .id = 'Site'
  ) %>% 
  ggplot(aes(TauCS_lag1, yhat, color = Site)) +
  geom_smooth(se = F, span = .6, aes(lty = Site)) +
  xlab('Wind stress') + 
  ylab('') +
  scale_x_continuous(limits = c(-0.05,0.02), breaks = seq(-0.5,0.2,.02)) +
  scale_y_continuous(limits = c(0,2200))

# SOI
SOI_pp <- 
  bind_rows(
    Wainui =partial(model_wai, pred.var = "SOI_lag1", inv.link = exp),
    Clova = partial(model_pel, pred.var = "SOI_lag1", inv.link = exp),
    .id = 'Site'
  ) %>% 
  ggplot(aes(SOI_lag1, yhat, color = Site)) +
  geom_smooth(se = F, span = .6, aes(lty = Site)) +
  labs(x = 'SOI',
       y = italic(Perna) ~ m ^ -1,
       parse = T) +
  scale_y_continuous(limits = c(0,2200))

# Tide range
tide_pp <- 
  bind_rows(
    Wainui = partial(model_wai, pred.var = "tidal_range_lag0", inv.link = exp),
    Clova = partial(model_pel, pred.var = "tidal_range_lag0", inv.link = exp),
    .id = 'Site'
  ) %>% 
  ggplot(aes(tidal_range_lag0, yhat, color = Site)) +
  geom_smooth(se = F, span = .6, aes(lty = Site)) +
  xlab('Tidal range (m)') + 
  ylab('') +
  scale_x_continuous(limits = c(1,4), breaks = seq(1,4,1)) +
  scale_y_continuous(limits = c(0,2200))


# temp
tem_pp <- 
  bind_rows(
    Wainui = partial(model_wai, pred.var = "temp_anom_lag1", inv.link = exp),
    Clova = partial(model_pel, pred.var = "temp_anom_lag1", inv.link = exp),
  .id = 'Site'
) %>% 
  ggplot(aes(temp_anom_lag1, yhat, color = Site)) +
  geom_smooth(se = F, span = .6, aes(lty = Site)) +
  xlab('Temperature anomaly') + 
  ylab('') +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,1)) +
  scale_y_continuous(limits = c(0,2200))


partial_plots_all <- 
  ggarrange(
  month_pp,
  year_pp,
  wind_pp,
  SOI_pp,
  tem_pp,
  tide_pp,
  common.legend = T,
  legend = 'bottom', 
  ncol = 3, nrow = 2
)

ggsave(
  partial_plots_all,
  filename = 'figures/partial_plots.tiff',
  width = 18,
  height = 10,
  units = "cm",
  dpi = 900,
  compression = 'lzw'
)
