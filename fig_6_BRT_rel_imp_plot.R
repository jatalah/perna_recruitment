# MS figures--------------
# load('.RData')
library(ggpubr)
library(viridis)
library(tidyverse)
source('H:/javiera/Stats/R folder/theme_javier.R')
theme_set(theme_javier())

# read data ----
# Wainui BRT
model_wai_dat <- read_csv('data/model_wai_relimp_dat.csv') %>% data.frame()
#Pelorus BRT data 
model_pel_dat <- read_csv('data/model_pel_relimp_dat.csv') %>% data.frame()

# create bar plot Rel. Imp. BRT model-------------
brt_plot_relimp_pel <- 
  ggplot(model_pel_dat, aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = '', y = 'Relative influence (%)') +
  ggtitle('B. Clova') +
  scale_fill_viridis(discrete = T , guide = F) +
  theme(axis.text.y = element_blank())


brt_plot_relimp_wai <- 
  ggplot(model_wai_dat,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = '', y = 'Relative influence (%)') +
  ggtitle('A. Wainui') +
  scale_fill_viridis(discrete = T , guide = F)

brt_plot_relimp_wai
brt_plot_relimp_pel

# BRT plots-------
rel_imp_brt <-
  ggarrange(
    brt_plot_relimp_wai,
    brt_plot_relimp_pel,
    ncol = 2,
    common.legend = T,
    legend = 'right',
    widths = c(1.3, 1)
  )
rel_imp_brt

ggsave(
  rel_imp_brt,
  filename = 'figures/rel_imp_brt.tiff',
  width = 20,
  height = 12,
  units = "cm",
  dpi = 1200,
  compression = 'lzw'
)
