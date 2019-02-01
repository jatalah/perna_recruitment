rm(list=ls())

library(ggpubr)
library(tidyverse)

source('scripts/theme_javier.R')

# load data--------
perna_pel <- 
  read_csv('data/perna_pel_predictions.csv') %>% 
  mutate(cat_perna = cut(
      exp(gbm_log_perna),
      breaks = c(0, 500, 1.5e+03, 3e+5),
      labels =  c('Poor', 'Moderate', 'Good')))


perna_wai <- read_csv('data/perna_wai_predictions.csv') %>% 
  mutate(cat_perna = cut(
    exp(gbm_log_perna),
    breaks = c(0, 500, 1.5e+03, 3e+5),
    labels =  c('Poor', 'Moderate', 'Good')))


# overall classsification----
round(table(perna_pel$cat_perna)/nrow(perna_pel)*100,0)
round(table(perna_wai$cat_perna)/nrow(perna_wai)*100,0)

# sketch plots for each site-------
wai_seasonal_predictions <-
  ggplot(perna_wai, aes(x = month,
                        y = exp(gbm_log_perna))) +
  geom_hline(
    yintercept = c(.5e+03,
                   1.5e+03),
    lty = 2,
    col = 'gray70'
  ) +
  geom_point(alpha = .8,
             position = position_jitter(width = 0.3),
             color = 1,
             aes(fill = cat_perna) ,
             shape = 21) +
  geom_smooth(
    method = 'loess',
    se = F,
    span = .5,
    color = 1
  ) +
  scale_y_log10(breaks = c( 10, 100,  500,1.5e+03, 5e+03, 1.5e+4), limits = c(1,3e+4)) +
  scale_fill_brewer(name = '', palette = 'Spectral') +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  theme_javier() +
  labs(x = 'Month',
       y = italic(Perna) ~ m ^ -1,
       parse = T) +
  ggtitle('A. Wainui')

wai_seasonal_predictions

# pelorus monthly predictions plots-------
pel_seasonal_predictions <-
  ggplot(perna_pel, aes(x = month,
                        y = exp(gbm_log_perna))) +
  geom_hline(
    yintercept = c(.5e+03, #Poor
                   1.5e+3),
    #Very good
    lty = 2,
    col = 'gray70'
  ) +
  scale_fill_brewer(name = '', palette = 'Spectral') +
  geom_point(alpha = .8,
             position = position_jitter(width = 0.3),
             aes(fill = cat_perna),
             color = 1,
             shape = 21) +
  geom_smooth(
    method = 'loess',
    se = F,
    color = 1,
    size = 1,
    span = .5
  ) +
  scale_y_log10(breaks = c( 10, 100, 500, 1500, 5e+3,1.5e+4), limits = c(1,3e+4)) +
  theme_javier() +
  labs(x = 'Month',
       y = italic(Perna) ~ m ^ -1,
       parse = T) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  # scale_fill_manual(values = c('red', 'gold','mediumseagreen')) +
  ggtitle('B. Clova')

pel_seasonal_predictions 


# arrange plots and save ----
sesonal_predictions_plot <-
  ggarrange(
    wai_seasonal_predictions,
    pel_seasonal_predictions,
    common.legend = T,
    legend = 'bottom'
  )

ggsave(
  sesonal_predictions_plot,
  filename = 'figures/seasonal_predictions.tiff',
  width = 12 * 1.5,
  height = 5 * 1.5,
  units = "cm",
  dpi = 600,
  compression = 'lzw'
)
