# load libraries----------------------
library(gbm)
library(caret)
library(tidyverse)
library(plotmo)
library(ggpmisc)
library(ggpubr)
set.seed(123)

# read data and was
# walk forwards 48 months
source('scripts/extract data and process/createIrregTimeSlices.R')

# read data-----------
perna_pel <-
  readRDS('data/spat_pel.RDS') %>% 
  drop_na(log_perna, SOI_lag1)

# correlations and VIF----
source('H:/javiera/Stats/R folder/corvif.R')
cor_preds <- 
  perna_pel %>% 
  select(month, year, SOI_lag1, TauCS_lag1, tidal_range_lag0, temp_anom_lag1) %>% 
  cor(.)

corrplot(
  cor_preds,
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  diag = F,
  method = 'circle')

corvif(
  perna_pel %>% select(
    month,
    year,
    SOI_lag1,
    TauCS_lag1,
    tidal_range_lag0,
    temp_anom_lag1
  )
)

# create train and test data time-slices--------------- 
resamples_pel <-
  createIrregTimeSlices(
    perna_pel$Date,
    slices = 48,
    horizon = 1,
    fixedWindow = FALSE,
    unit = 'month',
    plot = T
  )

# tune grid-----
tune.grid <- 
  expand.grid(
    .n.trees = c(100:1000),
    .interaction.depth = 3,
    .shrinkage = c(0.1),
    .n.minobsinnode = (10)
  )

# train model------
model_pel <- 
  train(
  log_perna ~
    Depth +
    month +
    year +
    SOI_lag1 +
    TauCS_lag1 +
    tidal_range_lag0 +
    temp_anom_lag1,
  data = perna_pel,
  method = 'gbm',
  preProcess = c("BoxCox", "scale", 'center'),
  trControl = trainControl(
    'timeslice',
    index = resamples_pel$train,
    indexOut = resamples_pel$test,
    verboseIter = TRUE,
    allowParallel = TRUE
  ),
  tuneGrid = tune.grid
)


# model summaries---------- 
print(model_pel)
par(mar = c(5, 7, 3, 1))
summary(model_pel$finalModel,las = 2, main = 'Pelorus Perna BRT mdoel')

model_pel$results %>%
  as_tibble() %>%
  arrange(desc(Rsquared)) %>%
  print(n = 10)
top_n(model_pel$results, 1, Rsquared)
varImp(model_pel)

# save model--------------------
saveRDS(model_pel,'models/gbm_pel_caret.rds')
model_pel <- readRDS('models/gbm_pel_caret.rds')

# model predictions---------
perna_pel$gbm_log_perna <- predict(model_pel, newdata = perna_pel)

# plot model----------
model_pel_dat <- 
  summary(model_pel) %>% 
  data.frame() %>% 
  mutate(var = fct_recode(var,
                          Year = "year",
                          Month = "month",
                          'Tidal range' = "tidal_range_lag0",
                          'Westerly winds' = "TauCS_lag1",
                          'Temperature anomaly' = "temp_anom_lag1",
                          SOI = 'SOI_lag1'),
         var = fct_relevel(var, .x = 'Year', 'Month', 'Depth'),
         var = fct_reorder(var,desc(var)))

write_csv(model_pel_dat, 'data/model_pel_relimp_dat.csv')

# create bar plot Rel. Imp. BRT model-------------
brt_plot_relimp_pel <- 
  ggplot(model_pel_dat,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = '', y = 'Relative influence (%)') +
  ggtitle('B. Clova') +
  scale_fill_viridis(discrete = T , guide = F) +
  theme_bw() +
  theme(axis.text.y =element_blank(),
      panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank()) 

brt_plot_relimp_pel

# partial effect plots---------------
plotmo(
  model_pel,
  ylim=NA,
  all1 = T,
  degree2 = F,
  do.par=TRUE,
  mfrow =c(1,7),
  inverse.func=exp,
  nrug= 0,
  pmethod="plotmo",
  pt.col = 0,
  caption = "Clova",
  main = c("Depth","Month", "Year", "SOI", "Wind", "Tidal range", "Temperature anomaly" )
)

# # 2-way interactions------
# plotmo(
#   model_pel,
#   # smooth.col = 2,
#   # smooth.f = .7,
#   ylim=NA,
#   all2 = F,
#   caption = '',
#   degree2 = T,
#   do.par=TRUE,
#   mfrow =c(2,3),
#   nrug= 0,
#   pmethod = 'plotmo',
#   pt.col = 0,
#   clip=FALSE,
#   inverse.func=exp
# )


# Check residuals----------
plotres(model_pel)


# Obs vs fitted plot--------------
pel_plot_fitted_vs_obs <- 
  ggplot(perna_pel, aes(gbm_log_perna, log_perna)) + 
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1, lty = 2 , col = 2) +
  theme_light() +
  scale_x_continuous(limits = c(0,8), breaks = seq(0,8,2)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,2)) +
  labs( x= 'Predicted Perna (log)', y = 'Observed Perna (log)') +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 3
  ) +
  theme_javier()

pel_plot_fitted_vs_obs

# obs vs. forcasted predictions------------------
# get the forcasted predictiosn##
test_pel_rows <- resamples_pel$test %>% flatten_dbl()

gbm_test_log_perna_pel <- predict(model_pel, newdata = perna_pel[test_pel_rows,])

pel_plot_forcast <- 
  ggplot(perna_pel[test_pel_rows,], aes(gbm_test_log_perna_pel, log_perna)) + 
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1, lty = 2 , col = 2) +
  theme_light() +
  scale_x_continuous(limits = c(0,12), breaks = seq(0,12,2)) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,2)) +
  labs( x= 'Forcasted Perna (log)', y = 'Observed Perna (log)') +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 3
  ) +
  theme_javier()

pel_plot_forcast

pel_predictions_plot <- ggarrange(pel_plot_fitted_vs_obs, pel_plot_forcast)
pel_predictions_plot

# GOF----
library(hydroGOF)
gof(perna_pel$gbm_log_perna,perna_pel$log_perna)
gof(perna_pel[test_pel_rows,]$gbm_log_perna, perna_pel[test_pel_rows,]$log_perna)

# seasonal predictions -------------
perna_pel$cat_perna <-
  cut(
    exp(perna_pel$gbm_log_perna),
    breaks = c(0, 500, 1.5e+03, 3e+5),
    labels =  c('Poor', 'Moderate', 'Good')
    # breaks = c(0, 1000, 2e+03, 5e+03, 10000, 30000),
    # labels =  c('Poor', 'Good', 'Very Good', 'Excellent', 'Ultimate')
  )

write_csv(perna_pel, 'data/perna_pel_predictions.csv')

# table categorical catches------
table(perna_pel$cat_perna)/nrow(perna_pel)*100

pel_seasonal_predictions <- 
  ggplot(perna_pel, aes(
    x = month,
    y = exp(gbm_log_perna)
  )) +
  geom_point(alpha = .8, position = position_jitter(width = 0.3), aes(color = cat_perna)) +
  geom_smooth(method = 'loess', se = F, color = 1, size = 1, span = .5) +
  scale_y_log10(breaks = c(1, 10, 100, 500, 1000, 3000)) +
  theme_bw(base_size = 12) +
  labs(x = 'Month', y = italic(Perna)~m^-1, parse = T) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  geom_hline(
    yintercept = c(.5e+03, #Poor
                   1e+03, # Good
                   3e+03),#Very good
    lty = 2,
    col = 'gray70'
  ) +
  scale_color_viridis_d(name = '', begin = 0, end = .8) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank()) +
  ggtitle('B. Clova')

pel_seasonal_predictions 


