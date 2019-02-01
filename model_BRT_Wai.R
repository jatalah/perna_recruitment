# load libraries----------------------
library(gbm)
library(caret)
library(tidyverse)
library(plotmo)
library(ggpmisc)
library(ggpubr)
library(viridis)
library(corrplot)

# read data----
perna_wai <-
  readRDS('data/spat_wai.RDS') %>% 
  drop_na(log_perna, SOI_lag1) 

# correlations and VIF----
source('H:/javiera/Stats/R folder/corvif.R')
cor_preds <- 
  perna_wai %>% 
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
  perna_wai %>% select(
    month,
    year,
    SOI_lag1,
    TauCS_lag1,
    tidal_range_lag0,
    temp_anom_lag1
  )
)

# Train model using walk forwards 24 months
source('scripts/extract data and process/createIrregTimeSlices.R')

# create train and test data time-slices--------------- 
resamples_wai <-
  createIrregTimeSlices(
    perna_wai$Date,
    slices = 48, #48
    horizon = 1,
    fixedWindow = FALSE,
    unit = 'month',
    plot = TRUE
  )

# tune grid-----
tune.grid <- 
  expand.grid(
    .n.trees = c(100:1000),
    .interaction.depth = 3,
    .shrinkage = c(0.1), # 0.1
    .n.minobsinnode = (10)
  )
set.seed(12345)

# Train the BRT model  ------------------
model_wai <-
  train(
    log_perna ~
      Depth +
      month +
      year +
      SOI_lag1 +
      TauCS_lag1 +
      tidal_range_lag0 +
      temp_anom_lag1,
    data = perna_wai,
    method = 'gbm',
    preProcess = c("BoxCox", "scale", 'center'),
    trControl = trainControl(
      'timeslice',
      index = resamples_wai$train,
      indexOut = resamples_wai$test,
      verboseIter = TRUE,
      allowParallel = TRUE
    ),
    tuneGrid = tune.grid
  )


# model -summaries----------------
print(model_wai)
par(mar = c(5, 7, 3, 1))
summary(model_wai$finalModel, las = 2, main = 'Pelorus Perna BRT mdoel')
perna_wai$gbm_log_perna <- predict(model_wai, newdata = perna_wai)

model_wai$results %>%
  as_tibble() %>%
  arrange(RMSE) %>%
  print(n = 10)

top_n(model_wai$results, 10, RMSE)
varImp(model_wai)

# save model--------------------
saveRDS(model_wai,'models/gbm_wai_caret.rds')
model_wai <- readRDS('models/gbm_wai_caret.rds')

# data predictions ---------
perna_wai$gbm_log_perna <- predict(model_wai, newdata = perna_wai)

# plot rel. imp. model----------
model_wai_dat <- 
  summary(model_wai) %>% 
  data.frame() %>% 
  mutate(var = fct_recode(var,
                          Year = "year",
                          Month = "month",
                          'Tidal range' = "tidal_range_lag0",
                          'Winds' = "TauCS_lag1",
                          'Temperature anomaly' = "temp_anom_lag1",
                          SOI = 'SOI_lag1'),
         var = fct_relevel(var, .x = 'Year', 'Month', 'Depth'),
         var = fct_reorder(var,desc(var))) %>% 
  write_csv('data/model_wai_relimp_dat.csv')


brt_plot_relimp_wai <- 
  ggplot(model_wai_dat,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  theme_javier() +
  labs(x = 'Predictor variables', y = 'Relative influence (%)') +
  ggtitle('A. Wainui') +
  scale_fill_viridis(discrete = T , guide = F)

brt_plot_relimp_wai

# partial effect plots---------------
plotmo(
  model_wai,
  # smooth.col = 2,
  # smooth.f = .5,
  ylim=NA,
  all1 = T,
  degree2 = F,
  do.par=TRUE,
  mfrow =c(1,7),
  nrug= 0,
  pmethod = 'plotmo',
  pt.col = 0,
  clip=FALSE,
  inverse.func=exp,
  caption = "A. Wainui",
  main = c("Depth","Month", "Year", "SOI", "Wind", "Tidal range", "Temperature anomaly" )
)


# Check residuals----------
plotres(model_wai)

# obs vs. predicted---------------
wai_plot_fitted_vs_obs <- 
  ggplot(perna_wai, aes(gbm_log_perna, log_perna)) + 
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1, lty = 2 , col = 2) +
  theme_light() +
  scale_x_continuous(limits = c(0,12), breaks = seq(0,12,2)) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,2)) +
  labs( x= 'Predicted Perna (log)', y = 'Observed Perna (log)') +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 3
  ) +
  theme_javier()
wai_plot_fitted_vs_obs

# obs vs. forcasted predictions------------------

# get the forcasted predictiosn##
test_wai_rows <- resamples_wai$test %>% flatten_dbl()
gbm_test_log_perna <- predict(model_wai, newdata = perna_wai[test_wai_rows,])

wai_plot_forcast <- 
  ggplot(perna_wai[test_wai_rows,], aes(gbm_test_log_perna, log_perna)) + 
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1, lty = 2 , col = 2) +
  theme_light() +
  scale_x_continuous(limits = c(0,12), breaks = seq(0,12,2)) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,2)) +
  labs( x= 'Forecasted Perna (log)', y = 'Observed Perna (log)') +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = ..rr.label..),
    parse = TRUE,
    size = 3
  ) +
  theme_javier()
wai_plot_forcast

wai_predictions_plot <- ggarrange(wai_plot_fitted_vs_obs, wai_plot_forcast)
wai_predictions_plot

# GOF----
library(hydroGOF)
gof(perna_wai$gbm_log_perna,perna_wai$log_perna)
gof(perna_wai[test_wai_rows,]$gbm_log_perna, perna_wai[test_wai_rows,]$log_perna)

# seasonal predictions ---------------
perna_wai$cat_perna <-
  cut(
    exp(perna_wai$gbm_log_perna),
    breaks = c(0, 500, 1.5e+03, 3e+5),
    labels =  c('Poor', 'Moderate', 'Good')
    # breaks = c(0, 1000, 2e+03, 5e+03, 10000, 30000),
    # labels =  c('Poor', 'Good', 'Very Good', 'Excellent', 'Ultimate')
  )

# save predictions with data-----
write_csv(perna_wai, 'data/perna_wai_predictions.csv')

wai_seasonal_predictions <- 
  ggplot(perna_wai, aes(
    x = month,
    y = exp(gbm_log_perna)
  )) +
  geom_point(alpha = .8, position = position_jitter(width = 0.3), aes(color = cat_perna)) +
  geom_smooth(method = 'loess', se = F, span = .5, color = 1) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 2e+03,5e+03, 10000)) +
  theme_bw(base_size = 12) +
  scale_color_viridis_d(name = '') +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  geom_hline(
    yintercept = c(1e+03, #Poor
                   2e+03, # Good
                   5e+03, #Very Good
                   1e+04),#Excellent
    lty = 2,
    col = 'gray70'
  ) +
  theme_javier() +
  labs(x = 'Month', y = italic(Perna)~m^-1, parse = T) +
  ggtitle('A. Wainui')

wai_seasonal_predictions

# Long- term log predictions plots  -----------
perna_wai %>% 
  gather(key, value, c(gbm_log_perna,log_perna)) %>% 
  ggplot(., aes(
  x = YearMonth,
  y = value, color = key
)) +
  geom_path() +
  theme_bw()

perna_wai %>% 
  gather(key, value, c(gbm_log_perna,log_perna)) %>% 
  ggplot(., aes(
    x = YearMonth,
    y = value, color = key
  )) +
  geom_path() +
  theme_bw(base_size = 10) 

# long term fitted values not log transformed-----
perna_pel %>% 
  mutate(gbm_perna  = exp(gbm_log_perna)) %>% 
  gather(key, value, c(gbm_perna,Perna)) %>% 
  ggplot(., aes(
    x = YearMonth,
    y = value, color = key
  )) +
  geom_line() +
  theme_bw()

perna_wai %>% 
  mutate(gbm_perna  = exp(gbm_log_perna)) %>% 
  gather(key, value, c(gbm_perna,Perna)) %>% 
  ggplot(., aes(
    x = YearMonth,
    y = value, color = key
  )) +
  geom_line() +
  theme_bw()
