# load libraries, functions and data
library(tidyverse)

# standard error function
se <- function(x) sd(x, na.rm = TRUE)/sqrt(length(x))
source('H:/javiera/Stats/R folder/theme_javier.R')
perna <- readRDS('data/spat_all.RDS')

##summarise data by year
longterm_data <-
  perna %>%
  group_by(year,Region) %>%
  summarise(mean = mean(Perna, na.rm = T),
            se = se(Perna),
            n = n()) %>%
  mutate(
    ci = qt(1 - (0.05 / 2), n - 1) * se,
    lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se,
    lower_se = mean - se,
    upper_se = mean + se
  ) %>%
  filter(Region != 'QCS') %>%  
  ungroup() %>% 
  mutate(Region = fct_recode(
    Region,
    "Golden Bay" = "GB",
    "Tasman Bay" = "TB",
    "Pelorus" = "PLS"
  ),
  Region = fct_relevel(Region, "Golden Bay",  "Tasman Bay"))

# summarise data by month
seas_data <-
  perna %>%
  group_by(month, Region) %>%
  summarise(mean = mean(Perna, na.rm = T),
            se = se(Perna),
            n = n()) %>%
  mutate(
    lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se,
    lower_se = mean - se,
    upper_se = mean + se,
    mon = month.abb[month]
  ) %>%
  filter(Region != 'QCS') %>%
  mutate(Region = fct_recode(
    Region,
    "Golden Bay" = "GB",
    "Tasman Bay" = "TB",
    "Pelorus" = "PLS"
  ),
  Region = fct_relevel(Region, "Golden Bay",  "Tasman Bay"))

# long term plot 
p1 <- 
  ggplot(longterm_data, aes(x = year)) +
  labs(x = 'Year', y = italic(Perna)~m^-1, parse = T) +
  geom_path(aes(y = mean, group = 1, color = Region)) +
  theme_javier() +
  geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = 0.3) +
  # geom_smooth(aes(y = mean), se = F, lty = 2, color = 1) +
  facet_wrap( ~ Region) +
  scale_x_continuous(breaks = seq(1992,2018,4)) +
  scale_color_discrete(guide = F)


# Seasonal plot
p2 <- 
  ggplot(seas_data, aes(x = month)) +
  geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = .3) +
  labs(x = 'Month', y = italic(Perna)~m^-1, parse = T) +
  theme_javier() +
  geom_path(aes(y = mean, group = 1, color = Region)) +
  # geom_smooth(aes(y = mean), se = F, lty = 2, color = 1) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap( ~ Region) +
  scale_color_discrete(guide = F)

# save plots together
library(ggpubr)
ggsave(
  ggarrange(p1, p2, nrow = 2,labels = 'AUTO',common.legend = F, legend = 'right'),
  filename = 'figures/times_series.tiff',
  width = 20,
  height = 12,
  units = "cm",
  dpi = 1200,
  compression = 'lzw'
)

