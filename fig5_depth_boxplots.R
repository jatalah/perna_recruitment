# load libraries, functions and data -------
library(tidyverse)
perna <- readRDS('data/spat_all.RDS')

# standard error function
se <- function(x) sd(x, na.rm = TRUE)/sqrt(length(x))
source('H:/javiera/Stats/R folder/theme_javier.R')

#Depth patterns exploration -------
perna %>% 
  filter(Region == 'PLS') %>% 
  ggplot(., aes(Depth, Perna, color = Area))+
  geom_point(alpha = .6,  position = position_jitter(width = 0.1)) +
  theme_javier() +
  coord_flip() +
  scale_x_reverse(breaks = seq(1,20,1)) 
  # scale_y_log10() +
  # geom_smooth(se = F)


unique(perna$Depth)  

# depth data summary ------
depth_data <-
  perna %>%
  select(Region, Perna, Depth) %>%
  filter(Region !="GB"|Depth<6) %>% 
  filter(Region !="TB"|Depth>3) %>% 
  mutate(depth_cat = cut(
    .$Depth,
    labels = seq(2, 14, 2),
    breaks = seq(1, 15, 2)
  )) %>%
  filter(Region != 'QCS') %>%
  drop_na() %>%
  mutate(Region = fct_recode(
    Region,
    "Golden Bay" = "GB",
    "Tasman Bay" = "TB",
    "Pelorus" = "PLS"
  ),
  Region = fct_relevel(Region, "Golden Bay",  "Tasman Bay"))

# create boxplot------
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}


depth_boxplot <- 
  ggplot(depth_data, aes(
    x = factor(fct_rev(depth_cat)),
    y = Perna,
    color = Region
  )) +
  geom_boxplot() +
  facet_wrap(~ Region) +
  theme_javier() +
  coord_flip() +
  scale_y_log10(label = scales::comma) +
  labs(x = 'Depth (m)',
       y = italic(Perna) ~ m ^ -1,
       parse = T) +
  scale_color_discrete(guide = F) +
  stat_summary(
    fun.data = give.n,
    geom = "text",
    position = position_nudge(x = .5),
    size = 3
  )
depth_boxplot

# save plot-----
ggsave(
  depth_boxplot,
  filename = 'figures/depth_plots.tiff',
  width = 20,
  height = 12,
  units = "cm",
  dpi = 1200,
  compression = 'lzw'
)

# get n by depth and region---
depth_data %>% 
  group_by(depth_cat, Region) %>% 
  summarise(n()) %>% 
  arrange(Region)





ggplot(depth_data, aes(
  x = factor(fct_rev(depth_cat)),
  y = Perna,
  color = Region
)) +
  geom_violin() +
  facet_wrap(~ Region) +
  theme_javier() +
  coord_flip() +
  scale_y_log10(label = scales::comma) +
  labs(x = 'Depth (m)',
       y = italic(Perna) ~ m ^ -1,
       parse = T) +
  scale_color_discrete(guide = F) +
  stat_summary(
    fun.data = give.n,
    geom = "text",
    # position = position_nudge(x = .5),
    size = 3
  )
