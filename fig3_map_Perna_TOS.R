# load libraries
library(mapdata)
library(viridis)
library(tidyverse)

# read data
perna <- read_csv('data-raw/perna.csv')

# top of the south Island map
nz <- map_data('nzHires')
p <- 
  ggplot(nz) +
  geom_polygon(aes(x = long, y = lat, group = group),
  colour = "black",
  fill = 'grey80') + 
  theme_bw() +
  coord_map(xlim = c(172.5, 174.5), ylim = c(-40.49, -41.5))

# Map with stations----
site_map_data <- 
  perna %>%
  select(Lat,Long,Area, Region) %>% 
  group_by(Area) %>% 
  summarise_all(first) %>% 
  mutate(Type = "Spat")


p +
  ylab('Longitude') + xlab('Latitude') +
  geom_point(aes(
    x = Long,
    y = Lat,
 ), data = site_map_data,
  color =2,
  position = position_jitter(width = 0.05)) +
  scale_size_area(max_size = 10, breaks = c(0, 100, 250,500,1000,2500, 5000, 6000)) +
  annotate(x = 173.3, y = -41, geom = 'text', label ='Tasman Bay') +
  annotate(x = 173, y = -40.7, geom = 'text', label ='Golden Bay') +
  annotate(x = 174.2, y = -40.85, geom = 'text', label ='Pelorus Sounds') +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "#00ffffff", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    breaks = c(0, 100, 250,500,1000,2500, 5000, 6000)
  ) +
  guides(fill = guide_legend(), size = guide_legend())




# Map with mean Perna abundance----
# summarise Perna data 
perna_mean <-
  perna %>% 
  group_by(Lat, Long) %>% 
  summarise_at(vars(Perna), mean, na.rm = T)

# map mean abundance
map <- 
  p +
  ylab('Longitude') + xlab('Latitude') +
  geom_point(aes(
    x = Long,
    y = Lat,
    size = Perna,
    fill = Perna,
  ), data = perna_mean,
  color =1,
  shape = 21,
  position = position_jitter(width = 0.05)) +
  scale_size_area(max_size = 10, breaks = c(0, 100, 250,500,1000,2500, 5000, 6000)) +
  annotate(x = 173.3, y = -41, geom = 'text', label ='Tasman Bay') +
  annotate(x = 173, y = -40.7, geom = 'text', label ='Golden Bay') +
  annotate(x = 174.2, y = -40.85, geom = 'text', label ='Pelorus Sounds') +
  scale_fill_gradientn(
    colours = c("red", "yellow", "green", "#00ffffff", "darkblue"),
    values = c(1.0, 0.5, 0.3, 0.2, 0.1, 0),
    breaks = c(0, 100, 250,500,1000,2500, 5000, 6000)
  ) +
  guides(fill = guide_legend(), size = guide_legend())

Cairo::CairoWin();map

# save plots together
ggsave(
  map,
  filename = 'figures/map_mean_perna.tiff',
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = 'lzw'
)


## Spatial summary tables-----

perna %>% 
  group_by(Region) %>% 
  summarise_at(vars(Perna), se)


perna %>% 
  group_by(Area) %>% 
  summarise_at(vars(Perna), mean) %>% 
  arrange(desc(Perna))

perna %>% 
  group_by(Area) %>% 
  filter(Region =='PLS') %>% 
  summarise_at(vars(Perna), mean) %>% 
  arrange(desc(Perna))


perna %>% 
  filter(Area =='Skiddaw')
