# load libraries
library(mapdata)
library(viridis)
library(tidyverse)
library(maptools)
library(gridExtra)
library(Cairo)
library(ggpubr)

##Map of both NZ islands
## add kaitaia and TOS labels-----------
x2 = c( 173, 173.1)
y2 = c(-42.35, -35.1)
name2 = c('Top of the South','Kaitaia')
labels_regions2 <- data.frame(x2, y2, name2)

# map low res-------
map.nz <- map_data(map = "nz")

nz_map <- 
  ggplot(map.nz, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = 'black', fill = 'transparent') + 
  theme_bw() + 
  ylab("") +  xlab("") +
  coord_map(xlim = c(166, 179), ylim = c(-47.5, -34)) +
  annotate(
    "rect",
    xmin = 172.7,
    xmax = 174.35,
    ymin = -41.35,
    ymax = -40.55,
    color = 'red',
    fill = 'transparent',
    size = .7
  ) +  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()# get rid of legend panel bg
  ) +
  geom_label(data = labels_regions2,
             aes(x = x2,
                 y = y2,
                 label = name2),
             inherit.aes = FALSE,
             size = 3)




nz_map

# read data
perna <- read_csv('data-raw/perna.csv')

# top of the south Island map his res-------------
nz <- map_data('nzHires')
source('H:/javiera/Stats/R folder/Scale_bar.R')
p <- 
  ggplot(nz) +
  geom_polygon(aes(x = long, y = lat, group = group),
               colour = "black",
               fill = 'grey80') + 
  theme_bw() +
  coord_map(xlim = c(172.5, 174.5), ylim = c(-40.4, -41.45))

# Map with stations----
site_map_data <- 
  perna %>%
  select(Lat,Long,Area, Region) %>% 
  group_by(Area) %>% 
  summarise_all(first) %>% 
  mutate(Type = "Spat")


##Detail of top of the South
# labels for regions--------
xx = c(174, 173.25, 173)
yy = c(-41.0, -41.1, -40.7)
name = c('Pelorus',
         'Tasman Bay',
         'Golden Bay')
labels_regions <- data.frame(xx, yy, name)


# labels for climate staions----------
x_clim <- c(-40.66600, -40.54700)
y_clim <- c(174.0010, 173.0000)
name_clim <- c('Stephens Island','Farewell Spit')
labels_clim <- data.frame(x_clim,y_clim,name_clim)


# labels for tide staions----------
x_tide <- c(-40.8218960, -41.275162)
y_tide <- c(172.895966, 173.770666)
name_tide <- c('Tarakohe','Havelock')
labels_tide <- data.frame(x_tide, y_tide, name_tide)


# labels for modelled sites-----------
x_mod <- c(173.05, 174.1)
y_mod <- c(-40.8,-41.1)
names_mod <- c('Wainui','Clova')
labels_mod <- data.frame(x_mod,y_mod,names_mod)


# maps of detailed sampling sites -----------------
map_TOS <-
  p +
  scaleBar(
    lon = 172.6,
    lat = -41.4,
    distanceLon = 10,
    distanceLat = 2,
    distanceLegend = 5,
    dist.unit = "km",
    arrow.length = 10,
    arrow.distance = 10,
    arrow.North.size = 5,
    legend.size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  geom_label(data = labels_regions, # region labels
             aes(x = xx,
                 y = yy,
                 label = name),
             size = 5) +
  geom_point(
    data = site_map_data,     # add sampling sites
    aes(y = Lat, x = Long),
    col = 1,
    fill = 2,
    shape = 21,
    size = 3
  ) +
  geom_label(data = labels_clim, # add climate stations labels
             aes(x = y_clim,
                 y = x_clim - 0.035,
                 label = name_clim),
             size = 3) +
  geom_point(
    data = labels_clim,     # add climate stations points
    aes(y = x_clim, x = y_clim),
    col = 1,
    fill = 3,
    shape = 24,
    size = 3
  ) +
  geom_point(
    data = labels_tide,     # add tide stations points
    aes(y = x_tide, x = y_tide),
    col = 1,
    fill = 5,
    shape = 25,
    size = 3
  ) +
  geom_label(data = labels_tide, # add climate stations labels
             aes(x = y_tide,
                 y = x_tide - 0.035,
                 label = name_tide),
             size = 3) +
  geom_label(data = labels_mod,
            aes(y = y_mod,
                 x = x_mod,
                 label = names_mod),
             size = 3) +
  geom_point(
    data =  filter(site_map_data, Area == "Wainui NE" | Area == 'Clova Bay'), 
    aes(y = Lat, x = Long),
    col = 1,
    fill = 4,
    shape = 22,
    size = 3
  )
  


map_TOS

# save two plots together with NZ as an inset-----

library(cowplot)
fig1_map <- ggdraw() +
  draw_plot(map_TOS) +
  draw_plot(nz_map, x = 0.753, y = .63, width = .3, height = .3)

ggsave(
  fig1_map,
    filename = 'figures/fig1_map.tiff',
    width = 22.5,
    height = 18,
    units = "cm",
    dpi = 600,
    compression = 'lzw'
  )

# ##Xmas rope photo
# library(png)
# library(grid)
# img <- readPNG("xmas_rope.png")
# g <- rasterGrob(img, interpolate = TRUE)
# 
# xmas  <- 
#   qplot(1:10, 1:10, geom = "blank") + annotation_custom(g) + theme_bw() +
#   xlab('') + ylab('') + theme(
#     axis.ticks = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   ) + annotate(
#     "text",
#     x = 9,
#     y = 9.5,
#     label = "D",
#     size = 6,
#     fontface = "bold",
#     color = ' black'
#   )




