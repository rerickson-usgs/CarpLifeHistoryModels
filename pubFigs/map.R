## packages used
library(maps) # used for state outlines
library(tidyverse) # used for data manipulation, also includes ggplot2 for plotting
library(ggrepel) # used for geom_label_repel to dodge text
library(rgdal) ## used to readin data
## devtools::install_github("3wen/legendMap")
library(legendMap) ## Used for north arrow and distance key
library(gridExtra) ## used for insets

## Extract out state outlines

All_states_outline <-
    as_tibble(data.frame(with(maps::map('state'), cbind(x,y))))
States_outline <-
    data.frame(with(maps::map('state', c("Illinois", "West Virginia",
                                                       "Indiana", "Kentucky")), cbind(x,y))) %>%
    as_tibble()

## Read in River sampling locations
River_Key <- read_csv("RiverKey.txt")

## Rivers
lines.rivers <- sf::st_read('./main_river_Stems/IL_Ohio_Miss_MainRivers.shp')


riverPlot <-
    ggplot(lines.rivers) +
    geom_path(data = States_outline, aes(x = x, y= y))  +
    geom_sf(color = 'blue') +
    coord_sf( xlim = c(-93,-78), ylim = c(36,43)) +
    theme_minimal()  +
    scale_bar(lon = -93, lat = 42.6,
              distance_lon = 100, distance_lat = 10, distance_legend = 50,
              dist_unit = "km", orientation = TRUE,
              arrow_length = 100, arrow_distance = -700, arrow_north_size = 5) +
    geom_point(data = River_Key, aes(x = x, y = y), color = 'red', size = 3) +
    geom_label_repel(data = River_Key, aes(x = x, y = y, label = Pool), size =2.5,
                     box.padding = 0.45, point.padding = 0.20) +
    ylab("Latitude") +
    xlab("Longitude")
print(riverPlot)

ggsave("riverPlot.pdf", width = 9, height = 6)
ggsave("riverPlot.jpg", width = 9, height = 6)

xmin <-    States_outline %>%
    summarize(min(x, na.rm = TRUE)) %>%
    `[[`(1)
ymin <-    States_outline %>%
    summarize(min(y, na.rm = TRUE)) %>%
    `[[`(1)
xmax <-    States_outline %>%
    summarize(max(x, na.rm = TRUE)) %>%
    `[[`(1)
ymax <-    States_outline %>%
    summarize(max(y, na.rm = TRUE)) %>%
    `[[`(1)

bbPlot <- tibble(
    x = c(xmin, xmin, xmax, xmax, xmin),
    y = c(ymin, ymax, ymax, ymin, ymin))
bbPlot

## insert plot
All_states_outline

USMap <- ggplot(lines.rivers) +
    geom_path(data = All_states_outline, aes(x = x, y = y),) +
    ylab(NULL) +
    xlab(NULL) +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
          ) +
    geom_polygon(data = bbPlot, aes(x = x, y = y), fill = NA, color = 'red', size = 0.6)


## Plot inset map
pdf(file = "riverPlotWithInset.pdf", width = 8/1.15, height = 6/1.15)
grid.newpage()
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.35, height = 0.25, x = 0.78, y = 0.77) #plot area for the inset map
print(riverPlot,vp=v1)
print(USMap,vp=v2)
dev.off()


## Plot inset map
jpeg(file = "riverPlotWithInset.jpeg", width = 7, height = 5)
grid.newpage()
v1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.35, height = 0.25, x = 0.78, y = 0.77) #plot area for the inset map
print(riverPlot,vp=v1)
print(USMap,vp=v2)
dev.off()
