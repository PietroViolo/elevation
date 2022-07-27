#---------------------------------------------------------------------------#
# Nom : elevation.r                                                         #
# Description : Tracks elevation of a territory using elevatr               #
# Auteur : Pietro Violo                                                     #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))
options(scipen=999)

# Library
library(elevatr)
library(rayshader)
library(tidyverse)
library(ggnewscale)

# South america coordinates box

# -83.63 to -29.27
# -57.23 to 13.22

# Sample 1000 points

lon <- seq(from = -83.60, to = -29.30, by = 0.05)

lat <- seq(from = -57.20, to = 13.20, by = 0.05)

south_america <- expand.grid(lon, lat)

colnames(south_america) <- c("x", "y")

projection <- "EPSG:4326"

  
south_america_elevation <- get_elev_point(
    south_america,
    units = "meters",
    src = c("aws"),
    prj = projection,
    z = 7)


south_america$elevation <- south_america_elevation$elevation

gradient <- c("#1d3557",
              "#50877c",
              "#aac58e",
              "#d1ca9d",
              "#cbb982",
              "#e09a30",
              "#b47a01",
              "#9f5c29",
              "#f94144",
              "#ffffff")

breaks <- c(-8000,0,200,500,1000,2000,3000,4000,5000,6000)

south_america <- south_america %>% mutate(elevation = ifelse(elevation < -100,-100,elevation))



# south_america <- south_america %>% mutate(elevation = case_when(elevation < 0 ~ -8000,
#                                                                 elevation >=0 & elevation < 200 ~ 0,
#                                                                 elevation >=200 & elevation < 500 ~ 200,
#                                                                 elevation >=500 & elevation < 1000 ~ 500,
#                                                                 elevation >=1000 & elevation < 2000 ~ 1000,
#                                                                 elevation >=2000 & elevation < 3000 ~ 2000,
#                                                                 elevation >=3000 & elevation < 4000 ~ 3000,
#                                                                 elevation >=4000 & elevation < 5000 ~ 4000,
#                                                                 elevation >=5000 & elevation < 6000 ~ 5000,
#                                                                 elevation >=6000 & elevation < 7000 ~ 6000),
#                                           elevation = factor(elevation, levels = order))

pp <- ggplot(south_america,aes( x = x, y =y, fill = elevation)) +
  geom_raster() +
  scale_fill_gradientn(values = scales::rescale(c(
                          -100,0,
                           1,200,
                           201,500,
                           501,1000,
                           1001,2000,
                           2001, 3000,
                           3001, 4000,
                           4001, 5000,
                           5001, 6000)),
                       space = "Lab",
                       colors = gradient,
                       breaks = breaks,
                       limits = c(-100,6000)) +
  theme(legend.position="right",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#1d3557", color = NA),
        panel.background = element_rect(fill = "#1d3557", color = NA), 
        legend.background = element_rect(fill = "#1d3557", color = NA),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.key.size = unit(2,"cm"))






# Plot

#par(mfrow = c(1, 2))
#plot_gg(pp, width = 5, height = 7, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 9, height = 12, scale = 100, multicore = TRUE, windowsize = c(2000, 2000),
        shadow_intensity = 0.1,
        raytrace = TRUE)

render_camera(fov = 80, zoom = 0.5, theta = 0, phi = 90)



png("south_america_elevation.png", res = 300, width = 5000, height = 5000)

render_snapshot()

dev.off()





#### For Europe #####


lon <- seq(from = -11.69, to = 41.92, by = 0.2)

lat <- seq(from = 35.46, to = 71.58, by = 0.2)

western_europe <- expand.grid(lon, lat)

colnames(western_europe) <- c("x", "y")

projection <- "EPSG:4326"


western_europe_elevation <- get_elev_point(
  western_europe,
  units = "meters",
  src = c("aws"),
  prj = projection,
  z = 7)


western_europe$elevation <- western_europe_elevation$elevation

gradient <- c("#1d3557",
              "#50877c",
              "#aac58e",
              "#cbb982",
              "#e09a30",
              "#f94144")


western_europe <- western_europe %>% mutate(elevation = ifelse(elevation < -500,-500,elevation))

breaks <- c(-500,-100,-50,0,50,100,200,300,500,1000,1500,2000,2500,3000, 5000)





gradient <- c("#03045E", #bleu
              "#023E8A", #vert
              "#3a5a40",
              "#588157", #jaune
              "#fff3b0",
              "#a98467",
              "#c32f27",
              "#ffffff")


pp <- ggplot(western_europe,aes( x = x, y =y, fill = elevation)) +
  geom_raster() +
  scale_fill_gradientn(values = scales::rescale(c(
    -500,-101,
    -100,-51,
    -50, 0,
    1, 50,
    51, 100,
    101, 200,
    201,300,
    301, 500,
    501,750,
    751, 1000,
    1001,1500,
    1501, 2000,
    2001, 2500,
    2501,3000,
    3001, 4500)),
    space = "Lab",
    colors = gradient,
    breaks = breaks,
    limits = c(-500,4500)) +
  theme(legend.position="right",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#1d3557", color = NA),
        panel.background = element_rect(fill = "#1d3557", color = NA), 
        legend.background = element_rect(fill = "#1d3557", color = NA),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.key.size = unit(4,"cm"),
        legend.text = element_text(color = "white"))


plot_gg(pp, width = 18.75, height = 15, scale = 150, multicore = TRUE, windowsize = c(2300, 3000),
        shadow_intensity = 0.15,
        raytrace = TRUE)





render_camera(fov = 80, zoom = 0.5, theta = 0, phi = 90)



png("western_europe_elevation.png", res = 400, width = 3000, height = 2000)

render_snapshot()

dev.off()



