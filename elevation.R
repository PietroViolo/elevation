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

lon <- seq(from = -83.63, to = -29.27, by = 0.1)

lat <- seq(from = -57.23, to = 13.22, by = 0.1)

south_america <- expand.grid(lon, lat)

colnames(south_america) <- c("x", "y")

projection <- "EPSG:4326"

  
south_america_elevation <- get_elev_point(
    south_america,
    units = "meters",
    src = c("aws"),
    prj = projection)


south_america$elevation <- south_america_elevation$elevation

gradient <- c("#277da1", "#4d908e", "#90be6d", "#f9c74f", "#f8961e",
              "#f94144")

pp <- ggplot(south_america,aes( x = x, y =y, fill = elevation, width = 0.5, height = 0.5)) +
  geom_raster()  +
  theme(legend.position="right",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
  scale_fill_gradientn(
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill",
    colors = gradient,
    limits = c(-8000, 6000),
    breaks = c(-8000,0, 100, 200, 500, 6000),
    values = scales::rescale(c(-8000,0,
                               1,300,
                               301,500,
                               501,2000,
                               2001,6000))
    
  ) 
  
pp

#breaks = c(0.5,1,2,4),
#limits = c(0.5,4),

# Plot

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 7, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 7, scale = 300, multicore = TRUE, windowsize = c(2000, 1600))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)
