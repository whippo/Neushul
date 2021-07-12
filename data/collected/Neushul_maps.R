#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Neushul Algal Survey Redux - Extraction Data                                   ##
# Data are current as of 2021-07-05                                              ##
# Data source: M. Neushul, Studies of Subtidal Marine Vegetation in Western      ##  
#                   Washington, Ecology, Vol. 48, No. 1 (Jan., 1967), pp. 83-94  ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2021-07-07                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY: Photo quadrat data analysis.


# Required Files (check that script is loading latest version):
# Neushul_Dropcam_Deployments.csv
# NeushulSiteDetailsPreplan.csv
# Snorkel_GPS.csv

# Associated Scripts:
# none

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2021-07-12 script created



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(vegan)
library(ggpubr)
library(ggmap)
library(algaeClassify)
library(vistime)
library(plotly)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# site details extracted
site_deets <- read_csv("data/extraction/NeushulSiteDetailsPreplan.csv")
site_deets <- site_deets %>%
  filter(site_number %in% c("27", "3", "33"))
drop_points <- read_csv("data/collected/Neushul_Dropcam_Deployments.csv", 
                        col_types = cols(depth_m = col_double(), 
                                         transect = col_character()))
Dive_GPS <- read_csv("data/collected/Dive_GPS.csv", 
                     col_types = cols(transect = col_character()))
Snorkel_GPS <- read_csv("data/collected/Snorkel_GPS.csv")


FHL <- get_stamenmap(bbox = c(left = -123.010, bottom = 48.534, right = -122.995, top = 48.544), maptype = "watercolor", crop = TRUE, force = FALSE, zoom = 16)
ggmap(FHL) +
  geom_line(aes(x = lon, y = lat), data = Snorkel_GPS, alpha = .5, color = "blue", size = 4) +
  geom_line(aes(x = lon, y = lat), data = filter(Dive_GPS, transect == "27"), alpha = 1, color = "pink", size = 6) +
  geom_line(aes(x = lon, y = lat), data = filter(Dive_GPS, transect == "3"), alpha = 1, color = "pink", size = 6) +
  geom_line(aes(x = lon, y = lat), data = filter(Dive_GPS, transect == "33"), alpha = 1, color = "pink", size = 6) +
  geom_point(aes(x = lon, y = lat), data = drop_points,
             alpha = .5, color="darkred", size = 3)

bigmap <-  get_stamenmap(bbox = c(left = -123.018, bottom = 48.53, right = -122.99, top = 48.547), maptype = "watercolor", crop = FALSE, force = FALSE, zoom = 16)
bigmap

ggmap(bigmap)

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

