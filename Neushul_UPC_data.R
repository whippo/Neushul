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
# annotations.csv

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

# 2021-07-05 Script created
# 2021-07-07 downloaded most recent version of annotations to test scripts


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(vegan)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# SCUBA UPC
Neushul_Dive_UPC <- read_csv("data/collected/Neushul_Dive_UPC.csv", 
                             col_types = cols(transect = col_character()))
# correct tide heights for MLLW
Neushul_Dive_UPC <- Neushul_Dive_UPC %>%
  group_by(transect) %>%
  mutate(depth_MLLW = case_when(transect == '33' ~ depth_m + 0.4,
                                transect == '3' ~ depth_m + 0.45,
                                transect == '27' ~ depth_m + 0.45))
                                                 

# SNORKEL UPC
Neushul_Snorkel_UPC <- read_csv("data/collected/Neushul_Snorkel_UPC.csv", 
                                col_types = cols(transect = col_character()))
# correct tide heights for MLLW
Neushul_Snorkel_UPC <- Neushul_Snorkel_UPC %>%
  group_by(date) %>%
  mutate(depth_MLLW = case_when(date == '2021-07-06' ~ depth_m - 1.5,
                                date == '2021-07-10' ~ depth_m + 0.3))

# Neushul dominant cover from figures
Neushul_Xsection_data_extractions <- read_csv("data/extraction/Neushul_Xsection_data-extractions.csv", 
                                              col_types = cols(`table-number` = col_character()))


### Extract overlapping scuba transects
Neushul_UPC_overlap <- Neushul_Xsection_data_extractions %>%
  filter(transect %in% c('3', '27', '33'))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DATA ANALYSES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#   


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

