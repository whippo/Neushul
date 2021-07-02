#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Neushul Algal Survey Redux - Extraction Data                                   ##
# Data are current as of 2021-05-07                                              ##
# Data source: M. Neushul, Studies of Subtidal Marine Vegetation in Western      ##  
#                   Washington, Ecology, Vol. 48, No. 1 (Jan., 1967), pp. 83-94  ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2021-05-07                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY: Analysis of data from algal survey in San Juan Islands from 1967.


# Required Files (check that script is loading latest version):
# Neushul_Table_1.csv
# Neushul_Xsection_data-extractions.csv
# NeushulSiteDetailsPreplan.csv

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

# 2021-05-07 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(algaeClassify)
library(ggmap)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# site details extracted
site_deets <- read_csv("data/extraction/NeushulSiteDetailsPreplan.csv")

# data from table 1
Table_1 <- read_csv("data/extraction/Neushul_Table_1.csv")

# data from cross-section figures
x_sections <- read_csv("data/extraction/Neushul_Xsection_data-extractions.csv")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# extract list of genus/species from table 1 and check against database
algae_list <- spp_list_algaebase(Table_1, phyto.name = 'Name', lakename = "",
                   long = FALSE, write = TRUE)
write_csv(algae_list, "algae_list.csv")


# Back calculate values from correlation coefficients (figure 2)
cc_matrix <- read_csv("data/extraction/Neushul_Figure_2.csv")

# from the paper:
# sqrt(raw counts + 0.5) to make normal dist
# transformed again for mean of 0 and stdev of unity (1) -> x'i = (xi-x)/s (means and stdev
# are from Table 1)

brown_frequency <- Table_1 %>%
  select(Name, 'Mean frequency', SD) %>%
  filter(!(is.na(SD)))


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

