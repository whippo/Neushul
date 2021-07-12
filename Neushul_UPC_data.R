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
# correct wrong distances
Neushul_Dive_UPC <- Neushul_Dive_UPC %>%
  mutate(distance_ft = case_when(transect == '33' ~ distance_ft,
                              transect == '3' ~ distance_ft,
                              transect == '27' ~ distance_ft + 110))
                                                 

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

# export corrected depths to csv
MLLW_depths_snorkscub <- Neushul_Dive_UPC %>%
  select(date, scubaSnorkel, transect, distance_ft, depth_MLLW) 
MLLW_depths_snorkscub <- bind_rows(MLLW_depths_snorkscub, Neushul_Snorkel_UPC[,c(1, 4, 6, 11, 17)])  
write_csv(MLLW_depths_snorkscub, "MLLW_depths_snorkscub.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DATA ANALYSES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#   join Neushul data with dive surveys
dive_1 <- Neushul_Dive_UPC %>%
  select(transect, distance_ft, depth_MLLW, substrate, alga, associates) %>%
  separate(alga, c("alga1", "alga2", "alga3", "alga4"), sep = "\\|") %>%
  separate(associates, c('ass1', 'ass2', 'ass3', 'ass4', 'ass5'), sep = "\\|") %>%
  unite("ID", transect, distance_ft, sep = "_", remove = FALSE)

neush_1 <- Neushul_UPC_overlap %>%
  select(transect, segment, depth, substrate, `table-ID`) %>%
  separate(segment, "distance_ft") %>%
  mutate(distance_ft = as.numeric(distance_ft) + 5) %>%
  separate(depth, "depth_MLLW" ) %>%
  separate(substrate, 'substrate') %>%
  unite("ID", transect, distance_ft, sep = "_", remove = FALSE)

joined_UPC <- select(dive_1, ID:alga1) %>%
  full_join(neush_1, by = "ID")


# add braoder categories for comparison

joined_UPC <- joined_UPC %>%
  mutate(cover_2021 = case_when(alga1 == "none" ~ "none",
                         alga1 == "CALL"  ~ "red filamentous",
                         alga1 == "NEFI" ~ "N. fimbriatum",
                         alga1 == "NEFI " ~ "N. fimbriatum",
                         alga1 == "lacy red #1 " ~ "red filamentous",
                         alga1 == "SALANI" ~ "Laminariales spp.",
                         alga1 == "SALANI " ~ "Laminariales spp.",
                         alga1 == "COCO " ~ "C. costata",
                         alga1 ==  "big red blade #1 " ~ "red blade",
                         alga1 == "CORM" ~ "red blade",
                         alga1 == "FRGA" ~ "red blade",
                         alga1 == "large red blade" ~ "red blade",
                         alga1 == "feathered red" ~ "red filamentous",
                         alga1 == "MASP " ~ "red blade",
                         alga1 == "MASP" ~ "red blade",
                         alga1 == "ULVA" ~ "Ulva spp.",
                         alga1 == "“red desmarestia”" ~ "red blade",
                         alga1 == "DESP" ~ "Desmarestia spp.",
                         alga1 == "SAGA" ~ "red filamentous",
                         alga1 == "SAGA " ~ "red filamentous",
                         alga1 == "SPPE" ~ "red blade",
                         alga1 == "NELU " ~ "N. luetkeana",
                         alga1 == "SAMU" ~ "S. muticum"))  
joined_UPC <- joined_UPC %>%
  mutate(cover_1963 = case_when(`table-ID` == "Crustose red algae" ~ "crustose red",
                                 `table-ID` == "Desmarestia spp."  ~ "Desmarestia spp.",
                                 `table-ID` == "Callophyllis spp." ~ "red filamentous",
                                 `table-ID` == "Fryeela gardneri" ~ "red blade",
                                 `table-ID` == "Agarum fimbriatum" ~ "N. fimbriatum", 
                                 `table-ID` == "Laminaria spp." ~ "Laminariales spp.",
                                 `table-ID` == "Ulvoids" ~ "Ulva spp.",
                                 `table-ID` == "Alaria spp." ~ "Laminariales spp.",
                                 `table-ID` == "Fuscus sp." ~ "Fucus spp.",
                                 `table-ID` == "Costaria costata" ~ "C. costata",
                                 `table-ID` == "Agarahiella coulteri" ~ "red filamentous",
                                 `table-ID` == "Gloiopeltis furcata" ~ "red filamentous",
                                 `table-ID` == "Diatom crust" ~ "brown filamentous",
                                 `table-ID` == "Zostera marina" ~ "Z. marina",
                                 `table-ID` == "Rhodymenia pertusa" ~ "red blade",
                                 `table-ID` == "Hydroids" ~ "none"))  
# filter out non-overlapping transect portions
filtered_UPC <- joined_UPC %>%
  filter(!is.na(cover_1963) & !is.na(cover_2021))
filtered_UPC$one <- 1
#assign trasect number to every occurence
filtered_UPC <- filtered_UPC %>%
  separate(ID, "transect", remove = FALSE)
# short summed occurences per transect
joined_UPC_short <- filtered_UPC %>%
  group_by(transect, cover_1963, cover_2021, one) %>%
  summarise(sum(one))
names(joined_UPC_short)[names(joined_UPC_short)=="sum(one)"] <- "count"



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIGURES                                                                      ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ggplot(joined_UPC_short, aes(x = cover_1963, y = count)) +
  geom_boxplot()
ggplot(joined_UPC_short, aes(x = cover_2021, y = count)) +
  geom_boxplot()

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

