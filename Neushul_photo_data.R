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
library(viridis)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# annotations from CoralNet dropcam
annotations <- read_csv("data/collected/annotations.csv", 
                        col_types = cols(Date = col_character(), 
                                         `Date annotated` = col_character(), 
                                         Deployment = col_character(), Drop_time = col_character(), 
                                         Transect = col_character()))
# correct depths to MLLW
annotations <- annotations %>%
  group_by(Date) %>%
  mutate(depth_MLLW = case_when(Date == '2021-07-01' ~ Depth - 0.9,
                                Date == '2021-07-07' ~ Depth - 1.2))

# export MLLW depths to csv
MLLW_depths_dropcam <- annotations %>%
  ungroup() %>%
  select(Transect, Deployment, depth_MLLW) %>%
  unite(ID, Transect, Deployment, sep = "_") %>%
  distinct(ID, depth_MLLW)
write_csv(MLLW_depths_dropcam, "MLLW_depths_dropcam.csv")

# add red, green, brown labels
annotations <- annotations %>%
  mutate(phylum = case_when(Label == "KELP" ~ "Brown",
                            Label == "CALL" ~ "Red",
                            Label == "CHCA" ~ "Red",
                            Label == "COCO" ~ "Brown",
                            Label == "CORM" ~ "Red",
                            Label == "DESP" ~ "Brown",
                            Label == "FRGA" ~ "Red",
                            Label == "FUDI" ~ "Brown",
                            Label == "MAFB" ~ "Brown",
                            Label == "MASP" ~ "Red",
                            Label == "NEFI" ~ "Brown",
                            Label == "NELU" ~ "Brown",
                            Label == "RAFI" ~ "Red",
                            Label == "SAMU" ~ "Brown",
                            Label == "UBS" ~ "Red",
                            Label == "ULVA" ~ "Green"))


# labelset from CoralNet
labelset <- read_csv("data/collected/labelset.csv")
# fix incorrect functional group
labelset[2,3] <- "Algae"
# rename Short Code to match Label, and remove 'Name' conflict
labelset <- rename(labelset, Label = "Short Code")
labelset <- rename(labelset, Cat_Name = "Name")

# annotations without obscured category

anno_noobs <- annotations %>%
  filter(Label != "OBSC")

# sum phyla by photo
anno_noobs$count <- 1

#join function groups to labels
anno_noobs <- left_join(anno_noobs, labelset, by = "Label")

# ranked number of labels per photo
anno_ranks <- anno_noobs %>%
  group_by(Name, depth_MLLW, Transect, Label) %>%
  summarise(sum(count))
names(anno_ranks)[names(anno_ranks)=="sum(count)"] <- "count"

# short summed phyle per photo
anno_noobs_short <- anno_noobs %>%
  group_by(Name, Transect, depth_MLLW, phylum) %>%
  summarise(sum(count))
names(anno_noobs_short)[names(anno_noobs_short)=="sum(count)"] <- "count"



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DATA ANALYSES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#nMDS of photo groups 

# make data wide
anno_wide <- anno_ranks %>%
  pivot_wider(names_from = Label, values_from = count, values_fill = 0)

photo_mds <- metaMDS(anno_wide[,4:22], distance = "altGower")
photo_mds_points <- photo_mds$points
photo_mds_points <- data.frame(photo_mds_points)
plot_data <- data.frame(anno_wide[,3], photo_mds_points)
ggplot(plot_data, aes(x=MDS1, y=MDS2, fill = Transect)) + 
  theme_classic() +
  geom_point(aes(x=MDS1, y=MDS2, color = Transect), size = 4) +
  scale_color_viridis(discrete = TRUE)

# no good solution

# MDS at the transect level
anno_wide$depth_MLLW <- as.character(anno_wide$depth_MLLW)
anno_trans_wide <- anno_wide %>%
  group_by(Transect) %>%
  summarise(across(where(is.numeric), sum))

trans_mds <- metaMDS(anno_trans_wide[,2:20], distance = "altGower")
trans_mds_points <- trans_mds$points
trans_mds_points <- data.frame(trans_mds_points)
plot_data <- data.frame(anno_trans_wide[,1], trans_mds_points)
plot_data$Transect <- factor(plot_data$Transect, levels = c("31", "32", "34", "1", "3", "7", "10", "13", "33", "18"))
ggplot(plot_data, aes(x=MDS1, y=MDS2, fill = Transect)) + 
  theme_classic() +
  geom_point(aes(x=MDS1, y=MDS2, color = Transect), size = 4) +
  scale_color_viridis(discrete = TRUE)

# mean depths of transects
anno_noobs %>%
  group_by(Transect) %>%
  summarise(mean_depth = mean(depth_MLLW)) %>%
  ungroup() %>%
  group_by(mean_depth) %>%
  arrange(desc(mean_depth))

psych::pairs.panels(anno_noobs)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIGURES                                                                      ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# FIGURE A - total number of each category observed across all photos
ggplot(annotations, aes(x = forcats::fct_infreq(Label))) +
  geom_bar()

# FIGURE B - mean amount of red, brown, and greens detected
ggplot(filter(anno_noobs_short, phylum != "NA"), aes(x = phylum, y = count)) +
  geom_boxplot()

# Figure C - photo phylum composition by depth
ggplot(filter(anno_noobs_short, phylum != "NA"), aes(x = depth_MLLW, y = count, color = phylum)) +
  geom_point(size = 4) +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0, end = 0.9) +
  theme_classic()

# Figure D - label by depth
ggplot(anno_noobs, aes(x = depth_MLLW, y = count, fill = Label)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE) +
  theme_classic()

# FIGURE E - function cover by depth
ggplot(anno_noobs, aes(x = depth_MLLW, y = count, fill =  `Functional Group`)) +
  geom_col() +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  theme_classic()

# FIGURE F - 
ggplot(filter(anno_noobs, aes(x = depth_MLLW, y = count, color = Label))) +
  geom_point(size = 4) +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0, end = 0.9) +
  theme_classic()

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

