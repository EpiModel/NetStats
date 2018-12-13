## ART-Net Study 2018     ##
## Creating Shiny Dataset ##
## 2018-12-13             ##

# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)

# Read in cleaning and datasets ---------

## Data Cleaning/Management Script
source("Analyses/Data_Cleaning.R", echo = FALSE)

l <- artnetLong
l$ONGOING <- as.numeric(l$p_ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

# Create and join datasets -------------

# Total
df <- l %>%
  filter(p_RAI == 1 | p_IAI == 1 | p_ROI == 1 | p_IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(totdegree = sum(ongoing2))
df4 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1 | p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(maintotdegree = sum(ongoing2))
df7 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1 | p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(castotdegree = sum(ongoing2))

df2 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(allaionlydegree = sum(ongoing2))
df3 <- l %>%
  filter(p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(alloionlydegree = sum(ongoing2))
df5 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(mainaionlydegree = sum(ongoing2))
df6 <- l %>%
  filter(p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(mainoionlydegree = sum(ongoing2))
df8 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(casaionlydegree = sum(ongoing2))
df9 <- l %>%
  filter(p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(casoionlydegree = sum(ongoing2))


# Create Shiny Datasets --------------
# Create merged dataframes
artnet4shiny <- artnet
artnet4shiny <- left_join(artnet4shiny, df, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df4, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df7, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df2, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df3, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df5, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df6, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df8, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df9, by = "AMIS_ID")
saveRDS(artnet4shiny, file = "Output/artnet4shiny.rda", compress = "xz")

artnetlong4shiny <- artnetLong
saveRDS(artnetlong4shiny, file = "Output/artnetlong4shiny.rda", compress = "xz")
