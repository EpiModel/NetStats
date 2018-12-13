## ART-Net Study 2018   ##
## Table 5 Matrices     ##
## 2018-12-13           ##

# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)

# Read in cleaning and datasets ---------

## Data Cleaning/Management Script
source("Analyses/Data_Cleaning.R", echo = FALSE)

# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$p_ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

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

# Create merged dataframes
artnet2 <- left_join(artnet, df, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df4, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df7, by = "AMIS_ID")

# Table 5 -  Partnership matrix --------------
matrix <- artnet2
table(matrix$maintotdegree, matrix$castotdegree, useNA = "always")
matrix$maintotdegree[which(artnet2$maintotdegree > 2)] <- 2
matrix$castotdegree[which(artnet2$castotdegree > 3)] <- 3
table(matrix$maintotdegree, matrix$castotdegree, useNA = "always")
prop.table(table(matrix$maintotdegree, matrix$castotdegree, useNA = "always"))
table5 <- rbind(cbind(length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 0)),
                      100 * round(length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 0)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 1)),
                      100 * round(length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 1)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 2)),
                      100 * round(length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 2)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 3)),
                      100 * round(length(which(matrix$maintotdegree == 0 & matrix$castotdegree == 3)) / length(matrix$maintotdegree), 3)),
                cbind(length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 0)),
                      100 * round(length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 0)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 1)),
                      100 * round(length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 1)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 2)),
                      100 * round(length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 2)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 3)),
                      100 * round(length(which(matrix$maintotdegree == 1 & matrix$castotdegree == 3)) / length(matrix$maintotdegree), 3)),
                cbind(length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 0)),
                      100 * round(length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 0)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 1)),
                      100 * round(length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 1)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 2)),
                      100 * round(length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 2)) / length(matrix$maintotdegree), 3),
                      length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 3)),
                      100 * round(length(which(matrix$maintotdegree == 2 & matrix$castotdegree == 3)) / length(matrix$maintotdegree), 3)))

colnames(table5) <- c("0 Casual Partners N", "0 Casual Partners %",
                      "1 Casual Partner N", "1 Casual Partner %",
                      "2 Casual Partners N", "2 Casual Partners %",
                      "3 Casual Partners N", "3 Casual Partners %")
rownames(table5) <- c("0 Main Partners", "1 Main Partner", "2 Main Partners")
write.csv(table5, file = "Output/table5.csv")

# Race/ethnicity
tot <- artnetLong %>% count(race.cat, partracecat)
a <- table(artnet$race.cat)
egorace <- cbind(rbind(sum(artnet$race.cat == "black"),
                       sum(artnet$race.cat == "hispanic"),
                       sum(artnet$race.cat == "other"),
                       sum(artnet$race.cat == "white")),
                 rbind(rbind(tot$n[1:5]),
                       rbind(tot$n[6:10]),
                       rbind(tot$n[11:15]),
                       rbind(tot$n[16:20])))
colnames(egorace) <- c("Ego N",
                       "Black Part",
                       "Hispanic Part",
                       "Other Part",
                       "White Part",
                       "NA")
rownames(egorace) <- c("Black Ego",
                       "Hispanic Ego",
                       "Other Ego",
                       "White Ego")
View(egorace)

# HIV status
tot2 <- artnetLong %>% count(hiv, partstatus)
egohiv <- cbind(rbind(sum(artnet$hiv == 0),
                      sum(artnet$hiv == 1)),
                rbind(
                  cbind(rbind(tot2$n[c(1)]),
                        rbind(tot2$n[c(2)]),
                        rbind(tot2$n[c(3)]),
                        rbind(tot2$n[c(4)])),
                  cbind(rbind(tot2$n[c(5)]),
                        rbind(tot2$n[c(6)]),
                        rbind(tot2$n[c(7)]),
                        rbind(tot2$n[c(8)]))))
colnames(egohiv) <- c("Ego N",
                      "Negative Part",
                      "Positive Part",
                      "Unknown Part",
                      "NA")
rownames(egohiv) <- c("Negative Ego",
                      "Positive Ego")
View(egohiv)

write.csv(egorace, file = "Output/egorace.csv")
write.csv(egohiv, file = "Output/egohiv.csv")
