## ART-Net Study 2018   ##
## Table 1 Demographics ##
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


# Total past year partners

nrow(artnet)
names(artnet)
table(artnet$M_MP12OANUM)
summary(artnet$M_MP12OANUM)

summary(artnet$cuml.pnum)
summary(artnet$M_MP12ANUM2)
summary(artnet$MMCONC)

# Table 1 - Demographic Characteristics --------------

# Total sample
total <- cbind("Total", nrow(artnet), nrow(artnet)/nrow(artnet),
               nrow(intermed), nrow(intermed) /
                 nrow(intermed), nrow(amis), nrow(amis) / nrow(amis))

# Race/ethnicity
a <- artnet %>% count(race.cat)
raceart <- cbind(cbind(c(a$race.cat[1], a$race.cat[4], a$race.cat[2], a$race.cat[3])),
                 cbind(c(a$n[1], a$n[4], a$n[2], a$n[3])),
                 cbind(rbind((100 * a$n[1] / sum(a$n)),
                             (100 * a$n[4] / sum(a$n)),
                             (100 * a$n[2] / sum(a$n)),
                             (100 * a$n[3] / sum(a$n)))))
b <- intermed %>% count(race.cat)
raceint <- cbind(cbind(c(b$n[1], b$n[4], b$n[2], b$n[3])),
                 cbind(rbind((100 * b$n[1] / sum(b$n)),
                             (100 * b$n[4] / sum(b$n)),
                             (100 * b$n[2] / sum(b$n)),
                             (100 * b$n[3] / sum(b$n)))))
c <- amis %>% count(race.cat)
raceamis <- cbind(cbind(c(c$n[1], c$n[4], c$n[2], c$n[3])),
                  cbind(rbind((100 * c$n[1] / sum(c$n)),
                              (100 * c$n[4] / sum(c$n)),
                              (100 * c$n[2] / sum(c$n)),
                              (100 * c$n[3] / sum(c$n)))))

# Age
a <- artnet %>% count(age.cat)
ageart <- cbind(cbind(c(a$age.cat[1], a$age.cat[2], a$age.cat[3],
                        a$age.cat[4], a$age.cat[5], "66+")),
                cbind(c(a$n[1], a$n[2], a$n[3], a$n[4], a$n[5],
                        ifelse(length(which(artnet$age.cat == "66+")) > 0, a$n[6], 0))),
                cbind(rbind((100 * a$n[1] / sum(a$n)),
                            (100 * a$n[2] / sum(a$n)),
                            (100 * a$n[3] / sum(a$n)),
                            (100 * a$n[4] / sum(a$n)),
                            (100 * a$n[5] / sum(a$n)),
                            (100 * a$n[6] / sum(a$n)))))
b <- intermed %>% count(age.cat)
ageint <- cbind(cbind(c(b$n[1], b$n[2], b$n[3], b$n[4], b$n[5],
                        ifelse(length(which(intermed$age.cat == "66+")) > 0, b$n[6], 0))),
                cbind(rbind((100 * b$n[1] / sum(b$n)),
                            (100 * b$n[2] / sum(b$n)),
                            (100 * b$n[3] / sum(b$n)),
                            (100 * b$n[4] / sum(b$n)),
                            (100 * b$n[5] / sum(b$n)),
                            (100 * b$n[6] / sum(b$n)))))
c <- amis %>% count(age.cat)
ageamis <- cbind(cbind(c(c$n[1], c$n[2], c$n[3], c$n[4], c$n[5],
                         ifelse(length(which(amis$age.cat == "66+")) > 0, c$n[6], 0))),
                 cbind(rbind((100 * c$n[1] / sum(c$n)),
                             (100 * c$n[2] / sum(c$n)),
                             (100 * c$n[3] / sum(c$n)),
                             (100 * c$n[4] / sum(c$n)),
                             (100 * c$n[5] / sum(c$n)),
                             (100 * c$n[6] / sum(c$n)))))
# Region / Division
a <- artnet %>% count(region)
adiv <- artnet %>% count(division)
regart <- cbind(cbind(c(a$region[4], adiv$division[6], adiv$division[4],
                        a$region[1], adiv$division[8], adiv$division[1],
                        a$region[3], adiv$division[9], adiv$division[2],
                        adiv$division[7],
                        a$region[2], adiv$division[3], adiv$division[5], NA)),
                cbind(c(a$n[4], adiv$n[6], adiv$n[4],
                        a$n[1], adiv$n[8], adiv$n[1],
                        a$n[3], adiv$n[9], adiv$n[2],
                        adiv$n[7],
                        a$n[2], adiv$n[3], adiv$n[5], NA)),
                cbind(rbind(100 * a$n[4] / sum(a$n),
                            100 * adiv$n[6] / sum(adiv$n),
                            100 * adiv$n[4] / sum(adiv$n),
                            100 * a$n[1] / sum(a$n),
                            100 * adiv$n[8] / sum(adiv$n),
                            100 * adiv$n[1] / sum(adiv$n),
                            100 * a$n[3] / sum(a$n),
                            100 * adiv$n[9] / sum(adiv$n),
                            100 * adiv$n[2] / sum(adiv$n),
                            100 * adiv$n[7] / sum(adiv$n),
                            100 * a$n[2] / sum(a$n),
                            100 * adiv$n[3] / sum(adiv$n),
                            100 * adiv$n[5] / sum(adiv$n),
                            NA
                )))
b <- intermed %>% count(region)
bdiv <- intermed %>% count(division)
regint <- cbind(cbind(c(b$n[5], bdiv$n[7], bdiv$n[5],
                        b$n[2], bdiv$n[9], bdiv$n[2],
                        b$n[4], bdiv$n[10], bdiv$n[3], bdiv$n[8],
                        b$n[3], bdiv$n[4], bdiv$n[6], bdiv$n[1])),
                cbind(rbind(100 * b$n[5] / sum(b$n[2], b$n[3], b$n[4], b$n[5]),
                            100 * bdiv$n[7] / sum(bdiv$n[2:10]),
                            100 * bdiv$n[5] / sum(bdiv$n[2:10]),
                            100 * b$n[2] / sum(b$n[2], b$n[3], b$n[4], b$n[5]),
                            100 * bdiv$n[9] / sum(bdiv$n[2:10]),
                            100 * bdiv$n[2] / sum(bdiv$n[2:10]),
                            100 * b$n[4] / sum(b$n[2], b$n[3], b$n[4], b$n[5]),
                            100 * bdiv$n[10] / sum(bdiv$n[2:10]),
                            100 * bdiv$n[3] / sum(bdiv$n[2:10]),
                            100 * bdiv$n[8] / sum(bdiv$n[2:10]),
                            100 * b$n[3] / sum(b$n[2], b$n[3], b$n[4], b$n[5]),
                            100 * bdiv$n[4] / sum(bdiv$n[2:10]),
                            100 * bdiv$n[6] / sum(bdiv$n[2:10]),
                            100 * bdiv$n[1] / sum(bdiv$n)
                )))
c <- amis %>% count(region)
cdiv <- amis %>% count(division)
regamis <- cbind(cbind(c(c$n[5], cdiv$n[7], cdiv$n[5],
                         c$n[2], cdiv$n[9], cdiv$n[2],
                         c$n[4], cdiv$n[10], cdiv$n[3], cdiv$n[8],
                         c$n[3], cdiv$n[4], cdiv$n[6], cdiv$n[1])),
                 cbind(rbind(100 * c$n[5] / sum(c$n[2], c$n[3], c$n[4], c$n[5]),
                             100 * cdiv$n[7] / sum(cdiv$n[2:10]),
                             100 * cdiv$n[5] / sum(cdiv$n[2:10]),
                             100 * c$n[2] / sum(c$n[2], c$n[3], c$n[4], c$n[5]),
                             100 * cdiv$n[9] / sum(cdiv$n[2:10]),
                             100 * cdiv$n[2] / sum(cdiv$n[2:10]),
                             100 * c$n[4] / sum(c$n[2], c$n[3], c$n[4], c$n[5]),
                             100 * cdiv$n[10] / sum(cdiv$n[2:10]),
                             100 * cdiv$n[3] / sum(cdiv$n[2:10]),
                             100 * cdiv$n[8] / sum(cdiv$n[2:10]),
                             100 * c$n[3] / sum(c$n[2], c$n[3], c$n[4], c$n[5]),
                             100 * cdiv$n[4] / sum(cdiv$n[2:10]),
                             100 * cdiv$n[6] / sum(cdiv$n[2:10]),
                             100 * cdiv$n[1] / sum(cdiv$n)
                 )))

# Urbanicity
a <- artnet %>% count(NCHS_2013)
urbanart <- cbind(cbind(c("Large Central Metro", "Large Fringe Metro", "Medium Metro",
                          "Small Metro", "Micropolitan", "Noncore", "NA")),
                  cbind(c(a$n[1:6], NA)),
                  cbind(rbind((100 * a$n[1] / sum(a$n)),
                              (100 * a$n[2] / sum(a$n)),
                              (100 * a$n[3] / sum(a$n)),
                              (100 * a$n[4] / sum(a$n)),
                              (100 * a$n[5] / sum(a$n)),
                              (100 * a$n[6] / sum(a$n)),
                              NA)))

b <- intermed %>% count(NCHS_2013)
urbanint <- cbind(cbind(c(b$n[1:7])),
                  cbind(rbind((100 * b$n[1] / sum(b$n[1:6])),
                              (100 * b$n[2] / sum(b$n[1:6])),
                              (100 * b$n[3] / sum(b$n[1:6])),
                              (100 * b$n[4] / sum(b$n[1:6])),
                              (100 * b$n[5] / sum(b$n[1:6])),
                              (100 * b$n[6] / sum(b$n[1:6])),
                              (100 * b$n[7] / sum(b$n)))))

c <- amis %>% count(NCHS_2013)
urbanamis <- cbind(cbind(c(c$n[1:7])),
                   cbind(rbind((100 * c$n[1] / sum(c$n[1:6])),
                               (100 * c$n[2] / sum(c$n[1:6])),
                               (100 * c$n[3] / sum(c$n[1:6])),
                               (100 * c$n[4] / sum(c$n[1:6])),
                               (100 * c$n[5] / sum(c$n[1:6])),
                               (100 * c$n[6] / sum(c$n[1:6])),
                               (100 * c$n[7] / sum(c$n)))))

# Sexual role
a <- artnet %>% count(roletype)
roleart <- cbind(cbind(c(a$roletype[3], a$roletype[2], a$roletype[4], a$roletype[1])),
                 cbind(c(a$n[3], a$n[2], a$n[4], a$n[1])),
                 cbind(rbind((100 * a$n[3] / sum(a$n[2:4])),
                             (100 * a$n[2] / sum(a$n[2:4])),
                             (100 * a$n[4] / sum(a$n[2:4])),
                             (100 * a$n[1] / sum(a$n)))))

roleint <- matrix(NA, 4, 2)

roleamis <- matrix(NA, 4, 2)

# Education
a <- artnet %>% count(education)
educart <- cbind(cbind(c(a$education[4], a$education[3], a$education[5],
                         a$education[2], a$education[1]),
                       cbind(c(a$n[4], a$n[3], a$n[5], a$n[2], a$n[1])),
                       cbind(rbind((100 * a$n[4] / sum(a$n[2:5])),
                                   (100 * a$n[3] / sum(a$n[2:5])),
                                   (100 * a$n[5] / sum(a$n[2:5])),
                                   (100 * a$n[2] / sum(a$n[2:5])),
                                   NA))))

b <- intermed %>% count(education)
educint <- cbind(cbind(c(b$n[4], b$n[3], b$n[5], b$n[2], b$n[1])),
                 cbind(rbind((100 * b$n[4] / sum(b$n[2:5])),
                             (100 * b$n[3] / sum(b$n[2:5])),
                             (100 * b$n[5] / sum(b$n[2:5])),
                             (100 * b$n[2] / sum(b$n[2:5])),
                             (100 * b$n[1] / sum(b$n)))))

c <- amis %>% count(education)
educamis <- cbind(cbind(c(c$n[4], c$n[3], c$n[5], c$n[2], c$n[1])),
                  cbind(rbind((100 * c$n[4] / sum(c$n[2:5])),
                              (100 * c$n[3] / sum(c$n[2:5])),
                              (100 * c$n[5] / sum(c$n[2:5])),
                              (100 * c$n[2] / sum(c$n[2:5])),
                              (100 * c$n[1] / sum(c$n)))))

# HIV Status
a <- artnet %>% count(hiv)
statart <- cbind(cbind(c("Negative", "Positive", "Unknown", NA)),
                 cbind(c(a$n[1], a$n[2], a$n[3], NA)), #a$n[4]),
                 cbind(rbind((100 * a$n[1] / sum(a$n[1:3])),
                             (100 * a$n[2] / sum(a$n[1:3])),
                             (100 * a$n[3] / sum(a$n[1:3])),
                             NA))) #(100 * a$n[4] / sum(a$n)))))

b <- intermed %>% count(hiv)
statint <- cbind(cbind(c(b$n[1], b$n[2], b$n[3], NA)), #b$n[4]),
                 cbind(rbind((100 * b$n[1] / sum(b$n[1:3])),
                             (100 * b$n[2] / sum(b$n[1:3])),
                             (100 * b$n[3] / sum(b$n[1:3])),
                             NA))) # (100 * b$n[4] / sum(b$n)))))

c <- amis %>% count(hiv)
statamis <- cbind(cbind(c(c$n[1], c$n[2], c$n[3], NA)), #)),
                  cbind(rbind((100 * c$n[1] / sum(c$n[1:3])),
                              (100 * c$n[2] / sum(c$n[1:3])),
                              (100 * c$n[3] / sum(c$n[1:3])),
                              NA))) #(100 * c$n[4] / sum(c$n)))))

# HIV Testing
a <- artnet %>% count(hivtest)
testart <- cbind(cbind(c(a$hivtest[2], a$hivtest[3], a$hivtest[1])),
                 cbind(c(a$n[c(2, 3, 1)])),
                 cbind(rbind((100 * a$n[2] / sum(a$n[2:3])),
                             (100 * a$n[3] / sum(a$n[2:3])),
                             (100 * a$n[1] / sum(a$n)))))

b <- intermed %>% count(hivtest)
testint <- cbind(cbind(c(b$n[c(2, 3, 1)])),
                 cbind(rbind((100 * b$n[2] / sum(b$n[2:3])),
                             (100 * b$n[3] / sum(b$n[2:3])),
                             (100 * b$n[1] / sum(b$n)))))

c <- amis %>% count(hivtest)
testamis <- cbind(cbind(c(c$n[c(2, 3, 1)])),
                  cbind(rbind((100 * c$n[2] / sum(c$n[2:3])),
                              (100 * c$n[3] / sum(c$n[2:3])),
                              (100 * c$n[1] / sum(c$n)))))

# Number of tests in last 2 years
numtestsart <- rbind(cbind("Tests",
                           mean(artnet$TEST2YRS, na.rm = TRUE),
                           sd(artnet$TEST2YRS, na.rm = TRUE),
                           median(artnet$TEST2YRS, na.rm = TRUE)),
                     cbind("NA #",
                           length(which(is.na(artnet$TEST2YRS))),
                           length(which(is.na(artnet$TEST2YRS))),
                           length(which(is.na(artnet$TEST2YRS)))))
numtestsint <- rbind(cbind(mean(intermed$TEST2YRS, na.rm = TRUE),
                           sd(intermed$TEST2YRS, na.rm = TRUE),
                           median(intermed$TEST2YRS, na.rm = TRUE)),
                     cbind(length(which(is.na(intermed$TEST2YRS))),
                           length(which(is.na(intermed$TEST2YRS))),
                           length(which(is.na(intermed$TEST2YRS)))))
numtestsamis <- rbind(cbind(mean(amis$TEST2YRS, na.rm = TRUE),
                            sd(amis$TEST2YRS, na.rm = TRUE),
                            median(amis$TEST2YRS, na.rm = TRUE)),
                      cbind(length(which(is.na(amis$TEST2YRS))),
                            length(which(is.na(amis$TEST2YRS))),
                            length(which(is.na(amis$TEST2YRS)))))


# Output table
table1a <- rbind(total,
                 cbind(raceart, raceint, raceamis),
                 cbind(ageart, ageint, ageamis),
                 cbind(regart, regint, regamis),
                 cbind(urbanart, urbanint, urbanamis),
                 cbind(roleart, roleint, roleamis),
                 cbind(educart, educint, educamis),
                 cbind(statart, statint, statamis),
                 cbind(testart, testint, testamis))
colnames(table1a) <- c("Category", "ART-Net N", "ART-Net %",
                       "intermediate N", "intermediate %", "AMIS N", "AMIS %")
write.csv(table1a, file = "Output/table1a.csv")

table1b <- cbind(numtestsart, numtestsint, numtestsamis)
colnames(table1b) <- c("Category",
                       "ART-Net Mean", "ART-Net SD", "ART-Net Median",
                       "Intermed Mean", "Intermed SD", "Intermed Median",
                       "AMIS Mean", "AMIS SD", "AMIS Median")
write.csv(table1b, file = "Output/table1b.csv")

