## ART-Net Study 2018   ##
## Data Analysis Script ##
## 2018-02-21           ##

# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)

# Read in cleaning and datasets ---------

## Data Cleaning/Management Script
source("Kevin_Cleaning.R", echo = TRUE)

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
                        a$age.cat[4], a$age.cat[5], a$age.cat[6], "66+")),
                 cbind(c(a$n[1], a$n[2], a$n[3], a$n[4], a$n[5], a$n[6],
                       ifelse(length(which(artnet$age.cat == "66+")) > 0, a$n[7], 0))),
                 cbind(rbind((100 * a$n[1] / sum(a$n)),
                             (100 * a$n[2] / sum(a$n)),
                             (100 * a$n[3] / sum(a$n)),
                             (100 * a$n[4] / sum(a$n)),
                             (100 * a$n[5] / sum(a$n)),
                             (100 * a$n[6] / sum(a$n)),
                             (100 * a$n[7] / sum(a$n)))))
b <- intermed %>% count(age.cat)
ageint <- cbind(cbind(c(b$n[1], b$n[2], b$n[3], b$n[4], b$n[5], b$n[6],
                        ifelse(length(which(intermed$age.cat == "66+")) > 0, b$n[7], 0))),
                cbind(rbind((100 * b$n[1] / sum(b$n)),
                            (100 * b$n[2] / sum(b$n)),
                            (100 * b$n[3] / sum(b$n)),
                            (100 * b$n[4] / sum(b$n)),
                            (100 * b$n[5] / sum(b$n)),
                            (100 * b$n[6] / sum(b$n)),
                            (100 * b$n[7] / sum(b$n)))))
c <- amis %>% count(age.cat)
ageamis <- cbind(cbind(c(c$n[1], c$n[2], c$n[3], c$n[4], c$n[5], c$n[6],
                         ifelse(length(which(amis$age.cat == "66+")) > 0, c$n[7], 0))),
                 cbind(rbind((100 * c$n[1] / sum(c$n)),
                             (100 * c$n[2] / sum(c$n)),
                             (100 * c$n[3] / sum(c$n)),
                             (100 * c$n[4] / sum(c$n)),
                             (100 * c$n[5] / sum(c$n)),
                             (100 * c$n[6] / sum(c$n)),
                             (100 * c$n[7] / sum(c$n)))))
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
regint <- cbind(cbind(c(b$n[4], bdiv$n[6], bdiv$n[4],
                        b$n[1], bdiv$n[8], bdiv$n[1],
                        b$n[3], bdiv$n[9], bdiv$n[2],
                        bdiv$n[7],
                        b$n[2], bdiv$n[3], bdiv$n[5], bdiv$n[10])),
                cbind(rbind(100 * b$n[4] / sum(b$n[1],b$n[2], b$n[3], b$n[4]),
                            100 * bdiv$n[6] / sum(bdiv$n[1:9]),
                            100 * bdiv$n[4] / sum(bdiv$n[1:9]),
                            100 * b$n[1] / sum(b$n[1],b$n[2], b$n[3], b$n[4]),
                            100 * bdiv$n[8] / sum(bdiv$n[1:9]),
                            100 * bdiv$n[1] / sum(bdiv$n[1:9]),
                            100 * b$n[3] / sum(b$n[1],b$n[2], b$n[3], b$n[4]),
                            100 * bdiv$n[9] / sum(bdiv$n[1:9]),
                            100 * bdiv$n[2] / sum(bdiv$n[1:9]),
                            100 * bdiv$n[7] / sum(bdiv$n[1:9]),
                            100 * b$n[2] / sum(b$n[1],b$n[2], b$n[3], b$n[4]),
                            100 * bdiv$n[3] / sum(bdiv$n[1:9]),
                            100 * bdiv$n[5] / sum(bdiv$n[1:9]),
                            100 * bdiv$n[10] / sum(bdiv$n)
                )))
c <- amis %>% count(region)
cdiv <- amis %>% count(division)
regamis <- cbind(cbind(c(c$n[4], cdiv$n[6], cdiv$n[4],
                         c$n[1], cdiv$n[8], cdiv$n[1],
                         c$n[3], cdiv$n[9], cdiv$n[2],
                         cdiv$n[7],
                         c$n[2], cdiv$n[3], cdiv$n[5], cdiv$n[10])),
                 cbind(rbind(100 * c$n[4] / sum(c$n[1],c$n[2], c$n[3], c$n[4]),
                             100 * cdiv$n[6] / sum(cdiv$n[1:9]),
                             100 * cdiv$n[4] / sum(cdiv$n[1:9]),
                             100 * c$n[1] / sum(c$n[1],c$n[2], c$n[3], c$n[4]),
                             100 * cdiv$n[8] / sum(cdiv$n[1:9]),
                             100 * cdiv$n[1] / sum(cdiv$n[1:9]),
                             100 * c$n[3] / sum(c$n[1],c$n[2], c$n[3], c$n[4]),
                             100 * cdiv$n[9] / sum(cdiv$n[1:9]),
                             100 * cdiv$n[2] / sum(cdiv$n[1:9]),
                             100 * cdiv$n[7] / sum(cdiv$n[1:9]),
                             100 * c$n[2] / sum(c$n[1],c$n[2], c$n[3], c$n[4]),
                             100 * cdiv$n[3] / sum(cdiv$n[1:9]),
                             100 * cdiv$n[5] / sum(cdiv$n[1:9]),
                             100 * cdiv$n[10] / sum(cdiv$n)
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
roleart <- cbind(cbind(c(a$roletype[1], a$roletype[3], a$roletype[2], a$roletype[4])),
                 cbind(c(a$n[1], a$n[3], a$n[2], a$n[4])),
                 cbind(rbind((100 * a$n[1] / sum(a$n[1:3])),
                             (100 * a$n[3] / sum(a$n[1:3])),
                             (100 * a$n[2] / sum(a$n[1:3])),
                             (100 * a$n[4] / sum(a$n)))))

roleint <- matrix(NA, 4, 2)

roleamis <- matrix(NA, 4, 2)

# Education
a <- artnet %>% count(education)
educart <- cbind(cbind(c(a$education[3], a$education[2], a$education[4],
                          a$education[1], a$education[5]),
                  cbind(c(a$n[3], a$n[2], a$n[4], a$n[1], a$n[5])),
                  cbind(rbind((100 * a$n[3] / sum(a$n[1:4])),
                              (100 * a$n[2] / sum(a$n[1:4])),
                              (100 * a$n[4] / sum(a$n[1:4])),
                              (100 * a$n[1] / sum(a$n[1:4])),
                              NA))))

b <- intermed %>% count(education)
educint <- cbind(cbind(c(b$n[3], b$n[2], b$n[4], b$n[1], b$n[5])),
                 cbind(rbind((100 * b$n[3] / sum(b$n[1:4])),
                             (100 * b$n[2] / sum(b$n[1:4])),
                             (100 * b$n[4] / sum(b$n[1:4])),
                             (100 * b$n[1] / sum(b$n[1:4])),
                             (100 * b$n[5] / sum(b$n)))))

c <- amis %>% count(education)
educamis <- cbind(cbind(c(c$n[3], c$n[2], c$n[4], c$n[1], c$n[5])),
                  cbind(rbind((100 * c$n[3] / sum(c$n[1:4])),
                              (100 * c$n[2] / sum(c$n[1:4])),
                              (100 * c$n[4] / sum(c$n[1:4])),
                              (100 * c$n[1] / sum(c$n[1:4])),
                              (100 * c$n[5] / sum(c$n)))))

# HIV Status
a <- artnet %>% count(hiv)
statart <- cbind(cbind(c("Negative", "Positive", NA)),
                 cbind(c(a$n[1], a$n[2], a$n[3])),
                 cbind(rbind((100 * a$n[1] / sum(a$n[1:2])),
                             (100 * a$n[2] / sum(a$n[1:2])),
                             (100 * a$n[3] / sum(a$n)))))

b <- intermed %>% count(hiv)
statint <- cbind(cbind(c(b$n[1], b$n[2], b$n[3])),
                 cbind(rbind((100 * b$n[1] / sum(b$n[1:2])),
                             (100 * b$n[2] / sum(b$n[1:2])),
                             (100 * b$n[3] / sum(b$n)))))

c <- amis %>% count(hiv)
statamis <- cbind(cbind(c(c$n[1], c$n[2], c$n[3])),
                  cbind(rbind((100 * c$n[1] / sum(c$n[1:2])),
                              (100 * c$n[2] / sum(c$n[1:2])),
                              (100 * c$n[3] / sum(c$n)))))

# HIV Testing
a <- artnet %>% count(hivtest)
testart <- cbind(cbind(c(a$hivtest[1], a$hivtest[2], a$hivtest[3])),
                 cbind(c(a$n[1:3])),
                 cbind(rbind((100 * a$n[1] / sum(a$n[1:2])),
                             (100 * a$n[2] / sum(a$n[1:2])),
                             (100 * a$n[3] / sum(a$n)))))

b <- intermed %>% count(hivtest)
testint <- cbind(cbind(c(b$n[1:3])),
                 cbind(rbind((100 * b$n[1] / sum(b$n[1:2])),
                             (100 * b$n[2] / sum(b$n[1:2])),
                             (100 * b$n[3] / sum(b$n)))))

c <- amis %>% count(hivtest)
testamis <- cbind(cbind(c(c$n[1:3])),
                  cbind(rbind((100 * c$n[1] / sum(c$n[1:2])),
                              (100 * c$n[2] / sum(c$n[1:2])),
                              (100 * c$n[3] / sum(c$n)))))

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
write.csv(table1a, file = "table1a.csv")

table1b <- cbind(numtestsart, numtestsint, numtestsamis)
colnames(table1b) <- c("Category",
                       "ART-Net Mean", "ART-Net SD", "ART-Net Median",
                       "Intermed Mean", "Intermed SD", "Intermed Median",
                       "AMIS Mean", "AMIS SD", "AMIS Median")
write.csv(table1b, file = "table1b.csv")



# Table 2 - Mean degree --------------

## General mean degree calculation

# Issue: people with no reported AI or OI activity? Look at wide to long
nrow(artnetLong[which(artnetLong$RAI == 0 & artnetLong$IAI == 0 &
                        artnetLong$IOI == 0 & artnetLong$ROI == 0), ]) #118 with 0 Yes values

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

df2 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(aionlydegree = sum(ongoing2))

df3 <- l %>%
  filter(p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(oionlydegree = sum(ongoing2))

# Create merged dataframe
artnet2 <- left_join(artnet, df, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df2, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df3, by = "AMIS_ID")

artnet2$totdegree
table(artnet2$totdegree, useNA = "always")
table(artnet2$aionlydegree, useNA = "always")
table(artnet2$oionlydegree, useNA = "always")
# table(artnet2$reportedpartners, useNA = "always")

# If missing degree values, then set to 0
artnet2$totdegree <- ifelse(is.na(artnet2$totdegree), 0, artnet2$totdegree)
artnet2$aionlydegree <- ifelse(is.na(artnet2$aionlydegree), 0, artnet2$aionlydegree)
artnet2$oionlydegree <- ifelse(is.na(artnet2$oionlydegree), 0, artnet2$oionlydegree)
#artnet2$reported_partners <- ifelse(is.na(artnet2$reported_partners), 0, artnet2$reported_partners)
summary(artnet2$totdegree)
table(artnet2$totdegree, useNA = "always")

# artnetLong2 <- artnetLong[which(artnetLong$ptype %in% c(1, 2)), ]
#
# ids <- artnet$AMIS_ID
# artnet$totdegree <- NA
# artnet$aionlydegree <- NA
# artnet$oionlydegree <- NA
# artnet$maintotdegree <- NA
# artnet$mainaionlydegree <- NA
# artnet$mainoionlydegree <- NA
# artnet$castotdegree <- NA
# artnet$casaionlydegree <- NA
# artnet$casoionlydegree <- NA
# artnet$reported_partners <- NA
#
# for (id in ids) {
#    df <- artnetLong2[which(artnetLong$AMIS_ID == id),]
#
#    reportedPartners <- nrow(df)
#    ongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                            (df$RAI == 1 | df$IAI == 1 |
#                                            df$ROI == 1 | df$IOI == 1))])
#    aiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                               (df$RAI == 1 | df$IAI == 1))])
#    oiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                               (df$ROI == 1 | df$IOI == 1))])
#
#    mainongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                             (df$RAI == 1 | df$IAI == 1 |
#                                                df$ROI == 1 | df$IOI == 1) &
#                                               df$ptype == 1)])
#    mainaiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                               (df$RAI == 1 | df$IAI == 1) &
#                                                 df$ptype == 1)])
#    mainoiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                               (df$ROI == 1 | df$IOI == 1) &
#                                                 df$ptype == 1)])
#    casongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                             (df$RAI == 1 | df$IAI == 1 |
#                                                df$ROI == 1 | df$IOI == 1) &
#                                               df$ptype == 2)])
#    casaiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                               (df$RAI == 1 | df$IAI == 1) &
#                                                 df$ptype == 2)])
#    casoiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
#                                               (df$ROI == 1 | df$IOI == 1) &
#                                                 df$ptype == 2)])
#
#    artnet$totdegree[which(artnet$AMIS_ID == id)] <- ongpartners
#    artnet$aionlydegree[which(artnet$AMIS_ID == id)] <- aiongpartners
#    artnet$oionlydegree[which(artnet$AMIS_ID == id)] <- oiongpartners
#    artnet$reported_partners[which(artnet$AMIS_ID == id)] <- reportedPartners
#
#    artnet$maintotdegree[which(artnet$AMIS_ID == id)] <- mainongpartners
#    artnet$mainaionlydegree[which(artnet$AMIS_ID == id)] <- mainaiongpartners
#    artnet$mainoionlydegree[which(artnet$AMIS_ID == id)] <- mainoiongpartners
#    artnet$castotdegree[which(artnet$AMIS_ID == id)] <- casongpartners
#    artnet$casaionlydegree[which(artnet$AMIS_ID == id)] <- casaiongpartners
#    artnet$casoionlydegree[which(artnet$AMIS_ID == id)] <- casoiongpartners
#
#  }

# City
city <- glm(totdegree ~ city - 1, family = "poisson", data = artnet2)
city <- round(cbind(exp(coef(city)), exp(confint(city))), 2)
cityai <- glm(aionlydegree ~ city - 1, family = "poisson", data = artnet2)
cityai <- round(cbind(exp(coef(cityai)), exp(confint(cityai))), 2)
cityoi <- glm(oionlydegree ~ city - 1, family = "poisson", data = artnet2)
cityai <- round(cbind(exp(coef(cityoi)), exp(confint(cityoi))), 2)

# Overall Poisson:
total <- glm(totdegree ~ 1, family = "poisson", data = artnet2)
total <- round(cbind(exp(coef(total)), rbind(exp(confint(total)))), 2)
totalai <- glm(aionlydegree ~ 1, family = "poisson", data = artnet2)
totalai <- round(cbind(exp(coef(totalai)), rbind(exp(confint(totalai)))), 2)
totaloi <- glm(oionlydegree ~ 1, family = "poisson", data = artnet2)
totaloi <- round(cbind(exp(coef(totaloi)), rbind(exp(confint(totaloi)))), 2)

# double check predictions against empirical means
group_by(artnet2) %>% summarize(mean(totdegree))
group_by(artnet2) %>% summarize(mean(aionlydegree))
group_by(artnet2) %>% summarize(mean(oionlydegree))

total <- cbind("Total",
               total[1, 1],
               paste0(total[1, 2],
                      " - ",
                      total[1, 3]),
               totalai[1, 1],
               paste0(totalai[1, 2],
                      " - ",
                      totalai[1, 3]),
               totaloi[1, 1],
               paste0(totaloi[1, 2],
                      " - ",
                      totaloi[1, 3]))

# Race/ethnicity
race <- glm(totdegree ~ race.cat - 1, family = "poisson", data = artnet2)
race <- round(cbind(exp(coef(race)), exp(confint(race))), 2)
raceai <- glm(aionlydegree ~ race.cat - 1, family = "poisson", data = artnet2)
raceai <- round(cbind(exp(coef(raceai)), exp(confint(raceai))), 2)
raceoi <- glm(oionlydegree ~ race.cat - 1, family = "poisson", data = artnet2)
raceoi <- round(cbind(exp(coef(raceoi)), exp(confint(raceoi))), 2)

# double check predictions against empirical means
group_by(artnet2, race.cat) %>% summarize(mean(totdegree))
group_by(artnet2, race.cat) %>% summarize(mean(aionlydegree))
group_by(artnet2, race.cat) %>% summarize(mean(oionlydegree))

black <- cbind("black",
               race[1, 1],
               paste0(race[1, 2],
                      " - ",
                      race[1, 3]),
               raceai[1, 1],
               paste0(raceai[1, 2],
                      " - ",
                      raceai[1, 3]),
               raceoi[1, 1],
               paste0(raceoi[1, 2],
                      " - ",
                      raceoi[1, 3]))

white <- cbind("white",
               race[4, 1],
               paste0(race[4, 2],
                      " - ",
                      race[4, 3]),
               raceai[4, 1],
               paste0(raceai[4, 2],
                      " - ",
                      raceai[4, 3]),
               raceoi[4, 1],
               paste0(raceoi[4, 2],
                      " - ",
                      raceoi[4, 3]))

hispanic <- cbind("hispanic",
                  race[2, 1],
                  paste0(race[2, 2],
                         " - ",
                         race[2, 3]),
                  raceai[2, 1],
                  paste0(raceai[2, 2],
                         " - ",
                         raceai[2, 3]),
                  raceoi[2, 1],
                  paste0(raceoi[2, 2],
                         " - ",
                         raceoi[2, 3]))

other <- cbind("other",
               race[3, 1],
               paste0(race[3, 2],
                      " - ",
                      race[3, 3]),
               raceai[3, 1],
               paste0(raceai[3, 2],
                      " - ",
                      raceai[3, 3]),
               raceoi[3, 1],
               paste0(raceoi[3, 2],
                      " - ",
                      raceoi[3, 3]))

# Age
age <- glm(totdegree ~ age.cat - 1, family = "poisson", data = artnet2)
age <- round(cbind(exp(coef(age)), exp(confint(age))), 2)
ageai <- glm(aionlydegree ~ age.cat - 1, family = "poisson", data = artnet2)
ageai <- round(cbind(exp(coef(ageai)), exp(confint(ageai))), 2)
ageoi <- glm(oionlydegree ~ age.cat - 1, family = "poisson", data = artnet2)
ageoi <- round(cbind(exp(coef(ageoi)), exp(confint(ageoi))), 2)

# double check predictions against empirical means
group_by(artnet2, age.cat) %>% summarize(mean(totdegree))
group_by(artnet2, age.cat) %>% summarize(mean(aionlydegree))
group_by(artnet2, age.cat) %>% summarize(mean(oionlydegree))


fifteen24 <- cbind("15-24",
                   age[1, 1],
                   paste0(age[1, 2],
                          " - ",
                          age[1, 3]),
                   ageai[1, 1],
                   paste0(ageai[1, 2],
                          " - ",
                          ageai[1, 3]),
                   ageoi[1, 1],
                   paste0(ageoi[1, 2],
                          " - ",
                          ageoi[1, 3]))


twentyfive29 <- cbind("25-29",
                      age[2, 1],
                      paste0(age[2, 2],
                             " - ",
                             age[2, 3]),
                      ageai[2, 1],
                      paste0(ageai[2, 2],
                             " - ",
                             ageai[2, 3]),
                      ageoi[2, 1],
                      paste0(ageoi[2, 2],
                             " - ",
                             ageoi[2, 3]))

thirty39 <- cbind("30-39",
                  age[3, 1],
                  paste0(age[3, 2],
                         " - ",
                         age[3, 3]),
                  ageai[3, 1],
                  paste0(ageai[3, 2],
                         " - ",
                         ageai[3, 3]),
                  ageoi[3, 1],
                  paste0(ageoi[3, 2],
                         " - ",
                         ageoi[3, 3]))

forty49 <- cbind("40-49",
                 age[4, 1],
                 paste0(age[4, 2],
                        " - ",
                        age[4, 3]),
                 ageai[4, 1],
                 paste0(ageai[4, 2],
                        " - ",
                        ageai[4, 3]),
                 ageoi[4, 1],
                 paste0(ageoi[4, 2],
                        " - ",
                        ageoi[4, 3]))

fifty59 <- cbind("50-59",
                 age[5, 1],
                 paste0(age[5, 2],
                        " - ",
                        age[5, 3]),
                 ageai[5, 1],
                 paste0(ageai[5, 2],
                        " - ",
                        ageai[5, 3]),
                 ageoi[5, 1],
                 paste0(ageoi[5, 2],
                        " - ",
                        ageoi[5, 3]))

sixty65 <- cbind("60-65",
                 age[6, 1],
                 paste0(age[6, 2],
                        " - ",
                        age[6, 3]),
                 ageai[6, 1],
                 paste0(ageai[6, 2],
                        " - ",
                        ageai[6, 3]),
                 ageoi[6, 1],
                 paste0(ageoi[6, 2],
                        " - ",
                        ageoi[6, 3]))


# Region and division
division <- glm(totdegree ~ division - 1, family = "poisson", data = artnet2)
division <- round(cbind(exp(coef(division)), exp(confint(division))), 2)
divisionai <- glm(aionlydegree ~ division - 1, family = "poisson", data = artnet2)
divisionai <- round(cbind(exp(coef(divisionai)), exp(confint(divisionai))), 2)
divisionoi <- glm(oionlydegree ~ division - 1, family = "poisson", data = artnet2)
divisionoi <- round(cbind(exp(coef(divisionoi)), exp(confint(divisionoi))), 2)

region <- glm(totdegree ~ region - 1, family = "poisson", data = artnet2)
region <- round(cbind(exp(coef(region)), exp(confint(region))), 2)
regionai <- glm(aionlydegree ~ region - 1, family = "poisson", data = artnet2)
regionai <- round(cbind(exp(coef(regionai)), exp(confint(regionai))), 2)
regionoi <- glm(oionlydegree ~ region - 1, family = "poisson", data = artnet2)
regionoi <- round(cbind(exp(coef(regionoi)), exp(confint(regionoi))), 2)

# double check predictions against empirical means
group_by(artnet2, region) %>% summarize(mean(totdegree))
group_by(artnet2, region) %>% summarize(mean(aionlydegree))
group_by(artnet2, region) %>% summarize(mean(oionlydegree))
group_by(artnet2, division) %>% summarize(mean(totdegree))
group_by(artnet2, division) %>% summarize(mean(aionlydegree))
group_by(artnet2, division) %>% summarize(mean(oionlydegree))


# Region and Division
West <- cbind("West",
              region[4, 1],
              paste0(region[4, 2],
                     " - ",
                     region[4, 3]),
              regionai[4, 1],
              paste0(regionai[4, 2],
                     " - ",
                     regionai[4, 3]),
              regionoi[4, 1],
              paste0(regionoi[4, 2],
                     " - ",
                     regionoi[4, 3]))

Pacific <- cbind("Pacific",
                 division[6, 1],
                 paste0(division[6, 2],
                        " - ",
                        division[6, 3]),
                 divisionai[6, 1],
                 paste0(divisionai[6, 2],
                        " - ",
                        divisionai[6, 3]),
                 divisionoi[6, 1],
                 paste0(divisionoi[6, 2],
                        " - ",
                        divisionoi[6, 3]))

Mountain <- cbind("Mountain",
                  division[4, 1],
                  paste0(division[4, 2],
                         " - ",
                         division[4, 3]),
                  divisionai[4, 1],
                  paste0(divisionai[4, 2],
                         " - ",
                         divisionai[4, 3]),
                  divisionoi[4, 1],
                  paste0(divisionoi[4, 2],
                         " - ",
                         divisionoi[4, 3]))

Midwest <- cbind("Midwest",
                 region[1, 1],
                 paste0(region[1, 2],
                        " - ",
                        region[1, 3]),
                 regionai[1, 1],
                 paste0(regionai[1, 2],
                        " - ",
                        regionai[1, 3]),
                 regionoi[1, 1],
                 paste0(regionoi[1, 2],
                        " - ",
                        regionoi[1, 3]))

WNC <- cbind("West North Central",
             division[8, 1],
             paste0(division[8, 2],
                    " - ",
                    division[8, 3]),
             divisionai[8, 1],
             paste0(divisionai[8, 2],
                    " - ",
                    divisionai[8, 3]),
             divisionoi[8, 1],
             paste0(divisionoi[8, 2],
                    " - ",
                    divisionoi[8, 3]))

ENC <- cbind("East North Central",
             division[1, 1],
             paste0(division[1, 2],
                    " - ",
                    division[1, 3]),
             divisionai[1, 1],
             paste0(divisionai[1, 2],
                    " - ",
                    divisionai[1, 3]),
             divisionoi[1, 1],
             paste0(divisionoi[1, 2],
                    " - ",
                    divisionoi[1, 3]))

South <- cbind("South",
               region[3, 1],
               paste0(region[3, 2],
                      " - ",
                      region[3, 3]),
               regionai[3, 1],
               paste0(regionai[3, 2],
                      " - ",
                      regionai[3, 3]),
               regionoi[3, 1],
               paste0(regionoi[3, 2],
                      " - ",
                      regionoi[3, 3]))

WSC <- cbind("West South Central",
             division[9, 1],
             paste0(division[9, 2],
                    " - ",
                    division[9, 3]),
             divisionai[9, 1],
             paste0(divisionai[9, 2],
                    " - ",
                    divisionai[9, 3]),
             divisionoi[9, 1],
             paste0(divisionoi[9, 2],
                    " - ",
                    divisionoi[9, 3]))

ESC <- cbind("East South Central",
             division[2, 1],
             paste0(division[2, 2],
                    " - ",
                    division[2, 3]),
             divisionai[2, 1],
             paste0(divisionai[2, 2],
                    " - ",
                    divisionai[2, 3]),
             divisionoi[2, 1],
             paste0(divisionoi[2, 2],
                    " - ",
                    divisionoi[2, 3]))
SA <- cbind("South Atlantic",
            division[7, 1],
            paste0(division[7, 2],
                   " - ",
                   division[7, 3]),
            divisionai[7, 1],
            paste0(divisionai[7, 2],
                   " - ",
                   divisionai[7, 3]),
            divisionoi[7, 1],
            paste0(divisionoi[7, 2],
                   " - ",
                   divisionoi[7, 3]))

Northeast <- cbind("Northeast",
                   region[2, 1],
                   paste0(region[2, 2],
                          " - ",
                          region[2, 3]),
                   regionai[2, 1],
                   paste0(regionai[2, 2],
                          " - ",
                          regionai[2, 3]),
                   regionoi[2, 1],
                   paste0(regionoi[2, 2],
                          " - ",
                          regionoi[2, 3]))

MA <- cbind("Middle Atlantic",
            division[3, 1],
            paste0(division[3, 2],
                   " - ",
                   division[3, 3]),
            divisionai[3, 1],
            paste0(divisionai[3, 2],
                   " - ",
                   divisionai[3, 3]),
            divisionoi[3, 1],
            paste0(divisionoi[3, 2],
                   " - ",
                   divisionoi[3, 3]))

NE <- cbind("New England",
            division[5, 1],
            paste0(division[5, 2],
                   " - ",
                   division[5, 3]),
            divisionai[5, 1],
            paste0(divisionai[5, 2],
                   " - ",
                   divisionai[5, 3]),
            divisionoi[5, 1],
            paste0(divisionoi[5, 2],
                   " - ",
                   divisionoi[5, 3]))

# Urbanicity
urban <- glm(totdegree ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urban <- round(cbind(exp(coef(urban)), exp(confint(urban))), 2)
urbanai <- glm(aionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urbanai <- round(cbind(exp(coef(urbanai)), exp(confint(urbanai))), 2)
urbanoi <- glm(oionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urbanoi <- round(cbind(exp(coef(urbanoi)), exp(confint(urbanoi))), 2)

# double check predictions against empirical means
group_by(artnet2, NCHSCHAR) %>% summarize(mean(totdegree))
group_by(artnet2, NCHSCHAR) %>% summarize(mean(aionlydegree))
group_by(artnet2, NCHSCHAR) %>% summarize(mean(oionlydegree))

LCM <- cbind("Large Central Metro",
             urban[1, 1],
             paste0(urban[1, 2],
                    " - ",
                    urban[1, 3]),
             urbanai[1, 1],
             paste0(urbanai[1, 2],
                    " - ",
                    urbanai[1, 3]),
             urbanoi[1, 1],
             paste0(urbanoi[1, 2],
                    " - ",
                    urbanoi[1, 3]))

LFM <- cbind("Large Fringe Metro",
             urban[2, 1],
             paste0(urban[2, 2],
                    " - ",
                    urban[2, 3]),
             urbanai[2, 1],
             paste0(urbanai[2, 2],
                    " - ",
                    urbanai[2, 3]),
             urbanoi[2, 1],
             paste0(urbanoi[2, 2],
                    " - ",
                    urbanoi[2, 3]))
Medium <- cbind("Medium Metro",
                urban[3, 1],
                paste0(urban[3, 2],
                       " - ",
                       urban[3, 3]),
                urbanai[3, 1],
                paste0(urbanai[3, 2],
                       " - ",
                       urbanai[3, 3]),
                urbanoi[3, 1],
                paste0(urbanoi[3, 2],
                       " - ",
                       urbanoi[3, 3]))
Small <- cbind("Small Metro",
               urban[6, 1],
               paste0(urban[6, 2],
                      " - ",
                      urban[6, 3]),
               urbanai[6, 1],
               paste0(urbanai[6, 2],
                      " - ",
                      urbanai[6, 3]),
               urbanoi[6, 1],
               paste0(urbanoi[6, 2],
                      " - ",
                      urbanoi[6, 3]))
Micro <- cbind("Micropolitan",
               urban[4, 1],
               paste0(urban[4, 2],
                      " - ",
                      urban[4, 3]),
               urbanai[4, 1],
               paste0(urbanai[4, 2],
                      " - ",
                      urbanai[4, 3]),
               urbanoi[4, 1],
               paste0(urbanoi[4, 2],
                      " - ",
                      urbanoi[4, 3]))
Noncore <- cbind("Noncore",
                 urban[5, 1],
                 paste0(urban[5, 2],
                        " - ",
                        urban[5, 3]),
                 urbanai[5, 1],
                 paste0(urbanai[5, 2],
                        " - ",
                        urbanai[5, 3]),
                 urbanoi[5, 1],
                 paste0(urbanoi[5, 2],
                        " - ",
                        urbanoi[5, 3]))


# HIV Status
hivstat <- glm(totdegree ~ hiv, family = "poisson", data = artnet2)
hivstat <- round(cbind(exp(coef(hivstat)), rbind(exp(confint(hivstat)))), 2)
hivstatv2 <- glm(totdegree ~ hiv - 1, family = "poisson", data = artnet2)
hivstatv2 <- round(cbind(exp(coef(hivstatv2)), rbind(exp(confint(hivstatv2)))), 2)

hivstatai <- glm(aionlydegree ~ hiv, family = "poisson", data = artnet2)
hivstatai <- round(cbind(exp(coef(hivstatai)), rbind(exp(confint(hivstatai)))), 2)
hivstataiv2 <- glm(aionlydegree ~ hiv - 1, family = "poisson", data = artnet2)
hivstataiv2 <- round(cbind(exp(coef(hivstataiv2)), rbind(exp(confint(hivstataiv2)))), 2)

hivstatoi <- glm(oionlydegree ~ hiv, family = "poisson", data = artnet2)
hivstatoi <- round(cbind(exp(coef(hivstatoi)), rbind(exp(confint(hivstatoi)))), 2)
hivstatoiv2 <- glm(oionlydegree ~ hiv - 1, family = "poisson", data = artnet2)
hivstatoiv2 <- round(cbind(exp(coef(hivstatoiv2)), rbind(exp(confint(hivstatoiv2)))), 2)

# double check predictions against empirical means
group_by(artnet2, hiv) %>% summarize(mean(totdegree))
group_by(artnet2, hiv) %>% summarize(mean(aionlydegree))
group_by(artnet2, hiv) %>% summarize(mean(oionlydegree))

HIVPos <- cbind("HIV Pos",
                hivstatv2[1, 1],
                paste0(hivstatv2[1, 2],
                       " - ",
                       hivstatv2[1, 3]),
                hivstataiv2[1, 1],
                paste0(hivstataiv2[1, 2],
                       " - ",
                       hivstataiv2[1, 3]),
                hivstatoiv2[1, 1],
                paste0(hivstatoiv2[1, 2],
                       " - ",
                       hivstatoiv2[1, 3]))
HIVNeg <- cbind("HIV Neg",
                hivstat[1, 1],
                paste0(hivstat[1, 2],
                       " - ",
                       hivstat[1, 3]),
                hivstatai[1, 1],
                paste0(hivstatai[1, 2],
                       " - ",
                       hivstatai[1, 3]),
                hivstatoi[1, 1],
                paste0(hivstatoi[1, 2],
                       " - ",
                       hivstatoi[1, 3]))

# Other types of statistics
# % of egos Concurrent
concurr <- cbind("Concurr",
                 paste0(length(which(artnet2$totdegree > 1)),
                        " (",
                        round(100 * length(which(artnet2$totdegree > 1)) / nrow(artnet2), 2), "%)"),
                 paste0(length(which(artnet2$aionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet2$aionlydegree > 1)) / nrow(artnet2), 2), "%)"),
                 paste0(length(which(artnet2$oionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet2$oionlydegree > 1)) / nrow(artnet2), 2), "%)"))

# Rate of one-offs
# Create OI partners variable
artnet2$oi.part <- rep(NA, nrow(artnet2))
artnet2$oi.part <- artnet2$cuml.pnum - artnet2$ai.part
artnet2$oi.part[artnet2$oi.part < 0] <- 0 # 1 person with -87

# Create count variables for AI or OI
d <- artnet2
l <- artnetLong
d <- l %>%
  filter(p_ROI == 1 | p_IOI == 1 | p_RAI == 1 | p_IAI == 1) %>%
  filter(ptype %in% 1:2) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(count.mc.aioi.part = n) %>%
  right_join(d, by = "AMIS_ID")
d$count.mc.aioi.part <- ifelse(is.na(d$count.mc.aioi.part), 0, d$count.mc.aioi.part)
d$count.mc.aioi.part
d$count.oo.aioi.part <- d$cuml.pnum - d$count.mc.aioi.part
d$count.oo.aioi.part <- pmax(0, d$count.oo.aioi.part)
data.frame(d$cuml.pnum, d$count.mc.aioi.part, d$count.oo.aioi.part)
summary(d$count.oo.aioi.part)

plot(density(d$count.oo.aioi.part, na.rm = TRUE, from = 0))
plot(density(d$count.oo.aioi.part, na.rm = TRUE, from = 0), xlim = c(0, 100))

# daily rate
d$rate.oo.aioi.part <- d$count.oo.aioi.part/365
d$rate.oo.aioi.part

# Create count variables for AI
d2 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1) %>%
  filter(ptype %in% 1:2) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(count.mc.ai.part = n) %>%
  right_join(d, by = "AMIS_ID")
d2$count.mc.ai.part <- ifelse(is.na(d2$count.mc.ai.part), 0, d2$count.mc.ai.part)
d2$count.mc.ai.part

d2$count.oo.ai.part <- d2$ai.part - d2$count.mc.ai.part
d2$count.oo.ai.part <- pmax(0, d2$count.oo.ai.part)
data.frame(d2$ai.part, d2$count.mc.ai.part, d2$count.oo.ai.part)
summary(d2$count.oo.ai.part)

plot(density(d2$count.oo.ai.part, na.rm = TRUE, from = 0))
plot(density(d2$count.oo.ai.part, na.rm = TRUE, from = 0), xlim = c(0, 100))

# daily rate
d2$rate.oo.ai.part <- d2$count.oo.ai.part/365
d2$rate.oo.ai.part

# Create count variables for OI
d3 <- l %>%
  filter(p_ROI == 1 | p_IOI == 1) %>%
  filter(ptype %in% 1:2) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(count.mc.oi.part = n) %>%
  right_join(d, by = "AMIS_ID")
d3$count.mc.oi.part <- ifelse(is.na(d3$count.mc.oi.part), 0, d3$count.mc.oi.part)
d3$count.mc.oi.part

d3$count.oo.oi.part <- d3$oi.part - d3$count.mc.oi.part
d3$count.oo.oi.part <- pmax(0, d3$count.oo.oi.part)
data.frame(d3$oi.part, d3$count.mc.oi.part, d3$count.oo.oi.part)
summary(d3$count.oo.oi.part)

plot(density(d3$count.oo.oi.part, na.rm = TRUE, from = 0))
plot(density(d3$count.oo.oi.part, na.rm = TRUE, from = 0), xlim = c(0, 100))

# doily rate
d3$rate.oo.oi.part <- d3$count.oo.oi.part/365
d3$rate.oo.oi.part

oneoff <- cbind("One-off", round(mean(d$rate.oo.aioi.part, na.rm = TRUE), 4),
                round(mean(d2$rate.oo.ai.part, na.rm = TRUE), 4),
                round(mean(d3$rate.oo.oi.part, na.rm = TRUE), 4))

# Output table
table2a <- rbind(total, black, white, hispanic, other,
                fifteen24, twentyfive29, thirty39, forty49, fifty59, sixty65,
                West, Pacific, Mountain, Midwest, WNC, ENC,
                South, WSC, ESC, SA, Northeast, MA, NE, LCM, LFM, Medium,
                Small, Micro, Noncore,
                HIVPos, HIVNeg)
colnames(table2a) <- c("Category", "Ong Either Mean", "Ong Either CI",
                      "Ong AI Mean", "Ongoing AI CI",
                      "Ong OI Mean", "Ong OI CI")
write.csv(table2a, file = "table2a.csv")

table2b <- rbind(concurr, oneoff)
colnames(table2b) <- c("Category", "AI/OI Mean", "AI Mean", "OI Mean")
write.csv(table2b, file = "table2b.csv")


# Table 3 - Duration of ongoing partnerships --------------

# Set up data frames
extant <- artnetLong[which(artnetLong$p_ONGOING == 1 & artnetLong$duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]

ongpartners <- length(extant$p_ONGOING[which(extant$p_ONGOING == 1 &
                                         (extant$p_RAI == 1 | extant$p_IAI == 1 |
                                            extant$p_ROI == 1 | extant$p_IOI == 1))])
aiongpartners <- length(extant$p_ONGOING[which(extant$p_ONGOING == 1 &
                                           (extant$p_RAI == 1 | extant$p_IAI == 1))])
oiongpartners <- length(extant$p_ONGOING[which(extant$p_ONGOING == 1 &
                                           (extant$p_ROI == 1 | extant$p_IOI == 1))])

bothmain <- extant[which((extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 1), ]
bothcas <- extant[which((extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 2), ]
aimain <- extant[which((extant$RAI == 1 | extant$IAI == 1) & extant$ptype == 1), ]
aicas <- extant[which((extant$RAI == 1 | extant$IAI == 1) & extant$ptype == 2), ]
oimain <- extant[which((extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 1), ]
oicas <- extant[which((extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 2), ]

# Total number of ongoing partnerships
total <- cbind("Total", paste0(nrow(extant), " (", 100 * nrow(extant) / nrow(extant), ")"),

               # Calculate mean, 95% CI
               round(mean(bothmain$duration, na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration, na.rm = TRUE), 1)),

               round(mean(bothcas$duration, na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration, na.rm = TRUE), 1)),

               round(mean(aimain$duration, na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration, na.rm = TRUE), 1)),

               round(mean(aicas$duration, na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration, na.rm = TRUE), 1)),

               round(mean(oimain$duration, na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration, na.rm = TRUE), 1)),

               round(mean(oicas$duration, na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration, na.rm = TRUE), 1)))


# Race/ethnicity
black <- cbind("black", paste0(nrow(extant[which(extant$race.cat == "black"), ]), " (", round(100 * nrow(extant[which(extant$race.cat == "black"), ]) /
                                 nrow(extant), 1), ")"),

               round(mean(bothmain$duration[bothmain$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration[bothmain$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration[bothmain$race.cat == "black"], na.rm = TRUE), 1)),

               round(mean(bothcas$duration[bothcas$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration[bothcas$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration[bothcas$race.cat == "black"], na.rm = TRUE), 1)),

               round(mean(aimain$duration[aimain$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration[aimain$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration[aimain$race.cat == "black"], na.rm = TRUE), 1)),

               round(mean(aicas$duration[aicas$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration[aicas$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration[aicas$race.cat == "black"], na.rm = TRUE), 1)),
               round(mean(oimain$duration[oimain$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration[oimain$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration[oimain$race.cat == "black"], na.rm = TRUE), 1)),

               round(mean(oicas$duration[oicas$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration[oicas$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration[oicas$race.cat == "black"], na.rm = TRUE), 1)))


white <- cbind("white", paste0(nrow(extant[which(extant$race.cat == "white"), ]), " (", round(100 * nrow(extant[which(extant$race.cat == "white"), ]) /
                                                                                                nrow(extant), 1), ")"),

               round(mean(bothmain$duration[bothmain$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration[bothmain$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration[bothmain$race.cat == "white"], na.rm = TRUE), 1)),

               round(mean(bothcas$duration[bothcas$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration[bothcas$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration[bothcas$race.cat == "white"], na.rm = TRUE), 1)),


               round(mean(aimain$duration[aimain$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration[aimain$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration[aimain$race.cat == "white"], na.rm = TRUE), 1)),


               round(mean(aicas$duration[aicas$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration[aicas$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration[aicas$race.cat == "white"], na.rm = TRUE), 1)),

               round(mean(oimain$duration[oimain$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration[oimain$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration[oimain$race.cat == "white"], na.rm = TRUE), 1)),

               round(mean(oicas$duration[oicas$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration[oicas$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration[oicas$race.cat == "white"], na.rm = TRUE), 1)))

hispanic <- cbind("hispanic", paste0(nrow(extant[which(extant$race.cat == "hispanic"), ]), " (", round(100 * nrow(extant[which(extant$race.cat == "hispanic"), ]) /
                                                                                                   nrow(extant), 1), ")"),

                  round(mean(bothmain$duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(bothmain$duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothmain$duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1)),

                  round(mean(bothcas$duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(bothcas$duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothcas$duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1)),


                  round(mean(aimain$duration[aimain$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(aimain$duration[aimain$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(aimain$duration[aimain$race.cat == "hispanic"], na.rm = TRUE), 1)),


                  round(mean(aicas$duration[aicas$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(aicas$duration[aicas$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(aicas$duration[aicas$race.cat == "hispanic"], na.rm = TRUE), 1)),

                  round(mean(oimain$duration[oimain$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(oimain$duration[oimain$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(oimain$duration[oimain$race.cat == "hispanic"], na.rm = TRUE), 1)),

                  round(mean(oicas$duration[oicas$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(oicas$duration[oicas$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(oicas$duration[oicas$race.cat == "hispanic"], na.rm = TRUE), 1)))

other <- cbind("other", paste0(nrow(extant[which(extant$race.cat == "other"), ]), " (", round(100 * nrow(extant[which(extant$race.cat == "other"), ]) /
                                                                                                nrow(extant), 1), ")"),

               round(mean(bothmain$duration[bothmain$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration[bothmain$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration[bothmain$race.cat == "other"], na.rm = TRUE), 1)),

               round(mean(bothcas$duration[bothcas$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration[bothcas$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration[bothcas$race.cat == "other"], na.rm = TRUE), 1)),


               round(mean(aimain$duration[aimain$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration[aimain$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration[aimain$race.cat == "other"], na.rm = TRUE), 1)),


               round(mean(aicas$duration[aicas$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration[aicas$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration[aicas$race.cat == "other"], na.rm = TRUE), 1)),

               round(mean(oimain$duration[oimain$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration[oimain$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration[oimain$race.cat == "other"], na.rm = TRUE), 1)),

               round(mean(oicas$duration[oicas$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration[oicas$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration[oicas$race.cat == "other"], na.rm = TRUE), 1)))

# Age
fifteen24 <- cbind("15-24", paste0(nrow(extant[which(extant$age.cat == "15-24"), ]), " (", round(100 * nrow(extant[which(extant$age.cat == "15-24"), ]) /
                                                                                                    nrow(extant), 1), ")"),

                   round(mean(bothmain$duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(bothmain$duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(bothmain$duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1)),

                   round(mean(bothcas$duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(bothcas$duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(bothcas$duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1)),


                   round(mean(aimain$duration[aimain$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(aimain$duration[aimain$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(aimain$duration[aimain$age.cat == "15-24"], na.rm = TRUE), 1)),


                   round(mean(aicas$duration[aicas$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(aicas$duration[aicas$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(aicas$duration[aicas$age.cat == "15-24"], na.rm = TRUE), 1)),

                   round(mean(oimain$duration[oimain$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(oimain$duration[oimain$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(oimain$duration[oimain$age.cat == "15-24"], na.rm = TRUE), 1)),

                   round(mean(oicas$duration[oicas$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(oicas$duration[oicas$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(oicas$duration[oicas$age.cat == "15-24"], na.rm = TRUE), 1)))

Twentyfive29 <- cbind("25-29", paste0(nrow(extant[which(extant$age.cat == "25-29"), ]), " (", round(100 * nrow(extant[which(extant$age.cat == "25-29"), ]) /
                                                                                                      nrow(extant), 1), ")"),

                      round(mean(bothmain$duration[bothmain$age.cat == "25-29"], na.rm = TRUE), 1),
                      paste0(round(sd(bothmain$duration[bothmain$age.cat == "25-29"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothmain$duration[bothmain$age.cat == "25-29"], na.rm = TRUE), 1)),

                      round(mean(bothcas$duration[bothcas$age.cat == "25-29"], na.rm = TRUE), 1),
                      paste0(round(sd(bothcas$duration[bothcas$age.cat == "25-29"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothcas$duration[bothcas$age.cat == "25-29"], na.rm = TRUE), 1)),


                      round(mean(aimain$duration[aimain$age.cat == "25-29"], na.rm = TRUE), 1),
                      paste0(round(sd(aimain$duration[aimain$age.cat == "25-29"], na.rm = TRUE), 1),
                             ", ",
                             round(median(aimain$duration[aimain$age.cat == "25-29"], na.rm = TRUE), 1)),


                      round(mean(aicas$duration[aicas$age.cat == "25-29"], na.rm = TRUE), 1),
                      paste0(round(sd(aicas$duration[aicas$age.cat == "25-29"], na.rm = TRUE), 1),
                             ", ",
                             round(median(aicas$duration[aicas$age.cat == "25-29"], na.rm = TRUE), 1)),

                      round(mean(oimain$duration[oimain$age.cat == "25-29"], na.rm = TRUE), 1),
                      paste0(round(sd(oimain$duration[oimain$age.cat == "25-29"], na.rm = TRUE), 1),
                             ", ",
                             round(median(oimain$duration[oimain$age.cat == "25-29"], na.rm = TRUE), 1)),

                      round(mean(oicas$duration[oicas$age.cat == "25-29"], na.rm = TRUE), 1),
                      paste0(round(sd(oicas$duration[oicas$age.cat == "25-29"], na.rm = TRUE), 1),
                             ", ",
                             round(median(oicas$duration[oicas$age.cat == "25-29"], na.rm = TRUE), 1)))

Thirty39 <- cbind("30-39", paste0(nrow(extant[which(extant$age.cat == "30-39"), ]), " (", round(100 * nrow(extant[which(extant$age.cat == "30-39"), ]) /
                                                                                                  nrow(extant), 1), ")"),

                  round(mean(bothmain$duration[bothmain$age.cat == "30-39"], na.rm = TRUE), 1),
                  paste0(round(sd(bothmain$duration[bothmain$age.cat == "30-39"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothmain$duration[bothmain$age.cat == "30-39"], na.rm = TRUE), 1)),

                  round(mean(bothcas$duration[bothcas$age.cat == "30-39"], na.rm = TRUE), 1),
                  paste0(round(sd(bothcas$duration[bothcas$age.cat == "30-39"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothcas$duration[bothcas$age.cat == "30-39"], na.rm = TRUE), 1)),


                  round(mean(aimain$duration[aimain$age.cat == "30-39"], na.rm = TRUE), 1),
                  paste0(round(sd(aimain$duration[aimain$age.cat == "30-39"], na.rm = TRUE), 1),
                         ", ",
                         round(median(aimain$duration[aimain$age.cat == "30-39"], na.rm = TRUE), 1)),


                  round(mean(aicas$duration[aicas$age.cat == "30-39"], na.rm = TRUE), 1),
                  paste0(round(sd(aicas$duration[aicas$age.cat == "30-39"], na.rm = TRUE), 1),
                         ", ",
                         round(median(aicas$duration[aicas$age.cat == "30-39"], na.rm = TRUE), 1)),

                  round(mean(oimain$duration[oimain$age.cat == "30-39"], na.rm = TRUE), 1),
                  paste0(round(sd(oimain$duration[oimain$age.cat == "30-39"], na.rm = TRUE), 1),
                         ", ",
                         round(median(oimain$duration[oimain$age.cat == "30-39"], na.rm = TRUE), 1)),

                  round(mean(oicas$duration[oicas$age.cat == "30-39"], na.rm = TRUE), 1),
                  paste0(round(sd(oicas$duration[oicas$age.cat == "30-39"], na.rm = TRUE), 1),
                         ", ",
                         round(median(oicas$duration[oicas$age.cat == "30-39"], na.rm = TRUE), 1)))

Forty49 <- cbind("40-49", paste0(nrow(extant[which(extant$age.cat == "40-49"), ]), " (", round(100 * nrow(extant[which(extant$age.cat == "40-49"), ]) /
                                                                                                 nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$age.cat == "40-49"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$age.cat == "40-49"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$age.cat == "40-49"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$age.cat == "40-49"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$age.cat == "40-49"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$age.cat == "40-49"], na.rm = TRUE), 1)),


                 round(mean(aimain$duration[aimain$age.cat == "40-49"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$age.cat == "40-49"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$age.cat == "40-49"], na.rm = TRUE), 1)),


                 round(mean(aicas$duration[aicas$age.cat == "40-49"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$age.cat == "40-49"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$age.cat == "40-49"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$age.cat == "40-49"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$age.cat == "40-49"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$age.cat == "40-49"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$age.cat == "40-49"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$age.cat == "40-49"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$age.cat == "40-49"], na.rm = TRUE), 1)))

Fifty59 <- cbind("50-59", paste0(nrow(extant[which(extant$age.cat == "50-59"), ]), " (", round(100 * nrow(extant[which(extant$age.cat == "50-59"), ]) /
                                                                                                 nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$age.cat == "50-59"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$age.cat == "50-59"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$age.cat == "50-59"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$age.cat == "50-59"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$age.cat == "50-59"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$age.cat == "50-59"], na.rm = TRUE), 1)),


                 round(mean(aimain$duration[aimain$age.cat == "50-59"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$age.cat == "50-59"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$age.cat == "50-59"], na.rm = TRUE), 1)),


                 round(mean(aicas$duration[aicas$age.cat == "50-59"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$age.cat == "50-59"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$age.cat == "50-59"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$age.cat == "50-59"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$age.cat == "50-59"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$age.cat == "50-59"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$age.cat == "50-59"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$age.cat == "50-59"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$age.cat == "50-59"], na.rm = TRUE), 1)))

Sixty65 <- cbind("60-65", paste0(nrow(extant[which(extant$age.cat == "60-65"), ]), " (", round(100 * nrow(extant[which(extant$age.cat == "60-65"), ]) /
                                                                                                 nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$age.cat == "60-65"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$age.cat == "60-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$age.cat == "60-65"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$age.cat == "60-65"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$age.cat == "60-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$age.cat == "60-65"], na.rm = TRUE), 1)),


                 round(mean(aimain$duration[aimain$age.cat == "60-65"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$age.cat == "60-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$age.cat == "60-65"], na.rm = TRUE), 1)),


                 round(mean(aicas$duration[aicas$age.cat == "60-65"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$age.cat == "60-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$age.cat == "60-65"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$age.cat == "60-65"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$age.cat == "60-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$age.cat == "60-65"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$age.cat == "60-65"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$age.cat == "60-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$age.cat == "60-65"], na.rm = TRUE), 1)))

# HIV Status
HIVPos <- cbind("HIV Pos", paste0(nrow(extant[which(extant$hiv == 1), ]), " (", round(100 * nrow(extant[which(extant$hiv == 1), ]) /
                         nrow(extant), 1), ")"),

                round(mean(bothmain$duration[bothmain$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$duration[bothmain$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$duration[bothmain$hiv == 1], na.rm = TRUE), 1)),

                round(mean(bothcas$duration[bothcas$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$duration[bothcas$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$duration[bothcas$hiv == 1], na.rm = TRUE), 1)),

                round(mean(aimain$duration[aimain$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(aimain$duration[aimain$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$duration[aimain$hiv == 1], na.rm = TRUE), 1)),

                round(mean(aicas$duration[aicas$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(aicas$duration[aicas$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$duration[aicas$hiv == 1], na.rm = TRUE), 1)),

                round(mean(oimain$duration[oimain$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(oimain$duration[oimain$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$duration[oimain$hiv == 1], na.rm = TRUE), 1)),

                round(mean(oicas$duration[oicas$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(oicas$duration[oicas$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$duration[oicas$hiv == 1], na.rm = TRUE), 1)))

HIVNeg <- cbind("HIV Neg",  paste0(nrow(extant[which(extant$hiv == 0), ]), " (", round(100 * nrow(extant[which(extant$hiv == 0), ]) /
                                                                                         nrow(extant), 1), ")"),

                round(mean(bothmain$duration[bothmain$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$duration[bothmain$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$duration[bothmain$hiv == 0], na.rm = TRUE), 1)),

                round(mean(bothcas$duration[bothcas$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$duration[bothcas$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$duration[bothcas$hiv == 0], na.rm = TRUE), 1)),

                round(mean(aimain$duration[aimain$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(aimain$duration[aimain$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$duration[aimain$hiv == 0], na.rm = TRUE), 1)),

                round(mean(aicas$duration[aicas$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(aicas$duration[aicas$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$duration[aicas$hiv == 0], na.rm = TRUE), 1)),

                round(mean(oimain$duration[oimain$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(oimain$duration[oimain$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$duration[oimain$hiv == 0], na.rm = TRUE), 1)),

                round(mean(oicas$duration[oicas$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(oicas$duration[oicas$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$duration[oicas$hiv == 0], na.rm = TRUE), 1)))

# Output table
table3 <- rbind(total, black, white, hispanic, other,
                fifteen24, Twentyfive29, Thirty39, Forty49, Fifty59, Sixty65,
                HIVPos, HIVNeg)
colnames(table3) <- c("Category", "N (%)", "Ongoing Either Main Mean", "Ongoing Either Main SD, Med",
                      "Ongoing Either Cas Mean", "Ongoing Either Cas SD, Med",
                      "Ongoing AI Main Mean", "Ongoing AI Main SD, Med",
                      "Ongoing AI Cas Mean", "Ongoing AI Cas SD, Med",
                      "Ongoing OI Main Mean", "Ongoing OI Main SD, Med",
                      "Ongoing OI Cas Mean", "Ongoing OI Cas SD, Med")
write.csv(table3, file = "table3.csv")


#all ongoing
extant <- artnetLong[which(artnetLong$p_ONGOING == 1 & artnetLong$ptype %in% c(1, 2)), ]

tot <- cbind("total", round(mean(extant$duration, na.rm = TRUE), 1),
         paste0(round(sd(extant$duration, na.rm = TRUE), 1),
                ", ",
                round(median(extant$duration, na.rm = TRUE), 1)))

black <- cbind("black", round(mean(extant$duration[which(extant$race.cat == "black")], na.rm = TRUE), 1),
             paste0(round(sd(extant$duration[which(extant$race.cat == "black")], na.rm = TRUE), 1),
                    ", ",
                    round(median(extant$duration[which(extant$race.cat == "black")], na.rm = TRUE), 1)))

white <- cbind("white", round(mean(extant$duration[which(extant$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(extant$duration[which(extant$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration[which(extant$race.cat == "white")], na.rm = TRUE), 1)))

hisp <- cbind("hispanic", round(mean(extant$duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1),
               paste0(round(sd(extant$duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1)))

other <- cbind("other", round(mean(extant$duration[which(extant$race.cat == "other")], na.rm = TRUE), 1),
              paste0(round(sd(extant$duration[which(extant$race.cat == "other")], na.rm = TRUE), 1),
                     ", ",
                     round(median(extant$duration[which(extant$race.cat == "other")], na.rm = TRUE), 1)))

age15_24 <- cbind("15_24", round(mean(extant$duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1),
              paste0(round(sd(extant$duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1),
                     ", ",
                     round(median(extant$duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1)))

age25_29 <- cbind("25_29", round(mean(extant$duration[which(extant$age.cat == "25-29")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$age.cat == "25-29")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$age.cat == "25-29")], na.rm = TRUE), 1)))

age30_39 <- cbind("30_39", round(mean(extant$duration[which(extant$age.cat == "30-39")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$age.cat == "30-39")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$age.cat == "30-39")], na.rm = TRUE), 1)))

age40_49 <- cbind("40_49", round(mean(extant$duration[which(extant$age.cat == "40-49")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$age.cat == "40-49")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$age.cat == "40-49")], na.rm = TRUE), 1)))

age50_59 <- cbind("50_59", round(mean(extant$duration[which(extant$age.cat == "50-59")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$age.cat == "50-59")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$age.cat == "50-59")], na.rm = TRUE), 1)))

age60_65 <- cbind("60_65", round(mean(extant$duration[which(extant$age.cat == "60-65")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$age.cat == "60-65")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$age.cat == "60-65")], na.rm = TRUE), 1)))

pos <- cbind("Positive", round(mean(extant$duration[which(extant$hiv == 1)], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$hiv == 1)], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$hiv == 1)], na.rm = TRUE), 1)))

neg <- cbind("Negative", round(mean(extant$duration[which(extant$hiv == 0)], na.rm = TRUE), 1),
             paste0(round(sd(extant$duration[which(extant$hiv == 0)], na.rm = TRUE), 1),
                    ", ",
                    round(median(extant$duration[which(extant$hiv == 0)], na.rm = TRUE), 1)))

table3_new <- rbind(tot, black, white, hisp, other,
                age15_24, age25_29, age30_39, age40_49, age50_59, age60_65,
                pos, neg)
write.csv(table3_new, file = "table3_new.csv")

# Table 4 - Mixing --------------
total <- cbind("Total", nrow(artnetLong), nrow(artnetLong) / nrow(artnetLong),
               nrow(artnetLong[which(artnetLong$ptype == 1), ]),
               nrow(artnetLong[which(artnetLong$ptype == 1), ]) /
                 nrow(artnetLong[which(artnetLong$ptype == 1), ]),
               nrow(artnetLong[which(artnetLong$ptype == 2), ]),
               nrow(artnetLong[which(artnetLong$ptype == 2), ]) /
                 nrow(artnetLong[which(artnetLong$ptype == 2), ]),
               nrow(artnetLong[which(artnetLong$ptype == 3), ]),
               nrow(artnetLong[which(artnetLong$ptype == 3), ]) /
                 nrow(artnetLong[which(artnetLong$ptype == 3), ]))

main <- artnetLong[which(artnetLong$ptype == 1), ]
cas <- artnetLong[which(artnetLong$ptype == 2), ]
inst <- artnetLong[which(artnetLong$ptype == 3), ]

# Race/ethnicity
tot <- artnetLong %>% count(race.cat, partracecat)
a <- main %>% count(race.cat, partracecat)
# Black-black, black-hisp, black-other, black-white, hisp-hisp, hisp-other,
# hisp-white, other-other
racemain <- cbind(cbind(c("Black-Black", "Black-Hisp", "Black-Other", "Black-White",
                          "Hisp-Hisp", "Hisp-Other", "Hisp-White", "Other-Other",
                          "Other-White", "White-White", NA)),
                  cbind(c(sum(tot$n[1]), sum(tot$n[c(2, 6)]), sum(tot$n[c(3, 11)]),
                          sum(tot$n[c(4, 16)]), sum(tot$n[7]), sum(tot$n[c(8, 12)]),
                          sum(tot$n[c(9, 17)]), sum(tot$n[c(13)]),
                          sum(tot$n[c(14, 18)]), sum(tot$n[19]), sum(tot$n[c(5, 10, 15, 20)]))),
                  rbind((100 * tot$n[1] / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(2, 6)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(3, 11)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(4, 16)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(7)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(8, 12)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(9, 17)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(13)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(14, 18)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(19)]) / sum(tot$n[c(1:4, 6:9, 11:14, 16:19)])),
                        (100 * sum(tot$n[c(5, 10, 15, 20)]) / sum(tot$n))),
                 cbind(c(sum(a$n[1]), sum(a$n[c(2, 5)]), sum(a$n[c(3, 10)]),
                             sum(a$n[c(4, 15)]), sum(a$n[6]), sum(a$n[c(7, 11)]),
                                 sum(a$n[c(8, 16)]), sum(a$n[c(12)]),
                                 sum(a$n[c(13, 17)]), sum(a$n[18]), sum(a$n[c(9, 14, 19)]))),
                 rbind((100 * a$n[1] / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(2, 5)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(3, 10)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(4, 15)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(6)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(7, 11)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(8, 16)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(12)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(13, 17)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(18)]) / sum(a$n[c(1:8, 10:13, 15:18)])),
                         (100 * sum(a$n[c(9, 14, 19)]) / sum(a$n))))

b <- cas %>% count(race.cat, partracecat)
racecas <- cbind(cbind(c(sum(b$n[1]), sum(b$n[c(2, 6)]), sum(b$n[c(3, 11)]),
                             sum(b$n[c(4, 16)]), sum(b$n[7]), sum(b$n[c(8, 12)]),
                             sum(b$n[c(9, 17)]), sum(b$n[c(13)]),
                             sum(b$n[c(14, 18)]), sum(b$n[19]), sum(b$n[c(5, 10, 15, 20)]))),
                 rbind((100 * b$n[1] / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(2, 6)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(3, 11)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(4, 16)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(7)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(8, 12)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(9, 17)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(13)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(14, 18)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(19)]) / sum(b$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(b$n[c(5, 10, 15, 20)]) / sum(b$n))))

c <- inst %>% count(race.cat, partracecat)
raceinst <- cbind(cbind(c(sum(c$n[1]), sum(c$n[c(2, 6)]), sum(c$n[c(3, 11)]),
                         sum(c$n[c(4, 16)]), sum(c$n[7]), sum(c$n[c(8, 12)]),
                         sum(c$n[c(9, 17)]), sum(c$n[c(13)]),
                         sum(c$n[c(14, 18)]), sum(c$n[19]), sum(c$n[c(5, 10, 15, 20)]))),
                 rbind((100 * c$n[1] / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(2, 6)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(3, 11)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(4, 16)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(7)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(8, 12)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(9, 17)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(13)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(14, 18)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(19)]) / sum(c$n[c(1:4, 6:9, 11:14, 16:19)])),
                       (100 * sum(c$n[c(5, 10, 15, 20)]) / sum(c$n))))

# # Age (Categorical)
# tot <- artnetLong %>% count(edgeage)
# a <- main %>% count(edgeage)
# b <- cas %>% count(edgeage)
# c <- inst %>% count(edgeage)
#
# agecatmain <- cbind(cbind(c(a$edgeage[1:4])),
#                     cbind(c(tot$n[1:4])),
#                     cbind(rbind((100 * tot$n[1] / sum(tot$n[1:3])),
#                                 (100 * tot$n[2] / sum(tot$n[1:3])),
#                                 (100 * tot$n[3] / sum(tot$n[1:3])),
#                                 (100 * tot$n[3] / sum(tot$n)))),
#                     cbind(c(a$n[1:4])),
#                     cbind(rbind((100 * a$n[1] / sum(a$n[1:3])),
#                     (100 * a$n[2] / sum(a$n[1:3])),
#                     (100 * a$n[3] / sum(a$n[1:3])),
#                     (100 * a$n[3] / sum(a$n)))))
#
# agecatcas <- cbind(cbind(c(b$n[1:4])),
#                    cbind(rbind((100 * b$n[1] / sum(b$n[1:3])),
#                                (100 * b$n[2] / sum(b$n[1:3])),
#                                (100 * b$n[3] / sum(b$n[1:3])),
#                                (100 * b$n[3] / sum(b$n)))))
#
# agecatinst <- cbind(cbind(c(c$n[1:4])),
#                     cbind(rbind((100 * c$n[1] / sum(c$n[1:3])),
#                                 (100 * c$n[2] / sum(c$n[1:3])),
#                                 (100 * c$n[3] / sum(c$n[1:3])),
#                                 (100 * c$n[3] / sum(c$n)))))

# HIV status
tot <- artnetLong %>% count(partstatuses)
a <- main %>% count(partstatuses)
b <- cas %>% count(partstatuses)
c <- inst %>% count(partstatuses)

hivmain <- cbind(cbind(c(tot$partstatuses[c(1:6)])),
                 cbind(c(tot$n[1:6])),
                 cbind(rbind((100 * tot$n[1] / sum(tot$n[1:5])),
                             (100 * tot$n[2] / sum(tot$n[1:5])),
                             (100 * tot$n[3] / sum(tot$n[1:5])),
                             (100 * tot$n[4] / sum(tot$n[1:5])),
                             (100 * tot$n[5] / sum(tot$n[1:5])),
                             (100 * tot$n[6] / sum(tot$n)))),
                 cbind(c(a$n[1:6])),
                 cbind(rbind((100 * a$n[1] / sum(a$n[1:5])),
                             (100 * a$n[2] / sum(a$n[1:5])),
                             (100 * a$n[3] / sum(a$n[1:5])),
                             (100 * a$n[4] / sum(a$n[1:5])),
                             (100 * a$n[5] / sum(a$n[1:5])),
                             (100 * a$n[6] / sum(a$n)))))
hivcas <- cbind(cbind(c(b$n[1:6])),
                cbind(rbind((100 * b$n[1] / sum(b$n[1:5])),
                            (100 * b$n[2] / sum(b$n[1:5])),
                            (100 * b$n[3] / sum(b$n[1:5])),
                            (100 * b$n[4] / sum(b$n[1:5])),
                            (100 * b$n[5] / sum(b$n[1:5])),
                            (100 * b$n[6] / sum(b$n)))))

hivinst <- cbind(cbind(c(c$n[1:6])),
                 cbind(rbind((100 * c$n[1] / sum(c$n[1:5])),
                             (100 * c$n[2] / sum(c$n[1:5])),
                             (100 * c$n[3] / sum(c$n[1:5])),
                             (100 * c$n[4] / sum(c$n[1:5])),
                             (100 * c$n[5] / sum(c$n[1:5])),
                             (100 * c$n[6] / sum(c$n)))))

# Age (sqrt)
agecontin <- rbind(cbind("Diff",
                         mean(artnetLong$edgeagediff, na.rm = TRUE),
                         sd(artnetLong$edgeagediff, na.rm = TRUE),
                         median(artnetLong$edgeagediff, na.rm = TRUE),
                         mean(main$edgeagediff, na.rm = TRUE),
                         sd(main$edgeagediff, na.rm = TRUE),
                         median(main$edgeagediff, na.rm = TRUE),
                         mean(cas$edgeagediff, na.rm = TRUE),
                         sd(cas$edgeagediff, na.rm = TRUE),
                         median(cas$edgeagediff, na.rm = TRUE),
                         mean(inst$edgeagediff, na.rm = TRUE),
                         sd(inst$edgeagediff, na.rm = TRUE),
                         median(inst$edgeagediff, na.rm = TRUE)))

sqrtagecontin <- rbind(cbind("Sqrt Diff",
                             mean(artnetLong$sqrtedgeagediff, na.rm = TRUE),
                             sd(artnetLong$sqrtedgeagediff, na.rm = TRUE),
                             median(artnetLong$sqrtedgeagediff, na.rm = TRUE),
                             mean(main$sqrtedgeagediff, na.rm = TRUE),
                             sd(main$sqrtedgeagediff, na.rm = TRUE),
                             median(main$sqrtedgeagediff, na.rm = TRUE),
                             mean(cas$sqrtedgeagediff, na.rm = TRUE),
                             sd(cas$sqrtedgeagediff, na.rm = TRUE),
                             median(cas$sqrtedgeagediff, na.rm = TRUE),
                             mean(inst$sqrtedgeagediff, na.rm = TRUE),
                             sd(inst$sqrtedgeagediff, na.rm = TRUE),
                             median(inst$sqrtedgeagediff, na.rm = TRUE)),
                       cbind("NA",
                             length(which(is.na(artnetLong$sqrtedgeagediff))),
                             length(which(is.na(artnetLong$sqrtedgeagediff))),
                             length(which(is.na(artnetLong$sqrtedgeagediff))),
                             length(which(is.na(main$sqrtedgeagediff))),
                             length(which(is.na(main$sqrtedgeagediff))),
                             length(which(is.na(main$sqrtedgeagediff))),
                             length(which(is.na(cas$sqrtedgeagediff))),
                             length(which(is.na(cas$sqrtedgeagediff))),
                             length(which(is.na(cas$sqrtedgeagediff))),
                             length(which(is.na(inst$sqrtedgeagediff))),
                             length(which(is.na(inst$sqrtedgeagediff))),
                             length(which(is.na(inst$sqrtedgeagediff)))))


# Output table
table4a <- rbind(total,
                cbind(racemain, racecas, raceinst),
                cbind(hivmain, hivcas, hivinst))
colnames(table4a) <- c("Category", "Total N", "Total %", "Main N", "Main %",
                      "Casual N", "Casual %", "Inst N", "Inst %")
table4b <- rbind(agecontin,
                sqrtagecontin)
colnames(table4b) <- c("Category", "Total Mean", "Total SD", "Total Median",
                       "Main Mean", "Main SD", "Main Median",
                       "Cas Mean", "Cas SD", "Cas Median",
                       "Inst Mean", "Inst SD", "Inst Median")

write.csv(table4a, file = "table4a.csv")
write.csv(table4b, file = "table4b.csv")

# Supp Table 1 Main Mean Degree --------------

# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$p_ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

df4 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1 | p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(maintotdegree = sum(ongoing2))

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

# Create merged dataframe
artnet3 <- left_join(artnet, df4, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df5, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df6, by = "AMIS_ID")

table(artnet3$maintotdegree, useNA = "always")
table(artnet3$mainaionlydegree, useNA = "always")
table(artnet3$mainoionlydegree, useNA = "always")

# If missing degree values, then set to 0
artnet3$maintotdegree <- ifelse(is.na(artnet3$maintotdegree), 0, artnet3$maintotdegree)
artnet3$mainaionlydegree <- ifelse(is.na(artnet3$mainaionlydegree), 0, artnet3$mainaionlydegree)
artnet3$mainoionlydegree <- ifelse(is.na(artnet3$mainoionlydegree), 0, artnet3$mainoionlydegree)

# City
city <- glm(maintotdegree ~ city - 1, family = "poisson", data = artnet3)
city <- round(cbind(exp(coef(city)), exp(confint(city))), 2)
cityai <- glm(mainaionlydegree ~ city - 1, family = "poisson", data = artnet3)
cityai <- round(cbind(exp(coef(cityai)), exp(confint(cityai))), 2)
cityoi <- glm(mainoionlydegree ~ city - 1, family = "poisson", data = artnet3)
cityai <- round(cbind(exp(coef(cityoi)), exp(confint(cityoi))), 2)

# Overall Poisson:
total <- glm(maintotdegree ~ 1, family = "poisson", data = artnet3)
total <- round(cbind(exp(coef(total)), rbind(exp(confint(total)))), 2)
totalai <- glm(mainaionlydegree ~ 1, family = "poisson", data = artnet3)
totalai <- round(cbind(exp(coef(totalai)), rbind(exp(confint(totalai)))), 2)
totaloi <- glm(mainoionlydegree ~ 1, family = "poisson", data = artnet3)
totaloi <- round(cbind(exp(coef(totaloi)), rbind(exp(confint(totaloi)))), 2)

# double check predictions against empirical means
group_by(artnet3) %>% summarize(mean(maintotdegree))
group_by(artnet3) %>% summarize(mean(mainaionlydegree))
group_by(artnet3) %>% summarize(mean(mainoionlydegree))

total <- cbind("Total",
               total[1, 1],
               paste0(total[1, 2],
                      " - ",
                      total[1, 3]),
               totalai[1, 1],
               paste0(totalai[1, 2],
                      " - ",
                      totalai[1, 3]),
               totaloi[1, 1],
               paste0(totaloi[1, 2],
                      " - ",
                      totaloi[1, 3]))

# Race/ethnicity
race <- glm(maintotdegree ~ race.cat - 1, family = "poisson", data = artnet3)
race <- round(cbind(exp(coef(race)), exp(confint(race))), 2)
raceai <- glm(mainaionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
raceai <- round(cbind(exp(coef(raceai)), exp(confint(raceai))), 2)
raceoi <- glm(mainoionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
raceoi <- round(cbind(exp(coef(raceoi)), exp(confint(raceoi))), 2)

# double check predictions against empirical means
group_by(artnet3, race.cat) %>% summarize(mean(maintotdegree))
group_by(artnet3, race.cat) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, race.cat) %>% summarize(mean(mainoionlydegree))

black <- cbind("black",
               race[1, 1],
               paste0(race[1, 2],
                      " - ",
                      race[1, 3]),
               raceai[1, 1],
               paste0(raceai[1, 2],
                      " - ",
                      raceai[1, 3]),
               raceoi[1, 1],
               paste0(raceoi[1, 2],
                      " - ",
                      raceoi[1, 3]))

white <- cbind("white",
               race[4, 1],
               paste0(race[4, 2],
                      " - ",
                      race[4, 3]),
               raceai[4, 1],
               paste0(raceai[4, 2],
                      " - ",
                      raceai[4, 3]),
               raceoi[4, 1],
               paste0(raceoi[4, 2],
                      " - ",
                      raceoi[4, 3]))

hispanic <- cbind("hispanic",
                  race[2, 1],
                  paste0(race[2, 2],
                         " - ",
                         race[2, 3]),
                  raceai[2, 1],
                  paste0(raceai[2, 2],
                         " - ",
                         raceai[2, 3]),
                  raceoi[2, 1],
                  paste0(raceoi[2, 2],
                         " - ",
                         raceoi[2, 3]))

other <- cbind("other",
               race[3, 1],
               paste0(race[3, 2],
                      " - ",
                      race[3, 3]),
               raceai[3, 1],
               paste0(raceai[3, 2],
                      " - ",
                      raceai[3, 3]),
               raceoi[3, 1],
               paste0(raceoi[3, 2],
                      " - ",
                      raceoi[3, 3]))

# Age
age <- glm(maintotdegree ~ age.cat - 1, family = "poisson", data = artnet3)
age <- round(cbind(exp(coef(age)), exp(confint(age))), 2)
ageai <- glm(mainaionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
ageai <- round(cbind(exp(coef(ageai)), exp(confint(ageai))), 2)
ageoi <- glm(mainoionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
ageoi <- round(cbind(exp(coef(ageoi)), exp(confint(ageoi))), 2)

# double check predictions against empirical means
group_by(artnet3, age.cat) %>% summarize(mean(maintotdegree))
group_by(artnet3, age.cat) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, age.cat) %>% summarize(mean(mainoionlydegree))


fifteen24 <- cbind("15-24",
                   age[1, 1],
                   paste0(age[1, 2],
                          " - ",
                          age[1, 3]),
                   ageai[1, 1],
                   paste0(ageai[1, 2],
                          " - ",
                          ageai[1, 3]),
                   ageoi[1, 1],
                   paste0(ageoi[1, 2],
                          " - ",
                          ageoi[1, 3]))


twentyfive29 <- cbind("25-29",
                      age[2, 1],
                      paste0(age[2, 2],
                             " - ",
                             age[2, 3]),
                      ageai[2, 1],
                      paste0(ageai[2, 2],
                             " - ",
                             ageai[2, 3]),
                      ageoi[2, 1],
                      paste0(ageoi[2, 2],
                             " - ",
                             ageoi[2, 3]))

thirty39 <- cbind("30-39",
                  age[3, 1],
                  paste0(age[3, 2],
                         " - ",
                         age[3, 3]),
                  ageai[3, 1],
                  paste0(ageai[3, 2],
                         " - ",
                         ageai[3, 3]),
                  ageoi[3, 1],
                  paste0(ageoi[3, 2],
                         " - ",
                         ageoi[3, 3]))

forty49 <- cbind("40-49",
                 age[4, 1],
                 paste0(age[4, 2],
                        " - ",
                        age[4, 3]),
                 ageai[4, 1],
                 paste0(ageai[4, 2],
                        " - ",
                        ageai[4, 3]),
                 ageoi[4, 1],
                 paste0(ageoi[4, 2],
                        " - ",
                        ageoi[4, 3]))

fifty59 <- cbind("50-59",
                 age[5, 1],
                 paste0(age[5, 2],
                        " - ",
                        age[5, 3]),
                 ageai[5, 1],
                 paste0(ageai[5, 2],
                        " - ",
                        ageai[5, 3]),
                 ageoi[5, 1],
                 paste0(ageoi[5, 2],
                        " - ",
                        ageoi[5, 3]))

sixty65 <- cbind("60-65",
                 age[6, 1],
                 paste0(age[6, 2],
                        " - ",
                        age[6, 3]),
                 ageai[6, 1],
                 paste0(ageai[6, 2],
                        " - ",
                        ageai[6, 3]),
                 ageoi[6, 1],
                 paste0(ageoi[6, 2],
                        " - ",
                        ageoi[6, 3]))


# Region and division
division <- glm(maintotdegree ~ division - 1, family = "poisson", data = artnet3)
division <- round(cbind(exp(coef(division)), exp(confint(division))), 2)
divisionai <- glm(mainaionlydegree ~ division - 1, family = "poisson", data = artnet3)
divisionai <- round(cbind(exp(coef(divisionai)), exp(confint(divisionai))), 2)
divisionoi <- glm(mainoionlydegree ~ division - 1, family = "poisson", data = artnet3)
divisionoi <- round(cbind(exp(coef(divisionoi)), exp(confint(divisionoi))), 2)

region <- glm(maintotdegree ~ region - 1, family = "poisson", data = artnet3)
region <- round(cbind(exp(coef(region)), exp(confint(region))), 2)
regionai <- glm(mainaionlydegree ~ region - 1, family = "poisson", data = artnet3)
regionai <- round(cbind(exp(coef(regionai)), exp(confint(regionai))), 2)
regionoi <- glm(mainoionlydegree ~ region - 1, family = "poisson", data = artnet3)
regionoi <- round(cbind(exp(coef(regionoi)), exp(confint(regionoi))), 2)

# double check predictions against empirical means
group_by(artnet3, region) %>% summarize(mean(maintotdegree))
group_by(artnet3, region) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, region) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, division) %>% summarize(mean(maintotdegree))
group_by(artnet3, division) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, division) %>% summarize(mean(mainoionlydegree))


# Region and Division
West <- cbind("West",
              region[4, 1],
              paste0(region[4, 2],
                     " - ",
                     region[4, 3]),
              regionai[4, 1],
              paste0(regionai[4, 2],
                     " - ",
                     regionai[4, 3]),
              regionoi[4, 1],
              paste0(regionoi[4, 2],
                     " - ",
                     regionoi[4, 3]))

Pacific <- cbind("Pacific",
                 division[6, 1],
                 paste0(division[6, 2],
                        " - ",
                        division[6, 3]),
                 divisionai[6, 1],
                 paste0(divisionai[6, 2],
                        " - ",
                        divisionai[6, 3]),
                 divisionoi[6, 1],
                 paste0(divisionoi[6, 2],
                        " - ",
                        divisionoi[6, 3]))

Mountain <- cbind("Mountain",
                  division[4, 1],
                  paste0(division[4, 2],
                         " - ",
                         division[4, 3]),
                  divisionai[4, 1],
                  paste0(divisionai[4, 2],
                         " - ",
                         divisionai[4, 3]),
                  divisionoi[4, 1],
                  paste0(divisionoi[4, 2],
                         " - ",
                         divisionoi[4, 3]))

Midwest <- cbind("Midwest",
                 region[1, 1],
                 paste0(region[1, 2],
                        " - ",
                        region[1, 3]),
                 regionai[1, 1],
                 paste0(regionai[1, 2],
                        " - ",
                        regionai[1, 3]),
                 regionoi[1, 1],
                 paste0(regionoi[1, 2],
                        " - ",
                        regionoi[1, 3]))

WNC <- cbind("West North Central",
             division[8, 1],
             paste0(division[8, 2],
                    " - ",
                    division[8, 3]),
             divisionai[8, 1],
             paste0(divisionai[8, 2],
                    " - ",
                    divisionai[8, 3]),
             divisionoi[8, 1],
             paste0(divisionoi[8, 2],
                    " - ",
                    divisionoi[8, 3]))

ENC <- cbind("East North Central",
             division[1, 1],
             paste0(division[1, 2],
                    " - ",
                    division[1, 3]),
             divisionai[1, 1],
             paste0(divisionai[1, 2],
                    " - ",
                    divisionai[1, 3]),
             divisionoi[1, 1],
             paste0(divisionoi[1, 2],
                    " - ",
                    divisionoi[1, 3]))

South <- cbind("South",
               region[3, 1],
               paste0(region[3, 2],
                      " - ",
                      region[3, 3]),
               regionai[3, 1],
               paste0(regionai[3, 2],
                      " - ",
                      regionai[3, 3]),
               regionoi[3, 1],
               paste0(regionoi[3, 2],
                      " - ",
                      regionoi[3, 3]))

WSC <- cbind("West South Central",
             division[9, 1],
             paste0(division[9, 2],
                    " - ",
                    division[9, 3]),
             divisionai[9, 1],
             paste0(divisionai[9, 2],
                    " - ",
                    divisionai[9, 3]),
             divisionoi[9, 1],
             paste0(divisionoi[9, 2],
                    " - ",
                    divisionoi[9, 3]))

ESC <- cbind("East South Central",
             division[2, 1],
             paste0(division[2, 2],
                    " - ",
                    division[2, 3]),
             divisionai[2, 1],
             paste0(divisionai[2, 2],
                    " - ",
                    divisionai[2, 3]),
             divisionoi[2, 1],
             paste0(divisionoi[2, 2],
                    " - ",
                    divisionoi[2, 3]))
SA <- cbind("South Atlantic",
            division[7, 1],
            paste0(division[7, 2],
                   " - ",
                   division[7, 3]),
            divisionai[7, 1],
            paste0(divisionai[7, 2],
                   " - ",
                   divisionai[7, 3]),
            divisionoi[7, 1],
            paste0(divisionoi[7, 2],
                   " - ",
                   divisionoi[7, 3]))

Northeast <- cbind("Northeast",
                   region[2, 1],
                   paste0(region[2, 2],
                          " - ",
                          region[2, 3]),
                   regionai[2, 1],
                   paste0(regionai[2, 2],
                          " - ",
                          regionai[2, 3]),
                   regionoi[2, 1],
                   paste0(regionoi[2, 2],
                          " - ",
                          regionoi[2, 3]))

MA <- cbind("Middle Atlantic",
            division[3, 1],
            paste0(division[3, 2],
                   " - ",
                   division[3, 3]),
            divisionai[3, 1],
            paste0(divisionai[3, 2],
                   " - ",
                   divisionai[3, 3]),
            divisionoi[3, 1],
            paste0(divisionoi[3, 2],
                   " - ",
                   divisionoi[3, 3]))

NE <- cbind("New England",
            division[5, 1],
            paste0(division[5, 2],
                   " - ",
                   division[5, 3]),
            divisionai[5, 1],
            paste0(divisionai[5, 2],
                   " - ",
                   divisionai[5, 3]),
            divisionoi[5, 1],
            paste0(divisionoi[5, 2],
                   " - ",
                   divisionoi[5, 3]))

# Urbanicity
urban <- glm(maintotdegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urban <- round(cbind(exp(coef(urban)), exp(confint(urban))), 2)
urbanai <- glm(mainaionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbanai <- round(cbind(exp(coef(urbanai)), exp(confint(urbanai))), 2)
urbanoi <- glm(mainoionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbanoi <- round(cbind(exp(coef(urbanoi)), exp(confint(urbanoi))), 2)

# double check predictions against empirical means
group_by(artnet3, NCHSCHAR) %>% summarize(mean(maintotdegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(mainoionlydegree))

LCM <- cbind("Large Central Metro",
             urban[1, 1],
             paste0(urban[1, 2],
                    " - ",
                    urban[1, 3]),
             urbanai[1, 1],
             paste0(urbanai[1, 2],
                    " - ",
                    urbanai[1, 3]),
             urbanoi[1, 1],
             paste0(urbanoi[1, 2],
                    " - ",
                    urbanoi[1, 3]))

LFM <- cbind("Large Fringe Metro",
             urban[2, 1],
             paste0(urban[2, 2],
                    " - ",
                    urban[2, 3]),
             urbanai[2, 1],
             paste0(urbanai[2, 2],
                    " - ",
                    urbanai[2, 3]),
             urbanoi[2, 1],
             paste0(urbanoi[2, 2],
                    " - ",
                    urbanoi[2, 3]))
Medium <- cbind("Medium Metro",
                urban[3, 1],
                paste0(urban[3, 2],
                       " - ",
                       urban[3, 3]),
                urbanai[3, 1],
                paste0(urbanai[3, 2],
                       " - ",
                       urbanai[3, 3]),
                urbanoi[3, 1],
                paste0(urbanoi[3, 2],
                       " - ",
                       urbanoi[3, 3]))
Small <- cbind("Small Metro",
               urban[6, 1],
               paste0(urban[6, 2],
                      " - ",
                      urban[6, 3]),
               urbanai[6, 1],
               paste0(urbanai[6, 2],
                      " - ",
                      urbanai[6, 3]),
               urbanoi[6, 1],
               paste0(urbanoi[6, 2],
                      " - ",
                      urbanoi[6, 3]))
Micro <- cbind("Micropolitan",
               urban[4, 1],
               paste0(urban[4, 2],
                      " - ",
                      urban[4, 3]),
               urbanai[4, 1],
               paste0(urbanai[4, 2],
                      " - ",
                      urbanai[4, 3]),
               urbanoi[4, 1],
               paste0(urbanoi[4, 2],
                      " - ",
                      urbanoi[4, 3]))
Noncore <- cbind("Noncore",
                 urban[5, 1],
                 paste0(urban[5, 2],
                        " - ",
                        urban[5, 3]),
                 urbanai[5, 1],
                 paste0(urbanai[5, 2],
                        " - ",
                        urbanai[5, 3]),
                 urbanoi[5, 1],
                 paste0(urbanoi[5, 2],
                        " - ",
                        urbanoi[5, 3]))


# HIV Status
hivstat <- glm(maintotdegree ~ hiv, family = "poisson", data = artnet3)
hivstat <- round(cbind(exp(coef(hivstat)), rbind(exp(confint(hivstat)))), 2)
hivstatv2 <- glm(maintotdegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatv2 <- round(cbind(exp(coef(hivstatv2)), rbind(exp(confint(hivstatv2)))), 2)

hivstatai <- glm(mainaionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatai <- round(cbind(exp(coef(hivstatai)), rbind(exp(confint(hivstatai)))), 2)
hivstataiv2 <- glm(mainaionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstataiv2 <- round(cbind(exp(coef(hivstataiv2)), rbind(exp(confint(hivstataiv2)))), 2)

hivstatoi <- glm(mainoionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatoi <- round(cbind(exp(coef(hivstatoi)), rbind(exp(confint(hivstatoi)))), 2)
hivstatoiv2 <- glm(mainoionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatoiv2 <- round(cbind(exp(coef(hivstatoiv2)), rbind(exp(confint(hivstatoiv2)))), 2)

# double check predictions against empirical means
group_by(artnet3, hiv) %>% summarize(mean(maintotdegree))
group_by(artnet3, hiv) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, hiv) %>% summarize(mean(mainoionlydegree))

HIVPos <- cbind("HIV Pos",
                hivstatv2[1, 1],
                paste0(hivstatv2[1, 2],
                       " - ",
                       hivstatv2[1, 3]),
                hivstataiv2[1, 1],
                paste0(hivstataiv2[1, 2],
                       " - ",
                       hivstataiv2[1, 3]),
                hivstatoiv2[1, 1],
                paste0(hivstatoiv2[1, 2],
                       " - ",
                       hivstatoiv2[1, 3]))
HIVNeg <- cbind("HIV Neg",
                hivstat[1, 1],
                paste0(hivstat[1, 2],
                       " - ",
                       hivstat[1, 3]),
                hivstatai[1, 1],
                paste0(hivstatai[1, 2],
                       " - ",
                       hivstatai[1, 3]),
                hivstatoi[1, 1],
                paste0(hivstatoi[1, 2],
                       " - ",
                       hivstatoi[1, 3]))


# Other types of statistics
# % of egos Concurrent
concurr <- cbind("Concurr",
                 paste0(length(which(artnet3$maintotdegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$maintotdegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$mainaionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$mainaionlydegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$mainoionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$mainoionlydegree > 1)) / nrow(artnet3), 2), "%)"))

# Rate of one-offs
# Create OI partners variable
# artnet3$oi.part <- rep(NA, nrow(artnet3))
# artnet3$oi.part <- artnet3$cuml.pnum - artnet3$ai.part
# artnet3$oi.part[artnet3$oi.part < 0] <- 0 # 1 person with -87
#
# # Create count variables for AI or OI
# d <- artnet3
# l <- artnetLong
# d <- l %>%
#   filter(ROI == 1 | IOI == 1 | RAI == 1 | IAI == 1) %>%
#   filter(ptype == 1) %>%
#   group_by(AMIS_ID) %>%
#   count() %>%
#   rename(count.mc.aioi.part = n) %>%
#   right_join(d, by = "AMIS_ID")
# d$count.mc.aioi.part <- ifelse(is.na(d$count.mc.aioi.part), 0, d$count.mc.aioi.part)
# d$count.mc.aioi.part
# d$count.oo.aioi.part <- d$cuml.pnum - d$count.mc.aioi.part
# d$count.oo.aioi.part <- pmax(0, d$count.oo.aioi.part)
# data.frame(d$cuml.pnum, d$count.mc.aioi.part, d$count.oo.aioi.part)
# summary(d$count.oo.aioi.part)
#
# plot(density(d$count.oo.aioi.part, na.rm = TRUE, from = 0))
# plot(density(d$count.oo.aioi.part, na.rm = TRUE, from = 0), xlim = c(0, 100))
#
# # daily rate
# d$rate.oo.aioi.part <- d$count.oo.aioi.part/365
# d$rate.oo.aioi.part
#
# # Create count variables for AI
# d2 <- l %>%
#   filter(RAI == 1 | IAI == 1) %>%
#   filter(ptype == 1) %>%
#   group_by(AMIS_ID) %>%
#   count() %>%
#   rename(count.mc.ai.part = n) %>%
#   right_join(d, by = "AMIS_ID")
# d2$count.mc.ai.part <- ifelse(is.na(d2$count.mc.ai.part), 0, d2$count.mc.ai.part)
# d2$count.mc.ai.part
#
# d2$count.oo.ai.part <- d2$ai.part - d2$count.mc.ai.part
# d2$count.oo.ai.part <- pmax(0, d2$count.oo.ai.part)
# data.frame(d2$ai.part, d2$count.mc.ai.part, d2$count.oo.ai.part)
# summary(d2$count.oo.ai.part)
#
# plot(density(d2$count.oo.ai.part, na.rm = TRUE, from = 0))
# plot(density(d2$count.oo.ai.part, na.rm = TRUE, from = 0), xlim = c(0, 100))
#
# # daily rate
# d2$rate.oo.ai.part <- d2$count.oo.ai.part/365
# d2$rate.oo.ai.part
#
# # Create count variables for OI
# d3 <- l %>%
#   filter(ROI == 1 | IOI == 1) %>%
#   filter(ptype == 1) %>%
#   group_by(AMIS_ID) %>%
#   count() %>%
#   rename(count.mc.oi.part = n) %>%
#   right_join(d, by = "AMIS_ID")
# d3$count.mc.oi.part <- ifelse(is.na(d3$count.mc.oi.part), 0, d3$count.mc.oi.part)
# d3$count.mc.oi.part
#
# d3$count.oo.oi.part <- d3$oi.part - d3$count.mc.oi.part
# d3$count.oo.oi.part <- pmax(0, d3$count.oo.oi.part)
# data.frame(d3$oi.part, d3$count.mc.oi.part, d3$count.oo.oi.part)
# summary(d3$count.oo.oi.part)
#
# plot(density(d3$count.oo.oi.part, na.rm = TRUE, from = 0))
# plot(density(d3$count.oo.oi.part, na.rm = TRUE, from = 0), xlim = c(0, 100))
#
# # doily rate
# d3$rate.oo.oi.part <- d3$count.oo.oi.part/365
# d3$rate.oo.oi.part
#
# oneoff <- cbind("One-off", round(mean(d$rate.oo.aioi.part, na.rm = TRUE), 4),
#                 round(mean(d2$rate.oo.ai.part, na.rm = TRUE), 4),
#                 round(mean(d3$rate.oo.oi.part, na.rm = TRUE), 4))


# Output table
stable1a <- rbind(total, black, white, hispanic, other,
                 fifteen24, twentyfive29, thirty39, forty49, fifty59, sixty65,
                 West, Pacific, Mountain, Midwest, WNC, ENC,
                 South, WSC, ESC, SA, Northeast, MA, NE, LCM, LFM, Medium,
                 Small, Micro, Noncore,
                 HIVPos, HIVNeg)
colnames(stable1a) <- c("Category", "Cas Ong Either Mean", " Cas Ong Either CI",
                       "Cas Ong AI Mean", "Cas Ong AI CI",
                       "Cas Ong OI Mean", "Cas Ong OI CI")
write.csv(stable1a, file = "stable1a.csv")

stable1b <- rbind(concurr)
# stable1b <- rbind(concurr, oneoff)
colnames(stable1b) <- c("Category", "AI/OI Mean", "AI Mean", "OI Mean")
write.csv(stable1b, file = "stable1b.csv")


# Supp Table 2 - Cas Mean Degree --------------
# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$p_ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

df7 <- l %>%
  filter(p_RAI == 1 | p_IAI == 1 | p_ROI == 1 | p_IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(castotdegree = sum(ongoing2))

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

# Create merged dataframe
artnet4 <- left_join(artnet, df7, by = "AMIS_ID")
artnet4 <- left_join(artnet4, df8, by = "AMIS_ID")
artnet4 <- left_join(artnet4, df9, by = "AMIS_ID")

artnet4$totdegree
table(artnet4$maintotdegree, useNA = "always")
table(artnet4$mainaionlydegree, useNA = "always")
table(artnet4$mainoionlydegree, useNA = "always")

# If missing degree values, then set to 0
artnet4$castotdegree <- ifelse(is.na(artnet4$castotdegree), 0, artnet4$castotdegree)
artnet4$casaionlydegree <- ifelse(is.na(artnet4$casaionlydegree), 0, artnet4$casaionlydegree)
artnet4$casoionlydegree <- ifelse(is.na(artnet4$casoionlydegree), 0, artnet4$casoionlydegree)


# City
city <- glm(castotdegree ~ city - 1, family = "poisson", data = artnet4)
city <- round(cbind(exp(coef(city)), exp(confint(city))), 2)
cityai <- glm(casaionlydegree ~ city - 1, family = "poisson", data = artnet4)
cityai <- round(cbind(exp(coef(cityai)), exp(confint(cityai))), 2)
cityoi <- glm(casoionlydegree ~ city - 1, family = "poisson", data = artnet4)
cityai <- round(cbind(exp(coef(cityoi)), exp(confint(cityoi))), 2)

# Overall Poisson:
total <- glm(castotdegree ~ 1, family = "poisson", data = artnet4)
total <- round(cbind(exp(coef(total)), rbind(exp(confint(total)))), 2)
totalai <- glm(casaionlydegree ~ 1, family = "poisson", data = artnet4)
totalai <- round(cbind(exp(coef(totalai)), rbind(exp(confint(totalai)))), 2)
totaloi <- glm(casoionlydegree ~ 1, family = "poisson", data = artnet4)
totaloi <- round(cbind(exp(coef(totaloi)), rbind(exp(confint(totaloi)))), 2)

# double check predictions against empirical means
group_by(artnet4) %>% summarize(mean(castotdegree))
group_by(artnet4) %>% summarize(mean(casaionlydegree))
group_by(artnet4) %>% summarize(mean(casoionlydegree))

total <- cbind("Total",
               total[1, 1],
               paste0(total[1, 2],
                      " - ",
                      total[1, 3]),
               totalai[1, 1],
               paste0(totalai[1, 2],
                      " - ",
                      totalai[1, 3]),
               totaloi[1, 1],
               paste0(totaloi[1, 2],
                      " - ",
                      totaloi[1, 3]))

# Race/ethnicity
race <- glm(castotdegree ~ race.cat - 1, family = "poisson", data = artnet4)
race <- round(cbind(exp(coef(race)), exp(confint(race))), 2)
raceai <- glm(casaionlydegree ~ race.cat - 1, family = "poisson", data = artnet4)
raceai <- round(cbind(exp(coef(raceai)), exp(confint(raceai))), 2)
raceoi <- glm(casoionlydegree ~ race.cat - 1, family = "poisson", data = artnet4)
raceoi <- round(cbind(exp(coef(raceoi)), exp(confint(raceoi))), 2)

# double check predictions against empirical means
group_by(artnet4, race.cat) %>% summarize(mean(castotdegree))
group_by(artnet4, race.cat) %>% summarize(mean(casaionlydegree))
group_by(artnet4, race.cat) %>% summarize(mean(casoionlydegree))

black <- cbind("black",
               race[1, 1],
               paste0(race[1, 2],
                      " - ",
                      race[1, 3]),
               raceai[1, 1],
               paste0(raceai[1, 2],
                      " - ",
                      raceai[1, 3]),
               raceoi[1, 1],
               paste0(raceoi[1, 2],
                      " - ",
                      raceoi[1, 3]))

white <- cbind("white",
               race[4, 1],
               paste0(race[4, 2],
                      " - ",
                      race[4, 3]),
               raceai[4, 1],
               paste0(raceai[4, 2],
                      " - ",
                      raceai[4, 3]),
               raceoi[4, 1],
               paste0(raceoi[4, 2],
                      " - ",
                      raceoi[4, 3]))

hispanic <- cbind("hispanic",
                  race[2, 1],
                  paste0(race[2, 2],
                         " - ",
                         race[2, 3]),
                  raceai[2, 1],
                  paste0(raceai[2, 2],
                         " - ",
                         raceai[2, 3]),
                  raceoi[2, 1],
                  paste0(raceoi[2, 2],
                         " - ",
                         raceoi[2, 3]))

other <- cbind("other",
               race[3, 1],
               paste0(race[3, 2],
                      " - ",
                      race[3, 3]),
               raceai[3, 1],
               paste0(raceai[3, 2],
                      " - ",
                      raceai[3, 3]),
               raceoi[3, 1],
               paste0(raceoi[3, 2],
                      " - ",
                      raceoi[3, 3]))

# Age
age <- glm(castotdegree ~ age.cat - 1, family = "poisson", data = artnet4)
age <- round(cbind(exp(coef(age)), exp(confint(age))), 2)
ageai <- glm(casaionlydegree ~ age.cat - 1, family = "poisson", data = artnet4)
ageai <- round(cbind(exp(coef(ageai)), exp(confint(ageai))), 2)
ageoi <- glm(casoionlydegree ~ age.cat - 1, family = "poisson", data = artnet4)
ageoi <- round(cbind(exp(coef(ageoi)), exp(confint(ageoi))), 2)

# double check predictions against empirical means
group_by(artnet4, age.cat) %>% summarize(mean(castotdegree))
group_by(artnet4, age.cat) %>% summarize(mean(casaionlydegree))
group_by(artnet4, age.cat) %>% summarize(mean(casoionlydegree))


fifteen24 <- cbind("15-24",
                   age[1, 1],
                   paste0(age[1, 2],
                          " - ",
                          age[1, 3]),
                   ageai[1, 1],
                   paste0(ageai[1, 2],
                          " - ",
                          ageai[1, 3]),
                   ageoi[1, 1],
                   paste0(ageoi[1, 2],
                          " - ",
                          ageoi[1, 3]))


twentyfive29 <- cbind("25-29",
                      age[2, 1],
                      paste0(age[2, 2],
                             " - ",
                             age[2, 3]),
                      ageai[2, 1],
                      paste0(ageai[2, 2],
                             " - ",
                             ageai[2, 3]),
                      ageoi[2, 1],
                      paste0(ageoi[2, 2],
                             " - ",
                             ageoi[2, 3]))

thirty39 <- cbind("30-39",
                  age[3, 1],
                  paste0(age[3, 2],
                         " - ",
                         age[3, 3]),
                  ageai[3, 1],
                  paste0(ageai[3, 2],
                         " - ",
                         ageai[3, 3]),
                  ageoi[3, 1],
                  paste0(ageoi[3, 2],
                         " - ",
                         ageoi[3, 3]))

forty49 <- cbind("40-49",
                 age[4, 1],
                 paste0(age[4, 2],
                        " - ",
                        age[4, 3]),
                 ageai[4, 1],
                 paste0(ageai[4, 2],
                        " - ",
                        ageai[4, 3]),
                 ageoi[4, 1],
                 paste0(ageoi[4, 2],
                        " - ",
                        ageoi[4, 3]))

fifty59 <- cbind("50-59",
                 age[5, 1],
                 paste0(age[5, 2],
                        " - ",
                        age[5, 3]),
                 ageai[5, 1],
                 paste0(ageai[5, 2],
                        " - ",
                        ageai[5, 3]),
                 ageoi[5, 1],
                 paste0(ageoi[5, 2],
                        " - ",
                        ageoi[5, 3]))

sixty65 <- cbind("60-65",
                 age[6, 1],
                 paste0(age[6, 2],
                        " - ",
                        age[6, 3]),
                 ageai[6, 1],
                 paste0(ageai[6, 2],
                        " - ",
                        ageai[6, 3]),
                 ageoi[6, 1],
                 paste0(ageoi[6, 2],
                        " - ",
                        ageoi[6, 3]))


# Region and division
division <- glm(castotdegree ~ division - 1, family = "poisson", data = artnet4)
division <- round(cbind(exp(coef(division)), exp(confint(division))), 2)
divisionai <- glm(casaionlydegree ~ division - 1, family = "poisson", data = artnet4)
divisionai <- round(cbind(exp(coef(divisionai)), exp(confint(divisionai))), 2)
divisionoi <- glm(casoionlydegree ~ division - 1, family = "poisson", data = artnet4)
divisionoi <- round(cbind(exp(coef(divisionoi)), exp(confint(divisionoi))), 2)

region <- glm(castotdegree ~ region - 1, family = "poisson", data = artnet4)
region <- round(cbind(exp(coef(region)), exp(confint(region))), 2)
regionai <- glm(casaionlydegree ~ region - 1, family = "poisson", data = artnet4)
regionai <- round(cbind(exp(coef(regionai)), exp(confint(regionai))), 2)
regionoi <- glm(casoionlydegree ~ region - 1, family = "poisson", data = artnet4)
regionoi <- round(cbind(exp(coef(regionoi)), exp(confint(regionoi))), 2)

# double check predictions against empirical means
group_by(artnet4, region) %>% summarize(mean(castotdegree))
group_by(artnet4, region) %>% summarize(mean(casaionlydegree))
group_by(artnet4, region) %>% summarize(mean(casoionlydegree))
group_by(artnet4, division) %>% summarize(mean(castotdegree))
group_by(artnet4, division) %>% summarize(mean(casaionlydegree))
group_by(artnet4, division) %>% summarize(mean(casoionlydegree))


# Region and Division
West <- cbind("West",
              region[4, 1],
              paste0(region[4, 2],
                     " - ",
                     region[4, 3]),
              regionai[4, 1],
              paste0(regionai[4, 2],
                     " - ",
                     regionai[4, 3]),
              regionoi[4, 1],
              paste0(regionoi[4, 2],
                     " - ",
                     regionoi[4, 3]))

Pacific <- cbind("Pacific",
                 division[6, 1],
                 paste0(division[6, 2],
                        " - ",
                        division[6, 3]),
                 divisionai[6, 1],
                 paste0(divisionai[6, 2],
                        " - ",
                        divisionai[6, 3]),
                 divisionoi[6, 1],
                 paste0(divisionoi[6, 2],
                        " - ",
                        divisionoi[6, 3]))

Mountain <- cbind("Mountain",
                  division[4, 1],
                  paste0(division[4, 2],
                         " - ",
                         division[4, 3]),
                  divisionai[4, 1],
                  paste0(divisionai[4, 2],
                         " - ",
                         divisionai[4, 3]),
                  divisionoi[4, 1],
                  paste0(divisionoi[4, 2],
                         " - ",
                         divisionoi[4, 3]))

Midwest <- cbind("Midwest",
                 region[1, 1],
                 paste0(region[1, 2],
                        " - ",
                        region[1, 3]),
                 regionai[1, 1],
                 paste0(regionai[1, 2],
                        " - ",
                        regionai[1, 3]),
                 regionoi[1, 1],
                 paste0(regionoi[1, 2],
                        " - ",
                        regionoi[1, 3]))

WNC <- cbind("West North Central",
             division[8, 1],
             paste0(division[8, 2],
                    " - ",
                    division[8, 3]),
             divisionai[8, 1],
             paste0(divisionai[8, 2],
                    " - ",
                    divisionai[8, 3]),
             divisionoi[8, 1],
             paste0(divisionoi[8, 2],
                    " - ",
                    divisionoi[8, 3]))

ENC <- cbind("East North Central",
             division[1, 1],
             paste0(division[1, 2],
                    " - ",
                    division[1, 3]),
             divisionai[1, 1],
             paste0(divisionai[1, 2],
                    " - ",
                    divisionai[1, 3]),
             divisionoi[1, 1],
             paste0(divisionoi[1, 2],
                    " - ",
                    divisionoi[1, 3]))

South <- cbind("South",
               region[3, 1],
               paste0(region[3, 2],
                      " - ",
                      region[3, 3]),
               regionai[3, 1],
               paste0(regionai[3, 2],
                      " - ",
                      regionai[3, 3]),
               regionoi[3, 1],
               paste0(regionoi[3, 2],
                      " - ",
                      regionoi[3, 3]))

WSC <- cbind("West South Central",
             division[9, 1],
             paste0(division[9, 2],
                    " - ",
                    division[9, 3]),
             divisionai[9, 1],
             paste0(divisionai[9, 2],
                    " - ",
                    divisionai[9, 3]),
             divisionoi[9, 1],
             paste0(divisionoi[9, 2],
                    " - ",
                    divisionoi[9, 3]))

ESC <- cbind("East South Central",
             division[2, 1],
             paste0(division[2, 2],
                    " - ",
                    division[2, 3]),
             divisionai[2, 1],
             paste0(divisionai[2, 2],
                    " - ",
                    divisionai[2, 3]),
             divisionoi[2, 1],
             paste0(divisionoi[2, 2],
                    " - ",
                    divisionoi[2, 3]))
SA <- cbind("South Atlantic",
            division[7, 1],
            paste0(division[7, 2],
                   " - ",
                   division[7, 3]),
            divisionai[7, 1],
            paste0(divisionai[7, 2],
                   " - ",
                   divisionai[7, 3]),
            divisionoi[7, 1],
            paste0(divisionoi[7, 2],
                   " - ",
                   divisionoi[7, 3]))

Northeast <- cbind("Northeast",
                   region[2, 1],
                   paste0(region[2, 2],
                          " - ",
                          region[2, 3]),
                   regionai[2, 1],
                   paste0(regionai[2, 2],
                          " - ",
                          regionai[2, 3]),
                   regionoi[2, 1],
                   paste0(regionoi[2, 2],
                          " - ",
                          regionoi[2, 3]))

MA <- cbind("Middle Atlantic",
            division[3, 1],
            paste0(division[3, 2],
                   " - ",
                   division[3, 3]),
            divisionai[3, 1],
            paste0(divisionai[3, 2],
                   " - ",
                   divisionai[3, 3]),
            divisionoi[3, 1],
            paste0(divisionoi[3, 2],
                   " - ",
                   divisionoi[3, 3]))

NE <- cbind("New England",
            division[5, 1],
            paste0(division[5, 2],
                   " - ",
                   division[5, 3]),
            divisionai[5, 1],
            paste0(divisionai[5, 2],
                   " - ",
                   divisionai[5, 3]),
            divisionoi[5, 1],
            paste0(divisionoi[5, 2],
                   " - ",
                   divisionoi[5, 3]))

# Urbanicity
urban <- glm(castotdegree ~ NCHSCHAR - 1, family = "poisson", data = artnet4)
urban <- round(cbind(exp(coef(urban)), exp(confint(urban))), 2)
urbanai <- glm(casaionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet4)
urbanai <- round(cbind(exp(coef(urbanai)), exp(confint(urbanai))), 2)
urbanoi <- glm(casoionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet4)
urbanoi <- round(cbind(exp(coef(urbanoi)), exp(confint(urbanoi))), 2)

# double check predictions against empirical means
group_by(artnet4, NCHSCHAR) %>% summarize(mean(castotdegree))
group_by(artnet4, NCHSCHAR) %>% summarize(mean(casaionlydegree))
group_by(artnet4, NCHSCHAR) %>% summarize(mean(casoionlydegree))

LCM <- cbind("Large Central Metro",
             urban[1, 1],
             paste0(urban[1, 2],
                    " - ",
                    urban[1, 3]),
             urbanai[1, 1],
             paste0(urbanai[1, 2],
                    " - ",
                    urbanai[1, 3]),
             urbanoi[1, 1],
             paste0(urbanoi[1, 2],
                    " - ",
                    urbanoi[1, 3]))

LFM <- cbind("Large Fringe Metro",
             urban[2, 1],
             paste0(urban[2, 2],
                    " - ",
                    urban[2, 3]),
             urbanai[2, 1],
             paste0(urbanai[2, 2],
                    " - ",
                    urbanai[2, 3]),
             urbanoi[2, 1],
             paste0(urbanoi[2, 2],
                    " - ",
                    urbanoi[2, 3]))
Medium <- cbind("Medium Metro",
                urban[3, 1],
                paste0(urban[3, 2],
                       " - ",
                       urban[3, 3]),
                urbanai[3, 1],
                paste0(urbanai[3, 2],
                       " - ",
                       urbanai[3, 3]),
                urbanoi[3, 1],
                paste0(urbanoi[3, 2],
                       " - ",
                       urbanoi[3, 3]))
Small <- cbind("Small Metro",
               urban[6, 1],
               paste0(urban[6, 2],
                      " - ",
                      urban[6, 3]),
               urbanai[6, 1],
               paste0(urbanai[6, 2],
                      " - ",
                      urbanai[6, 3]),
               urbanoi[6, 1],
               paste0(urbanoi[6, 2],
                      " - ",
                      urbanoi[6, 3]))
Micro <- cbind("Micropolitan",
               urban[4, 1],
               paste0(urban[4, 2],
                      " - ",
                      urban[4, 3]),
               urbanai[4, 1],
               paste0(urbanai[4, 2],
                      " - ",
                      urbanai[4, 3]),
               urbanoi[4, 1],
               paste0(urbanoi[4, 2],
                      " - ",
                      urbanoi[4, 3]))
Noncore <- cbind("Noncore",
                 urban[5, 1],
                 paste0(urban[5, 2],
                        " - ",
                        urban[5, 3]),
                 urbanai[5, 1],
                 paste0(urbanai[5, 2],
                        " - ",
                        urbanai[5, 3]),
                 urbanoi[5, 1],
                 paste0(urbanoi[5, 2],
                        " - ",
                        urbanoi[5, 3]))


# HIV Status
hivstat <- glm(castotdegree ~ hiv, family = "poisson", data = artnet4)
hivstat <- round(cbind(exp(coef(hivstat)), rbind(exp(confint(hivstat)))), 2)
hivstatv2 <- glm(castotdegree ~ hiv - 1, family = "poisson", data = artnet4)
hivstatv2 <- round(cbind(exp(coef(hivstatv2)), rbind(exp(confint(hivstatv2)))), 2)

hivstatai <- glm(casaionlydegree ~ hiv, family = "poisson", data = artnet4)
hivstatai <- round(cbind(exp(coef(hivstatai)), rbind(exp(confint(hivstatai)))), 2)
hivstataiv2 <- glm(casaionlydegree ~ hiv - 1, family = "poisson", data = artnet4)
hivstataiv2 <- round(cbind(exp(coef(hivstataiv2)), rbind(exp(confint(hivstataiv2)))), 2)

hivstatoi <- glm(casoionlydegree ~ hiv, family = "poisson", data = artnet4)
hivstatoi <- round(cbind(exp(coef(hivstatoi)), rbind(exp(confint(hivstatoi)))), 2)
hivstatoiv2 <- glm(casoionlydegree ~ hiv - 1, family = "poisson", data = artnet4)
hivstatoiv2 <- round(cbind(exp(coef(hivstatoiv2)), rbind(exp(confint(hivstatoiv2)))), 2)

# double check predictions against empirical means
group_by(artnet4, hiv) %>% summarize(mean(castotdegree))
group_by(artnet4, hiv) %>% summarize(mean(casaionlydegree))
group_by(artnet4, hiv) %>% summarize(mean(casoionlydegree))

HIVPos <- cbind("HIV Pos",
                hivstatv2[1, 1],
                paste0(hivstatv2[1, 2],
                       " - ",
                       hivstatv2[1, 3]),
                hivstataiv2[1, 1],
                paste0(hivstataiv2[1, 2],
                       " - ",
                       hivstataiv2[1, 3]),
                hivstatoiv2[1, 1],
                paste0(hivstatoiv2[1, 2],
                       " - ",
                       hivstatoiv2[1, 3]))
HIVNeg <- cbind("HIV Neg",
                hivstat[1, 1],
                paste0(hivstat[1, 2],
                       " - ",
                       hivstat[1, 3]),
                hivstatai[1, 1],
                paste0(hivstatai[1, 2],
                       " - ",
                       hivstatai[1, 3]),
                hivstatoi[1, 1],
                paste0(hivstatoi[1, 2],
                       " - ",
                       hivstatoi[1, 3]))

# Other types of statistics
# % of egos Concurrent
concurr <- cbind("Concurr",
                 paste0(length(which(artnet4$castotdegree > 1)),
                        " (",
                        round(100 * length(which(artnet4$castotdegree > 1)) / nrow(artnet4), 2), "%)"),
                 paste0(length(which(artnet4$casaionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet4$casaionlydegree > 1)) / nrow(artnet4), 2), "%)"),
                 paste0(length(which(artnet4$casoionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet4$casoionlydegree > 1)) / nrow(artnet4), 2), "%)"))

# # Rate of one-offs
# # Create OI partners variable
# artnet4$oi.part <- rep(NA, nrow(artnet4))
# artnet4$oi.part <- artnet4$cuml.pnum - artnet4$ai.part
# artnet4$oi.part[artnet4$oi.part < 0] <- 0 # 1 person with -87
#
# # Create count variables for AI or OI
# d <- artnet4
# l <- artnetLong
# d <- l %>%
#   filter(ROI == 1 | IOI == 1 | RAI == 1 | IAI == 1) %>%
#   filter(ptype == 2) %>%
#   group_by(AMIS_ID) %>%
#   count() %>%
#   rename(count.mc.aioi.part = n) %>%
#   right_join(d, by = "AMIS_ID")
# d$count.mc.aioi.part <- ifelse(is.na(d$count.mc.aioi.part), 0, d$count.mc.aioi.part)
# d$count.mc.aioi.part
# d$count.oo.aioi.part <- d$cuml.pnum - d$count.mc.aioi.part
# d$count.oo.aioi.part <- pmax(0, d$count.oo.aioi.part)
# data.frame(d$cuml.pnum, d$count.mc.aioi.part, d$count.oo.aioi.part)
# summary(d$count.oo.aioi.part)
#
# plot(density(d$count.oo.aioi.part, na.rm = TRUE, from = 0))
# plot(density(d$count.oo.aioi.part, na.rm = TRUE, from = 0), xlim = c(0, 100))
#
# # daily rate
# d$rate.oo.aioi.part <- d$count.oo.aioi.part/365
# d$rate.oo.aioi.part
#
# # Create count variables for AI
# d2 <- l %>%
#   filter(RAI == 1 | IAI == 1) %>%
#   filter(ptype == 2) %>%
#   group_by(AMIS_ID) %>%
#   count() %>%
#   rename(count.mc.ai.part = n) %>%
#   right_join(d, by = "AMIS_ID")
# d2$count.mc.ai.part <- ifelse(is.na(d2$count.mc.ai.part), 0, d2$count.mc.ai.part)
# d2$count.mc.ai.part
#
# d2$count.oo.ai.part <- d2$ai.part - d2$count.mc.ai.part
# d2$count.oo.ai.part <- pmax(0, d2$count.oo.ai.part)
# data.frame(d2$ai.part, d2$count.mc.ai.part, d2$count.oo.ai.part)
# summary(d2$count.oo.ai.part)
#
# plot(density(d2$count.oo.ai.part, na.rm = TRUE, from = 0))
# plot(density(d2$count.oo.ai.part, na.rm = TRUE, from = 0), xlim = c(0, 100))
#
# # daily rate
# d2$rate.oo.ai.part <- d2$count.oo.ai.part/365
# d2$rate.oo.ai.part
#
# # Create count variables for OI
# d3 <- l %>%
#   filter(ROI == 1 | IOI == 1) %>%
#   filter(ptype  == 2) %>%
#   group_by(AMIS_ID) %>%
#   count() %>%
#   rename(count.mc.oi.part = n) %>%
#   right_join(d, by = "AMIS_ID")
# d3$count.mc.oi.part <- ifelse(is.na(d3$count.mc.oi.part), 0, d3$count.mc.oi.part)
# d3$count.mc.oi.part
#
# d3$count.oo.oi.part <- d3$oi.part - d3$count.mc.oi.part
# d3$count.oo.oi.part <- pmax(0, d3$count.oo.oi.part)
# data.frame(d3$oi.part, d3$count.mc.oi.part, d3$count.oo.oi.part)
# summary(d3$count.oo.oi.part)
#
# plot(density(d3$count.oo.oi.part, na.rm = TRUE, from = 0))
# plot(density(d3$count.oo.oi.part, na.rm = TRUE, from = 0), xlim = c(0, 100))
#
# # doily rate
# d3$rate.oo.oi.part <- d3$count.oo.oi.part/365
# d3$rate.oo.oi.part
#
# oneoff <- cbind("One-off", round(mean(d$rate.oo.aioi.part, na.rm = TRUE), 4),
#                 round(mean(d2$rate.oo.ai.part, na.rm = TRUE), 4),
#                 round(mean(d3$rate.oo.oi.part, na.rm = TRUE), 4))


# Output table
stable2a <- rbind(total, black, white, hispanic, other,
                 fifteen24, twentyfive29, thirty39, forty49, fifty59, sixty65,
                 West, Pacific, Mountain, Midwest, WNC, ENC,
                 South, WSC, ESC, SA, Northeast, MA, NE, LCM, LFM, Medium,
                 Small, Micro, Noncore,
                 HIVPos, HIVNeg)
colnames(stable2a) <- c("Category", "Cas Ong Either Mean", " Cas Ong Either CI",
                       "Cas Ong AI Mean", "Cas Ong AI CI",
                       "Cas Ong OI Mean", "Cas Ong OI CI")
write.csv(stable2a, file = "stable2a.csv")

stable2b <- rbind(concurr)
# stable2b <- rbind(concurr, oneoff)
colnames(stable2b) <- c("Category", "AI/OI Mean", "AI Mean", "OI Mean")
write.csv(stable2b, file = "stable2b.csv")


# Create Shiny Individ-Level Datasets --------------
artnet4shiny <- artnet2
artnet4shiny <- left_join(artnet4shiny, df4, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df5, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df6, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df7, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df8, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df9, by = "AMIS_ID")
saveRDS(artnet4shiny, file = "artnet4shiny.rda", compress = "xz")

artnetlong4shiny <- artnetLong
saveRDS(artnetlong4shiny, file = "artnetlong4shiny.rda", compress = "xz")



