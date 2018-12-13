## ART-Net Study 2018   ##
## Data Analysis Script ##
## 2018-08-03           ##

# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)

# Read in cleaning and datasets ---------

## Data Cleaning/Management Script
source("Analyses/Data_Cleaning.R", echo = FALSE)

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
write.csv(table1a, file = "Output/table1a.csv")

table1b <- cbind(numtestsart, numtestsint, numtestsamis)
colnames(table1b) <- c("Category",
                       "ART-Net Mean", "ART-Net SD", "ART-Net Median",
                       "Intermed Mean", "Intermed SD", "Intermed Median",
                       "AMIS Mean", "AMIS SD", "AMIS Median")
write.csv(table1b, file = "Output/table1b.csv")



# Table 2 - Mean degree --------------

## General mean degree calculation
# Issue: people with no reported AI or OI activity? Look at wide to long
nrow(artnetLong[which(artnetLong$RAI == 0 & artnetLong$IAI == 0 &
                        artnetLong$IOI == 0 & artnetLong$ROI == 0), ])
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
table(artnet2$totdegree, useNA = "always")
table(artnet2$maintotdegree, useNA = "always")
table(artnet2$Castotdegree, useNA = "always")

# If missing degree values, then set to 0
artnet2$totdegree <- ifelse(is.na(artnet2$totdegree), 0, artnet2$totdegree)
artnet2$maintotdegree <- ifelse(is.na(artnet2$maintotdegree), 0, artnet2$maintotdegree)
artnet2$castotdegree <- ifelse(is.na(artnet2$castotdegree), 0, artnet2$castotdegree)

# City
city <- glm(totdegree ~ city - 1, family = "poisson", data = artnet2)
city <- round(cbind(exp(coef(city)), exp(confint(city))), 2)
citymain <- glm(maintotdegree ~ city - 1, family = "poisson", data = artnet2)
citymain <- round(cbind(exp(coef(citymain)), exp(confint(citymain))), 2)
citycas <- glm(castotdegree ~ city - 1, family = "poisson", data = artnet2)
citycas <- round(cbind(exp(coef(citycas)), exp(confint(citycas))), 2)

# Overall Poisson:
total <- glm(totdegree ~ 1, family = "poisson", data = artnet2)
total <- round(cbind(exp(coef(total)), rbind(exp(confint(total)))), 2)
totalmain <- glm(maintotdegree ~ 1, family = "poisson", data = artnet2)
totalmain <- round(cbind(exp(coef(totalmain)), rbind(exp(confint(totalmain)))), 2)
totalcas <- glm(castotdegree ~ 1, family = "poisson", data = artnet2)
totalcas <- round(cbind(exp(coef(totalcas)), rbind(exp(confint(totalcas)))), 2)

# double check predictions against empirical means
group_by(artnet2) %>% summarize(mean(totdegree))
group_by(artnet2) %>% summarize(mean(maintotdegree))
group_by(artnet2) %>% summarize(mean(castotdegree))

total <- cbind("Total",
               total[1, 1],
               paste0(total[1, 2],
                      " - ",
                      total[1, 3]),
               totalmain[1, 1],
               paste0(totalmain[1, 2],
                      " - ",
                      totalmain[1, 3]),
               totalcas[1, 1],
               paste0(totalcas[1, 2],
                      " - ",
                      totalcas[1, 3]))

# Race/ethnicity
race <- glm(totdegree ~ race.cat - 1, family = "poisson", data = artnet2)
race <- round(cbind(exp(coef(race)), exp(confint(race))), 2)
racemain <- glm(maintotdegree ~ race.cat - 1, family = "poisson", data = artnet2)
racemain <- round(cbind(exp(coef(racemain)), exp(confint(racemain))), 2)
racecas <- glm(castotdegree ~ race.cat - 1, family = "poisson", data = artnet2)
racecas <- round(cbind(exp(coef(racecas)), exp(confint(racecas))), 2)

# double check predictions against empirical means
group_by(artnet2, race.cat) %>% summarize(mean(totdegree))
group_by(artnet2, race.cat) %>% summarize(mean(maintotdegree))
group_by(artnet2, race.cat) %>% summarize(mean(castotdegree))

black <- cbind("black",
               race[1, 1],
               paste0(race[1, 2],
                      " - ",
                      race[1, 3]),
               racemain[1, 1],
               paste0(racemain[1, 2],
                      " - ",
                      racemain[1, 3]),
               racecas[1, 1],
               paste0(racecas[1, 2],
                      " - ",
                      racecas[1, 3]))

white <- cbind("white",
               race[4, 1],
               paste0(race[4, 2],
                      " - ",
                      race[4, 3]),
               racemain[4, 1],
               paste0(racemain[4, 2],
                      " - ",
                      racemain[4, 3]),
               racecas[4, 1],
               paste0(racecas[4, 2],
                      " - ",
                      racecas[4, 3]))

hispanic <- cbind("hispanic",
                  race[2, 1],
                  paste0(race[2, 2],
                         " - ",
                         race[2, 3]),
                  racemain[2, 1],
                  paste0(racemain[2, 2],
                         " - ",
                         racemain[2, 3]),
                  racemain[2, 1],
                  paste0(racecas[2, 2],
                         " - ",
                         racecas[2, 3]))

other <- cbind("other",
               race[3, 1],
               paste0(race[3, 2],
                      " - ",
                      race[3, 3]),
               racemain[3, 1],
               paste0(racemain[3, 2],
                      " - ",
                      racemain[3, 3]),
               racecas[3, 1],
               paste0(racecas[3, 2],
                      " - ",
                      racecas[3, 3]))

# Age
age <- glm(totdegree ~ age.cat - 1, family = "poisson", data = artnet2)
age <- round(cbind(exp(coef(age)), exp(confint(age))), 2)
agemain <- glm(maintotdegree ~ age.cat - 1, family = "poisson", data = artnet2)
agemain <- round(cbind(exp(coef(agemain)), exp(confint(agemain))), 2)
agecas <- glm(castotdegree ~ age.cat - 1, family = "poisson", data = artnet2)
agecas <- round(cbind(exp(coef(agecas)), exp(confint(agecas))), 2)

# double check predictions against empirical means
group_by(artnet2, age.cat) %>% summarize(mean(totdegree))
group_by(artnet2, age.cat) %>% summarize(mean(maintotdegree))
group_by(artnet2, age.cat) %>% summarize(mean(castotdegree))

fifteen24 <- cbind("15-24",
                   age[1, 1],
                   paste0(age[1, 2],
                          " - ",
                          age[1, 3]),
                   agemain[1, 1],
                   paste0(agemain[1, 2],
                          " - ",
                          agemain[1, 3]),
                   agecas[1, 1],
                   paste0(agecas[1, 2],
                          " - ",
                          agecas[1, 3]))


twentyfive34 <- cbind("25-34",
                      age[2, 1],
                      paste0(age[2, 2],
                             " - ",
                             age[2, 3]),
                      agemain[2, 1],
                      paste0(agemain[2, 2],
                             " - ",
                             agemain[2, 3]),
                      agecas[2, 1],
                      paste0(agecas[2, 2],
                             " - ",
                             agecas[2, 3]))

thirtyfive44 <- cbind("35-44",
                  age[3, 1],
                  paste0(age[3, 2],
                         " - ",
                         age[3, 3]),
                  agemain[3, 1],
                  paste0(agemain[3, 2],
                         " - ",
                         agemain[3, 3]),
                  agecas[3, 1],
                  paste0(agecas[3, 2],
                         " - ",
                         agecas[3, 3]))

fortyfive54 <- cbind("45-54",
                 age[4, 1],
                 paste0(age[4, 2],
                        " - ",
                        age[4, 3]),
                 agemain[4, 1],
                 paste0(agemain[4, 2],
                        " - ",
                        agemain[4, 3]),
                 agecas[4, 1],
                 paste0(agecas[4, 2],
                        " - ",
                        agecas[4, 3]))

fiftyfive65 <- cbind("55-65",
                 age[5, 1],
                 paste0(age[5, 2],
                        " - ",
                        age[5, 3]),
                 agemain[5, 1],
                 paste0(agemain[5, 2],
                        " - ",
                        agemain[5, 3]),
                 agecas[5, 1],
                 paste0(agecas[5, 2],
                        " - ",
                        agecas[5, 3]))

# Region and division
division <- glm(totdegree ~ division - 1, family = "poisson", data = artnet2)
division <- round(cbind(exp(coef(division)), exp(confint(division))), 2)
divisionmain <- glm(maintotdegree ~ division - 1, family = "poisson", data = artnet2)
divisionmain <- round(cbind(exp(coef(divisionmain)), exp(confint(divisionmain))), 2)
divisioncas <- glm(castotdegree ~ division - 1, family = "poisson", data = artnet2)
divisioncas <- round(cbind(exp(coef(divisioncas)), exp(confint(divisioncas))), 2)

region <- glm(totdegree ~ region - 1, family = "poisson", data = artnet2)
region <- round(cbind(exp(coef(region)), exp(confint(region))), 2)
regionmain <- glm(maintotdegree ~ region - 1, family = "poisson", data = artnet2)
regionmain <- round(cbind(exp(coef(regionmain)), exp(confint(regionmain))), 2)
regioncas <- glm(castotdegree ~ region - 1, family = "poisson", data = artnet2)
regioncas <- round(cbind(exp(coef(regioncas)), exp(confint(regioncas))), 2)

# double check predictions against empirical means
group_by(artnet2, region) %>% summarize(mean(totdegree))
group_by(artnet2, region) %>% summarize(mean(maintotdegree))
group_by(artnet2, region) %>% summarize(mean(castotdegree))
group_by(artnet2, division) %>% summarize(mean(totdegree))
group_by(artnet2, division) %>% summarize(mean(maintotdegree))
group_by(artnet2, division) %>% summarize(mean(castotdegree))

# Region and Division
West <- cbind("West",
              region[4, 1],
              paste0(region[4, 2],
                     " - ",
                     region[4, 3]),
              regionmain[4, 1],
              paste0(regionmain[4, 2],
                     " - ",
                     regionmain[4, 3]),
              regioncas[4, 1],
              paste0(regioncas[4, 2],
                     " - ",
                     regioncas[4, 3]))

Pacific <- cbind("Pacific",
                 division[6, 1],
                 paste0(division[6, 2],
                        " - ",
                        division[6, 3]),
                 divisionmain[6, 1],
                 paste0(divisionmain[6, 2],
                        " - ",
                        divisionmain[6, 3]),
                 divisioncas[6, 1],
                 paste0(divisioncas[6, 2],
                        " - ",
                        divisioncas[6, 3]))

Mountain <- cbind("Mountain",
                  division[4, 1],
                  paste0(division[4, 2],
                         " - ",
                         division[4, 3]),
                  divisionmain[4, 1],
                  paste0(divisionmain[4, 2],
                         " - ",
                         divisionmain[4, 3]),
                  divisioncas[4, 1],
                  paste0(divisioncas[4, 2],
                         " - ",
                         divisioncas[4, 3]))

Midwest <- cbind("Midwest",
                 region[1, 1],
                 paste0(region[1, 2],
                        " - ",
                        region[1, 3]),
                 regionmain[1, 1],
                 paste0(regionmain[1, 2],
                        " - ",
                        regionmain[1, 3]),
                 regioncas[1, 1],
                 paste0(regioncas[1, 2],
                        " - ",
                        regioncas[1, 3]))

WNC <- cbind("West North Central",
             division[8, 1],
             paste0(division[8, 2],
                    " - ",
                    division[8, 3]),
             divisionmain[8, 1],
             paste0(divisionmain[8, 2],
                    " - ",
                    divisionmain[8, 3]),
             divisioncas[8, 1],
             paste0(divisioncas[8, 2],
                    " - ",
                    divisioncas[8, 3]))

ENC <- cbind("East North Central",
             division[1, 1],
             paste0(division[1, 2],
                    " - ",
                    division[1, 3]),
             divisionmain[1, 1],
             paste0(divisionmain[1, 2],
                    " - ",
                    divisionmain[1, 3]),
             divisioncas[1, 1],
             paste0(divisioncas[1, 2],
                    " - ",
                    divisioncas[1, 3]))

South <- cbind("South",
               region[3, 1],
               paste0(region[3, 2],
                      " - ",
                      region[3, 3]),
               regionmain[3, 1],
               paste0(regionmain[3, 2],
                      " - ",
                      regionmain[3, 3]),
               regioncas[3, 1],
               paste0(regioncas[3, 2],
                      " - ",
                      regioncas[3, 3]))

WSC <- cbind("West South Central",
             division[9, 1],
             paste0(division[9, 2],
                    " - ",
                    division[9, 3]),
             divisionmain[9, 1],
             paste0(divisionmain[9, 2],
                    " - ",
                    divisionmain[9, 3]),
             divisioncas[9, 1],
             paste0(divisioncas[9, 2],
                    " - ",
                    divisioncas[9, 3]))

ESC <- cbind("East South Central",
             division[2, 1],
             paste0(division[2, 2],
                    " - ",
                    division[2, 3]),
             divisionmain[2, 1],
             paste0(divisionmain[2, 2],
                    " - ",
                    divisionmain[2, 3]),
             divisioncas[2, 1],
             paste0(divisioncas[2, 2],
                    " - ",
                    divisioncas[2, 3]))
SA <- cbind("South Atlantic",
            division[7, 1],
            paste0(division[7, 2],
                   " - ",
                   division[7, 3]),
            divisionmain[7, 1],
            paste0(divisionmain[7, 2],
                   " - ",
                   divisionmain[7, 3]),
            divisioncas[7, 1],
            paste0(divisioncas[7, 2],
                   " - ",
                   divisioncas[7, 3]))

Northeast <- cbind("Northeast",
                   region[2, 1],
                   paste0(region[2, 2],
                          " - ",
                          region[2, 3]),
                   regionmain[2, 1],
                   paste0(regionmain[2, 2],
                          " - ",
                          regionmain[2, 3]),
                   regioncas[2, 1],
                   paste0(regioncas[2, 2],
                          " - ",
                          regioncas[2, 3]))

MA <- cbind("Middle Atlantic",
            division[3, 1],
            paste0(division[3, 2],
                   " - ",
                   division[3, 3]),
            divisionmain[3, 1],
            paste0(divisionmain[3, 2],
                   " - ",
                   divisionmain[3, 3]),
            divisioncas[3, 1],
            paste0(divisioncas[3, 2],
                   " - ",
                   divisioncas[3, 3]))

NE <- cbind("New England",
            division[5, 1],
            paste0(division[5, 2],
                   " - ",
                   division[5, 3]),
            divisionmain[5, 1],
            paste0(divisionmain[5, 2],
                   " - ",
                   divisionmain[5, 3]),
            divisioncas[5, 1],
            paste0(divisioncas[5, 2],
                   " - ",
                   divisioncas[5, 3]))

# Urbanicity
urban <- glm(totdegree ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urban <- round(cbind(exp(coef(urban)), exp(confint(urban))), 2)
urbanmain <- glm(maintotdegree ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urbanmain <- round(cbind(exp(coef(urbanmain)), exp(confint(urbanmain))), 2)
urbancas <- glm(castotdegree ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urbancas <- round(cbind(exp(coef(urbancas)), exp(confint(urbancas))), 2)

# double check predictions against empirical means
group_by(artnet2, NCHSCHAR) %>% summarize(mean(totdegree))
group_by(artnet2, NCHSCHAR) %>% summarize(mean(maintotdegree))
group_by(artnet2, NCHSCHAR) %>% summarize(mean(castotdegree))

LCM <- cbind("Large Central Metro",
             urban[1, 1],
             paste0(urban[1, 2],
                    " - ",
                    urban[1, 3]),
             urbanmain[1, 1],
             paste0(urbanmain[1, 2],
                    " - ",
                    urbanmain[1, 3]),
             urbancas[1, 1],
             paste0(urbancas[1, 2],
                    " - ",
                    urbancas[1, 3]))

LFM <- cbind("Large Fringe Metro",
             urban[2, 1],
             paste0(urban[2, 2],
                    " - ",
                    urban[2, 3]),
             urbanmain[2, 1],
             paste0(urbanmain[2, 2],
                    " - ",
                    urbanmain[2, 3]),
             urbancas[2, 1],
             paste0(urbancas[2, 2],
                    " - ",
                    urbancas[2, 3]))
Medium <- cbind("Medium Metro",
                urban[3, 1],
                paste0(urban[3, 2],
                       " - ",
                       urban[3, 3]),
                urbanmain[3, 1],
                paste0(urbanmain[3, 2],
                       " - ",
                       urbanmain[3, 3]),
                urbancas[3, 1],
                paste0(urbancas[3, 2],
                       " - ",
                       urbancas[3, 3]))
Small <- cbind("Small Metro",
               urban[6, 1],
               paste0(urban[6, 2],
                      " - ",
                      urban[6, 3]),
               urbanmain[6, 1],
               paste0(urbanmain[6, 2],
                      " - ",
                      urbanmain[6, 3]),
               urbancas[6, 1],
               paste0(urbancas[6, 2],
                      " - ",
                      urbancas[6, 3]))
Micro <- cbind("Micropolitan",
               urban[4, 1],
               paste0(urban[4, 2],
                      " - ",
                      urban[4, 3]),
               urbanmain[4, 1],
               paste0(urbanmain[4, 2],
                      " - ",
                      urbanmain[4, 3]),
               urbancas[4, 1],
               paste0(urbancas[4, 2],
                      " - ",
                      urbancas[4, 3]))
Noncore <- cbind("Noncore",
                 urban[5, 1],
                 paste0(urban[5, 2],
                        " - ",
                        urban[5, 3]),
                 urbanmain[5, 1],
                 paste0(urbanmain[5, 2],
                        " - ",
                        urbanmain[5, 3]),
                 urbancas[5, 1],
                 paste0(urbancas[5, 2],
                        " - ",
                        urbancas[5, 3]))

# HIV Status
hivstat <- glm(totdegree ~ hiv, family = "poisson", data = artnet2)
hivstat <- round(cbind(exp(coef(hivstat)), rbind(exp(confint(hivstat)))), 2)
hivstatv2 <- glm(totdegree ~ hiv - 1, family = "poisson", data = artnet2)
hivstatv2 <- round(cbind(exp(coef(hivstatv2)), rbind(exp(confint(hivstatv2)))), 2)

hivstatmain <- glm(maintotdegree ~ hiv, family = "poisson", data = artnet2)
hivstatmain <- round(cbind(exp(coef(hivstatmain)), rbind(exp(confint(hivstatmain)))), 2)
hivstatmainv2 <- glm(maintotdegree ~ hiv - 1, family = "poisson", data = artnet2)
hivstatmainv2 <- round(cbind(exp(coef(hivstatmainv2)), rbind(exp(confint(hivstatmainv2)))), 2)

hivstatcas <- glm(castotdegree ~ hiv, family = "poisson", data = artnet2)
hivstatcas <- round(cbind(exp(coef(hivstatcas)), rbind(exp(confint(hivstatcas)))), 2)
hivstatcasv2 <- glm(castotdegree ~ hiv - 1, family = "poisson", data = artnet2)
hivstatcasv2 <- round(cbind(exp(coef(hivstatcasv2)), rbind(exp(confint(hivstatcasv2)))), 2)

# double check predictions against empirical means
group_by(artnet2, hiv) %>% summarize(mean(totdegree))
group_by(artnet2, hiv) %>% summarize(mean(maintotdegree))
group_by(artnet2, hiv) %>% summarize(mean(castotdegree))

HIVPos <- cbind("HIV Pos",
                hivstatv2[1, 1],
                paste0(hivstatv2[1, 2],
                       " - ",
                       hivstatv2[1, 3]),
                hivstatmainv2[1, 1],
                paste0(hivstatmainv2[1, 2],
                       " - ",
                       hivstatmainv2[1, 3]),
                hivstatcasv2[1, 1],
                paste0(hivstatcasv2[1, 2],
                       " - ",
                       hivstatcasv2[1, 3]))
HIVNeg <- cbind("HIV Neg",
                hivstat[1, 1],
                paste0(hivstat[1, 2],
                       " - ",
                       hivstat[1, 3]),
                hivstatmain[1, 1],
                paste0(hivstatmain[1, 2],
                       " - ",
                       hivstatmain[1, 3]),
                hivstatcas[1, 1],
                paste0(hivstatcas[1, 2],
                       " - ",
                       hivstatcas[1, 3]))

# Other types of statistics
# % of egos Concurrent
concurr <- cbind("Concurr",
                 paste0(length(which(artnet2$totdegree > 1)),
                        " (",
                        round(100 * length(which(artnet2$totdegree > 1)) / nrow(artnet2), 2), "%)"),
                 paste0(length(which(artnet2$maintotdegree > 1)),
                        " (",
                        round(100 * length(which(artnet2$maintotdegree > 1)) / nrow(artnet2), 2), "%)"),
                 paste0(length(which(artnet2$castotdegree > 1)),
                        " (",
                        round(100 * length(which(artnet2$castotdegree > 1)) / nrow(artnet2), 2), "%)"))

# Rate of one-offs -----------
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

# weekly rate
d$rate.oo.aioi.part <- d$count.oo.aioi.part/52
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

# weekly rate
d2$rate.oo.ai.part <- d2$count.oo.ai.part/52
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

# weekly rate
d3$rate.oo.oi.part <- d3$count.oo.oi.part/52
d3$rate.oo.oi.part

# Reduce new datasets to relevant data
da <- d[, c("AMIS_ID", "count.oo.aioi.part", "rate.oo.aioi.part")]
db <- d2[, c("AMIS_ID", "count.oo.ai.part", "rate.oo.ai.part")]
dc <- d3[, c("AMIS_ID", "count.oo.oi.part", "rate.oo.oi.part")]


# Create merged dataframes
artnet2 <- left_join(artnet2, da, by = "AMIS_ID")
artnet2 <- left_join(artnet2, db, by = "AMIS_ID")
artnet2 <- left_join(artnet2, dc, by = "AMIS_ID")

# Overall Poisson:
totaloo <- glm(rate.oo.aioi.part ~ 1, family = "poisson", data = d)
totaloo <- round(cbind(exp(coef(totaloo)), rbind(exp(confint(totaloo)))), 3)

# double check predictions against empirical means
group_by(d) %>% summarize(mean(rate.oo.aioi.part))

totaloo <- cbind("Total",
                 totaloo[1, 1],
               paste0(totaloo[1, 2],
                      " - ",
                      totaloo[1, 3]))

# Race/ethnicity
raceoo <- glm(rate.oo.aioi.part ~ race.cat - 1, family = "poisson", data = d)
raceoo <- round(cbind(exp(coef(raceoo)), exp(confint(raceoo))), 3)

# double check predictions against empirical means
group_by(d, race.cat) %>% summarize(mean(rate.oo.aioi.part))

blackoo <- cbind("black",
               raceoo[1, 1],
               paste0(raceoo[1, 2],
                      " - ",
                      raceoo[1, 3]))

whiteoo <- cbind("white",
               raceoo[4, 1],
               paste0(raceoo[4, 2],
                      " - ",
                      raceoo[4, 3]))

hispanicoo <- cbind("hispanic",
                  raceoo[2, 1],
                  paste0(raceoo[2, 2],
                         " - ",
                         raceoo[2, 3]))

otheroo <- cbind("other",
               raceoo[3, 1],
               paste0(raceoo[3, 2],
                      " - ",
                      raceoo[3, 3]))

# Age
ageoo <- glm(rate.oo.aioi.part ~ age.cat - 1, family = "poisson", data = d)
ageoo <- round(cbind(exp(coef(ageoo)), exp(confint(ageoo))), 3)


# double check predictions against empirical means
group_by(d, age.cat) %>% summarize(mean(totdegree))

fifteen24oo <- cbind("15-24",
                   ageoo[1, 1],
                   paste0(ageoo[1, 2],
                          " - ",
                          ageoo[1, 3]))


twentyfive34oo <- cbind("25-34",
                      ageoo[2, 1],
                      paste0(ageoo[2, 2],
                             " - ",
                             ageoo[2, 3]))

thirtyfive44oo <- cbind("35-44",
                      ageoo[3, 1],
                      paste0(ageoo[3, 2],
                             " - ",
                             ageoo[3, 3]))

fortyfive54oo <- cbind("45-54",
                     ageoo[4, 1],
                     paste0(ageoo[4, 2],
                            " - ",
                            ageoo[4, 3]))

fiftyfive65oo <- cbind("55-65",
                     ageoo[5, 1],
                     paste0(ageoo[5, 2],
                            " - ",
                            ageoo[5, 3]))

# Region and division
divisionoo <- glm(rate.oo.aioi.part ~ division - 1, family = "poisson", data = d)
divisionoo <- round(cbind(exp(coef(divisionoo)), exp(confint(divisionoo))), 3)
regionoo <- glm(rate.oo.aioi.part ~ region - 1, family = "poisson", data = d)
regionoo <- round(cbind(exp(coef(regionoo)), exp(confint(regionoo))), 3)


# double check predictions against empirical means
group_by(d, region) %>% summarize(mean(rate.oo.aioi.part))
group_by(d, division) %>% summarize(mean(rate.oo.aioi.part))

# Region and Division
Westoo <- cbind("West",
              regionoo[4, 1],
              paste0(regionoo[4, 2],
                     " - ",
                     regionoo[4, 3]))

Pacificoo <- cbind("Pacific",
                 divisionoo[6, 1],
                 paste0(divisionoo[6, 2],
                        " - ",
                        divisionoo[6, 3]))

Mountainoo <- cbind("Mountain",
                  divisionoo[4, 1],
                  paste0(divisionoo[4, 2],
                         " - ",
                         divisionoo[4, 3]))

Midwestoo <- cbind("Midwest",
                 regionoo[1, 1],
                 paste0(regionoo[1, 2],
                        " - ",
                        regionoo[1, 3]))

WNCoo <- cbind("West North Central",
             divisionoo[8, 1],
             paste0(divisionoo[8, 2],
                    " - ",
                    divisionoo[8, 3]))

ENCoo <- cbind("East North Central",
             divisionoo[1, 1],
             paste0(divisionoo[1, 2],
                    " - ",
                    divisionoo[1, 3]))

Southoo <- cbind("South",
               regionoo[3, 1],
               paste0(regionoo[3, 2],
                      " - ",
                      regionoo[3, 3]))

WSCoo <- cbind("West South Central",
             divisionoo[9, 1],
             paste0(divisionoo[9, 2],
                    " - ",
                    divisionoo[9, 3]))

ESCoo <- cbind("East South Central",
             divisionoo[2, 1],
             paste0(divisionoo[2, 2],
                    " - ",
                    divisionoo[2, 3]))
SAoo <- cbind("South Atlantic",
            divisionoo[7, 1],
            paste0(divisionoo[7, 2],
                   " - ",
                   divisionoo[7, 3]))

Northeastoo <- cbind("Northeast",
                   regionoo[2, 1],
                   paste0(regionoo[2, 2],
                          " - ",
                          regionoo[2, 3]))

MAoo <- cbind("Middle Atlantic",
            divisionoo[3, 1],
            paste0(divisionoo[3, 2],
                   " - ",
                   divisionoo[3, 3]))

NEoo <- cbind("New England",
            divisionoo[5, 1],
            paste0(divisionoo[5, 2],
                   " - ",
                   divisionoo[5, 3]))

# Urbanicity
urbanoo <- glm(rate.oo.aioi.part ~ NCHSCHAR - 1, family = "poisson", data = d)
urbanoo <- round(cbind(exp(coef(urbanoo)), exp(confint(urbanoo))), 3)


# double check predictions against empirical means
group_by(d, NCHSCHAR) %>% summarize(mean(rate.oo.aioi.part))

LCMoo <- cbind("Large Central Metro",
             urbanoo[1, 1],
             paste0(urbanoo[1, 2],
                    " - ",
                    urbanoo[1, 3]))

LFMoo <- cbind("Large Fringe Metro",
             urbanoo[2, 1],
             paste0(urbanoo[2, 2],
                    " - ",
                    urbanoo[2, 3]))
Mediumoo <- cbind("Medium Metro",
                urbanoo[3, 1],
                paste0(urbanoo[3, 2],
                       " - ",
                       urbanoo[3, 3]))
Smalloo <- cbind("Small Metro",
               urbanoo[6, 1],
               paste0(urbanoo[6, 2],
                      " - ",
                      urbanoo[6, 3]))
Microoo <- cbind("Micropolitan",
               urbanoo[4, 1],
               paste0(urbanoo[4, 2],
                      " - ",
                      urbanoo[4, 3]))
Noncoreoo <- cbind("Noncore",
                 urbanoo[5, 1],
                 paste0(urbanoo[5, 2],
                        " - ",
                        urbanoo[5, 3]))

# HIV Status
hivstatoo <- glm(rate.oo.aioi.part ~ hiv, family = "poisson", data = d)
hivstatoo <- round(cbind(exp(coef(hivstatoo)), rbind(exp(confint(hivstatoo)))), 3)
hivstatv2oo <- glm(rate.oo.aioi.part ~ hiv - 1, family = "poisson", data = d)
hivstatv2oo <- round(cbind(exp(coef(hivstatv2oo)), rbind(exp(confint(hivstatv2oo)))), 3)

# double check predictions against empirical means
group_by(d, hiv) %>% summarize(mean(rate.oo.aioi.part))

HIVPosoo <- cbind("HIV Pos",
                hivstatv2oo[1, 1],
                paste0(hivstatv2oo[1, 2],
                       " - ",
                       hivstatv2oo[1, 3]))
HIVNegoo <- cbind("HIV Neg",
                hivstatoo[1, 1],
                paste0(hivstatoo[1, 2],
                       " - ",
                       hivstatoo[1, 3]))

# Output table
table2a <- cbind(rbind(total, black, white, hispanic, other,
                 fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                 West, Pacific, Mountain, Midwest, WNC, ENC,
                 South, WSC, ESC, SA, Northeast, MA, NE, LCM, LFM, Medium,
                 Small, Micro, Noncore,
                 HIVPos, HIVNeg),
                rbind(totaloo, blackoo, whiteoo, hispanicoo, otheroo,
                      fifteen24oo, twentyfive34oo, thirtyfive44oo, fortyfive54oo, fiftyfive65oo,
                      Westoo, Pacificoo, Mountainoo, Midwestoo, WNCoo, ENCoo,
                      Southoo, WSCoo, ESCoo, SAoo, Northeastoo, MAoo, NEoo, LCMoo, LFMoo, Mediumoo,
                      Smalloo, Microoo, Noncoreoo,
                      HIVPosoo, HIVNegoo))
colnames(table2a) <- c("Category", "Ong Either Mean", "Ong Either CI",
                       "Ong Main Mean", "Ongoing Main CI",
                       "Ong Cas Mean", "Ong Cas CI", "Category", "One-Off", "One-off CI")
write.csv(table2a, file = "Output/table2a.csv")

table2b <- rbind(concurr)
colnames(table2b) <- c("Category", "Total Mean", "Main Mean", "Cas Mean")
write.csv(table2b, file = "Output/table2b.csv")

# Table 3 - Duration of ongoing partnerships --------------

# Set up data frames
extant <- artnetLong[which(artnetLong$p_ONGOING == 1 & artnetLong$p_duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]

# Subset to those active
extant <- extant[which(extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1), ]

# Number of rows with no reported activity
nrow(extant[which(extant$p_RAI == 0 & extant$p_IAI == 0 & extant$p_ROI == 0 & extant$p_IOI == 0), ])

bothmain <- extant[which((extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1) & extant$ptype == 1), ]
bothcas <- extant[which((extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1) & extant$ptype == 2), ]


# Total number of ongoing partnerships
total <- cbind("Total", paste0(nrow(extant), " (", 100 * nrow(extant) / nrow(extant), ")"),

               round(mean(extant$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(extant$p_duration, na.rm = TRUE), 1),
                      ", ",
               round(median(extant$p_duration, na.rm = TRUE), 1)),

               # Calculate mean, 95% CI
               round(mean(bothmain$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bothmain$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$p_duration, na.rm = TRUE), 1)),

               round(mean(bothcas$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bothcas$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$p_duration, na.rm = TRUE), 1)))

# Race/ethnicity
black <- cbind("black",
               paste0(nrow(extant[which(extant$race.cat == "black"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "black"), ]) /
                              nrow(extant), 1), ")"),

               round(mean(extant$p_duration[which(extant$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(extant$p_duration[which(extant$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$p_duration[which(extant$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(bothmain$p_duration[bothmain$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$p_duration[bothmain$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$p_duration[bothmain$race.cat == "black"], na.rm = TRUE), 1)),

               round(mean(bothcas$p_duration[bothcas$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$p_duration[bothcas$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$p_duration[bothcas$race.cat == "black"], na.rm = TRUE), 1)))


white <- cbind("white",
               paste0(nrow(extant[which(extant$race.cat == "white"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "white"), ]) /
                              nrow(extant), 1), ")"),
               round(mean(extant$p_duration[which(extant$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(extant$p_duration[which(extant$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$p_duration[which(extant$race.cat == "white")], na.rm = TRUE), 1)),
               round(mean(bothmain$p_duration[bothmain$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$p_duration[bothmain$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$p_duration[bothmain$race.cat == "white"], na.rm = TRUE), 1)),

               round(mean(bothcas$p_duration[bothcas$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$p_duration[bothcas$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$p_duration[bothcas$race.cat == "white"], na.rm = TRUE), 1)))

hispanic <- cbind("hispanic",
                  paste0(nrow(extant[which(extant$race.cat == "hispanic"), ]),
                         " (",
                         round(100 * nrow(extant[which(extant$race.cat == "hispanic"), ]) /
                                 nrow(extant), 1), ")"),
                  round(mean(extant$p_duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$p_duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$p_duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1)),
                  round(mean(bothmain$p_duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(bothmain$p_duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothmain$p_duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1)),

                  round(mean(bothcas$p_duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(bothcas$p_duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothcas$p_duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1)))

other <- cbind("other",
               paste0(nrow(extant[which(extant$race.cat == "other"), ]),
                      " (", round(100 * nrow(extant[which(extant$race.cat == "other"), ]) /
                                    nrow(extant), 1), ")"),
               round(mean(extant$p_duration[which(extant$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(extant$p_duration[which(extant$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$p_duration[which(extant$race.cat == "other")], na.rm = TRUE), 1)),
               round(mean(bothmain$p_duration[bothmain$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$p_duration[bothmain$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$p_duration[bothmain$race.cat == "other"], na.rm = TRUE), 1)),

               round(mean(bothcas$p_duration[bothcas$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$p_duration[bothcas$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$p_duration[bothcas$race.cat == "other"], na.rm = TRUE), 1)))


# Age
fifteen24 <- cbind("15-24",
                   paste0(nrow(extant[which(extant$age.cat == "15-24"), ]),
                          " (",
                          round(100 * nrow(extant[which(extant$age.cat == "15-24"), ]) /
                                  nrow(extant), 1), ")"),
                   round(mean(extant$p_duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(extant$p_duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(extant$p_duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1)),
                   round(mean(bothmain$p_duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(bothmain$p_duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(bothmain$p_duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1)),

                   round(mean(bothcas$p_duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(bothcas$p_duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(bothcas$p_duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1)))

twentyfive34 <- cbind("25-34",
                      paste0(nrow(extant[which(extant$age.cat == "25-34"), ]),
                             " (",
                             round(100 * nrow(extant[which(extant$age.cat == "25-34"), ]) /
                                     nrow(extant), 1), ")"),
                      round(mean(extant$p_duration[which(extant$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(extant$p_duration[which(extant$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(extant$p_duration[which(extant$age.cat == "25-34")], na.rm = TRUE), 1)),
                      round(mean(bothmain$p_duration[bothmain$age.cat == "25-34"], na.rm = TRUE), 1),
                      paste0(round(sd(bothmain$p_duration[bothmain$age.cat == "25-34"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothmain$p_duration[bothmain$age.cat == "25-34"], na.rm = TRUE), 1)),

                      round(mean(bothcas$p_duration[bothcas$age.cat == "25-34"], na.rm = TRUE), 1),
                      paste0(round(sd(bothcas$p_duration[bothcas$age.cat == "25-34"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothcas$p_duration[bothcas$age.cat == "25-34"], na.rm = TRUE), 1)))

thirtyfive44 <- cbind("35-44",
                  paste0(nrow(extant[which(extant$age.cat == "35-44"), ]),
                         " (",
                         round(100 * nrow(extant[which(extant$age.cat == "35-44"), ]) /
                                 nrow(extant), 1), ")"),
                  round(mean(extant$p_duration[which(extant$age.cat == "35-44")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$p_duration[which(extant$age.cat == "35-44")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$p_duration[which(extant$age.cat == "35-44")], na.rm = TRUE), 1)),
                  round(mean(bothmain$p_duration[bothmain$age.cat == "35-44"], na.rm = TRUE), 1),
                  paste0(round(sd(bothmain$p_duration[bothmain$age.cat == "35-44"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothmain$p_duration[bothmain$age.cat == "35-44"], na.rm = TRUE), 1)),

                  round(mean(bothcas$p_duration[bothcas$age.cat == "35-44"], na.rm = TRUE), 1),
                  paste0(round(sd(bothcas$p_duration[bothcas$age.cat == "35-44"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothcas$p_duration[bothcas$age.cat == "35-44"], na.rm = TRUE), 1)))

fortyfive54 <- cbind("45-54",
                 paste0(nrow(extant[which(extant$age.cat == "45-54"), ]),
                        " (",
                        round(100 * nrow(extant[which(extant$age.cat == "45-54"), ]) /
                                nrow(extant), 1), ")"),
                 round(mean(extant$p_duration[which(extant$age.cat == "45-54")], na.rm = TRUE), 1),
                 paste0(round(sd(extant$p_duration[which(extant$age.cat == "45-54")], na.rm = TRUE), 1),
                        ", ",
                        round(median(extant$p_duration[which(extant$age.cat == "45-54")], na.rm = TRUE), 1)),
                 round(mean(bothmain$p_duration[bothmain$age.cat == "45-54"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$p_duration[bothmain$age.cat == "45-54"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$p_duration[bothmain$age.cat == "45-54"], na.rm = TRUE), 1)),

                 round(mean(bothcas$p_duration[bothcas$age.cat == "45-54"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$p_duration[bothcas$age.cat == "45-54"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$p_duration[bothcas$age.cat == "45-54"], na.rm = TRUE), 1)))


fiftyfive65 <- cbind("55-65",
                 paste0(nrow(extant[which(extant$age.cat == "55-65"), ]),
                        " (",
                        round(100 * nrow(extant[which(extant$age.cat == "55-65"), ]) /
                                nrow(extant), 1), ")"),
                 round(mean(extant$p_duration[which(extant$age.cat == "55-65")], na.rm = TRUE), 1),
                 paste0(round(sd(extant$p_duration[which(extant$age.cat == "55-65")], na.rm = TRUE), 1),
                        ", ",
                        round(median(extant$p_duration[which(extant$age.cat == "55-65")], na.rm = TRUE), 1)),
                 round(mean(bothmain$p_duration[bothmain$age.cat == "55-65"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$p_duration[bothmain$age.cat == "55-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$p_duration[bothmain$age.cat == "55-65"], na.rm = TRUE), 1)),

                 round(mean(bothcas$p_duration[bothcas$age.cat == "55-65"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$p_duration[bothcas$age.cat == "55-65"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$p_duration[bothcas$age.cat == "55-65"], na.rm = TRUE), 1)))

# HIV Status
HIVPos <- cbind("HIV Pos",
                paste0(nrow(extant[which(extant$hiv == 1), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == 1), ]) /
                               nrow(extant), 1), ")"),
                round(mean(extant$p_duration[which(extant$hiv == 1)], na.rm = TRUE), 1),
                paste0(round(sd(extant$p_duration[which(extant$hiv == 1)], na.rm = TRUE), 1),
                       ", ",
                       round(median(extant$p_duration[which(extant$hiv == 1)], na.rm = TRUE), 1)),
                round(mean(bothmain$p_duration[bothmain$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$p_duration[bothmain$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$p_duration[bothmain$hiv == 1], na.rm = TRUE), 1)),

                round(mean(bothcas$p_duration[bothcas$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$p_duration[bothcas$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$p_duration[bothcas$hiv == 1], na.rm = TRUE), 1)))

HIVNeg <- cbind("HIV Neg",
                paste0(nrow(extant[which(extant$hiv == 0), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == 0), ]) /
                               nrow(extant), 1), ")"),
                round(mean(extant$p_duration[which(extant$hiv == 0)], na.rm = TRUE), 1),
                paste0(round(sd(extant$p_duration[which(extant$hiv == 0)], na.rm = TRUE), 1),
                       ", ",
                       round(median(extant$p_duration[which(extant$hiv == 0)], na.rm = TRUE), 1)),
                round(mean(bothmain$p_duration[bothmain$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$p_duration[bothmain$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$p_duration[bothmain$hiv == 0], na.rm = TRUE), 1)),

                round(mean(bothcas$p_duration[bothcas$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$p_duration[bothcas$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$p_duration[bothcas$hiv == 0], na.rm = TRUE), 1)))

# Output table
table3 <- rbind(total, black, white, hispanic, other,
                fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                HIVPos, HIVNeg)
colnames(table3) <- c("Category", "N (%)", "Total Mean", "Total SD, Med",
                      "Main Degree Mean", "Main Degree SD, Med", "Cas Degree Mean",
                      "Cas Degree SD, Med")
write.csv(table3, file = "Output/table3.csv")


# Table 3b - Duration by partner matching --------------

# Set up data frames
extant <- artnetLong[which(artnetLong$p_ONGOING == 1 & artnetLong$p_duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]

# Subset to those active
extant <- extant[which(extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1), ]

# Number of rows with no reported activity
nrow(extant[which(extant$p_RAI == 0 & extant$p_IAI == 0 & extant$p_ROI == 0 & extant$p_IOI == 0), ])

bothmain <- extant[which((extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1) & extant$ptype == 1), ]
bothcas <- extant[which((extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1) & extant$ptype == 2), ]

# Race
# All
allrace <- which(!(is.na(extant$race.cat)))
bb <- which(extant$race.cat == "black" & extant$partracecat == "black")
ww <- which(extant$race.cat == "white" & extant$partracecat == "white")
hh <- which(extant$race.cat == "hispanic" & extant$partracecat == "hispanic")
oo <- which(extant$race.cat == "other" & extant$partracecat == "other")
matched <- c(bb, ww, hh, oo)
norace <- which(is.na(extant$partracecat))
givenrace <- setdiff(allrace, norace)
unmatchrace <- setdiff(givenrace, matched)

bb <- extant[bb, ]
ww <- extant[ww, ]
hh <- extant[hh, ]
oo <- extant[oo, ]
norace <- extant[norace, ]
givenrace <- extant[givenrace, ]
unmatchrace <- extant[unmatchrace, ]

#Main
allracem <- which(!(is.na(bothmain$race.cat)))
bbm <- which(bothmain$race.cat == "black" & bothmain$partracecat == "black")
wwm <- which(bothmain$race.cat == "white" & bothmain$partracecat == "white")
hhm <- which(bothmain$race.cat == "hispanic" & bothmain$partracecat == "hispanic")
oom <- which(bothmain$race.cat == "other" & bothmain$partracecat == "other")
matchedm <- c(bbm, wwm, hhm, oom)
noracem <- which(is.na(bothmain$partracecat))
givenracem <- setdiff(allracem, noracem)
unmatchracem <- setdiff(givenracem, matchedm)

bbm <- bothmain[bbm, ]
wwm <- bothmain[wwm, ]
hhm <- bothmain[hhm, ]
oom <- bothmain[oom, ]
noracem <- bothmain[noracem, ]
givenracem <- bothmain[givenracem, ]
unmatchracem <- bothmain[unmatchracem, ]

#Cas
allracec <- which(!(is.na(bothcas$race.cat)))
bbc <- which(bothcas$race.cat == "black" & bothcas$partracecat == "black")
wwc <- which(bothcas$race.cat == "white" & bothcas$partracecat == "white")
hhc <- which(bothcas$race.cat == "hispanic" & bothcas$partracecat == "hispanic")
ooc <- which(bothcas$race.cat == "other" & bothcas$partracecat == "other")
matchedc <- c(bbc, wwc, hhc, ooc)
noracec <- which(is.na(bothcas$partracecat))
givenracec <- setdiff(allracec, noracec)
unmatchracec <- setdiff(givenracec, matchedc)

bbc <- bothcas[bbc, ]
wwc <- bothcas[wwc, ]
hhc <- bothcas[hhc, ]
ooc <- bothcas[ooc, ]
noracec <- bothcas[noracec, ]
givenracec <- bothcas[givenracec, ]
unmatchracec <- bothcas[unmatchracec, ]

# HIV
# All
allhiv <- which(!(is.na(extant$hiv)))
nn <- which(extant$hiv == 0 & extant$partstatus == "Negative")
pp <- which(extant$hiv == 1 & extant$partstatus == "Positive")
matched <- c(nn, pp)
nohiv <- which(is.na(extant$partstatus))
givenhiv <- setdiff(allhiv, nohiv)
unmatchhiv <- setdiff(givenhiv, matched)

nn <- extant[nn, ]
pp <- extant[pp, ]
nohiv <- extant[nohiv, ]
givenhiv <- extant[givenhiv, ]
unmatchhiv <- extant[unmatchhiv, ]

#Main
allhivm <- which(!(is.na(bothmain$hiv)))
nnm <- which(bothmain$hiv == 0 & bothmain$partstatus == "Negative")
ppm <- which(bothmain$hiv == 1 & bothmain$partstatus == "Positive")
matchedm <- c(nn, pp)
nohivm <- which(is.na(bothmain$partstatus))
givenhivm <- setdiff(allhivm, nohivm)
unmatchhivm <- setdiff(givenhivm, matchedm)

nnm <- bothmain[nnm, ]
ppm <- bothmain[ppm, ]
nohivm <- bothmain[nohivm, ]
givenhivm <- bothmain[givenhivm, ]
unmatchhivm <- bothmain[unmatchhivm, ]

#Cas
allhivc <- which(!(is.na(bothcas$hiv)))
nnc <- which(bothcas$hiv == 0 & bothcas$partstatus == "Negative")
ppc <- which(bothcas$hiv == 1 & bothcas$partstatus == "Positive")
matchedc <- c(nnc, ppc)
nohivc <- which(is.na(bothcas$partstatus))
givenhivc <- setdiff(allhivc, nohivc)
unmatchhivc <- setdiff(givenhivc, matchedc)

nnc <- bothcas[nnc, ]
ppc <- bothcas[ppc, ]
nohivc <- bothcas[nohivc, ]
givenhivc <- bothcas[givenhivc, ]
unmatchhivc <- bothcas[unmatchhivc, ]

# Age
# All
allage <- which(!(is.na(extant$race.cat)))
fifteen24 <- which(extant$age.cat == "15-24" & extant$partage.cat == "15-24")
twentyfive34 <- which(extant$age.cat == "25-34" & extant$partage.cat == "25-34")
thirtyfive44 <- which(extant$age.cat == "35-44" & extant$partage.cat == "35-44")
fortyfive54 <- which(extant$age.cat == "45-54" & extant$partage.cat == "45-54")
fiftyfive65 <- which(extant$age.cat == "55-65" & extant$partage.cat == "55-65")
matched <- c(fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65)
noage <- which(is.na(extant$partage.cat))
givenage <- setdiff(allage, noage)
unmatchage <- setdiff(givenage, matched)

fifteen24 <- extant[fifteen24, ]
twentyfive34 <- extant[twentyfive34, ]
thirtyfive44 <- extant[thirtyfive44, ]
fortyfive54 <- extant[fortyfive54, ]
fiftyfive65 <- extant[fiftyfive65, ]
noage <- extant[noage, ]
givenage <- extant[givenage, ]
unmatchage <- extant[unmatchage, ]

#Main
allagem <- which(!(is.na(bothmain$race.cat)))
fifteen24m <- which(bothmain$age.cat == "15-24" & bothmain$partage.cat == "15-24")
twentyfive34m <- which(bothmain$age.cat == "25-34" & bothmain$partage.cat == "25-34")
thirtyfive44m <- which(bothmain$age.cat == "35-44" & bothmain$partage.cat == "35-44")
fortyfive54m <- which(bothmain$age.cat == "45-54" & bothmain$partage.cat == "45-54")
fiftyfive65m <- which(bothmain$age.cat == "55-65" & bothmain$partage.cat == "55-65")
matchedm <- c(fifteen24m, twentyfive34m, thirtyfive44m, fortyfive54m, fiftyfive65m)
noagem <- which(is.na(bothmain$partage.cat))
givenagem <- setdiff(allagem, noagem)
unmatchagem <- setdiff(givenagem, matchedm)

fifteen24m <- bothmain[fifteen24m, ]
twentyfive34m <- bothmain[twentyfive34m, ]
thirtyfive44m <- bothmain[thirtyfive44m, ]
fortyfive54m <- bothmain[fortyfive54m, ]
fiftyfive65m <- bothmain[fiftyfive65m, ]
noagem <- bothmain[noagem, ]
givenagem <- bothmain[givenagem, ]
unmatchagem <- bothmain[unmatchagem, ]

#Cas
allagec <- which(!(is.na(bothcas$race.cat)))
fifteen24c <- which(bothcas$age.cat == "15-24" & bothcas$partage.cat == "15-24")
twentyfive34c <- which(bothcas$age.cat == "25-34" & bothcas$partage.cat == "25-34")
thirtyfive44c <- which(bothcas$age.cat == "35-44" & bothcas$partage.cat == "35-44")
fortyfive54c <- which(bothcas$age.cat == "45-54" & bothcas$partage.cat == "45-54")
fiftyfive65c <- which(bothcas$age.cat == "55-65" & bothcas$partage.cat == "55-65")
matchedc <- c(fifteen24c, twentyfive34c, thirtyfive44c, fortyfive54c, fiftyfive65c)
noagec <- which(is.na(bothcas$partage.cat))
givenagec <- setdiff(allagec, noagec)
unmatchagec <- setdiff(givenagec, matchedc)

fifteen24c <- bothcas[fifteen24c, ]
twentyfive34c <- bothcas[twentyfive34c, ]
thirtyfive44c <- bothcas[thirtyfive44c, ]
fortyfive54c <- bothcas[fortyfive54c, ]
fiftyfive65c <- bothcas[fiftyfive65c, ]
noagec <- bothcas[noagec, ]
givenagec <- bothcas[givenagec, ]
unmatchagec <- bothcas[unmatchagec, ]

# Total number of ongoing partnerships
total <- cbind("Total",
               paste0(nrow(extant)),
               paste0( 100 * nrow(extant) / nrow(extant), ")"),
               NA, NA, NA, NA, NA, NA,
               # All
               round(mean(extant$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(extant$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$p_duration, na.rm = TRUE), 1)),

               # Main
               round(mean(bothmain$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bothmain$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$p_duration, na.rm = TRUE), 1)),

               # Cas
               round(mean(bothcas$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bothcas$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$p_duration, na.rm = TRUE), 1)))

# Race/ethnicity
blackblack <- cbind("blackblack",
               paste0(nrow(bb)),
               paste0(
                      round(100 * nrow(bb) /
                              nrow(givenrace), 1), ")"),
               paste0(nrow(norace)),
                      paste0(
                      round(100 * nrow(norace) /
                              nrow(extant), 1), ")"),
               paste0(nrow(noracem)),
               paste0(
                      round(100 * nrow(noracem) /
                              nrow(bothmain), 1), ")"),
               paste0(nrow(noracec)),
               paste0(
                      round(100 * nrow(noracec) /
                              nrow(bothcas), 1), ")"),

               # All
               round(mean(bb$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bb$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bb$p_duration, na.rm = TRUE), 1)),

               # Main
               round(mean(bbm$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bbm$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bbm$p_duration, na.rm = TRUE), 1)),

               # Cas
               round(mean(bbc$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(bbc$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bbc$p_duration, na.rm = TRUE), 1)))


whitewhite <- cbind("whitewhite",
                    paste0(nrow(ww)),
                    paste0(
                           round(100 * nrow(ww) /
                                   nrow(givenrace), 1), ")"),
                    paste0(nrow(norace)),
                    paste0(
                           round(100 * nrow(norace) /
                                   nrow(extant), 1), ")"),
                    paste0(nrow(noracem)),
                    paste0(
                           round(100 * nrow(noracem) /
                                   nrow(bothmain), 1), ")"),
                    paste0(nrow(noracec)),
                    paste0(
                           round(100 * nrow(noracec) /
                                   nrow(bothcas), 1), ")"),
               # All
               round(mean(ww$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(ww$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(ww$p_duration, na.rm = TRUE), 1)),

               # Main
               round(mean(wwm$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(wwm$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(wwm$p_duration, na.rm = TRUE), 1)),

               # Cas
               round(mean(wwc$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(wwc$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(wwc$p_duration, na.rm = TRUE), 1)))

hisphisp <- cbind("hisphisp",
                  paste0(nrow(hh)),
                  paste0(
                         round(100 * nrow(hh) /
                                 nrow(givenrace), 1), ")"),
                  paste0(nrow(norace)),
                  paste0(
                         round(100 * nrow(norace) /
                                 nrow(extant), 1), ")"),
                  paste0(nrow(noracem)),
                  paste0(
                         round(100 * nrow(noracem) /
                                 nrow(bothmain), 1), ")"),
                  paste0(nrow(noracec)),
                  paste0(
                         round(100 * nrow(noracec) /
                                 nrow(bothcas), 1), ")"),
                  # All
                  round(mean(hh$p_duration, na.rm = TRUE), 1),
                  paste0(round(sd(hh$p_duration, na.rm = TRUE), 1),
                         ", ",
                         round(median(hh$p_duration, na.rm = TRUE), 1)),

                  # Main
                  round(mean(hhm$p_duration, na.rm = TRUE), 1),
                  paste0(round(sd(hhm$p_duration, na.rm = TRUE), 1),
                         ", ",
                         round(median(hhm$p_duration, na.rm = TRUE), 1)),

                  # Cas
                  round(mean(hhc$p_duration, na.rm = TRUE), 1),
                  paste0(round(sd(hhc$p_duration, na.rm = TRUE), 1),
                         ", ",
                         round(median(hhc$p_duration, na.rm = TRUE), 1)))

otherother <- cbind("otherother",
                    paste0(nrow(oo)),
                    paste0(
                           round(100 * nrow(oo) /
                                   nrow(givenrace), 1), ")"),
                    paste0(nrow(norace)),
                    paste0(
                           round(100 * nrow(norace) /
                                   nrow(extant), 1), ")"),
                    paste0(nrow(noracem)),
                    paste0(
                           round(100 * nrow(noracem) /
                                   nrow(bothmain), 1), ")"),
                    paste0(nrow(noracec)),
                    paste0(
                           round(100 * nrow(noracec) /
                                   nrow(bothcas), 1), ")"),
                    # All
                    round(mean(oo$p_duration, na.rm = TRUE), 1),
                    paste0(round(sd(oo$p_duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(oo$p_duration, na.rm = TRUE), 1)),

                    # Main
                    round(mean(oom$p_duration, na.rm = TRUE), 1),
                    paste0(round(sd(oom$p_duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(oom$p_duration, na.rm = TRUE), 1)),

                    # Cas
                    round(mean(ooc$p_duration, na.rm = TRUE), 1),
                    paste0(round(sd(ooc$p_duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(ooc$p_duration, na.rm = TRUE), 1)))
unmatchedrace <- cbind("unmatchedrace",
                       paste0(nrow(unmatchrace)),
                       paste0(
                              round(100 * nrow(unmatchrace) /
                                      nrow(givenrace), 1), ")"),
                       paste0(nrow(norace)),
                       paste0(
                              round(100 * nrow(norace) /
                                      nrow(extant), 1), ")"),
                       paste0(nrow(noracem)),
                       paste0(
                              round(100 * nrow(noracem) /
                                      nrow(bothmain), 1), ")"),
                       paste0(nrow(noracec)),
                       paste0(
                              round(100 * nrow(noracec) /
                                      nrow(bothcas), 1), ")"),
                    # All
                    round(mean(unmatchrace$p_duration, na.rm = TRUE), 1),
                    paste0(round(sd(unmatchrace$p_duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(unmatchrace$p_duration, na.rm = TRUE), 1)),

                    # Main
                    round(mean(unmatchracem$p_duration, na.rm = TRUE), 1),
                    paste0(round(sd(unmatchracem$p_duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(unmatchracem$p_duration, na.rm = TRUE), 1)),

                    # Cas
                    round(mean(unmatchracec$p_duration, na.rm = TRUE), 1),
                    paste0(round(sd(unmatchracec$p_duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(unmatchracec$p_duration, na.rm = TRUE), 1)))



# HIV Status
negneg <- cbind("negneg",
                paste0(nrow(nn)),
                paste0(
                       round(100 * nrow(nn) /
                               nrow(givenhiv), 1), ")"),
                paste0(nrow(nohiv)),
                paste0(
                       round(100 * nrow(nohiv) /
                               nrow(extant), 1), ")"),
                paste0(nrow(nohivm)),
                paste0(
                       round(100 * nrow(nohivm) /
                               nrow(bothmain), 1), ")"),
                paste0(nrow(nohivc)),
                paste0(
                       round(100 * nrow(nohivc) /
                               nrow(bothcas), 1), ")"),
                # All
                round(mean(nn$p_duration, na.rm = TRUE), 1),
                paste0(round(sd(nn$p_duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(nn$p_duration, na.rm = TRUE), 1)),

                # Main
                round(mean(nnm$p_duration, na.rm = TRUE), 1),
                paste0(round(sd(nnm$p_duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(nnm$p_duration, na.rm = TRUE), 1)),

                # Cas
                round(mean(nnc$p_duration, na.rm = TRUE), 1),
                paste0(round(sd(nnc$p_duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(nnc$p_duration, na.rm = TRUE), 1)))

pospos <- cbind("pospos",
                paste0(nrow(pp)),
                paste0(
                       round(100 * nrow(pp) /
                               nrow(givenhiv), 1), ")"),
                paste0(nrow(nohiv)),
                paste0(
                       round(100 * nrow(nohiv) /
                               nrow(extant), 1), ")"),
                paste0(nrow(nohivm)),
                paste0(
                       round(100 * nrow(nohivm) /
                               nrow(bothmain), 1), ")"),
                paste0(nrow(nohivc)),
                paste0(
                       round(100 * nrow(nohivc) /
                               nrow(bothcas), 1), ")"),
                # All
                round(mean(pp$p_duration, na.rm = TRUE), 1),
                paste0(round(sd(pp$p_duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(pp$p_duration, na.rm = TRUE), 1)),

                # Main
                round(mean(ppm$p_duration, na.rm = TRUE), 1),
                paste0(round(sd(ppm$p_duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(ppm$p_duration, na.rm = TRUE), 1)),

                # Cas
                round(mean(ppc$p_duration, na.rm = TRUE), 1),
                paste0(round(sd(ppc$p_duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(ppc$p_duration, na.rm = TRUE), 1)))
unmatchedhiv <- cbind("unmatchhiv",
                      paste0(nrow(unmatchhiv)),
                      paste0(
                             round(100 * nrow(unmatchhiv) /
                                     nrow(givenhiv), 1), ")"),
                      paste0(nrow(nohiv)),
                      paste0(
                             round(100 * nrow(nohiv) /
                                     nrow(extant), 1), ")"),
                      paste0(nrow(nohivm)),
                      paste0(
                             round(100 * nrow(nohivm) /
                                     nrow(bothmain), 1), ")"),
                      paste0(nrow(nohivc)),
                      paste0(
                             round(100 * nrow(nohivc) /
                                     nrow(bothcas), 1), ")"),
                   # All
                   round(mean(unmatchhiv$p_duration, na.rm = TRUE), 1),
                   paste0(round(sd(unmatchhiv$p_duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(unmatchhiv$p_duration, na.rm = TRUE), 1)),

                   # Main
                   round(mean(unmatchhivm$p_duration, na.rm = TRUE), 1),
                   paste0(round(sd(unmatchhivm$p_duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(unmatchhivm$p_duration, na.rm = TRUE), 1)),

                   # Cas
                   round(mean(unmatchhivc$p_duration, na.rm = TRUE), 1),
                   paste0(round(sd(unmatchhivc$p_duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(unmatchhivc$p_duration, na.rm = TRUE), 1)))

# Age
fifteen24 <- cbind("15-24",
                   paste0(nrow(fifteen24)),
                   paste0(
                          round(100 * nrow(fifteen24) /
                                  nrow(givenage), 1), ")"),
                   paste0(nrow(noage)),
                   paste0(
                          round(100 * nrow(noage) /
                                  nrow(extant), 1), ")"),
                   paste0(nrow(noagem)),
                   paste0(
                          round(100 * nrow(noagem) /
                                  nrow(bothmain), 1), ")"),
                   paste0(nrow(noagec)),
                   paste0(
                          round(100 * nrow(noagec) /
                                  nrow(bothcas), 1), ")"),
                   # All
                   round(mean(fifteen24$p_duration, na.rm = TRUE), 1),
                   paste0(round(sd(fifteen24$p_duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(fifteen24$p_duration, na.rm = TRUE), 1)),

                   # Main
                   round(mean(fifteen24m$p_duration, na.rm = TRUE), 1),
                   paste0(round(sd(fifteen24m$p_duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(fifteen24m$p_duration, na.rm = TRUE), 1)),

                   # Cas
                   round(mean(fifteen24c$p_duration, na.rm = TRUE), 1),
                   paste0(round(sd(fifteen24c$p_duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(fifteen24c$p_duration, na.rm = TRUE), 1)))

twentyfive34 <- cbind("25-34",
                      paste0(nrow(twentyfive34)),
                      paste0(
                             round(100 * nrow(twentyfive34) /
                                     nrow(givenage), 1), ")"),
                      paste0(nrow(noage)),
                      paste0(
                             round(100 * nrow(noage) /
                                     nrow(extant), 1), ")"),
                      paste0(nrow(noagem)),
                      paste0(
                             round(100 * nrow(noagem) /
                                     nrow(bothmain), 1), ")"),
                      paste0(nrow(noagec)),
                      paste0(
                             round(100 * nrow(noagec) /
                                     nrow(bothcas), 1), ")"),
                      # All
                      round(mean(twentyfive34$p_duration, na.rm = TRUE), 1),
                      paste0(round(sd(twentyfive34$p_duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(twentyfive34$p_duration, na.rm = TRUE), 1)),

                      # Main
                      round(mean(twentyfive34m$p_duration, na.rm = TRUE), 1),
                      paste0(round(sd(twentyfive34m$p_duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(twentyfive34m$p_duration, na.rm = TRUE), 1)),

                      # Cas
                      round(mean(twentyfive34c$p_duration, na.rm = TRUE), 1),
                      paste0(round(sd(twentyfive34c$p_duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(twentyfive34c$p_duration, na.rm = TRUE), 1)))

thirtyfive44 <- cbind("35-44",
                      paste0(nrow(thirtyfive44)),
                      paste0(
                             round(100 * nrow(thirtyfive44) /
                                     nrow(givenage), 1), ")"),
                      paste0(nrow(noage)),
                      paste0(
                             round(100 * nrow(noage) /
                                     nrow(extant), 1), ")"),
                      paste0(nrow(noagem)),
                      paste0(
                             round(100 * nrow(noagem) /
                                     nrow(bothmain), 1), ")"),
                      paste0(nrow(noagec)),
                      paste0(
                             round(100 * nrow(noagec) /
                                     nrow(bothcas), 1), ")"),
                      # All
                      round(mean(thirtyfive44$p_duration, na.rm = TRUE), 1),
                      paste0(round(sd(thirtyfive44$p_duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(thirtyfive44$p_duration, na.rm = TRUE), 1)),

                      # Main
                      round(mean(thirtyfive44m$p_duration, na.rm = TRUE), 1),
                      paste0(round(sd(thirtyfive44m$p_duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(thirtyfive44m$p_duration, na.rm = TRUE), 1)),

                      # Cas
                      round(mean(thirtyfive44c$p_duration, na.rm = TRUE), 1),
                      paste0(round(sd(thirtyfive44c$p_duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(thirtyfive44c$p_duration, na.rm = TRUE), 1)))

fortyfive54 <- cbind("45-54",
                     paste0(nrow(fortyfive54)),
                     paste0(
                            round(100 * nrow(fortyfive54) /
                                    nrow(givenage), 1), ")"),
                     paste0(nrow(noage)),
                     paste0(
                            round(100 * nrow(noage) /
                                    nrow(extant), 1), ")"),
                     paste0(nrow(noagem)),
                     paste0(
                            round(100 * nrow(noagem) /
                                    nrow(bothmain), 1), ")"),
                     paste0(nrow(noagec)),
                     paste0(
                            round(100 * nrow(noagec) /
                                    nrow(bothcas), 1), ")"),
                     # All
                     round(mean(fortyfive54$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(fortyfive54$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fortyfive54$p_duration, na.rm = TRUE), 1)),

                     # Main
                     round(mean(fortyfive54m$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(fortyfive54m$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fortyfive54m$p_duration, na.rm = TRUE), 1)),

                     # Cas
                     round(mean(fortyfive54c$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(fortyfive54c$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fortyfive54c$p_duration, na.rm = TRUE), 1)))


fiftyfive65 <- cbind("55-65",
                     paste0(nrow(fiftyfive65)),
                     paste0(
                            round(100 * nrow(fiftyfive65) /
                                    nrow(givenage), 1), ")"),
                     paste0(nrow(noage)),
                     paste0(
                            round(100 * nrow(noage) /
                                    nrow(extant), 1), ")"),
                     paste0(nrow(noagem)),
                     paste0(
                            round(100 * nrow(noagem) /
                                    nrow(bothmain), 1), ")"),
                     paste0(nrow(noagec)),
                     paste0(
                            round(100 * nrow(noagec) /
                                    nrow(bothcas), 1), ")"),
                     # All
                     round(mean(fiftyfive65$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(fiftyfive65$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fiftyfive65$p_duration, na.rm = TRUE), 1)),

                     # Main
                     round(mean(fiftyfive65m$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(fiftyfive65m$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fiftyfive65m$p_duration, na.rm = TRUE), 1)),

                     # Cas
                     round(mean(fiftyfive65c$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(fiftyfive65c$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fiftyfive65c$p_duration, na.rm = TRUE), 1)))

unmatchedage <- cbind("unmatchedage",
                      paste0(nrow(unmatchage)),
                      paste0(
                             round(100 * nrow(unmatchage) /
                                     nrow(givenage), 1), ")"),
                      paste0(nrow(noage)),
                      paste0(
                             round(100 * nrow(noage) /
                                     nrow(extant), 1), ")"),
                      paste0(nrow(noagem)),
                      paste0(
                             round(100 * nrow(noagem) /
                                     nrow(bothmain), 1), ")"),
                      paste0(nrow(noagec)),
                      paste0(
                             round(100 * nrow(noagec) /
                                     nrow(bothcas), 1), ")"),
                     # All
                     round(mean(unmatchage$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(unmatchage$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(unmatchage$p_duration, na.rm = TRUE), 1)),

                     # Main
                     round(mean(unmatchagem$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(unmatchagem$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(unmatchagem$p_duration, na.rm = TRUE), 1)),

                     # Cas
                     round(mean(unmatchagec$p_duration, na.rm = TRUE), 1),
                     paste0(round(sd(unmatchagec$p_duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(unmatchagec$p_duration, na.rm = TRUE), 1)))

# Output table
table3b <- rbind(total, blackblack, whitewhite, hisphisp, otherother, unmatchedrace,
                fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65, unmatchedage,
                negneg, pospos, unmatchedhiv)
colnames(table3b) <- c("Category", "N", "(%)", "NA N All", "NA % All","NA N Main",
                       "NA % Main", "NA N Cas", "NA % Cas",
                       "Total Mean", "Total SD, Med","Main Duration Mean",
                       "Main Duration SD, Med", "Cas Duration Mean",
                      "Cas Duration SD, Med")
write.csv(table3b, file = "Output/table3b.csv")

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


# Age (categorical)
tot <- artnetLong %>% count(age.cat, partage.cat)
a <- main %>% count(age.cat, partage.cat)

agemain <- cbind(cbind(c("15-24", "25-34", "35-44", "45-54", "55-65", "Unmatched", NA)),
                cbind(c(sum(tot$n[1]), sum(tot$n[8]), sum(tot$n[16]),
                        sum(tot$n[24]), sum(tot$n[32]),
                        sum(tot$n[c(2:5, 7, 9:12, 14:15, 17:19, 21:23, 25:26, 28:31, 33)]),
                        sum(tot$n[c(6, 13, 20, 27, 34)]))),
                cbind(rbind((100 * tot$n[1] / sum(tot$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                            (100 * tot$n[8] / sum(tot$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                            (100 * tot$n[16] / sum(tot$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                            (100 * tot$n[24] / sum(tot$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                            (100 * tot$n[32] / sum(tot$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                            (100 * sum(tot$n[c(2:5, 7, 9:12, 14:15, 17:19, 21:23, 25:26, 28:31)]) / sum(tot$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                            (100 * sum(tot$n[c(6, 13, 20, 27, 34)]) / sum(tot$n)))),
                cbind(c(sum(a$n[1]), sum(a$n[8]), sum(a$n[16]),
                        sum(a$n[23]), sum(a$n[31]), sum(a$n[c(2:5, 7, 9:12, 14:15, 17:22, 24:25, 27:30, 32)]),
                        sum(a$n[c(6, 13, 26, 33)]))),
                 rbind((100 * a$n[1] / sum(a$n[c(1:5, 7:12, 14:25, 27:32)])),
                       (100 * sum(a$n[8]) / sum(a$n[c(1:5, 7:12, 14:25, 27:32)])),
                       (100 * sum(a$n[16]) / sum(a$n[c(1:5, 7:12, 14:25, 27:32)])),
                       (100 * sum(a$n[23]) / sum(a$n[c(1:5, 7:12, 14:25, 27:32)])),
                       (100 * sum(a$n[31]) / sum(a$n[c(1:5, 7:12, 14:25, 27:32)])),
                       (100 * sum(a$n[c(2:5, 7, 9:12, 14:15, 17:22, 24:25, 27:30, 32)]) / sum(a$n[c(1:5, 7:12, 14:25, 27:32)])),
                       (100 * sum(a$n[c(6, 13, 26, 33)]) / sum(a$n))))
b <- cas %>% count(age.cat, partage.cat)
agecas <- cbind(cbind(c(sum(b$n[1]), sum(b$n[8]), sum(b$n[16]),
                        sum(b$n[24]), sum(b$n[32]), sum(b$n[c(2:5, 7, 9:12, 14:15, 17:19, 21:23, 25:26, 28:31, 33)]),
                        sum(b$n[c(6, 13, 20, 27, 34)]))),
                rbind((100 * b$n[1] / sum(b$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                      (100 * sum(b$n[8]) / sum(b$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                      (100 * sum(b$n[16]) / sum(b$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                      (100 * sum(b$n[24]) / sum(b$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                      (100 * sum(b$n[32]) / sum(b$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                      (100 * sum(b$n[c(2:5, 7, 9:12, 14:15, 17:19, 21:23, 25:26, 28:31, 33)]) / sum(b$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                      (100 * sum(b$n[c(6, 13, 20, 27, 34)]) / sum(b$n))))
c <- inst %>% count(age.cat, partage.cat)
ageinst <- cbind(cbind(c(sum(c$n[1]), sum(c$n[8]), sum(c$n[16]),
                         sum(c$n[24]), sum(c$n[32]), sum(c$n[c(2:5, 7, 9:12, 14:15, 17:19, 21:23, 25:26, 28:31, 33)]),
                         sum(c$n[c(6, 13, 20, 27, 34)]))),
                 rbind((100 * c$n[1] / sum(c$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                       (100 * sum(c$n[8]) / sum(c$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                       (100 * sum(c$n[16]) / sum(c$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                       (100 * sum(c$n[23]) / sum(c$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                       (100 * sum(c$n[31]) / sum(c$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                       (100 * sum(c$n[c(2:5, 7, 9:12, 14:15, 17:19, 21:23, 25:26, 28:31, 33)]) / sum(c$n[c(1:5, 7:12, 14:19, 21:26, 28:33)])),
                       (100 * sum(c$n[c(6, 13, 26, 33)]) / sum(c$n))))

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
                 cbind(hivmain, hivcas, hivinst),
                 cbind(agemain, agecas, ageinst))
colnames(table4a) <- c("Category", "Total N", "Total %", "Main N", "Main %",
                       "Casual N", "Casual %", "Inst N", "Inst %")
table4b <- rbind(agecontin,
                 sqrtagecontin)
colnames(table4b) <- c("Category", "Total Mean", "Total SD", "Total Median",
                       "Main Mean", "Main SD", "Main Median",
                       "Cas Mean", "Cas SD", "Cas Median",
                       "Inst Mean", "Inst SD", "Inst Median")

write.csv(table4a, file = "Output/table4a.csv")
write.csv(table4b, file = "Output/table4b.csv")

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


# Supp Table 1 - AI and OI Degree --------------
# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$p_ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

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

# Create merged dataframe
artnet3 <- left_join(artnet, df2, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df3, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df5, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df6, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df8, by = "AMIS_ID")
artnet3 <- left_join(artnet3, df9, by = "AMIS_ID")

# If missing degree values, then set to 0
artnet3$allaionlydegree <- ifelse(is.na(artnet3$allaionlydegree), 0, artnet3$allaionlydegree)
artnet3$alloionlydegree <- ifelse(is.na(artnet3$alloionlydegree), 0, artnet3$alloionlydegree)
artnet3$mainaionlydegree <- ifelse(is.na(artnet3$mainaionlydegree), 0, artnet3$mainaionlydegree)
artnet3$mainoionlydegree <- ifelse(is.na(artnet3$mainoionlydegree), 0, artnet3$mainoionlydegree)
artnet3$casaionlydegree <- ifelse(is.na(artnet3$casaionlydegree), 0, artnet3$casaionlydegree)
artnet3$casoionlydegree <- ifelse(is.na(artnet3$casoionlydegree), 0, artnet3$casoionlydegree)

# City
citytotai <- glm(allaionlydegree ~ city - 1, family = "poisson", data = artnet3)
citytotai <- round(cbind(exp(coef(citytotai)), exp(confint(citytotai))), 2)
citytotoi <- glm(alloionlydegree ~ city - 1, family = "poisson", data = artnet3)
citytotoi <- round(cbind(exp(coef(citytotoi)), exp(confint(citytotoi))), 2)
citymainai <- glm(mainaionlydegree ~ city - 1, family = "poisson", data = artnet3)
citymainai <- round(cbind(exp(coef(citymainai)), exp(confint(citymainai))), 2)
citymainoi <- glm(mainoionlydegree ~ city - 1, family = "poisson", data = artnet3)
citymainoi <- round(cbind(exp(coef(citymainoi)), exp(confint(citymainoi))), 2)
citycasai <- glm(casaionlydegree ~ city - 1, family = "poisson", data = artnet3)
citycasai <- round(cbind(exp(coef(citycasai)), exp(confint(citycasai))), 2)
citycasoi <- glm(casoionlydegree ~ city - 1, family = "poisson", data = artnet3)
citycasoi <- round(cbind(exp(coef(citycasoi)), exp(confint(citycasoi))), 2)

# Overall Poisson:
totalallai <- glm(allaionlydegree ~ 1, family = "poisson", data = artnet3)
totalallai <- round(cbind(exp(coef(totalallai)), rbind(exp(confint(totalallai)))), 2)
totalalloi <- glm(alloionlydegree ~ 1, family = "poisson", data = artnet3)
totalalloi <- round(cbind(exp(coef(totalalloi)), rbind(exp(confint(totalalloi)))), 2)
totalmainai <- glm(mainaionlydegree ~ 1, family = "poisson", data = artnet3)
totalmainai <- round(cbind(exp(coef(totalmainai)), rbind(exp(confint(totalmainai)))), 2)
totalmainoi <- glm(mainoionlydegree ~ 1, family = "poisson", data = artnet3)
totalmainoi <- round(cbind(exp(coef(totalmainoi)), rbind(exp(confint(totalmainoi)))), 2)
totalcasai <- glm(casaionlydegree ~ 1, family = "poisson", data = artnet3)
totalcasai <- round(cbind(exp(coef(totalcasai)), rbind(exp(confint(totalcasai)))), 2)
totalcasoi <- glm(casoionlydegree ~ 1, family = "poisson", data = artnet3)
totalcasoi <- round(cbind(exp(coef(totalcasoi)), rbind(exp(confint(totalcasoi)))), 2)

# double check predictions against empirical means
group_by(artnet3) %>% summarize(mean(allaionlydegree))
group_by(artnet3) %>% summarize(mean(alloionlydegree))
group_by(artnet3) %>% summarize(mean(mainaionlydegree))
group_by(artnet3) %>% summarize(mean(mainoionlydegree))
group_by(artnet3) %>% summarize(mean(casaionlydegree))
group_by(artnet3) %>% summarize(mean(casoionlydegree))


total <- cbind("Total",
               totalallai[1, 1],
               paste0(totalallai[1, 2],
                      " - ",
                      totalallai[1, 3]),
               totalalloi[1, 1],
               paste0(totalalloi[1, 2],
                      " - ",
                      totalalloi[1, 3]),

               totalmainai[1, 1],
               paste0(totalmainai[1, 2],
                      " - ",
                      totalmainai[1, 3]),
               totalmainoi[1, 1],
               paste0(totalmainoi[1, 2],
                      " - ",
                      totalmainoi[1, 3]),

               totalcasai[1, 1],
               paste0(totalcasai[1, 2],
                      " - ",
                      totalcasai[1, 3]),
               totalcasoi[1, 1],
               paste0(totalcasoi[1, 2],
                      " - ",
                      totalcasoi[1, 3]))

# Race/ethnicity
raceallai <- glm(allaionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
raceallai <- round(cbind(exp(coef(raceallai)), rbind(exp(confint(raceallai)))), 2)
racealloi <- glm(alloionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
racealloi <- round(cbind(exp(coef(racealloi)), rbind(exp(confint(racealloi)))), 2)
racemainai <- glm(mainaionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
racemainai <- round(cbind(exp(coef(racemainai)), rbind(exp(confint(racemainai)))), 2)
racemainoi <- glm(mainoionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
racemainoi <- round(cbind(exp(coef(racemainoi)), rbind(exp(confint(racemainoi)))), 2)
racecasai <- glm(casaionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
racecasai <- round(cbind(exp(coef(racecasai)), rbind(exp(confint(racecasai)))), 2)
racecasoi <- glm(casoionlydegree ~ race.cat - 1, family = "poisson", data = artnet3)
racecasoi <- round(cbind(exp(coef(racecasoi)), rbind(exp(confint(racecasoi)))), 2)

# double check predictions against empirical means
group_by(artnet3, race.cat) %>% summarize(mean(allaionlydegree))
group_by(artnet3, race.cat) %>% summarize(mean(alloionlydegree))
group_by(artnet3, race.cat) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, race.cat) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, race.cat) %>% summarize(mean(casaionlydegree))
group_by(artnet3, race.cat) %>% summarize(mean(casoionlydegree))

black <- cbind("black",
               raceallai[1, 1],
               paste0(raceallai[1, 2],
                      " - ",
                      raceallai[1, 3]),
               racealloi[1, 1],
               paste0(racealloi[1, 2],
                      " - ",
                      racealloi[1, 3]),

               racemainai[1, 1],
               paste0(racemainai[1, 2],
                      " - ",
                      racemainai[1, 3]),
               racemainoi[1, 1],
               paste0(racemainoi[1, 2],
                      " - ",
                      racemainoi[1, 3]),

               racecasai[1, 1],
               paste0(racecasai[1, 2],
                      " - ",
                      racecasai[1, 3]),
               racecasoi[1, 1],
               paste0(racecasoi[1, 2],
                      " - ",
                      racecasoi[1, 3]))

white <- cbind("white",
               raceallai[4, 1],
               paste0(raceallai[4, 2],
                      " - ",
                      raceallai[4, 3]),
               racealloi[4, 1],
               paste0(racealloi[4, 2],
                      " - ",
                      racealloi[4, 3]),

               racemainai[4, 1],
               paste0(racemainai[4, 2],
                      " - ",
                      racemainai[4, 3]),
               racemainoi[4, 1],
               paste0(racemainoi[4, 2],
                      " - ",
                      racemainoi[4, 3]),

               racecasai[4, 1],
               paste0(racecasai[4, 2],
                      " - ",
                      racecasai[4, 3]),
               racecasoi[4, 1],
               paste0(racecasoi[4, 2],
                      " - ",
                      racecasoi[4, 3]))

hispanic <- cbind("hispanic",
                  raceallai[2, 1],
                  paste0(raceallai[2, 2],
                         " - ",
                         raceallai[2, 3]),
                  racealloi[2, 1],
                  paste0(racealloi[2, 2],
                         " - ",
                         racealloi[2, 3]),

                  racemainai[2, 1],
                  paste0(racemainai[2, 2],
                         " - ",
                         racemainai[2, 3]),
                  racemainoi[2, 1],
                  paste0(racemainoi[2, 2],
                         " - ",
                         racemainoi[2, 3]),

                  racecasai[2, 1],
                  paste0(racecasai[2, 2],
                         " - ",
                         racecasai[2, 3]),
                  racecasoi[2, 1],
                  paste0(racecasoi[2, 2],
                         " - ",
                         racecasoi[2, 3]))

other <- cbind("other",
               raceallai[3, 1],
               paste0(raceallai[3, 2],
                      " - ",
                      raceallai[3, 3]),
               racealloi[3, 1],
               paste0(racealloi[3, 2],
                      " - ",
                      racealloi[3, 3]),

               racemainai[3, 1],
               paste0(racemainai[3, 2],
                      " - ",
                      racemainai[3, 3]),
               racemainoi[3, 1],
               paste0(racemainoi[3, 2],
                      " - ",
                      racemainoi[3, 3]),

               racecasai[3, 1],
               paste0(racecasai[3, 2],
                      " - ",
                      racecasai[3, 3]),
               racecasoi[3, 1],
               paste0(racecasoi[3, 2],
                      " - ",
                      racecasoi[3, 3]))

# Age
ageallai <- glm(allaionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
ageallai <- round(cbind(exp(coef(ageallai)), rbind(exp(confint(ageallai)))), 2)
agealloi <- glm(alloionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
agealloi <- round(cbind(exp(coef(agealloi)), rbind(exp(confint(agealloi)))), 2)
agemainai <- glm(mainaionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
agemainai <- round(cbind(exp(coef(agemainai)), rbind(exp(confint(agemainai)))), 2)
agemainoi <- glm(mainoionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
agemainoi <- round(cbind(exp(coef(agemainoi)), rbind(exp(confint(agemainoi)))), 2)
agecasai <- glm(casaionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
agecasai <- round(cbind(exp(coef(agecasai)), rbind(exp(confint(agecasai)))), 2)
agecasoi <- glm(casoionlydegree ~ age.cat - 1, family = "poisson", data = artnet3)
agecasoi <- round(cbind(exp(coef(agecasoi)), rbind(exp(confint(agecasoi)))), 2)

# double check predictions against empirical means
group_by(artnet3, age.cat) %>% summarize(mean(allaionlydegree))
group_by(artnet3, age.cat) %>% summarize(mean(alloionlydegree))
group_by(artnet3, age.cat) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, age.cat) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, age.cat) %>% summarize(mean(casaionlydegree))
group_by(artnet3, age.cat) %>% summarize(mean(casoionlydegree))


fifteen24 <- cbind("15-24",
                   ageallai[1, 1],
                   paste0(ageallai[1, 2],
                          " - ",
                          ageallai[1, 3]),
                   agealloi[1, 1],
                   paste0(agealloi[1, 2],
                          " - ",
                          agealloi[1, 3]),

                   agemainai[1, 1],
                   paste0(agemainai[1, 2],
                          " - ",
                          agemainai[1, 3]),
                   agemainoi[1, 1],
                   paste0(agemainoi[1, 2],
                          " - ",
                          agemainoi[1, 3]),

                   agecasai[1, 1],
                   paste0(agecasai[1, 2],
                          " - ",
                          agecasai[1, 3]),
                   agecasoi[1, 1],
                   paste0(agecasoi[1, 2],
                          " - ",
                          agecasoi[1, 3]))


twentyfive34 <- cbind("25-34",
                      ageallai[2, 1],
                      paste0(ageallai[2, 2],
                             " - ",
                             ageallai[2, 3]),
                      agealloi[2, 1],
                      paste0(agealloi[2, 2],
                             " - ",
                             agealloi[2, 3]),

                      agemainai[2, 1],
                      paste0(agemainai[2, 2],
                             " - ",
                             agemainai[2, 3]),
                      agemainoi[2, 1],
                      paste0(agemainoi[2, 2],
                             " - ",
                             agemainoi[2, 3]),

                      agecasai[2, 1],
                      paste0(agecasai[2, 2],
                             " - ",
                             agecasai[2, 3]),
                      agecasoi[2, 1],
                      paste0(agecasoi[2, 2],
                             " - ",
                             agecasoi[2, 3]))

thirtyfive44 <- cbind("35-44",
                  ageallai[3, 1],
                  paste0(ageallai[3, 2],
                         " - ",
                         ageallai[3, 3]),
                  agealloi[3, 1],
                  paste0(agealloi[3, 2],
                         " - ",
                         agealloi[3, 3]),

                  agemainai[3, 1],
                  paste0(agemainai[3, 2],
                         " - ",
                         agemainai[3, 3]),
                  agemainoi[3, 1],
                  paste0(agemainoi[3, 2],
                         " - ",
                         agemainoi[3, 3]),

                  agecasai[3, 1],
                  paste0(agecasai[3, 2],
                         " - ",
                         agecasai[3, 3]),
                  agecasoi[3, 1],
                  paste0(agecasoi[3, 2],
                         " - ",
                         agecasoi[3, 3]))

fortyfive54 <- cbind("45-54",
                 ageallai[4, 1],
                 paste0(ageallai[4, 2],
                        " - ",
                        ageallai[4, 3]),
                 agealloi[4, 1],
                 paste0(agealloi[4, 2],
                        " - ",
                        agealloi[4, 3]),

                 agemainai[4, 1],
                 paste0(agemainai[4, 2],
                        " - ",
                        agemainai[4, 3]),
                 agemainoi[4, 1],
                 paste0(agemainoi[4, 2],
                        " - ",
                        agemainoi[4, 3]),

                 agecasai[4, 1],
                 paste0(agecasai[4, 2],
                        " - ",
                        agecasai[4, 3]),
                 agecasoi[4, 1],
                 paste0(agecasoi[4, 2],
                        " - ",
                        agecasoi[4, 3]))

fiftyfive65 <- cbind("55-65",
                 ageallai[5, 1],
                 paste0(ageallai[5, 2],
                        " - ",
                        ageallai[5, 3]),
                 agealloi[5, 1],
                 paste0(agealloi[5, 2],
                        " - ",
                        agealloi[5, 3]),

                 agemainai[5, 1],
                 paste0(agemainai[5, 2],
                        " - ",
                        agemainai[5, 3]),
                 agemainoi[5, 1],
                 paste0(agemainoi[5, 2],
                        " - ",
                        agemainoi[5, 3]),

                 agecasai[5, 1],
                 paste0(agecasai[5, 2],
                        " - ",
                        agecasai[5, 3]),
                 agecasoi[5, 1],
                 paste0(agecasoi[5, 2],
                        " - ",
                        agecasoi[5, 3]))

# Region and division
divallai <- glm(allaionlydegree ~ division - 1, family = "poisson", data = artnet3)
divallai <- round(cbind(exp(coef(divallai)), rbind(exp(confint(divallai)))), 2)
divalloi <- glm(alloionlydegree ~ division - 1, family = "poisson", data = artnet3)
divalloi <- round(cbind(exp(coef(divalloi)), rbind(exp(confint(divalloi)))), 2)
divmainai <- glm(mainaionlydegree ~ division - 1, family = "poisson", data = artnet3)
divmainai <- round(cbind(exp(coef(divmainai)), rbind(exp(confint(divmainai)))), 2)
divmainoi <- glm(mainoionlydegree ~ division - 1, family = "poisson", data = artnet3)
divmainoi <- round(cbind(exp(coef(divmainoi)), rbind(exp(confint(divmainoi)))), 2)
divcasai <- glm(casaionlydegree ~ division - 1, family = "poisson", data = artnet3)
divcasai <- round(cbind(exp(coef(divcasai)), rbind(exp(confint(divcasai)))), 2)
divcasoi <- glm(casoionlydegree ~ division - 1, family = "poisson", data = artnet3)
divcasoi <- round(cbind(exp(coef(divcasoi)), rbind(exp(confint(divcasoi)))), 2)

regallai <- glm(allaionlydegree ~ region - 1, family = "poisson", data = artnet3)
regallai <- round(cbind(exp(coef(regallai)), rbind(exp(confint(regallai)))), 2)
regalloi <- glm(alloionlydegree ~ region - 1, family = "poisson", data = artnet3)
regalloi <- round(cbind(exp(coef(regalloi)), rbind(exp(confint(regalloi)))), 2)
regmainai <- glm(mainaionlydegree ~ region - 1, family = "poisson", data = artnet3)
regmainai <- round(cbind(exp(coef(regmainai)), rbind(exp(confint(regmainai)))), 2)
regmainoi <- glm(mainoionlydegree ~ region - 1, family = "poisson", data = artnet3)
regmainoi <- round(cbind(exp(coef(regmainoi)), rbind(exp(confint(regmainoi)))), 2)
regcasai <- glm(casaionlydegree ~ region - 1, family = "poisson", data = artnet3)
regcasai <- round(cbind(exp(coef(regcasai)), rbind(exp(confint(regcasai)))), 2)
regcasoi <- glm(casoionlydegree ~ region - 1, family = "poisson", data = artnet3)
regcasoi <- round(cbind(exp(coef(regcasoi)), rbind(exp(confint(regcasoi)))), 2)

# double check predictions against empirical means
group_by(artnet3, division) %>% summarize(mean(allaionlydegree))
group_by(artnet3, division) %>% summarize(mean(alloionlydegree))
group_by(artnet3, division) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, division) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, division) %>% summarize(mean(casaionlydegree))
group_by(artnet3, division) %>% summarize(mean(casoionlydegree))
group_by(artnet3, region) %>% summarize(mean(allaionlydegree))
group_by(artnet3, region) %>% summarize(mean(alloionlydegree))
group_by(artnet3, region) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, region) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, region) %>% summarize(mean(casaionlydegree))
group_by(artnet3, region) %>% summarize(mean(casoionlydegree))


# Region and Division
West <- cbind("West",
              regallai[4, 1],
              paste0(regallai[4, 2],
                     " - ",
                     regallai[4, 3]),
              regalloi[4, 1],
              paste0(regalloi[4, 2],
                     " - ",
                     regalloi[4, 3]),

              regmainai[4, 1],
              paste0(regmainai[4, 2],
                     " - ",
                     regmainai[4, 3]),
              regmainoi[4, 1],
              paste0(regmainoi[4, 2],
                     " - ",
                     regmainoi[4, 3]),

              regcasai[4, 1],
              paste0(regcasai[4, 2],
                     " - ",
                     regcasai[4, 3]),
              regcasoi[4, 1],
              paste0(regcasoi[4, 2],
                     " - ",
                     regcasoi[4, 3]))

Pacific <- cbind("Pacific",
                 divallai[6, 1],
                 paste0(divallai[6, 2],
                        " - ",
                        divallai[6, 3]),
                 divalloi[6, 1],
                 paste0(divalloi[6, 2],
                        " - ",
                        divalloi[6, 3]),

                 divmainai[6, 1],
                 paste0(divmainai[6, 2],
                        " - ",
                        divmainai[6, 3]),
                 divmainoi[6, 1],
                 paste0(divmainoi[6, 2],
                        " - ",
                        divmainoi[6, 3]),

                 divcasai[6, 1],
                 paste0(divcasai[6, 2],
                        " - ",
                        divcasai[6, 3]),
                 divcasoi[6, 1],
                 paste0(divcasoi[6, 2],
                        " - ",
                        divcasoi[6, 3]))

Mountain <- cbind("Mountain",
                  divallai[4, 1],
                  paste0(divallai[4, 2],
                         " - ",
                         divallai[4, 3]),
                  divalloi[4, 1],
                  paste0(divalloi[4, 2],
                         " - ",
                         divalloi[4, 3]),

                  divmainai[4, 1],
                  paste0(divmainai[4, 2],
                         " - ",
                         divmainai[4, 3]),
                  divmainoi[4, 1],
                  paste0(divmainoi[4, 2],
                         " - ",
                         divmainoi[4, 3]),

                  divcasai[4, 1],
                  paste0(divcasai[4, 2],
                         " - ",
                         divcasai[4, 3]),
                  divcasoi[4, 1],
                  paste0(divcasoi[4, 2],
                         " - ",
                         divcasoi[4, 3]))

Midwest <- cbind("Midwest",
                 regallai[1, 1],
                 paste0(regallai[1, 2],
                        " - ",
                        regallai[1, 3]),
                 regalloi[1, 1],
                 paste0(regalloi[1, 2],
                        " - ",
                        regalloi[1, 3]),

                 regmainai[1, 1],
                 paste0(regmainai[1, 2],
                        " - ",
                        regmainai[1, 3]),
                 regmainoi[1, 1],
                 paste0(regmainoi[1, 2],
                        " - ",
                        regmainoi[1, 3]),

                 regcasai[1, 1],
                 paste0(regcasai[1, 2],
                        " - ",
                        regcasai[1, 3]),
                 regcasoi[1, 1],
                 paste0(regcasoi[1, 2],
                        " - ",
                        regcasoi[1, 3]))

WNC <- cbind("West North Central",
             divallai[8, 1],
             paste0(divallai[8, 2],
                    " - ",
                    divallai[8, 3]),
             divalloi[8, 1],
             paste0(divalloi[8, 2],
                    " - ",
                    divalloi[8, 3]),

             divmainai[8, 1],
             paste0(divmainai[8, 2],
                    " - ",
                    divmainai[8, 3]),
             divmainoi[8, 1],
             paste0(divmainoi[8, 2],
                    " - ",
                    divmainoi[8, 3]),

             divcasai[8, 1],
             paste0(divcasai[8, 2],
                    " - ",
                    divcasai[8, 3]),
             divcasoi[8, 1],
             paste0(divcasoi[8, 2],
                    " - ",
                    divcasoi[8, 3]))

ENC <- cbind("East North Central",
             divallai[1, 1],
             paste0(divallai[1, 2],
                    " - ",
                    divallai[1, 3]),
             divalloi[1, 1],
             paste0(divalloi[1, 2],
                    " - ",
                    divalloi[1, 3]),

             divmainai[1, 1],
             paste0(divmainai[1, 2],
                    " - ",
                    divmainai[1, 3]),
             divmainoi[1, 1],
             paste0(divmainoi[1, 2],
                    " - ",
                    divmainoi[1, 3]),

             divcasai[1, 1],
             paste0(divcasai[1, 2],
                    " - ",
                    divcasai[1, 3]),
             divcasoi[1, 1],
             paste0(divcasoi[1, 2],
                    " - ",
                    divcasoi[1, 3]))

South <- cbind("South",
               regallai[3, 1],
               paste0(regallai[3, 2],
                      " - ",
                      regallai[3, 3]),
               regalloi[3, 1],
               paste0(regalloi[3, 2],
                      " - ",
                      regalloi[3, 3]),

               regmainai[3, 1],
               paste0(regmainai[3, 2],
                      " - ",
                      regmainai[3, 3]),
               regmainoi[3, 1],
               paste0(regmainoi[3, 2],
                      " - ",
                      regmainoi[3, 3]),

               regcasai[3, 1],
               paste0(regcasai[3, 2],
                      " - ",
                      regcasai[3, 3]),
               regcasoi[3, 1],
               paste0(regcasoi[3, 2],
                      " - ",
                      regcasoi[3, 3]))

WSC <- cbind("West South Central",
             divallai[9, 1],
             paste0(divallai[9, 2],
                    " - ",
                    divallai[9, 3]),
             divalloi[9, 1],
             paste0(divalloi[9, 2],
                    " - ",
                    divalloi[9, 3]),

             divmainai[9, 1],
             paste0(divmainai[9, 2],
                    " - ",
                    divmainai[9, 3]),
             divmainoi[9, 1],
             paste0(divmainoi[9, 2],
                    " - ",
                    divmainoi[9, 3]),

             divcasai[9, 1],
             paste0(divcasai[9, 2],
                    " - ",
                    divcasai[9, 3]),
             divcasoi[9, 1],
             paste0(divcasoi[9, 2],
                    " - ",
                    divcasoi[9, 3]))

ESC <- cbind("East South Central",
             divallai[2, 1],
             paste0(divallai[2, 2],
                    " - ",
                    divallai[2, 3]),
             divalloi[2, 1],
             paste0(divalloi[2, 2],
                    " - ",
                    divalloi[2, 3]),

             divmainai[2, 1],
             paste0(divmainai[2, 2],
                    " - ",
                    divmainai[2, 3]),
             divmainoi[2, 1],
             paste0(divmainoi[2, 2],
                    " - ",
                    divmainoi[2, 3]),

             divcasai[2, 1],
             paste0(divcasai[2, 2],
                    " - ",
                    divcasai[2, 3]),
             divcasoi[2, 1],
             paste0(divcasoi[2, 2],
                    " - ",
                    divcasoi[2, 3]))
SA <- cbind("South Atlantic",
            divallai[7, 1],
            paste0(divallai[7, 2],
                   " - ",
                   divallai[7, 3]),
            divalloi[7, 1],
            paste0(divalloi[7, 2],
                   " - ",
                   divalloi[7, 3]),

            divmainai[7, 1],
            paste0(divmainai[7, 2],
                   " - ",
                   divmainai[7, 3]),
            divmainoi[7, 1],
            paste0(divmainoi[7, 2],
                   " - ",
                   divmainoi[7, 3]),

            divcasai[7, 1],
            paste0(divcasai[7, 2],
                   " - ",
                   divcasai[7, 3]),
            divcasoi[7, 1],
            paste0(divcasoi[7, 2],
                   " - ",
                   divcasoi[7, 3]))

Northeast <- cbind("Northeast",
                   regallai[2, 1],
                   paste0(regallai[2, 2],
                          " - ",
                          regallai[2, 3]),
                   regalloi[2, 1],
                   paste0(regalloi[2, 2],
                          " - ",
                          regalloi[2, 3]),

                   regmainai[2, 1],
                   paste0(regmainai[2, 2],
                          " - ",
                          regmainai[2, 3]),
                   regmainoi[2, 1],
                   paste0(regmainoi[2, 2],
                          " - ",
                          regmainoi[2, 3]),

                   regcasai[2, 1],
                   paste0(regcasai[2, 2],
                          " - ",
                          regcasai[2, 3]),
                   regcasoi[2, 1],
                   paste0(regcasoi[2, 2],
                          " - ",
                          regcasoi[2, 3]))

MA <- cbind("Middle Atlantic",
            divallai[3, 1],
            paste0(divallai[3, 2],
                   " - ",
                   divallai[3, 3]),
            divalloi[3, 1],
            paste0(divalloi[3, 2],
                   " - ",
                   divalloi[3, 3]),

            divmainai[3, 1],
            paste0(divmainai[3, 2],
                   " - ",
                   divmainai[3, 3]),
            divmainoi[3, 1],
            paste0(divmainoi[3, 2],
                   " - ",
                   divmainoi[3, 3]),

            divcasai[3, 1],
            paste0(divcasai[3, 2],
                   " - ",
                   divcasai[3, 3]),
            divcasoi[3, 1],
            paste0(divcasoi[3, 2],
                   " - ",
                   divcasoi[3, 3]))

NE <- cbind("New England",
            divallai[5, 1],
            paste0(divallai[5, 2],
                   " - ",
                   divallai[5, 3]),
            divalloi[5, 1],
            paste0(divalloi[5, 2],
                   " - ",
                   divalloi[5, 3]),

            divmainai[5, 1],
            paste0(divmainai[5, 2],
                   " - ",
                   divmainai[5, 3]),
            divmainoi[5, 1],
            paste0(divmainoi[5, 2],
                   " - ",
                   divmainoi[5, 3]),

            divcasai[5, 1],
            paste0(divcasai[5, 2],
                   " - ",
                   divcasai[5, 3]),
            divcasoi[5, 1],
            paste0(divcasoi[5, 2],
                   " - ",
                   divcasoi[5, 3]))

# Urbanicity
urbanallai <- glm(allaionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbanallai <- round(cbind(exp(coef(urbanallai)), rbind(exp(confint(urbanallai)))), 2)
urbanalloi <- glm(alloionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbanalloi <- round(cbind(exp(coef(urbanalloi)), rbind(exp(confint(urbanalloi)))), 2)
urbanmainai <- glm(mainaionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbanmainai <- round(cbind(exp(coef(urbanmainai)), rbind(exp(confint(urbanmainai)))), 2)
urbanmainoi <- glm(mainoionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbanmainoi <- round(cbind(exp(coef(urbanmainoi)), rbind(exp(confint(urbanmainoi)))), 2)
urbancasai <- glm(casaionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbancasai <- round(cbind(exp(coef(urbancasai)), rbind(exp(confint(urbancasai)))), 2)
urbancasoi <- glm(casoionlydegree ~ NCHSCHAR - 1, family = "poisson", data = artnet3)
urbancasoi <- round(cbind(exp(coef(urbancasoi)), rbind(exp(confint(urbancasoi)))), 2)

# double check predictions against empirical means
group_by(artnet3, NCHSCHAR) %>% summarize(mean(allaionlydegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(alloionlydegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(casaionlydegree))
group_by(artnet3, NCHSCHAR) %>% summarize(mean(casoionlydegree))


LCM <- cbind("Large Central Metro",
             urbanallai[1, 1],
             paste0(urbanallai[1, 2],
                    " - ",
                    urbanallai[1, 3]),
             urbanalloi[1, 1],
             paste0(urbanalloi[1, 2],
                    " - ",
                    urbanalloi[1, 3]),

             urbanmainai[1, 1],
             paste0(urbanmainai[1, 2],
                    " - ",
                    urbanmainai[1, 3]),
             urbanmainoi[1, 1],
             paste0(urbanmainoi[1, 2],
                    " - ",
                    urbanmainoi[1, 3]),

             urbancasai[1, 1],
             paste0(urbancasai[1, 2],
                    " - ",
                    urbancasai[1, 3]),
             urbancasoi[1, 1],
             paste0(urbancasoi[1, 2],
                    " - ",
                    urbancasoi[1, 3]))

LFM <- cbind("Large Fringe Metro",
             urbanallai[2, 1],
             paste0(urbanallai[2, 2],
                    " - ",
                    urbanallai[2, 3]),
             urbanalloi[2, 1],
             paste0(urbanalloi[2, 2],
                    " - ",
                    urbanalloi[2, 3]),

             urbanmainai[2, 1],
             paste0(urbanmainai[2, 2],
                    " - ",
                    urbanmainai[2, 3]),
             urbanmainoi[2, 1],
             paste0(urbanmainoi[2, 2],
                    " - ",
                    urbanmainoi[2, 3]),

             urbancasai[2, 1],
             paste0(urbancasai[2, 2],
                    " - ",
                    urbancasai[2, 3]),
             urbancasoi[2, 1],
             paste0(urbancasoi[2, 2],
                    " - ",
                    urbancasoi[2, 3]))
Medium <- cbind("Medium Metro",
                urbanallai[3, 1],
                paste0(urbanallai[3, 2],
                       " - ",
                       urbanallai[3, 3]),
                urbanalloi[3, 1],
                paste0(urbanalloi[3, 2],
                       " - ",
                       urbanalloi[3, 3]),

                urbanmainai[3, 1],
                paste0(urbanmainai[3, 2],
                       " - ",
                       urbanmainai[3, 3]),
                urbanmainoi[3, 1],
                paste0(urbanmainoi[3, 2],
                       " - ",
                       urbanmainoi[3, 3]),

                urbancasai[3, 1],
                paste0(urbancasai[3, 2],
                       " - ",
                       urbancasai[3, 3]),
                urbancasoi[3, 1],
                paste0(urbancasoi[3, 2],
                       " - ",
                       urbancasoi[3, 3]))
Small <- cbind("Small Metro",
               urbanallai[6, 1],
               paste0(urbanallai[6, 2],
                      " - ",
                      urbanallai[6, 3]),
               urbanalloi[6, 1],
               paste0(urbanalloi[6, 2],
                      " - ",
                      urbanalloi[6, 3]),

               urbanmainai[6, 1],
               paste0(urbanmainai[6, 2],
                      " - ",
                      urbanmainai[6, 3]),
               urbanmainoi[6, 1],
               paste0(urbanmainoi[6, 2],
                      " - ",
                      urbanmainoi[6, 3]),

               urbancasai[6, 1],
               paste0(urbancasai[6, 2],
                      " - ",
                      urbancasai[6, 3]),
               urbancasoi[6, 1],
               paste0(urbancasoi[6, 2],
                      " - ",
                      urbancasoi[6, 3]))
Micro <- cbind("Micropolitan",
               urbanallai[4, 1],
               paste0(urbanallai[4, 2],
                      " - ",
                      urbanallai[4, 3]),
               urbanalloi[4, 1],
               paste0(urbanalloi[4, 2],
                      " - ",
                      urbanalloi[4, 3]),

               urbanmainai[4, 1],
               paste0(urbanmainai[4, 2],
                      " - ",
                      urbanmainai[4, 3]),
               urbanmainoi[4, 1],
               paste0(urbanmainoi[4, 2],
                      " - ",
                      urbanmainoi[4, 3]),

               urbancasai[4, 1],
               paste0(urbancasai[4, 2],
                      " - ",
                      urbancasai[4, 3]),
               urbancasoi[4, 1],
               paste0(urbancasoi[4, 2],
                      " - ",
                      urbancasoi[4, 3]))
Noncore <- cbind("Noncore",
                 urbanallai[5, 1],
                 paste0(urbanallai[5, 2],
                        " - ",
                        urbanallai[5, 3]),
                 urbanalloi[5, 1],
                 paste0(urbanalloi[5, 2],
                        " - ",
                        urbanalloi[5, 3]),

                 urbanmainai[5, 1],
                 paste0(urbanmainai[5, 2],
                        " - ",
                        urbanmainai[5, 3]),
                 urbanmainoi[5, 1],
                 paste0(urbanmainoi[5, 2],
                        " - ",
                        urbanmainoi[5, 3]),

                 urbancasai[5, 1],
                 paste0(urbancasai[5, 2],
                        " - ",
                        urbancasai[5, 3]),
                 urbancasoi[5, 1],
                 paste0(urbancasoi[5, 2],
                        " - ",
                        urbancasoi[5, 3]))

# HIV Status
hivstatallai <- glm(allaionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatallai <- round(cbind(exp(coef(hivstatallai)), rbind(exp(confint(hivstatallai)))), 2)
hivstatallaiv2 <- glm(allaionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatallaiv2 <- round(cbind(exp(coef(hivstatallaiv2)), rbind(exp(confint(hivstatallaiv2)))), 2)

hivstatalloi <- glm(alloionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatalloi <- round(cbind(exp(coef(hivstatalloi)), rbind(exp(confint(hivstatalloi)))), 2)
hivstatalloiv2 <- glm(alloionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatalloiv2 <- round(cbind(exp(coef(hivstatalloiv2)), rbind(exp(confint(hivstatalloiv2)))), 2)

hivstatmainai <- glm(mainaionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatmainai <- round(cbind(exp(coef(hivstatmainai)), rbind(exp(confint(hivstatmainai)))), 2)
hivstatmainaiv2 <- glm(mainaionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatmainaiv2 <- round(cbind(exp(coef(hivstatmainaiv2)), rbind(exp(confint(hivstatmainaiv2)))), 2)

hivstatmainoi <- glm(mainoionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatmainoi <- round(cbind(exp(coef(hivstatmainoi)), rbind(exp(confint(hivstatmainoi)))), 2)
hivstatmainoiv2 <- glm(mainoionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatmainoiv2 <- round(cbind(exp(coef(hivstatmainoiv2)), rbind(exp(confint(hivstatmainoiv2)))), 2)

hivstatcasai <- glm(casaionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatcasai <- round(cbind(exp(coef(hivstatcasai)), rbind(exp(confint(hivstatcasai)))), 2)
hivstatcasaiv2 <- glm(casaionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatcasaiv2 <- round(cbind(exp(coef(hivstatcasaiv2)), rbind(exp(confint(hivstatcasaiv2)))), 2)

hivstatcasoi <- glm(casoionlydegree ~ hiv, family = "poisson", data = artnet3)
hivstatcasoi <- round(cbind(exp(coef(hivstatcasoi)), rbind(exp(confint(hivstatcasoi)))), 2)
hivstatcasoiv2 <- glm(casoionlydegree ~ hiv - 1, family = "poisson", data = artnet3)
hivstatcasoiv2 <- round(cbind(exp(coef(hivstatcasoiv2)), rbind(exp(confint(hivstatcasoiv2)))), 2)

# double check predictions against empirical means
group_by(artnet3, hiv) %>% summarize(mean(allaionlydegree))
group_by(artnet3, hiv) %>% summarize(mean(alloionlydegree))
group_by(artnet3, hiv) %>% summarize(mean(mainaionlydegree))
group_by(artnet3, hiv) %>% summarize(mean(mainoionlydegree))
group_by(artnet3, hiv) %>% summarize(mean(casaionlydegree))
group_by(artnet3, hiv) %>% summarize(mean(casoionlydegree))


HIVPos <- cbind("HIV Pos",
                hivstatallaiv2[1, 1],
                paste0(hivstatallaiv2[1, 2],
                       " - ",
                       hivstatallaiv2[1, 3]),
                hivstatalloiv2[1, 1],
                paste0(hivstatalloiv2[1, 2],
                       " - ",
                       hivstatalloiv2[1, 3]),
                hivstatmainaiv2[1, 1],
                paste0(hivstatmainaiv2[1, 2],
                       " - ",
                       hivstatmainaiv2[1, 3]),
                hivstatmainoiv2[1, 1],
                paste0(hivstatmainoiv2[1, 2],
                       " - ",
                       hivstatmainoiv2[1, 3]),
                hivstatcasaiv2[1, 1],
                paste0(hivstatcasaiv2[1, 2],
                       " - ",
                       hivstatcasaiv2[1, 3]),
                hivstatcasoiv2[1, 1],
                paste0(hivstatcasoiv2[1, 2],
                       " - ",
                       hivstatcasoiv2[1, 3]))
HIVNeg <- cbind("HIV Neg",
                hivstatallai[1, 1],
                paste0(hivstatallai[1, 2],
                       " - ",
                       hivstatallai[1, 3]),
                hivstatalloi[1, 1],
                paste0(hivstatalloi[1, 2],
                       " - ",
                       hivstatalloi[1, 3]),
                hivstatmainai[1, 1],
                paste0(hivstatmainai[1, 2],
                       " - ",
                       hivstatmainai[1, 3]),
                hivstatmainoi[1, 1],
                paste0(hivstatmainoi[1, 2],
                       " - ",
                       hivstatmainoi[1, 3]),
                hivstatcasai[1, 1],
                paste0(hivstatcasai[1, 2],
                       " - ",
                       hivstatcasai[1, 3]),
                hivstatcasoi[1, 1],
                paste0(hivstatcasoi[1, 2],
                       " - ",
                       hivstatcasoi[1, 3]))

# Other types of statistics
# % of egos Concurrent
concurr <- cbind("Concurr",
                 paste0(length(which(artnet3$allaionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$allaionlydegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$alloionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$alloionlydegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$mainaionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$mainaionlydegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$mainoionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$mainoionlydegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$casaionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$casaionlydegree > 1)) / nrow(artnet3), 2), "%)"),
                 paste0(length(which(artnet3$casoionlydegree > 1)),
                        " (",
                        round(100 * length(which(artnet3$casoionlydegree > 1)) / nrow(artnet3), 2), "%)")
                 )

# Output table
stable1a <- rbind(total, black, white, hispanic, other,
                 fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                 West, Pacific, Mountain, Midwest, WNC, ENC,
                 South, WSC, ESC, SA, Northeast, MA, NE, LCM, LFM, Medium,
                 Small, Micro, Noncore,
                 HIVPos, HIVNeg)
colnames(stable1a) <- c("Category",
                        "All AI Mean", "All AI CI",
                        "All OI Mean", "All OI CI",
                        "Main AI Mean", "Main AI CI",
                        "Main OI Mean", "Main OI CI",
                        "Cas AI Mean", "Cas AI CI",
                        "Cas OI Mean", "Cas OI CI")
write.csv(stable1a, file = "Output/stable1a.csv")

stable1b <- rbind(concurr)
colnames(stable1b) <- c("Category",
                        "All AI N(%)", "All OI N(%)",
                        "Main AI N(%)", "Main OI N(%)",
                        "Cas AI N(%)", "Cas OI N(%)")
write.csv(stable1b, file = "Output/stable1b.csv")

# Supp Table 2 - One-Off Weekly Rate by AI and OI --------------
#### AI only ------
totalooai <- glm(rate.oo.ai.part ~ 1, family = "poisson", data = artnet2)
totalooai <- round(cbind(exp(coef(totalooai)), rbind(exp(confint(totalooai)))), 3)

totalooai <- cbind("Total",
                   totalooai[1, 1],
                   paste0(totalooai[1, 2],
                          " - ",
                          totalooai[1, 3]))

# Race/ethnicity
raceooai <- glm(rate.oo.ai.part ~ race.cat - 1, family = "poisson", data = artnet2)
raceooai <- round(cbind(exp(coef(raceooai)), exp(confint(raceooai))), 3)

# double check predictions against empirical means
group_by(d, race.cat) %>% summarize(mean(rate.oo.ai.part))

blackooai <- cbind("black",
                   raceooai[1, 1],
                   paste0(raceooai[1, 2],
                          " - ",
                          raceooai[1, 3]))

whiteooai <- cbind("white",
                   raceooai[4, 1],
                   paste0(raceooai[4, 2],
                          " - ",
                          raceooai[4, 3]))

hispanicooai <- cbind("hispanic",
                      raceooai[2, 1],
                      paste0(raceooai[2, 2],
                             " - ",
                             raceooai[2, 3]))

otherooai <- cbind("other",
                   raceooai[3, 1],
                   paste0(raceooai[3, 2],
                          " - ",
                          raceooai[3, 3]))

# Age
ageooai <- glm(rate.oo.ai.part ~ age.cat - 1, family = "poisson", data = artnet2)
ageooai <- round(cbind(exp(coef(ageooai)), exp(confint(ageooai))), 3)


# double check predictions against empirical means
group_by(d, age.cat) %>% summarize(mean(rate.oo.ai.part))

fifteen24ooai <- cbind("15-24",
                       ageooai[1, 1],
                       paste0(ageooai[1, 2],
                              " - ",
                              ageooai[1, 3]))


twentyfive34ooai <- cbind("25-34",
                          ageooai[2, 1],
                          paste0(ageooai[2, 2],
                                 " - ",
                                 ageooai[2, 3]))

thirtyfive44ooai <- cbind("35-44",
                          ageooai[3, 1],
                          paste0(ageooai[3, 2],
                                 " - ",
                                 ageooai[3, 3]))

fortyfive54ooai <- cbind("45-54",
                         ageooai[4, 1],
                         paste0(ageooai[4, 2],
                                " - ",
                                ageooai[4, 3]))

fiftyfive65ooai <- cbind("55-65",
                         ageooai[5, 1],
                         paste0(ageooai[5, 2],
                                " - ",
                                ageooai[5, 3]))

# Region and division
divisionooai <- glm(rate.oo.ai.part ~ division - 1, family = "poisson", data = artnet2)
divisionooai <- round(cbind(exp(coef(divisionooai)), exp(confint(divisionooai))), 3)
regionooai <- glm(rate.oo.ai.part ~ region - 1, family = "poisson", data = artnet2)
regionooai <- round(cbind(exp(coef(regionooai)), exp(confint(regionooai))), 3)

# double check predictions against empirical means
group_by(artnet2, region) %>% summarize(mean(rate.oo.ai.part))
group_by(artnet2, division) %>% summarize(mean(rate.oo.ai.part))

# Region and Division
Westooai <- cbind("West",
                  regionooai[4, 1],
                  paste0(regionooai[4, 2],
                         " - ",
                         regionooai[4, 3]))

Pacificooai <- cbind("Pacific",
                     divisionooai[6, 1],
                     paste0(divisionooai[6, 2],
                            " - ",
                            divisionooai[6, 3]))

Mountainooai <- cbind("Mountain",
                      divisionooai[4, 1],
                      paste0(divisionooai[4, 2],
                             " - ",
                             divisionooai[4, 3]))

Midwestooai <- cbind("Midwest",
                     regionooai[1, 1],
                     paste0(regionooai[1, 2],
                            " - ",
                            regionooai[1, 3]))

WNCooai <- cbind("West North Central",
                 divisionooai[8, 1],
                 paste0(divisionooai[8, 2],
                        " - ",
                        divisionooai[8, 3]))

ENCooai <- cbind("East North Central",
                 divisionooai[1, 1],
                 paste0(divisionooai[1, 2],
                        " - ",
                        divisionooai[1, 3]))

Southooai <- cbind("South",
                   regionooai[3, 1],
                   paste0(regionooai[3, 2],
                          " - ",
                          regionooai[3, 3]))

WSCooai <- cbind("West South Central",
                 divisionooai[9, 1],
                 paste0(divisionooai[9, 2],
                        " - ",
                        divisionooai[9, 3]))

ESCooai <- cbind("East South Central",
                 divisionooai[2, 1],
                 paste0(divisionooai[2, 2],
                        " - ",
                        divisionooai[2, 3]))
SAooai <- cbind("South Atlantic",
                divisionooai[7, 1],
                paste0(divisionooai[7, 2],
                       " - ",
                       divisionooai[7, 3]))

Northeastooai <- cbind("Northeast",
                       regionooai[2, 1],
                       paste0(regionooai[2, 2],
                              " - ",
                              regionooai[2, 3]))

MAooai <- cbind("Middle Atlantic",
                divisionooai[3, 1],
                paste0(divisionooai[3, 2],
                       " - ",
                       divisionooai[3, 3]))

NEooai <- cbind("New England",
                divisionooai[5, 1],
                paste0(divisionooai[5, 2],
                       " - ",
                       divisionooai[5, 3]))

# Urbanicity
urbanooai <- glm(rate.oo.ai.part ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urbanooai <- round(cbind(exp(coef(urbanooai)), exp(confint(urbanooai))), 3)

# double check predictions against empirical means
group_by(artnet2, NCHSCHAR) %>% summarize(mean(rate.oo.ai.part))

LCMooai <- cbind("Large Central Metro",
                 urbanooai[1, 1],
                 paste0(urbanooai[1, 2],
                        " - ",
                        urbanooai[1, 3]))

LFMooai <- cbind("Large Fringe Metro",
                 urbanooai[2, 1],
                 paste0(urbanooai[2, 2],
                        " - ",
                        urbanooai[2, 3]))
Mediumooai <- cbind("Medium Metro",
                    urbanooai[3, 1],
                    paste0(urbanooai[3, 2],
                           " - ",
                           urbanooai[3, 3]))
Smallooai <- cbind("Small Metro",
                   urbanooai[6, 1],
                   paste0(urbanooai[6, 2],
                          " - ",
                          urbanooai[6, 3]))
Microooai <- cbind("Micropolitan",
                   urbanooai[4, 1],
                   paste0(urbanooai[4, 2],
                          " - ",
                          urbanooai[4, 3]))
Noncoreooai <- cbind("Noncore",
                     urbanooai[5, 1],
                     paste0(urbanooai[5, 2],
                            " - ",
                            urbanooai[5, 3]))

# HIV Status
hivstatooai <- glm(rate.oo.ai.part ~ hiv, family = "poisson", data = artnet2)
hivstatooai <- round(cbind(exp(coef(hivstatooai)), rbind(exp(confint(hivstatooai)))), 3)
hivstatv2ooai <- glm(rate.oo.ai.part ~ hiv - 1, family = "poisson", data = artnet2)
hivstatv2ooai <- round(cbind(exp(coef(hivstatv2ooai)), rbind(exp(confint(hivstatv2ooai)))), 3)

# double check predictions against empirical means
group_by(artnet2, hiv) %>% summarize(mean(rate.oo.ai.part))

HIVPosooai <- cbind("HIV Pos",
                    hivstatv2ooai[1, 1],
                    paste0(hivstatv2ooai[1, 2],
                           " - ",
                           hivstatv2ooai[1, 3]))
HIVNegooai <- cbind("HIV Neg",
                    hivstatooai[1, 1],
                    paste0(hivstatooai[1, 2],
                           " - ",
                           hivstatooai[1, 3]))

#### OI only ------
totaloooi <- glm(rate.oo.oi.part ~ 1, family = "poisson", data = artnet2)
totaloooi <- round(cbind(exp(coef(totaloooi)), rbind(exp(confint(totaloooi)))), 3)

totaloooi <- cbind("Total",
                   totaloooi[1, 1],
                 paste0(totaloooi[1, 2],
                        " - ",
                        totaloooi[1, 3]))

# Race/ethnicity
raceoooi <- glm(rate.oo.oi.part ~ race.cat - 1, family = "poisson", data = artnet2)
raceoooi <- round(cbind(exp(coef(raceoooi)), exp(confint(raceoooi))), 3)

# double check predictions against empirical means
group_by(d, race.cat) %>% summarize(mean(rate.oo.oi.part))

blackoooi <- cbind("black",
                   raceoooi[1, 1],
                 paste0(raceoooi[1, 2],
                        " - ",
                        raceoooi[1, 3]))

whiteoooi <- cbind("white",
                   raceoooi[4, 1],
                 paste0(raceoooi[4, 2],
                        " - ",
                        raceoooi[4, 3]))

hispanicoooi <- cbind("hispanic",
                      raceoooi[2, 1],
                    paste0(raceoooi[2, 2],
                           " - ",
                           raceoooi[2, 3]))

otheroooi <- cbind("other",
                   raceoooi[3, 1],
                 paste0(raceoooi[3, 2],
                        " - ",
                        raceoooi[3, 3]))

# Age
ageoooi <- glm(rate.oo.oi.part ~ age.cat - 1, family = "poisson", data = artnet2)
ageoooi <- round(cbind(exp(coef(ageoooi)), exp(confint(ageoooi))), 3)


# double check predictions against empirical means
group_by(d, age.cat) %>% summarize(mean(rate.oo.oi.part))

fifteen24oooi <- cbind("15-24",
                       ageoooi[1, 1],
                     paste0(ageoooi[1, 2],
                            " - ",
                            ageoooi[1, 3]))


twentyfive34oooi <- cbind("25-34",
                          ageoooi[2, 1],
                        paste0(ageoooi[2, 2],
                               " - ",
                               ageoooi[2, 3]))

thirtyfive44oooi <- cbind("35-44",
                          ageoooi[3, 1],
                        paste0(ageoooi[3, 2],
                               " - ",
                               ageoooi[3, 3]))

fortyfive54oooi <- cbind("45-54",
                         ageoooi[4, 1],
                       paste0(ageoooi[4, 2],
                              " - ",
                              ageoooi[4, 3]))

fiftyfive65oooi <- cbind("55-65",
                         ageoooi[5, 1],
                       paste0(ageoooi[5, 2],
                              " - ",
                              ageoooi[5, 3]))

# Region and division
divisionoooi <- glm(rate.oo.oi.part ~ division - 1, family = "poisson", data = artnet2)
divisionoooi <- round(cbind(exp(coef(divisionoooi)), exp(confint(divisionoooi))), 3)
regionoooi <- glm(rate.oo.oi.part ~ region - 1, family = "poisson", data = artnet2)
regionoooi <- round(cbind(exp(coef(regionoooi)), exp(confint(regionoooi))), 3)

# double check predictions against empirical means
group_by(artnet2, region) %>% summarize(mean(rate.oo.oi.part))
group_by(artnet2, division) %>% summarize(mean(rate.oo.oi.part))

# Region and Division
Westoooi <- cbind("West",
                  regionoooi[4, 1],
                paste0(regionoooi[4, 2],
                       " - ",
                       regionoooi[4, 3]))

Pacificoooi <- cbind("Pacific",
                     divisionoooi[6, 1],
                   paste0(divisionoooi[6, 2],
                          " - ",
                          divisionoooi[6, 3]))

Mountainoooi <- cbind("Mountain",
                      divisionoooi[4, 1],
                    paste0(divisionoooi[4, 2],
                           " - ",
                           divisionoooi[4, 3]))

Midwestoooi <- cbind("Midwest",
                     regionoooi[1, 1],
                   paste0(regionoooi[1, 2],
                          " - ",
                          regionoooi[1, 3]))

WNCoooi <- cbind("West North Central",
                 divisionoooi[8, 1],
               paste0(divisionoooi[8, 2],
                      " - ",
                      divisionoooi[8, 3]))

ENCoooi <- cbind("East North Central",
                 divisionoooi[1, 1],
               paste0(divisionoooi[1, 2],
                      " - ",
                      divisionoooi[1, 3]))

Southoooi <- cbind("South",
                   regionoooi[3, 1],
                 paste0(regionoooi[3, 2],
                        " - ",
                        regionoooi[3, 3]))

WSCoooi <- cbind("West South Central",
                 divisionoooi[9, 1],
               paste0(divisionoooi[9, 2],
                      " - ",
                      divisionoooi[9, 3]))

ESCoooi <- cbind("East South Central",
                 divisionoooi[2, 1],
               paste0(divisionoooi[2, 2],
                      " - ",
                      divisionoooi[2, 3]))
SAoooi <- cbind("South Atlantic",
                divisionoooi[7, 1],
              paste0(divisionoooi[7, 2],
                     " - ",
                     divisionoooi[7, 3]))

Northeastoooi <- cbind("Northeast",
                       regionoooi[2, 1],
                     paste0(regionoooi[2, 2],
                            " - ",
                            regionoooi[2, 3]))

MAoooi <- cbind("Middle Atlantic",
                divisionoooi[3, 1],
              paste0(divisionoooi[3, 2],
                     " - ",
                     divisionoooi[3, 3]))

NEoooi <- cbind("New England",
                divisionoooi[5, 1],
              paste0(divisionoooi[5, 2],
                     " - ",
                     divisionoooi[5, 3]))

# Urbanicity
urbanoooi <- glm(rate.oo.oi.part ~ NCHSCHAR - 1, family = "poisson", data = artnet2)
urbanoooi <- round(cbind(exp(coef(urbanoooi)), exp(confint(urbanoooi))), 3)

# double check predictions against empirical means
group_by(artnet2, NCHSCHAR) %>% summarize(mean(rate.oo.oi.part))

LCMoooi <- cbind("Large Central Metro",
                 urbanoooi[1, 1],
               paste0(urbanoooi[1, 2],
                      " - ",
                      urbanoooi[1, 3]))

LFMoooi <- cbind("Large Fringe Metro",
                 urbanoooi[2, 1],
               paste0(urbanoooi[2, 2],
                      " - ",
                      urbanoooi[2, 3]))
Mediumoooi <- cbind("Medium Metro",
                    urbanoooi[3, 1],
                  paste0(urbanoooi[3, 2],
                         " - ",
                         urbanoooi[3, 3]))
Smalloooi <- cbind("Small Metro",
                   urbanoooi[6, 1],
                 paste0(urbanoooi[6, 2],
                        " - ",
                        urbanoooi[6, 3]))
Microoooi <- cbind("Micropolitan",
                   urbanoooi[4, 1],
                 paste0(urbanoooi[4, 2],
                        " - ",
                        urbanoooi[4, 3]))
Noncoreoooi <- cbind("Noncore",
                     urbanoooi[5, 1],
                   paste0(urbanoooi[5, 2],
                          " - ",
                          urbanoooi[5, 3]))

# HIV Status
hivstatoooi <- glm(rate.oo.oi.part ~ hiv, family = "poisson", data = artnet2)
hivstatoooi <- round(cbind(exp(coef(hivstatoooi)), rbind(exp(confint(hivstatoooi)))), 3)
hivstatv2oooi <- glm(rate.oo.oi.part ~ hiv - 1, family = "poisson", data = artnet2)
hivstatv2oooi <- round(cbind(exp(coef(hivstatv2oooi)), rbind(exp(confint(hivstatv2oooi)))), 3)

# double check predictions against empirical means
group_by(artnet2, hiv) %>% summarize(mean(rate.oo.oi.part))

HIVPosoooi <- cbind("HIV Pos",
                    hivstatv2oooi[1, 1],
                  paste0(hivstatv2oooi[1, 2],
                         " - ",
                         hivstatv2oooi[1, 3]))
HIVNegoooi <- cbind("HIV Neg",
                    hivstatoooi[1, 1],
                  paste0(hivstatoooi[1, 2],
                         " - ",
                         hivstatoooi[1, 3]))

# Output table
stable2 <- cbind(rbind(totalooai, blackooai, whiteooai, hispanicooai, otherooai,
                      fifteen24ooai, twentyfive34ooai, thirtyfive44ooai, fortyfive54ooai, fiftyfive65ooai,
                      Westooai, Pacificooai, Mountainooai, Midwestooai, WNCooai, ENCooai,
                      Southooai, WSCooai, ESCooai, SAooai, Northeastooai, MAooai, NEooai, LCMooai, LFMooai, Mediumooai,
                      Smallooai, Microooai, Noncoreooai,
                      HIVPosooai, HIVNegooai),
                rbind(totaloooi, blackoooi, whiteoooi, hispanicoooi, otheroooi,
                      fifteen24oooi, twentyfive34oooi, thirtyfive44oooi, fortyfive54oooi, fiftyfive65oooi,
                      Westoooi, Pacificoooi, Mountainoooi, Midwestoooi, WNCoooi, ENCoooi,
                      Southoooi, WSCoooi, ESCoooi, SAoooi, Northeastoooi, MAoooi, NEoooi, LCMoooi, LFMoooi, Mediumoooi,
                      Smalloooi, Microoooi, Noncoreoooi,
                      HIVPosoooi, HIVNegoooi))
colnames(stable2) <- c("Category", "AI  Mean", "AI CI", "Category", "OI Mean", "OI CI")
write.csv(stable2, file = "Output/stable2.csv")

# Supp Table 3 - Duration by AI and OI --------------
extant <- artnetLong[which(artnetLong$p_ONGOING == 1 & artnetLong$p_duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]
extant <- extant[which(extant$p_RAI == 1 | extant$p_IAI == 1 | extant$p_ROI == 1 | extant$p_IOI == 1), ]

aimain <- extant[which((extant$p_RAI == 1 | extant$p_IAI == 1) & extant$ptype == 1), ]
aicas <- extant[which((extant$p_RAI == 1 | extant$p_IAI == 1) & extant$ptype == 2), ]
oimain <- extant[which((extant$p_ROI == 1 | extant$p_IOI == 1) & extant$ptype == 1), ]
oicas <- extant[which((extant$p_ROI == 1 | extant$p_IOI == 1) & extant$ptype == 2), ]

# Total number of ongoing partnerships
total <- cbind("Total", paste0(nrow(extant), " (", 100 * nrow(extant) / nrow(extant), ")"),

               round(mean(aimain$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(aimain$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$p_duration, na.rm = TRUE), 1)),

               round(mean(aicas$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(aicas$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$p_duration, na.rm = TRUE), 1)),

               round(mean(oimain$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(oimain$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$p_duration, na.rm = TRUE), 1)),

               round(mean(oicas$p_duration, na.rm = TRUE), 1),
               paste0(round(sd(oicas$p_duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$p_duration, na.rm = TRUE), 1)))

# Race/ethnicity
black <- cbind("black",
               paste0(nrow(extant[which(extant$race.cat == "black"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "black"), ]) /
                              nrow(extant), 1), ")"),

               round(mean(aimain$p_duration[which(aimain$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(aimain$p_duration[which(aimain$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$p_duration[which(aimain$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(aicas$p_duration[which(aicas$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(aicas$p_duration[which(aicas$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$p_duration[which(aicas$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(oimain$p_duration[which(oimain$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(oimain$p_duration[which(oimain$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$p_duration[which(oimain$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(oicas$p_duration[which(oicas$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(oicas$p_duration[which(oicas$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$p_duration[which(oicas$race.cat == "black")], na.rm = TRUE), 1)))

white <- cbind("white",
               paste0(nrow(extant[which(extant$race.cat == "white"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "white"), ]) /
                              nrow(extant), 1), ")"),

               round(mean(aimain$p_duration[which(aimain$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(aimain$p_duration[which(aimain$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$p_duration[which(aimain$race.cat == "white")], na.rm = TRUE), 1)),

               round(mean(aicas$p_duration[which(aicas$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(aicas$p_duration[which(aicas$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$p_duration[which(aicas$race.cat == "white")], na.rm = TRUE), 1)),

               round(mean(oimain$p_duration[which(oimain$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(oimain$p_duration[which(oimain$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$p_duration[which(oimain$race.cat == "white")], na.rm = TRUE), 1)),

               round(mean(oicas$p_duration[which(oicas$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(oicas$p_duration[which(oicas$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$p_duration[which(oicas$race.cat == "white")], na.rm = TRUE), 1)))

hispanic <- cbind("hispanic",
                  paste0(nrow(extant[which(extant$race.cat == "hispanic"), ]),
                         " (",
                         round(100 * nrow(extant[which(extant$race.cat == "hispanic"), ]) /
                                 nrow(extant), 1), ")"),

                  round(mean(aimain$p_duration[which(aimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(aimain$p_duration[which(aimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(aimain$p_duration[which(aimain$race.cat == "hispanic")], na.rm = TRUE), 1)),

                  round(mean(aicas$p_duration[which(aicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(aicas$p_duration[which(aicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(aicas$p_duration[which(aicas$race.cat == "hispanic")], na.rm = TRUE), 1)),

                  round(mean(oimain$p_duration[which(oimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(oimain$p_duration[which(oimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(oimain$p_duration[which(oimain$race.cat == "hispanic")], na.rm = TRUE), 1)),

                  round(mean(oicas$p_duration[which(oicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(oicas$p_duration[which(oicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(oicas$p_duration[which(oicas$race.cat == "hispanic")], na.rm = TRUE), 1)))

other <- cbind("other",
               paste0(nrow(extant[which(extant$race.cat == "other"), ]),
                      " (", round(100 * nrow(extant[which(extant$race.cat == "other"), ]) /
                                    nrow(extant), 1), ")"),

               round(mean(aimain$p_duration[which(aimain$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(aimain$p_duration[which(aimain$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$p_duration[which(aimain$race.cat == "other")], na.rm = TRUE), 1)),

               round(mean(aicas$p_duration[which(aicas$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(aicas$p_duration[which(aicas$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$p_duration[which(aicas$race.cat == "other")], na.rm = TRUE), 1)),

               round(mean(oimain$p_duration[which(oimain$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(oimain$p_duration[which(oimain$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$p_duration[which(oimain$race.cat == "other")], na.rm = TRUE), 1)),

               round(mean(oicas$p_duration[which(oicas$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(oicas$p_duration[which(oicas$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$p_duration[which(oicas$race.cat == "other")], na.rm = TRUE), 1)))


# Age
fifteen24 <- cbind("15-24",
                   paste0(nrow(extant[which(extant$age.cat == "15-24"), ]),
                          " (",
                          round(100 * nrow(extant[which(extant$age.cat == "15-24"), ]) /
                                  nrow(extant), 1), ")"),

                   round(mean(aimain$p_duration[which(aimain$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(aimain$p_duration[which(aimain$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(aimain$p_duration[which(aimain$age.cat == "15-24")], na.rm = TRUE), 1)),

                   round(mean(aicas$p_duration[which(aicas$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(aicas$p_duration[which(aicas$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(aicas$p_duration[which(aicas$age.cat == "15-24")], na.rm = TRUE), 1)),
                   round(mean(oimain$p_duration[which(oimain$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(oimain$p_duration[which(oimain$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(oimain$p_duration[which(oimain$age.cat == "15-24")], na.rm = TRUE), 1)),

                   round(mean(oicas$p_duration[which(oicas$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(oicas$p_duration[which(oicas$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(oicas$p_duration[which(oicas$age.cat == "15-24")], na.rm = TRUE), 1)))

twentyfive34 <- cbind("25-34",
                      paste0(nrow(extant[which(extant$age.cat == "25-34"), ]),
                             " (",
                             round(100 * nrow(extant[which(extant$age.cat == "25-34"), ]) /
                                     nrow(extant), 1), ")"),

                      round(mean(aimain$p_duration[which(aimain$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(aimain$p_duration[which(aimain$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(aimain$p_duration[which(aimain$age.cat == "25-34")], na.rm = TRUE), 1)),

                      round(mean(aicas$p_duration[which(aicas$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(aicas$p_duration[which(aicas$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(aicas$p_duration[which(aicas$age.cat == "25-34")], na.rm = TRUE), 1)),
                      round(mean(oimain$p_duration[which(oimain$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(oimain$p_duration[which(oimain$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(oimain$p_duration[which(oimain$age.cat == "25-34")], na.rm = TRUE), 1)),

                      round(mean(oicas$p_duration[which(oicas$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(oicas$p_duration[which(oicas$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(oicas$p_duration[which(oicas$age.cat == "25-34")], na.rm = TRUE), 1)))

thirtyfive44 <- cbind("35-44",
                  paste0(nrow(extant[which(extant$age.cat == "35-44"), ]),
                         " (",
                         round(100 * nrow(extant[which(extant$age.cat == "35-44"), ]) /
                                 nrow(extant), 1), ")"),

                  round(mean(aimain$p_duration[which(aimain$age.cat == "35-44")], na.rm = TRUE), 1),
                  paste0(round(sd(aimain$p_duration[which(aimain$age.cat == "35-44")], na.rm = TRUE), 1),
                         ", ",
                         round(median(aimain$p_duration[which(aimain$age.cat == "35-44")], na.rm = TRUE), 1)),

                  round(mean(aicas$p_duration[which(aicas$age.cat == "35-44")], na.rm = TRUE), 1),
                  paste0(round(sd(aicas$p_duration[which(aicas$age.cat == "35-44")], na.rm = TRUE), 1),
                         ", ",
                         round(median(aicas$p_duration[which(aicas$age.cat == "35-44")], na.rm = TRUE), 1)),
                  round(mean(oimain$p_duration[which(oimain$age.cat == "35-44")], na.rm = TRUE), 1),
                  paste0(round(sd(oimain$p_duration[which(oimain$age.cat == "35-44")], na.rm = TRUE), 1),
                         ", ",
                         round(median(oimain$p_duration[which(oimain$age.cat == "35-44")], na.rm = TRUE), 1)),

                  round(mean(oicas$p_duration[which(oicas$age.cat == "35-44")], na.rm = TRUE), 1),
                  paste0(round(sd(oicas$p_duration[which(oicas$age.cat == "35-44")], na.rm = TRUE), 1),
                         ", ",
                         round(median(oicas$p_duration[which(oicas$age.cat == "35-44")], na.rm = TRUE), 1)))

fortyfive54 <- cbind("45-54",
                 paste0(nrow(extant[which(extant$age.cat == "45-54"), ]),
                        " (",
                        round(100 * nrow(extant[which(extant$age.cat == "45-54"), ]) /
                                nrow(extant), 1), ")"),

                 round(mean(aimain$p_duration[which(aimain$age.cat == "45-54")], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$p_duration[which(aimain$age.cat == "45-54")], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$p_duration[which(aimain$age.cat == "45-54")], na.rm = TRUE), 1)),

                 round(mean(aicas$p_duration[which(aicas$age.cat == "45-54")], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$p_duration[which(aicas$age.cat == "45-54")], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$p_duration[which(aicas$age.cat == "45-54")], na.rm = TRUE), 1)),
                 round(mean(oimain$p_duration[which(oimain$age.cat == "45-54")], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$p_duration[which(oimain$age.cat == "45-54")], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$p_duration[which(oimain$age.cat == "45-54")], na.rm = TRUE), 1)),

                 round(mean(oicas$p_duration[which(oicas$age.cat == "45-54")], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$p_duration[which(oicas$age.cat == "45-54")], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$p_duration[which(oicas$age.cat == "45-54")], na.rm = TRUE), 1)))


fiftyfive65 <- cbind("55-65",
                 paste0(nrow(extant[which(extant$age.cat == "55-65"), ]),
                        " (",
                        round(100 * nrow(extant[which(extant$age.cat == "55-65"), ]) /
                                nrow(extant), 1), ")"),

                 round(mean(aimain$p_duration[which(aimain$age.cat == "55-65")], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$p_duration[which(aimain$age.cat == "55-65")], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$p_duration[which(aimain$age.cat == "55-65")], na.rm = TRUE), 1)),

                 round(mean(aicas$p_duration[which(aicas$age.cat == "55-65")], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$p_duration[which(aicas$age.cat == "55-65")], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$p_duration[which(aicas$age.cat == "55-65")], na.rm = TRUE), 1)),
                 round(mean(oimain$p_duration[which(oimain$age.cat == "55-65")], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$p_duration[which(oimain$age.cat == "55-65")], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$p_duration[which(oimain$age.cat == "55-65")], na.rm = TRUE), 1)),

                 round(mean(oicas$p_duration[which(oicas$age.cat == "55-65")], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$p_duration[which(oicas$age.cat == "55-65")], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$p_duration[which(oicas$age.cat == "55-65")], na.rm = TRUE), 1)))

# HIV Status
HIVPos <- cbind("HIV Pos",
                paste0(nrow(extant[which(extant$hiv == 1), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == 1), ]) /
                               nrow(extant), 1), ")"),

                round(mean(aimain$p_duration[which(aimain$hiv == 1)], na.rm = TRUE), 1),
                paste0(round(sd(aimain$p_duration[which(aimain$hiv == 1)], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$p_duration[which(aimain$hiv == 1)], na.rm = TRUE), 1)),
                round(mean(aicas$p_duration[which(aicas$hiv == 1)], na.rm = TRUE), 1),
                paste0(round(sd(aicas$p_duration[which(aicas$hiv == 1)], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$p_duration[which(aicas$hiv == 1)], na.rm = TRUE), 1)),
                round(mean(oimain$p_duration[oimain$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(oimain$p_duration[oimain$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$p_duration[oimain$hiv == 1], na.rm = TRUE), 1)),

                round(mean(oicas$p_duration[oicas$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(oicas$p_duration[oicas$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$p_duration[oicas$hiv == 1], na.rm = TRUE), 1)))

HIVNeg <- cbind("HIV Neg",
                paste0(nrow(extant[which(extant$hiv == 0), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == 0), ]) /
                               nrow(extant), 1), ")"),

                round(mean(aimain$p_duration[which(aimain$hiv == 0)], na.rm = TRUE), 1),
                paste0(round(sd(aimain$p_duration[which(aimain$hiv == 0)], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$p_duration[which(aimain$hiv == 0)], na.rm = TRUE), 1)),
                round(mean(aicas$p_duration[which(aicas$hiv == 0)], na.rm = TRUE), 1),
                paste0(round(sd(aicas$p_duration[which(aicas$hiv == 0)], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$p_duration[which(aicas$hiv == 0)], na.rm = TRUE), 1)),
                round(mean(oimain$p_duration[oimain$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(oimain$p_duration[oimain$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$p_duration[oimain$hiv == 0], na.rm = TRUE), 1)),

                round(mean(oicas$p_duration[oicas$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(oicas$p_duration[oicas$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$p_duration[oicas$hiv == 0], na.rm = TRUE), 1)))

# Output table
stable3 <- rbind(total, black, white, hispanic, other,
                fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                HIVPos, HIVNeg)
colnames(stable3) <- c("Category", "N (%)", "AI Main Mean", "AI Main SD, Med",
                      "AI Cas Mean", "AI Cas SD, Med", "OI Main Mean", "OI Main SD, Med",
                      "OI Cas Mean", "OI Cas SD, Med")
write.csv(stable3, file = "Output/stable3.csv")

# Create Shiny Datasets --------------
artnet4shiny <- artnet2
artnet4shiny <- left_join(artnet4shiny, df2, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df3, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df5, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df6, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df8, by = "AMIS_ID")
artnet4shiny <- left_join(artnet4shiny, df9, by = "AMIS_ID")
saveRDS(artnet4shiny, file = "Output/artnet4shiny.rda", compress = "xz")

artnetlong4shiny <- artnetLong
saveRDS(artnetlong4shiny, file = "Output/artnetlong4shiny.rda", compress = "xz")

