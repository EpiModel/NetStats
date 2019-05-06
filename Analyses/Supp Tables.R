## ART-Net Study 2018   ##
## Supplementary Tables ##
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
l$ONGOING <- as.numeric(l$ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

# Total
df <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(totdegree = sum(ongoing2))
df4 <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(maintotdegree = sum(ongoing2))
df7 <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(castotdegree = sum(ongoing2))

# Create merged dataframes
artnet2 <- left_join(artnet, df, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df4, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df7, by = "AMIS_ID")

# Rate of one-offs -----------
# Create OI partners variable
artnet2$oi.part <- rep(NA, nrow(artnet2))
artnet2$oi.part <- artnet2$cuml.pnum - artnet2$ai.part
artnet2$oi.part[artnet2$oi.part < 0] <- 0 # 1 person with -87

# Create count variables for AI or OI
d <- artnet2
l <- artnetLong
d <- l %>%
  filter(ROI == 1 | IOI == 1 | RAI == 1 | IAI == 1) %>%
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
  filter(RAI == 1 | IAI == 1) %>%
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
  filter(ROI == 1 | IOI == 1) %>%
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


# Supp Table 1 - AI and OI Degree --------------
# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

df2 <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(allaionlydegree = sum(ongoing2))
df3 <- l %>%
  filter(ROI == 1 | IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(alloionlydegree = sum(ongoing2))
df5 <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(mainaionlydegree = sum(ongoing2))
df6 <- l %>%
  filter(ROI == 1 | IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype == 1) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(mainoionlydegree = sum(ongoing2))

df8 <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(casaionlydegree = sum(ongoing2))
df9 <- l %>%
  filter(ROI == 1 | IOI == 1) %>%
  # filter(RAI == 1 | IAI == 1) %>% # filter activity type
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
                hivstatallaiv2[2, 1],
                paste0(hivstatallaiv2[2, 2],
                       " - ",
                       hivstatallaiv2[2, 3]),
                hivstatalloiv2[2, 1],
                paste0(hivstatalloiv2[2, 2],
                       " - ",
                       hivstatalloiv2[2, 3]),
                hivstatmainaiv2[2, 1],
                paste0(hivstatmainaiv2[2, 2],
                       " - ",
                       hivstatmainaiv2[2, 3]),
                hivstatmainoiv2[2, 1],
                paste0(hivstatmainoiv2[2, 2],
                       " - ",
                       hivstatmainoiv2[2, 3]),
                hivstatcasaiv2[2, 1],
                paste0(hivstatcasaiv2[2, 2],
                       " - ",
                       hivstatcasaiv2[2, 3]),
                hivstatcasoiv2[2, 1],
                paste0(hivstatcasoiv2[2, 2],
                       " - ",
                       hivstatcasoiv2[2, 3]))
HIVNeg <- cbind("HIV Neg",
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

HIVUnk <- cbind("HIV Unk",
                hivstatallaiv2[3, 1],
                paste0(hivstatallaiv2[3, 2],
                       " - ",
                       hivstatallaiv2[3, 3]),
                hivstatalloiv2[3, 1],
                paste0(hivstatalloiv2[3, 2],
                       " - ",
                       hivstatalloiv2[3, 3]),
                hivstatmainaiv2[3, 1],
                paste0(hivstatmainaiv2[3, 2],
                       " - ",
                       hivstatmainaiv2[3, 3]),
                hivstatmainoiv2[3, 1],
                paste0(hivstatmainoiv2[3, 2],
                       " - ",
                       hivstatmainoiv2[3, 3]),
                hivstatcasaiv2[3, 1],
                paste0(hivstatcasaiv2[3, 2],
                       " - ",
                       hivstatcasaiv2[3, 3]),
                hivstatcasoiv2[3, 1],
                paste0(hivstatcasoiv2[3, 2],
                       " - ",
                       hivstatcasoiv2[3, 3]))
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
                  HIVNeg, HIVPos, HIVUnk)
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
group_by(artnet2, race.cat) %>% summarize(mean(rate.oo.ai.part))

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
group_by(artnet2, age.cat) %>% summarize(mean(rate.oo.ai.part))

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
                    hivstatv2ooai[2, 1],
                    paste0(hivstatv2ooai[2, 2],
                           " - ",
                           hivstatv2ooai[2, 3]))
HIVNegooai <- cbind("HIV Neg",
                    hivstatv2ooai[1, 1],
                    paste0(hivstatv2ooai[1, 2],
                           " - ",
                           hivstatv2ooai[1, 3]))
HIVUnkooai <- cbind("HIV Neg",
                    hivstatv2ooai[3, 1],
                    paste0(hivstatv2ooai[3, 2],
                           " - ",
                           hivstatv2ooai[3, 3]))

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
group_by(artnet2, race.cat) %>% summarize(mean(rate.oo.oi.part))

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
group_by(artnet2, age.cat) %>% summarize(mean(rate.oo.oi.part))

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
group_by(artnet2, hiv) %>% summarize(mean(rate.oo.oi.part, na.rm = TRUE))

HIVPosoooi <- cbind("HIV Pos",
                    hivstatv2oooi[2, 1],
                    paste0(hivstatv2oooi[2, 2],
                           " - ",
                           hivstatv2oooi[2, 3]))
HIVNegoooi <- cbind("HIV Neg",
                    hivstatv2oooi[1, 1],
                    paste0(hivstatv2oooi[1, 2],
                           " - ",
                           hivstatv2oooi[1, 3]))
HIVUnkoooi <- cbind("HIV Unk",
                    hivstatv2oooi[3, 1],
                    paste0(hivstatv2oooi[3, 2],
                           " - ",
                           hivstatv2oooi[3, 3]))

# Output table
stable2 <- cbind(rbind(totalooai, blackooai, whiteooai, hispanicooai, otherooai,
                       fifteen24ooai, twentyfive34ooai, thirtyfive44ooai, fortyfive54ooai, fiftyfive65ooai,
                       Westooai, Pacificooai, Mountainooai, Midwestooai, WNCooai, ENCooai,
                       Southooai, WSCooai, ESCooai, SAooai, Northeastooai, MAooai, NEooai, LCMooai, LFMooai, Mediumooai,
                       Smallooai, Microooai, Noncoreooai,
                       HIVNegooai, HIVPosooai, HIVUnkooai),
                 rbind(totaloooi, blackoooi, whiteoooi, hispanicoooi, otheroooi,
                       fifteen24oooi, twentyfive34oooi, thirtyfive44oooi, fortyfive54oooi, fiftyfive65oooi,
                       Westoooi, Pacificoooi, Mountainoooi, Midwestoooi, WNCoooi, ENCoooi,
                       Southoooi, WSCoooi, ESCoooi, SAoooi, Northeastoooi, MAoooi, NEoooi, LCMoooi, LFMoooi, Mediumoooi,
                       Smalloooi, Microoooi, Noncoreoooi,
                       HIVNegoooi, HIVPosoooi, HIVUnkoooi))
colnames(stable2) <- c("Category", "AI  Mean", "AI CI", "Category", "OI Mean", "OI CI")
write.csv(stable2, file = "Output/stable2.csv")

# Supp Table 3 - Duration by AI and OI --------------
extant <- artnetLong[which(artnetLong$ONGOING == 1 & artnetLong$duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]
extant <- extant[which(extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1), ]

aimain <- extant[which((extant$RAI == 1 | extant$IAI == 1) & extant$ptype == 1), ]
aicas <- extant[which((extant$RAI == 1 | extant$IAI == 1) & extant$ptype == 2), ]
oimain <- extant[which((extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 1), ]
oicas <- extant[which((extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 2), ]

# Total number of ongoing partnerships
total <- cbind("Total", paste0(nrow(extant), " (", 100 * nrow(extant) / nrow(extant), ")"),

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
black <- cbind("black",
               paste0(nrow(extant[which(extant$race.cat == "black"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "black"), ]) /
                              nrow(extant), 1), ")"),

               round(mean(aimain$duration[which(aimain$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration[which(aimain$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration[which(aimain$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(aicas$duration[which(aicas$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration[which(aicas$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration[which(aicas$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(oimain$duration[which(oimain$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration[which(oimain$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration[which(oimain$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(oicas$duration[which(oicas$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration[which(oicas$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration[which(oicas$race.cat == "black")], na.rm = TRUE), 1)))

white <- cbind("white",
               paste0(nrow(extant[which(extant$race.cat == "white"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "white"), ]) /
                              nrow(extant), 1), ")"),

               round(mean(aimain$duration[which(aimain$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration[which(aimain$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration[which(aimain$race.cat == "white")], na.rm = TRUE), 1)),

               round(mean(aicas$duration[which(aicas$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration[which(aicas$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration[which(aicas$race.cat == "white")], na.rm = TRUE), 1)),

               round(mean(oimain$duration[which(oimain$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration[which(oimain$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration[which(oimain$race.cat == "white")], na.rm = TRUE), 1)),

               round(mean(oicas$duration[which(oicas$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration[which(oicas$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration[which(oicas$race.cat == "white")], na.rm = TRUE), 1)))

hispanic <- cbind("hispanic",
                  paste0(nrow(extant[which(extant$race.cat == "hispanic"), ]),
                         " (",
                         round(100 * nrow(extant[which(extant$race.cat == "hispanic"), ]) /
                                 nrow(extant), 1), ")"),

                  round(mean(aimain$duration[which(aimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(aimain$duration[which(aimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(aimain$duration[which(aimain$race.cat == "hispanic")], na.rm = TRUE), 1)),

                  round(mean(aicas$duration[which(aicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(aicas$duration[which(aicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(aicas$duration[which(aicas$race.cat == "hispanic")], na.rm = TRUE), 1)),

                  round(mean(oimain$duration[which(oimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(oimain$duration[which(oimain$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(oimain$duration[which(oimain$race.cat == "hispanic")], na.rm = TRUE), 1)),

                  round(mean(oicas$duration[which(oicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(oicas$duration[which(oicas$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(oicas$duration[which(oicas$race.cat == "hispanic")], na.rm = TRUE), 1)))

other <- cbind("other",
               paste0(nrow(extant[which(extant$race.cat == "other"), ]),
                      " (", round(100 * nrow(extant[which(extant$race.cat == "other"), ]) /
                                    nrow(extant), 1), ")"),

               round(mean(aimain$duration[which(aimain$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(aimain$duration[which(aimain$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aimain$duration[which(aimain$race.cat == "other")], na.rm = TRUE), 1)),

               round(mean(aicas$duration[which(aicas$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(aicas$duration[which(aicas$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(aicas$duration[which(aicas$race.cat == "other")], na.rm = TRUE), 1)),

               round(mean(oimain$duration[which(oimain$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(oimain$duration[which(oimain$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oimain$duration[which(oimain$race.cat == "other")], na.rm = TRUE), 1)),

               round(mean(oicas$duration[which(oicas$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(oicas$duration[which(oicas$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(oicas$duration[which(oicas$race.cat == "other")], na.rm = TRUE), 1)))


# Age
fifteen24 <- cbind("15-24",
                   paste0(nrow(extant[which(extant$age.cat == "15-24"), ]),
                          " (",
                          round(100 * nrow(extant[which(extant$age.cat == "15-24"), ]) /
                                  nrow(extant), 1), ")"),

                   round(mean(aimain$duration[which(aimain$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(aimain$duration[which(aimain$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(aimain$duration[which(aimain$age.cat == "15-24")], na.rm = TRUE), 1)),

                   round(mean(aicas$duration[which(aicas$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(aicas$duration[which(aicas$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(aicas$duration[which(aicas$age.cat == "15-24")], na.rm = TRUE), 1)),
                   round(mean(oimain$duration[which(oimain$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(oimain$duration[which(oimain$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(oimain$duration[which(oimain$age.cat == "15-24")], na.rm = TRUE), 1)),

                   round(mean(oicas$duration[which(oicas$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(oicas$duration[which(oicas$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(oicas$duration[which(oicas$age.cat == "15-24")], na.rm = TRUE), 1)))

twentyfive34 <- cbind("25-34",
                      paste0(nrow(extant[which(extant$age.cat == "25-34"), ]),
                             " (",
                             round(100 * nrow(extant[which(extant$age.cat == "25-34"), ]) /
                                     nrow(extant), 1), ")"),

                      round(mean(aimain$duration[which(aimain$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(aimain$duration[which(aimain$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(aimain$duration[which(aimain$age.cat == "25-34")], na.rm = TRUE), 1)),

                      round(mean(aicas$duration[which(aicas$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(aicas$duration[which(aicas$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(aicas$duration[which(aicas$age.cat == "25-34")], na.rm = TRUE), 1)),
                      round(mean(oimain$duration[which(oimain$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(oimain$duration[which(oimain$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(oimain$duration[which(oimain$age.cat == "25-34")], na.rm = TRUE), 1)),

                      round(mean(oicas$duration[which(oicas$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(oicas$duration[which(oicas$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(oicas$duration[which(oicas$age.cat == "25-34")], na.rm = TRUE), 1)))

thirtyfive44 <- cbind("35-44",
                      paste0(nrow(extant[which(extant$age.cat == "35-44"), ]),
                             " (",
                             round(100 * nrow(extant[which(extant$age.cat == "35-44"), ]) /
                                     nrow(extant), 1), ")"),

                      round(mean(aimain$duration[which(aimain$age.cat == "35-44")], na.rm = TRUE), 1),
                      paste0(round(sd(aimain$duration[which(aimain$age.cat == "35-44")], na.rm = TRUE), 1),
                             ", ",
                             round(median(aimain$duration[which(aimain$age.cat == "35-44")], na.rm = TRUE), 1)),

                      round(mean(aicas$duration[which(aicas$age.cat == "35-44")], na.rm = TRUE), 1),
                      paste0(round(sd(aicas$duration[which(aicas$age.cat == "35-44")], na.rm = TRUE), 1),
                             ", ",
                             round(median(aicas$duration[which(aicas$age.cat == "35-44")], na.rm = TRUE), 1)),
                      round(mean(oimain$duration[which(oimain$age.cat == "35-44")], na.rm = TRUE), 1),
                      paste0(round(sd(oimain$duration[which(oimain$age.cat == "35-44")], na.rm = TRUE), 1),
                             ", ",
                             round(median(oimain$duration[which(oimain$age.cat == "35-44")], na.rm = TRUE), 1)),

                      round(mean(oicas$duration[which(oicas$age.cat == "35-44")], na.rm = TRUE), 1),
                      paste0(round(sd(oicas$duration[which(oicas$age.cat == "35-44")], na.rm = TRUE), 1),
                             ", ",
                             round(median(oicas$duration[which(oicas$age.cat == "35-44")], na.rm = TRUE), 1)))

fortyfive54 <- cbind("45-54",
                     paste0(nrow(extant[which(extant$age.cat == "45-54"), ]),
                            " (",
                            round(100 * nrow(extant[which(extant$age.cat == "45-54"), ]) /
                                    nrow(extant), 1), ")"),

                     round(mean(aimain$duration[which(aimain$age.cat == "45-54")], na.rm = TRUE), 1),
                     paste0(round(sd(aimain$duration[which(aimain$age.cat == "45-54")], na.rm = TRUE), 1),
                            ", ",
                            round(median(aimain$duration[which(aimain$age.cat == "45-54")], na.rm = TRUE), 1)),

                     round(mean(aicas$duration[which(aicas$age.cat == "45-54")], na.rm = TRUE), 1),
                     paste0(round(sd(aicas$duration[which(aicas$age.cat == "45-54")], na.rm = TRUE), 1),
                            ", ",
                            round(median(aicas$duration[which(aicas$age.cat == "45-54")], na.rm = TRUE), 1)),
                     round(mean(oimain$duration[which(oimain$age.cat == "45-54")], na.rm = TRUE), 1),
                     paste0(round(sd(oimain$duration[which(oimain$age.cat == "45-54")], na.rm = TRUE), 1),
                            ", ",
                            round(median(oimain$duration[which(oimain$age.cat == "45-54")], na.rm = TRUE), 1)),

                     round(mean(oicas$duration[which(oicas$age.cat == "45-54")], na.rm = TRUE), 1),
                     paste0(round(sd(oicas$duration[which(oicas$age.cat == "45-54")], na.rm = TRUE), 1),
                            ", ",
                            round(median(oicas$duration[which(oicas$age.cat == "45-54")], na.rm = TRUE), 1)))


fiftyfive65 <- cbind("55-65",
                     paste0(nrow(extant[which(extant$age.cat == "55-65"), ]),
                            " (",
                            round(100 * nrow(extant[which(extant$age.cat == "55-65"), ]) /
                                    nrow(extant), 1), ")"),

                     round(mean(aimain$duration[which(aimain$age.cat == "55-65")], na.rm = TRUE), 1),
                     paste0(round(sd(aimain$duration[which(aimain$age.cat == "55-65")], na.rm = TRUE), 1),
                            ", ",
                            round(median(aimain$duration[which(aimain$age.cat == "55-65")], na.rm = TRUE), 1)),

                     round(mean(aicas$duration[which(aicas$age.cat == "55-65")], na.rm = TRUE), 1),
                     paste0(round(sd(aicas$duration[which(aicas$age.cat == "55-65")], na.rm = TRUE), 1),
                            ", ",
                            round(median(aicas$duration[which(aicas$age.cat == "55-65")], na.rm = TRUE), 1)),
                     round(mean(oimain$duration[which(oimain$age.cat == "55-65")], na.rm = TRUE), 1),
                     paste0(round(sd(oimain$duration[which(oimain$age.cat == "55-65")], na.rm = TRUE), 1),
                            ", ",
                            round(median(oimain$duration[which(oimain$age.cat == "55-65")], na.rm = TRUE), 1)),

                     round(mean(oicas$duration[which(oicas$age.cat == "55-65")], na.rm = TRUE), 1),
                     paste0(round(sd(oicas$duration[which(oicas$age.cat == "55-65")], na.rm = TRUE), 1),
                            ", ",
                            round(median(oicas$duration[which(oicas$age.cat == "55-65")], na.rm = TRUE), 1)))

# HIV Status
HIVPos <- cbind("HIV Pos",
                paste0(nrow(extant[which(extant$hiv == "Positive"), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == "Positive"), ]) /
                               nrow(extant), 1), ")"),

                round(mean(aimain$duration[which(aimain$hiv == "Positive")], na.rm = TRUE), 1),
                paste0(round(sd(aimain$duration[which(aimain$hiv == "Positive")], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$duration[which(aimain$hiv == "Positive")], na.rm = TRUE), 1)),
                round(mean(aicas$duration[which(aicas$hiv == "Positive")], na.rm = TRUE), 1),
                paste0(round(sd(aicas$duration[which(aicas$hiv == "Positive")], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$duration[which(aicas$hiv == "Positive")], na.rm = TRUE), 1)),
                round(mean(oimain$duration[oimain$hiv == "Positive"], na.rm = TRUE), 1),
                paste0(round(sd(oimain$duration[oimain$hiv == "Positive"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$duration[oimain$hiv == "Positive"], na.rm = TRUE), 1)),

                round(mean(oicas$duration[oicas$hiv == "Positive"], na.rm = TRUE), 1),
                paste0(round(sd(oicas$duration[oicas$hiv == "Positive"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$duration[oicas$hiv == "Positive"], na.rm = TRUE), 1)))

HIVNeg <- cbind("HIV Neg",
                paste0(nrow(extant[which(extant$hiv == "Negative"), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == "Negative"), ]) /
                               nrow(extant), 1), ")"),

                round(mean(aimain$duration[which(aimain$hiv == "Negative")], na.rm = TRUE), 1),
                paste0(round(sd(aimain$duration[which(aimain$hiv == "Negative")], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$duration[which(aimain$hiv == "Negative")], na.rm = TRUE), 1)),
                round(mean(aicas$duration[which(aicas$hiv == "Negative")], na.rm = TRUE), 1),
                paste0(round(sd(aicas$duration[which(aicas$hiv == "Negative")], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$duration[which(aicas$hiv == "Negative")], na.rm = TRUE), 1)),
                round(mean(oimain$duration[oimain$hiv == "Negative"], na.rm = TRUE), 1),
                paste0(round(sd(oimain$duration[oimain$hiv == "Negative"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$duration[oimain$hiv == "Negative"], na.rm = TRUE), 1)),

                round(mean(oicas$duration[oicas$hiv == "Negative"], na.rm = TRUE), 1),
                paste0(round(sd(oicas$duration[oicas$hiv == "Negative"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$duration[oicas$hiv == "Negative"], na.rm = TRUE), 1)))

HIVUnk <- cbind("HIV Unk",
                paste0(nrow(extant[which(extant$hiv == "Unknown"), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == "Unknown"), ]) /
                               nrow(extant), 1), ")"),

                round(mean(aimain$duration[which(aimain$hiv == "Unknown")], na.rm = TRUE), 1),
                paste0(round(sd(aimain$duration[which(aimain$hiv == "Unknown")], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$duration[which(aimain$hiv == "Unknown")], na.rm = TRUE), 1)),
                round(mean(aicas$duration[which(aicas$hiv == "Unknown")], na.rm = TRUE), 1),
                paste0(round(sd(aicas$duration[which(aicas$hiv == "Unknown")], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$duration[which(aicas$hiv == "Unknown")], na.rm = TRUE), 1)),
                round(mean(oimain$duration[oimain$hiv == "Unknown"], na.rm = TRUE), 1),
                paste0(round(sd(oimain$duration[oimain$hiv == "Unknown"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$duration[oimain$hiv == "Unknown"], na.rm = TRUE), 1)),

                round(mean(oicas$duration[oicas$hiv == "Unknown"], na.rm = TRUE), 1),
                paste0(round(sd(oicas$duration[oicas$hiv == "Unknown"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$duration[oicas$hiv == "Unknown"], na.rm = TRUE), 1)))

# Output table
stable3 <- rbind(total, black, white, hispanic, other,
                 fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                 HIVNeg, HIVPos, HIVUnk)
colnames(stable3) <- c("Category", "N (%)", "AI Main Mean", "AI Main SD, Med",
                       "AI Cas Mean", "AI Cas SD, Med", "OI Main Mean", "OI Main SD, Med",
                       "OI Cas Mean", "OI Cas SD, Med")
write.csv(stable3, file = "Output/stable3.csv")


# Supp Table 4 - Duration of HIV controlling for age --------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(gridExtra)

## Data Cleaning/Management Script
source("Analyses/Data_Cleaning.R", echo = FALSE)

extant <- artnetLong[which(artnetLong$ONGOING == 1 & artnetLong$duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]
extant2 <- extant[which(extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1), ]
bothmain <- extant2[which(extant2$ptype == 1), ]
bothcas <- extant2[which(extant2$ptype == 2), ]

summary(extant2$duration)
summary(bothmain$duration)
summary(bothcas$duration)

# Linear model
# duration ~ ego hiv
# then duration ~ ego hiv + ego age
all_hiv_model <- lm(duration ~ hiv, data = extant2)
all_hiv <- round(cbind((coef(all_hiv_model)), (confint(all_hiv_model))), 3)
all_agehiv_model <- lm(duration ~ age + hiv, data = extant2)
all_agehiv <- round(cbind((coef(all_agehiv_model)), (confint(all_agehiv_model))), 3)

main_hiv_model <- lm(duration ~ hiv, data = bothmain)
main_hiv <- round(cbind((coef(main_hiv_model)), (confint(main_hiv_model))), 3)
main_agehiv_model <- lm(duration ~ age + hiv, data = bothmain)
main_agehiv <- round(cbind((coef(main_agehiv_model)), (confint(main_agehiv_model))), 3)

cas_hiv_model <- lm(duration ~ hiv, data = bothcas)
cas_hiv <- round(cbind((coef(cas_hiv_model)), (confint(cas_hiv_model))), 3)
cas_agehiv_model <- lm(duration ~ age + hiv, data = bothcas)
cas_agehiv <- round(cbind((coef(cas_agehiv_model)), (confint(cas_agehiv_model))), 3)

# Output table of coefficients
allcoeffs <- as.data.frame(cbind(rbind("Intercept",
                                       "HIV Positive",
                                       "HIV-Unknown",
                                       "Age"),
                                 rbind(all_hiv[1, 1],
                                       all_hiv[2, 1],
                                       all_hiv[3, 1],
                                       NA),
                                 rbind(all_agehiv[1, 1],
                                       all_agehiv[2, 1],
                                       all_agehiv[3, 1],
                                       all_agehiv[4, 1]),
                                 rbind(main_hiv[1, 1],
                                       main_hiv[2, 1],
                                       main_hiv[3, 1],
                                       NA),
                                 rbind(main_agehiv[1, 1],
                                       main_agehiv[2, 1],
                                       main_agehiv[3, 1],
                                       main_agehiv[4, 1]),
                                 rbind(cas_hiv[1, 1],
                                       cas_hiv[2, 1],
                                       cas_hiv[3, 1],
                                       NA),
                                 rbind(cas_agehiv[1, 1],
                                       cas_agehiv[2, 1],
                                       cas_agehiv[3, 1],
                                       cas_agehiv[4, 1])))
colnames(allcoeffs) <- c("Variable", "HIV Only (All)", "Age + HIV (All)",
                         "HIV Only (Main)", "Age + HIV (Main)",
                         "HIV Only (Cas)", "Age + HIV (Cas)")
View(allcoeffs)

# Prediction
new.df <- data.frame(age = seq(15, 65, by = 5))
new.df2 <- data.frame(age = c(15, 15, 15, 20, 20, 20, 25, 25, 25, 30, 30, 30,
                             35, 35, 35, 40, 40, 40, 45, 45, 45, 50, 50, 50,
                             55, 55, 55, 60, 60, 60, 65, 65, 65),
                     hiv = as.character(rep(c("Negative", "Positive", "Unknown"), 11)))
new.df2$hiv <- as.character(new.df2$hiv)
df <- cbind(new.df, predict(all_age_model, new.df, interval = "confidence"))
df_hiv <- cbind(new.df2, predict(all_agehiv_model, new.df2, interval = "confidence"))
colnames(df) <- c("Age", "Duration", "Lower", "Upper")
colnames(df_hiv) <- c("Age", "HIV", "Duration", "Lower", "Upper")

p1 <- ggplot(df, aes(x = Age, y = Duration)) +
  geom_line() +
  geom_point() +
  ggtitle("Duration Adjusted for Age") +
  theme_bw()
p2 <- ggplot(df_hiv, aes(x = Age, y = Duration, group = HIV, colour = HIV)) +
  geom_line() +
  geom_point() +
  ggtitle("Duration Adjusted for Age and HIV Status") +
  theme_bw()
grid.arrange(p1, p2)
#
# # Total number of ongoing partnerships
# total <- cbind("Total", paste0(nrow(extant), " (", 100 * nrow(extant) / nrow(extant), ")"),
#
#                round(mean(aimain$duration, na.rm = TRUE), 1),
#                paste0(round(sd(aimain$duration, na.rm = TRUE), 1),
#                       ", ",
#                       round(median(aimain$duration, na.rm = TRUE), 1)),
#
#                round(mean(aicas$duration, na.rm = TRUE), 1),
#                paste0(round(sd(aicas$duration, na.rm = TRUE), 1),
#                       ", ",
#                       round(median(aicas$duration, na.rm = TRUE), 1)),
#
#                round(mean(oimain$duration, na.rm = TRUE), 1),
#                paste0(round(sd(oimain$duration, na.rm = TRUE), 1),
#                       ", ",
#                       round(median(oimain$duration, na.rm = TRUE), 1)),
#
#                round(mean(oicas$duration, na.rm = TRUE), 1),
#                paste0(round(sd(oicas$duration, na.rm = TRUE), 1),
#                       ", ",
#                       round(median(oicas$duration, na.rm = TRUE), 1)))
#
# # HIV Status
# HIVPos <- cbind("HIV Pos",
#                 paste0(nrow(extant[which(extant$hiv == "Positive"), ]),
#                        " (",
#                        round(100 * nrow(extant[which(extant$hiv == "Positive"), ]) /
#                                nrow(extant), 1), ")"),
#
#                 round(mean(aimain$duration[which(aimain$hiv == "Positive")], na.rm = TRUE), 1),
#                 paste0(round(sd(aimain$duration[which(aimain$hiv == "Positive")], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(aimain$duration[which(aimain$hiv == "Positive")], na.rm = TRUE), 1)),
#                 round(mean(aicas$duration[which(aicas$hiv == "Positive")], na.rm = TRUE), 1),
#                 paste0(round(sd(aicas$duration[which(aicas$hiv == "Positive")], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(aicas$duration[which(aicas$hiv == "Positive")], na.rm = TRUE), 1)),
#                 round(mean(oimain$duration[oimain$hiv == "Positive"], na.rm = TRUE), 1),
#                 paste0(round(sd(oimain$duration[oimain$hiv == "Positive"], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(oimain$duration[oimain$hiv == "Positive"], na.rm = TRUE), 1)),
#
#                 round(mean(oicas$duration[oicas$hiv == "Positive"], na.rm = TRUE), 1),
#                 paste0(round(sd(oicas$duration[oicas$hiv == "Positive"], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(oicas$duration[oicas$hiv == "Positive"], na.rm = TRUE), 1)))
#
# HIVNeg <- cbind("HIV Neg",
#                 paste0(nrow(extant[which(extant$hiv == "Negative"), ]),
#                        " (",
#                        round(100 * nrow(extant[which(extant$hiv == "Negative"), ]) /
#                                nrow(extant), 1), ")"),
#
#                 round(mean(aimain$duration[which(aimain$hiv == "Negative")], na.rm = TRUE), 1),
#                 paste0(round(sd(aimain$duration[which(aimain$hiv == "Negative")], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(aimain$duration[which(aimain$hiv == "Negative")], na.rm = TRUE), 1)),
#                 round(mean(aicas$duration[which(aicas$hiv == "Negative")], na.rm = TRUE), 1),
#                 paste0(round(sd(aicas$duration[which(aicas$hiv == "Negative")], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(aicas$duration[which(aicas$hiv == "Negative")], na.rm = TRUE), 1)),
#                 round(mean(oimain$duration[oimain$hiv == "Negative"], na.rm = TRUE), 1),
#                 paste0(round(sd(oimain$duration[oimain$hiv == "Negative"], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(oimain$duration[oimain$hiv == "Negative"], na.rm = TRUE), 1)),
#
#                 round(mean(oicas$duration[oicas$hiv == "Negative"], na.rm = TRUE), 1),
#                 paste0(round(sd(oicas$duration[oicas$hiv == "Negative"], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(oicas$duration[oicas$hiv == "Negative"], na.rm = TRUE), 1)))
#
# HIVUnk <- cbind("HIV Unk",
#                 paste0(nrow(extant[which(extant$hiv == "Unknown"), ]),
#                        " (",
#                        round(100 * nrow(extant[which(extant$hiv == "Unknown"), ]) /
#                                nrow(extant), 1), ")"),
#
#                 round(mean(aimain$duration[which(aimain$hiv == "Unknown")], na.rm = TRUE), 1),
#                 paste0(round(sd(aimain$duration[which(aimain$hiv == "Unknown")], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(aimain$duration[which(aimain$hiv == "Unknown")], na.rm = TRUE), 1)),
#                 round(mean(aicas$duration[which(aicas$hiv == "Unknown")], na.rm = TRUE), 1),
#                 paste0(round(sd(aicas$duration[which(aicas$hiv == "Unknown")], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(aicas$duration[which(aicas$hiv == "Unknown")], na.rm = TRUE), 1)),
#                 round(mean(oimain$duration[oimain$hiv == "Unknown"], na.rm = TRUE), 1),
#                 paste0(round(sd(oimain$duration[oimain$hiv == "Unknown"], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(oimain$duration[oimain$hiv == "Unknown"], na.rm = TRUE), 1)),
#
#                 round(mean(oicas$duration[oicas$hiv == "Unknown"], na.rm = TRUE), 1),
#                 paste0(round(sd(oicas$duration[oicas$hiv == "Unknown"], na.rm = TRUE), 1),
#                        ", ",
#                        round(median(oicas$duration[oicas$hiv == "Unknown"], na.rm = TRUE), 1)))
#
# # Output table
# stable4 <- rbind(total, black, white, hispanic, other,
#                  fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
#                  HIVNeg, HIVPos, HIVUnk)
# colnames(stable4) <- c("Category", "N (%)", "AI Main Mean", "AI Main SD, Med",
#                        "AI Cas Mean", "AI Cas SD, Med", "OI Main Mean", "OI Main SD, Med",
#                        "OI Cas Mean", "OI Cas SD, Med")
# write.csv(stable4, file = "Output/stable4.csv")
