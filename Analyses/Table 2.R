## ART-Net Study 2018   ##
## Table 2 Mean Degree  ##
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

# Table 2 - Mean degree --------------

## General mean degree calculation
# Issue: people with no reported AI or OI activity? Look at wide to long
nrow(artnetLong[which(artnetLong$RAI == 0 & artnetLong$IAI == 0 &
                        artnetLong$IOI == 0 & artnetLong$ROI == 0), ])
# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

# Total
df <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(totdegree = sum(ongoing2))
df4 <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>% # filter activity type
  filter(ptype == 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(maintotdegree = sum(ongoing2))
df7 <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
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
                hivstatv2[2, 1],
                paste0(hivstatv2[2, 2],
                       " - ",
                       hivstatv2[2, 3]),
                hivstatmainv2[2, 1],
                paste0(hivstatmainv2[2, 2],
                       " - ",
                       hivstatmainv2[2, 3]),
                hivstatcasv2[2, 1],
                paste0(hivstatcasv2[2, 2],
                       " - ",
                       hivstatcasv2[2, 3]))
HIVNeg <- cbind("HIV Neg",
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
HIVUnk <- cbind("HIV Unk",
                hivstatv2[3, 1],
                paste0(hivstatv2[3, 2],
                       " - ",
                       hivstatv2[3, 3]),
                hivstatmainv2[3, 1],
                paste0(hivstatmainv2[3, 2],
                       " - ",
                       hivstatmainv2[3, 3]),
                hivstatcasv2[3, 1],
                paste0(hivstatcasv2[3, 2],
                       " - ",
                       hivstatcasv2[3, 3]))

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
group_by(d, hiv) %>% summarize(mean(rate.oo.aioi.part, na.rm = TRUE))

HIVPosoo <- cbind("HIV Pos",
                  hivstatv2oo[2, 1],
                  paste0(hivstatv2oo[2, 2],
                         " - ",
                         hivstatv2oo[2, 3]))
HIVNegoo <- cbind("HIV Neg",
                  hivstatv2oo[1, 1],
                  paste0(hivstatv2oo[1, 2],
                         " - ",
                         hivstatv2oo[1, 3]))
HIVUnkoo <- cbind("HIV Unk",
                  hivstatv2oo[3, 1],
                  paste0(hivstatv2oo[3, 2],
                         " - ",
                         hivstatv2oo[3, 3]))
# Output table
table2a <- cbind(rbind(total, black, white, hispanic, other,
                      fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                      West, Pacific, Mountain, Midwest, WNC, ENC,
                      South, WSC, ESC, SA, Northeast, MA, NE, LCM, LFM, Medium,
                      Small, Micro, Noncore,
                      HIVNeg, HIVPos, HIVUnk),
                rbind(totaloo, blackoo, whiteoo, hispanicoo, otheroo,
                      fifteen24oo, twentyfive34oo, thirtyfive44oo, fortyfive54oo, fiftyfive65oo,
                      Westoo, Pacificoo, Mountainoo, Midwestoo, WNCoo, ENCoo,
                      Southoo, WSCoo, ESCoo, SAoo, Northeastoo, MAoo, NEoo, LCMoo, LFMoo, Mediumoo,
                      Smalloo, Microoo, Noncoreoo,
                      HIVNegoo, HIVPosoo, HIVUnkoo))
colnames(table2a) <- c("Category", "Ong Either Mean", "Ong Either CI",
                      "Ong Main Mean", "Ongoing Main CI",
                      "Ong Cas Mean", "Ong Cas CI", "Category", "One-Off", "One-off CI")
write.csv(table2a, file = "Output/table2a.csv")

table2b <- rbind(concurr)
colnames(table2b) <- c("Category", "Total Mean", "Main Mean", "Cas Mean")
write.csv(table2b, file = "Output/table2b.csv")
