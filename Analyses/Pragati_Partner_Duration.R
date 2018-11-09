## ART-Net Study 2018   ##
## Data Analysis Script ##
## 2018-04-17           ##

# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)

# Read in cleaning and datasets ---------

## Data Cleaning/Management Script
source("Kevin_Descriptive.R", echo = TRUE)


extant <- artnetLong[which(artnetLong$ONGOING == 1 & artnetLong$duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]


# Total
ongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
                                         (df$RAI == 1 | df$IAI == 1 |
                                            df$ROI == 1 | df$IOI == 1))])
aiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
                                           (df$RAI == 1 | df$IAI == 1))])
oiongpartners <- length(df$ONGOING[which(df$ONGOING == 1 &
                                           (df$ROI == 1 | df$IOI == 1))])

bothmain <- extant[which((extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 1), ]
bothcas <- extant[which((extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 2), ]
aimain <- extant[which((extant$RAI == 1 | extant$IAI == 1) & extant$ptype == 1), ]
aicas <- extant[which((extant$RAI == 1 | extant$IAI == 1) & extant$ptype == 2), ]
oimain <- extant[which((extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 1), ]
oicas <- extant[which((extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 2), ]


tot <- cbind("total",  paste0(nrow(extant), " (", round(100 * nrow(extant) / nrow(extant), 1), ")"),

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

#black-black
black_black_bothmain <- bothmain[which(bothmain$race.cat == "black" & bothmain$partracecat == "black"),]
black_black_bothcas <- bothcas[which(bothcas$race.cat == "black" & bothcas$partracecat == "black"),]
black_black_aimain <- aimain[which(aimain$race.cat == "black" & aimain$partracecat == "black"),]
black_black_aicas <- aicas[which(aicas$race.cat == "black" & aicas$partracecat == "black"),]
black_black_oimain <- oimain[which(oimain$race.cat == "black" & oimain$partracecat == "black"),]
black_black_oicas <- oicas[which(oicas$race.cat == "black" & oicas$partracecat == "black"),]

black_black <- cbind("black-black",  paste0(nrow(extant[which(extant$race.cat == "black" & extant$partracecat == "black"),]), " (",
                                            round(100 * nrow(extant[which(extant$race.cat == "black" & extant$partracecat == "black"),]) /
                                                    nrow(extant), 1), ")"),

               round(mean(black_black_bothmain$duration, na.rm = TRUE), 1),
               paste0(round(sd(black_black_bothmain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(black_black_bothmain$duration, na.rm = TRUE), 1)),

               round(mean(black_black_bothcas$duration, na.rm = TRUE), 1),
               paste0(round(sd(black_black_bothcas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(black_black_bothcas$duration, na.rm = TRUE), 1)),

               round(mean(black_black_aimain$duration, na.rm = TRUE), 1),
               paste0(round(sd(black_black_aimain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(black_black_aimain$duration, na.rm = TRUE), 1)),

               round(mean(black_black_aicas$duration, na.rm = TRUE), 1),
               paste0(round(sd(black_black_aicas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(black_black_aicas$duration, na.rm = TRUE), 1)),

               round(mean(black_black_oimain$duration, na.rm = TRUE), 1),
               paste0(round(sd(black_black_oimain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(black_black_oimain$duration, na.rm = TRUE), 1)),

               round(mean(black_black_oicas$duration, na.rm = TRUE), 1),
               paste0(round(sd(black_black_oicas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(black_black_oicas$duration, na.rm = TRUE), 1)))


#black-hisp
black_hisp_bothmain <- bothmain[which((bothmain$race.cat == "black" & bothmain$partracecat == "hispanic") |
                                        (bothmain$race.cat == "hispanic" & bothmain$partracecat == "black")),]
black_hisp_bothcas <- bothcas[which((bothcas$race.cat == "black" & bothcas$partracecat == "hispanic") |
                                      (bothcas$race.cat == "hispanic" & bothcas$partracecat == "black")),]
black_hisp_aimain <- aimain[which((aimain$race.cat == "black" & aimain$partracecat == "hispanic") |
                                    (aimain$race.cat == "hispanic" & aimain$partracecat == "black")),]
black_hisp_aicas <- aicas[which((aicas$race.cat == "black" & aicas$partracecat == "hispanic") |
                                  (aicas$race.cat == "hispanic" & aicas$partracecat == "black")),]
black_hisp_oimain <- oimain[which((oimain$race.cat == "black" & oimain$partracecat == "hispanic") |
                                    (oimain$race.cat == "hispanic" & oimain$partracecat == "black")),]
black_hisp_oicas <- oicas[which((oicas$race.cat == "black" & oicas$partracecat == "hispanic") |
                                  (oicas$race.cat == "hispanic" & oicas$partracecat == "black")),]

black_hisp <- cbind("black-hispanic", paste0(nrow(extant[which((extant$race.cat == "black" & extant$partracecat == "hispanic") |
                                                            (extant$race.cat == "hispanic" & extant$partracecat == "black")),]), " (",
                                           round(100 * nrow(extant[which((extant$race.cat == "black" & extant$partracecat == "hispanic") |
                                                                      (extant$race.cat == "hispanic" & extant$partracecat == "black")),]) /
                                                   nrow(extant), 1), ")"),

                     round(mean(black_hisp_bothmain$duration, na.rm = TRUE), 1),
                     paste0(round(sd(black_hisp_bothmain$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(black_hisp_bothmain$duration, na.rm = TRUE), 1)),

                     round(mean(black_hisp_bothcas$duration, na.rm = TRUE), 1),
                     paste0(round(sd(black_hisp_bothcas$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(black_hisp_bothcas$duration, na.rm = TRUE), 1)),

                     round(mean(black_hisp_aimain$duration, na.rm = TRUE), 1),
                     paste0(round(sd(black_hisp_aimain$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(black_hisp_aimain$duration, na.rm = TRUE), 1)),

                     round(mean(black_hisp_aicas$duration, na.rm = TRUE), 1),
                     paste0(round(sd(black_hisp_aicas$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(black_hisp_aicas$duration, na.rm = TRUE), 1)),

                     round(mean(black_hisp_oimain$duration, na.rm = TRUE), 1),
                     paste0(round(sd(black_hisp_oimain$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(black_hisp_oimain$duration, na.rm = TRUE), 1)),

                     round(mean(black_hisp_oicas$duration, na.rm = TRUE), 1),
                     paste0(round(sd(black_hisp_oicas$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(black_hisp_oicas$duration, na.rm = TRUE), 1)))

black_other <- cbind("black-other", paste0(nrow(extant[which((extant$race.cat == "black" & extant$partracecat == "other") |
                                                              (extant$race.cat == "other" & extant$partracecat == "black")),]), " (",
                                          round(100 * nrow(extant[which((extant$race.cat == "black" & extant$partracecat == "other") |
                                                                          (extant$race.cat == "other" & extant$partracecat == "black")),]) /
                                                  nrow(extant), 1), ")"),

                    round(mean(bothmain[which((bothmain$race.cat == "black" & bothmain$partracecat == "other") |
                                              (bothmain$race.cat == "other" & bothmain$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                    paste0(round(sd(bothmain[which((bothmain$race.cat == "black" & bothmain$partracecat == "other") |
                                                   (bothmain$race.cat == "other" & bothmain$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(bothmain[which((bothmain$race.cat == "black" & bothmain$partracecat == "other") |
                                                 (bothmain$race.cat == "other" & bothmain$partracecat == "black")),]$duration, na.rm = TRUE), 1)),

                    round(mean(bothcas[which((bothcas$race.cat == "black" & bothcas$partracecat == "other") |
                                                (bothcas$race.cat == "other" & bothcas$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                    paste0(round(sd(bothcas[which((bothcas$race.cat == "black" & bothcas$partracecat == "other") |
                                                    (bothcas$race.cat == "other" & bothcas$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(bothcas[which((bothcas$race.cat == "black" & bothcas$partracecat == "other") |
                                                        (bothcas$race.cat == "other" & bothcas$partracecat == "black")),]$duration, na.rm = TRUE), 1)),

                    round(mean(aimain[which((aimain$race.cat == "black" & aimain$partracecat == "other") |
                                               (aimain$race.cat == "other" & aimain$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                    paste0(round(sd(aimain[which((aimain$race.cat == "black" & aimain$partracecat == "other") |
                                                   (aimain$race.cat == "other" & aimain$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(aimain[which((aimain$race.cat == "black" & aimain$partracecat == "other") |
                                                       (aimain$race.cat == "other" & aimain$partracecat == "black")),]$duration, na.rm = TRUE), 1)),

                    round(mean(aicas[which((aicas$race.cat == "black" & aicas$partracecat == "other") |
                                              (aicas$race.cat == "other" & aicas$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                    paste0(round(sd(aicas[which((aicas$race.cat == "black" & aicas$partracecat == "other") |
                                                  (aicas$race.cat == "other" & aicas$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(aicas[which((aicas$race.cat == "black" & aicas$partracecat == "other") |
                                                      (aicas$race.cat == "other" & aicas$partracecat == "black")),]$duration, na.rm = TRUE), 1)),

                    round(mean(oimain[which((oimain$race.cat == "black" & oimain$partracecat == "other") |
                                             (oimain$race.cat == "other" & oimain$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                    paste0(round(sd(oimain[which((oimain$race.cat == "black" & oimain$partracecat == "other") |
                                                   (oimain$race.cat == "other" & oimain$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(oimain[which((oimain$race.cat == "black" & oimain$partracecat == "other") |
                                                       (oimain$race.cat == "other" & oimain$partracecat == "black")),]$duration, na.rm = TRUE), 1)),

                    round(mean(oicas[which((oicas$race.cat == "black" & oicas$partracecat == "other") |
                                              (oicas$race.cat == "other" & oicas$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                    paste0(round(sd(oicas[which((oicas$race.cat == "black" & oicas$partracecat == "other") |
                                                  (oicas$race.cat == "other" & oicas$partracecat == "black")),]$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(oicas[which((oicas$race.cat == "black" & oicas$partracecat == "other") |
                                                      (oicas$race.cat == "other" & oicas$partracecat == "black")),]$duration, na.rm = TRUE), 1)))



#black-white
black_white_bothmain <- bothmain[which((bothmain$race.cat == "black" & bothmain$partracecat == "white") |
                                        (bothmain$race.cat == "white" & bothmain$partracecat == "black")),]
black_white_bothcas <- bothcas[which((bothcas$race.cat == "black" & bothcas$partracecat == "white") |
                                      (bothcas$race.cat == "white" & bothcas$partracecat == "black")),]
black_white_aimain <- aimain[which((aimain$race.cat == "black" & aimain$partracecat == "white") |
                                    (aimain$race.cat == "white" & aimain$partracecat == "black")),]
black_white_aicas <- aicas[which((aicas$race.cat == "black" & aicas$partracecat == "white") |
                                  (aicas$race.cat == "white" & aicas$partracecat == "black")),]
black_white_oimain <- oimain[which((oimain$race.cat == "black" & oimain$partracecat == "white") |
                                    (oimain$race.cat == "white" & oimain$partracecat == "black")),]
black_white_oicas <- oicas[which((oicas$race.cat == "black" & oicas$partracecat == "white") |
                                  (oicas$race.cat == "white" & oicas$partracecat == "black")),]

black_white <- cbind("black-white", paste0(nrow(extant[which((extant$race.cat == "black" & extant$partracecat == "white") |
                                                                 (extant$race.cat == "white" & extant$partracecat == "black")),]), " (",
                                             round(100 * nrow(extant[which((extant$race.cat == "black" & extant$partracecat == "white") |
                                                                             (extant$race.cat == "white" & extant$partracecat == "black")),]) /
                                                     nrow(extant), 1), ")"),

                    round(mean(black_white_bothmain$duration, na.rm = TRUE), 1),
                    paste0(round(sd(black_white_bothmain$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(black_white_bothmain$duration, na.rm = TRUE), 1)),

                    round(mean(black_white_bothcas$duration, na.rm = TRUE), 1),
                    paste0(round(sd(black_white_bothcas$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(black_white_bothcas$duration, na.rm = TRUE), 1)),

                    round(mean(black_white_aimain$duration, na.rm = TRUE), 1),
                    paste0(round(sd(black_white_aimain$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(black_white_aimain$duration, na.rm = TRUE), 1)),

                    round(mean(black_white_aicas$duration, na.rm = TRUE), 1),
                    paste0(round(sd(black_white_aicas$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(black_white_aicas$duration, na.rm = TRUE), 1)),

                    round(mean(black_white_oimain$duration, na.rm = TRUE), 1),
                    paste0(round(sd(black_white_oimain$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(black_white_oimain$duration, na.rm = TRUE), 1)),

                    round(mean(black_white_oicas$duration, na.rm = TRUE), 1),
                    paste0(round(sd(black_white_oicas$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(black_white_oicas$duration, na.rm = TRUE), 1)))

# hispanic-hispanic
hisp_hisp <- cbind("hispanic-hispanic", paste0(nrow(extant[which(extant$race.cat == "hispanic" & extant$partracecat == "hispanic"),]), " (",
                                           round(100 * nrow(extant[which(extant$race.cat == "hispanic" & extant$partracecat == "hispanic"),]) /
                                                   nrow(extant), 1), ")"),

                   round(mean(bothmain[which(bothmain$race.cat == "hispanic" & bothmain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(bothmain[which(bothmain$race.cat == "hispanic" & bothmain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(bothmain[which(bothmain$race.cat == "hispanic" & bothmain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1)),

                   round(mean(bothcas[which(bothcas$race.cat == "hispanic" & bothcas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(bothcas[which(bothcas$race.cat == "hispanic" & bothcas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(bothcas[which(bothcas$race.cat == "hispanic" & bothcas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1)),

                   round(mean(aimain[which(aimain$race.cat == "hispanic" & aimain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(aimain[which(aimain$race.cat == "hispanic" & aimain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(aimain[which(aimain$race.cat == "hispanic" & aimain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1)),

                   round(mean(aicas[which(aicas$race.cat == "hispanic" & aicas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(aicas[which(aicas$race.cat == "hispanic" & aicas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(aicas[which(aicas$race.cat == "hispanic" & aicas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1)),

                   round(mean(oimain[which(oimain$race.cat == "hispanic" & oimain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(oimain[which(oimain$race.cat == "hispanic" & oimain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(oimain[which(oimain$race.cat == "hispanic" & oimain$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1)),

                   round(mean(oicas[which(oicas$race.cat == "hispanic" & oicas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(oicas[which(oicas$race.cat == "hispanic" & oicas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(oicas[which(oicas$race.cat == "hispanic" & oicas$partracecat == "hispanic"),]$duration, na.rm = TRUE), 1)))


#hispanic-other
hispanic_other <- cbind("hispanic-other", paste0(nrow(extant[which((extant$race.cat == "hispanic" & extant$partracecat == "other") |
                                                               (extant$race.cat == "other" & extant$partracecat == "hispanic")),]), " (",
                                           round(100 * nrow(extant[which((extant$race.cat == "hispanic" & extant$partracecat == "other") |
                                                                           (extant$race.cat == "other" & extant$partracecat == "hispanic")),]) /
                                                   nrow(extant), 1), ")"),

                     round(mean(bothmain[which((bothmain$race.cat == "hispanic" & bothmain$partracecat == "other") |
                                                 (bothmain$race.cat == "other" & bothmain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(bothmain[which((bothmain$race.cat == "hispanic" & bothmain$partracecat == "other") |
                                                      (bothmain$race.cat == "other" & bothmain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(bothmain[which((bothmain$race.cat == "hispanic" & bothmain$partracecat == "other") |
                                                          (bothmain$race.cat == "other" & bothmain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                     round(mean(bothcas[which((bothcas$race.cat == "hispanic" & bothcas$partracecat == "other") |
                                                (bothcas$race.cat == "other" & bothcas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(bothcas[which((bothcas$race.cat == "hispanic" & bothcas$partracecat == "other") |
                                                     (bothcas$race.cat == "other" & bothcas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(bothcas[which((bothcas$race.cat == "hispanic" & bothcas$partracecat == "other") |
                                                         (bothcas$race.cat == "other" & bothcas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                     round(mean(aimain[which((aimain$race.cat == "hispanic" & aimain$partracecat == "other") |
                                               (aimain$race.cat == "other" & aimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(aimain[which((aimain$race.cat == "hispanic" & aimain$partracecat == "other") |
                                                    (aimain$race.cat == "other" & aimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(aimain[which((aimain$race.cat == "hispanic" & aimain$partracecat == "other") |
                                                        (aimain$race.cat == "other" & aimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                     round(mean(aicas[which((aicas$race.cat == "hispanic" & aicas$partracecat == "other") |
                                              (aicas$race.cat == "other" & aicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(aicas[which((aicas$race.cat == "hispanic" & aicas$partracecat == "other") |
                                                   (aicas$race.cat == "other" & aicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(aicas[which((aicas$race.cat == "hispanic" & aicas$partracecat == "other") |
                                                       (aicas$race.cat == "other" & aicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                     round(mean(oimain[which((oimain$race.cat == "hispanic" & oimain$partracecat == "other") |
                                               (oimain$race.cat == "other" & oimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(oimain[which((oimain$race.cat == "hispanic" & oimain$partracecat == "other") |
                                                    (oimain$race.cat == "other" & oimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(oimain[which((oimain$race.cat == "hispanic" & oimain$partracecat == "other") |
                                                        (oimain$race.cat == "other" & oimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                     round(mean(oicas[which((oicas$race.cat == "hispanic" & oicas$partracecat == "other") |
                                              (oicas$race.cat == "other" & oicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(oicas[which((oicas$race.cat == "hispanic" & oicas$partracecat == "other") |
                                                   (oicas$race.cat == "other" & oicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(oicas[which((oicas$race.cat == "hispanic" & oicas$partracecat == "other") |
                                                       (oicas$race.cat == "other" & oicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)))

#hispanic-white
hispanic_white <- cbind("hispanic-white", paste0(nrow(extant[which((extant$race.cat == "hispanic" & extant$partracecat == "white") |
                                                                     (extant$race.cat == "white" & extant$partracecat == "hispanic")),]), " (",
                                                 round(100 * nrow(extant[which((extant$race.cat == "hispanic" & extant$partracecat == "white") |
                                                                                 (extant$race.cat == "white" & extant$partracecat == "hispanic")),]) /
                                                         nrow(extant), 1), ")"),

                        round(mean(bothmain[which((bothmain$race.cat == "hispanic" & bothmain$partracecat == "white") |
                                                    (bothmain$race.cat == "white" & bothmain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(bothmain[which((bothmain$race.cat == "hispanic" & bothmain$partracecat == "white") |
                                                         (bothmain$race.cat == "white" & bothmain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(bothmain[which((bothmain$race.cat == "hispanic" & bothmain$partracecat == "white") |
                                                             (bothmain$race.cat == "white" & bothmain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                        round(mean(bothcas[which((bothcas$race.cat == "hispanic" & bothcas$partracecat == "white") |
                                                   (bothcas$race.cat == "white" & bothcas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(bothcas[which((bothcas$race.cat == "hispanic" & bothcas$partracecat == "white") |
                                                        (bothcas$race.cat == "white" & bothcas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(bothcas[which((bothcas$race.cat == "hispanic" & bothcas$partracecat == "white") |
                                                            (bothcas$race.cat == "white" & bothcas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                        round(mean(aimain[which((aimain$race.cat == "hispanic" & aimain$partracecat == "white") |
                                                  (aimain$race.cat == "white" & aimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(aimain[which((aimain$race.cat == "hispanic" & aimain$partracecat == "white") |
                                                       (aimain$race.cat == "white" & aimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(aimain[which((aimain$race.cat == "hispanic" & aimain$partracecat == "white") |
                                                           (aimain$race.cat == "white" & aimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                        round(mean(aicas[which((aicas$race.cat == "hispanic" & aicas$partracecat == "white") |
                                                 (aicas$race.cat == "white" & aicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(aicas[which((aicas$race.cat == "hispanic" & aicas$partracecat == "white") |
                                                      (aicas$race.cat == "white" & aicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(aicas[which((aicas$race.cat == "hispanic" & aicas$partracecat == "white") |
                                                          (aicas$race.cat == "white" & aicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                        round(mean(oimain[which((oimain$race.cat == "hispanic" & oimain$partracecat == "white") |
                                                  (oimain$race.cat == "white" & oimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(oimain[which((oimain$race.cat == "hispanic" & oimain$partracecat == "white") |
                                                       (oimain$race.cat == "white" & oimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(oimain[which((oimain$race.cat == "hispanic" & oimain$partracecat == "white") |
                                                           (oimain$race.cat == "white" & oimain$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)),

                        round(mean(oicas[which((oicas$race.cat == "hispanic" & oicas$partracecat == "white") |
                                                 (oicas$race.cat == "white" & oicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(oicas[which((oicas$race.cat == "hispanic" & oicas$partracecat == "white") |
                                                      (oicas$race.cat == "white" & oicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(oicas[which((oicas$race.cat == "hispanic" & oicas$partracecat == "white") |
                                                          (oicas$race.cat == "white" & oicas$partracecat == "hispanic")),]$duration, na.rm = TRUE), 1)))

# other-other
other_other <- cbind("other-other", paste0(nrow(extant[which(extant$race.cat == "other" & extant$partracecat == "other"),]), " (",
                                               round(100 * nrow(extant[which(extant$race.cat == "other" & extant$partracecat == "other"),]) /
                                                       nrow(extant), 1), ")"),

                   round(mean(bothmain[which(bothmain$race.cat == "other" & bothmain$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(bothmain[which(bothmain$race.cat == "other" & bothmain$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(bothmain[which(bothmain$race.cat == "other" & bothmain$partracecat == "other"),]$duration, na.rm = TRUE), 1)),

                   round(mean(bothcas[which(bothcas$race.cat == "other" & bothcas$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(bothcas[which(bothcas$race.cat == "other" & bothcas$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(bothcas[which(bothcas$race.cat == "other" & bothcas$partracecat == "other"),]$duration, na.rm = TRUE), 1)),

                   round(mean(aimain[which(aimain$race.cat == "other" & aimain$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(aimain[which(aimain$race.cat == "other" & aimain$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(aimain[which(aimain$race.cat == "other" & aimain$partracecat == "other"),]$duration, na.rm = TRUE), 1)),

                   round(mean(aicas[which(aicas$race.cat == "other" & aicas$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(aicas[which(aicas$race.cat == "other" & aicas$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(aicas[which(aicas$race.cat == "other" & aicas$partracecat == "other"),]$duration, na.rm = TRUE), 1)),

                   round(mean(oimain[which(oimain$race.cat == "other" & oimain$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(oimain[which(oimain$race.cat == "other" & oimain$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(oimain[which(oimain$race.cat == "other" & oimain$partracecat == "other"),]$duration, na.rm = TRUE), 1)),

                   round(mean(oicas[which(oicas$race.cat == "other" & oicas$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                   paste0(round(sd(oicas[which(oicas$race.cat == "other" & oicas$partracecat == "other"),]$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(oicas[which(oicas$race.cat == "other" & oicas$partracecat == "other"),]$duration, na.rm = TRUE), 1)))

#other-white
other_white <- cbind("other-white", paste0(nrow(extant[which((extant$race.cat == "other" & extant$partracecat == "white") |
                                                                     (extant$race.cat == "white" & extant$partracecat == "other")),]), " (",
                                                 round(100 * nrow(extant[which((extant$race.cat == "other" & extant$partracecat == "white") |
                                                                                 (extant$race.cat == "white" & extant$partracecat == "other")),]) /
                                                         nrow(extant), 1), ")"),

                        round(mean(bothmain[which((bothmain$race.cat == "other" & bothmain$partracecat == "white") |
                                                    (bothmain$race.cat == "white" & bothmain$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(bothmain[which((bothmain$race.cat == "other" & bothmain$partracecat == "white") |
                                                         (bothmain$race.cat == "white" & bothmain$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(bothmain[which((bothmain$race.cat == "other" & bothmain$partracecat == "white") |
                                                             (bothmain$race.cat == "white" & bothmain$partracecat == "other")),]$duration, na.rm = TRUE), 1)),

                        round(mean(bothcas[which((bothcas$race.cat == "other" & bothcas$partracecat == "white") |
                                                   (bothcas$race.cat == "white" & bothcas$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(bothcas[which((bothcas$race.cat == "other" & bothcas$partracecat == "white") |
                                                        (bothcas$race.cat == "white" & bothcas$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(bothcas[which((bothcas$race.cat == "other" & bothcas$partracecat == "white") |
                                                            (bothcas$race.cat == "white" & bothcas$partracecat == "other")),]$duration, na.rm = TRUE), 1)),

                        round(mean(aimain[which((aimain$race.cat == "other" & aimain$partracecat == "white") |
                                                  (aimain$race.cat == "white" & aimain$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(aimain[which((aimain$race.cat == "other" & aimain$partracecat == "white") |
                                                       (aimain$race.cat == "white" & aimain$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(aimain[which((aimain$race.cat == "other" & aimain$partracecat == "white") |
                                                           (aimain$race.cat == "white" & aimain$partracecat == "other")),]$duration, na.rm = TRUE), 1)),

                        round(mean(aicas[which((aicas$race.cat == "other" & aicas$partracecat == "white") |
                                                 (aicas$race.cat == "white" & aicas$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(aicas[which((aicas$race.cat == "other" & aicas$partracecat == "white") |
                                                      (aicas$race.cat == "white" & aicas$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(aicas[which((aicas$race.cat == "other" & aicas$partracecat == "white") |
                                                          (aicas$race.cat == "white" & aicas$partracecat == "other")),]$duration, na.rm = TRUE), 1)),

                        round(mean(oimain[which((oimain$race.cat == "other" & oimain$partracecat == "white") |
                                                  (oimain$race.cat == "white" & oimain$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(oimain[which((oimain$race.cat == "other" & oimain$partracecat == "white") |
                                                       (oimain$race.cat == "white" & oimain$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(oimain[which((oimain$race.cat == "other" & oimain$partracecat == "white") |
                                                           (oimain$race.cat == "white" & oimain$partracecat == "other")),]$duration, na.rm = TRUE), 1)),

                        round(mean(oicas[which((oicas$race.cat == "other" & oicas$partracecat == "white") |
                                                 (oicas$race.cat == "white" & oicas$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                        paste0(round(sd(oicas[which((oicas$race.cat == "other" & oicas$partracecat == "white") |
                                                      (oicas$race.cat == "white" & oicas$partracecat == "other")),]$duration, na.rm = TRUE), 1),
                               ", ",
                               round(median(oicas[which((oicas$race.cat == "other" & oicas$partracecat == "white") |
                                                          (oicas$race.cat == "white" & oicas$partracecat == "other")),]$duration, na.rm = TRUE), 1)))



# white-white
white_white <- cbind("white-white", paste0(nrow(extant[which(extant$race.cat == "white" & extant$partracecat == "white"),]), " (",
                                           round(100 * nrow(extant[which(extant$race.cat == "white" & extant$partracecat == "white"),]) /
                                                   nrow(extant), 1), ")"),

                     round(mean(bothmain[which(bothmain$race.cat == "white" & bothmain$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(bothmain[which(bothmain$race.cat == "white" & bothmain$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(bothmain[which(bothmain$race.cat == "white" & bothmain$partracecat == "white"),]$duration, na.rm = TRUE), 1)),

                     round(mean(bothcas[which(bothcas$race.cat == "white" & bothcas$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(bothcas[which(bothcas$race.cat == "white" & bothcas$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(bothcas[which(bothcas$race.cat == "white" & bothcas$partracecat == "white"),]$duration, na.rm = TRUE), 1)),

                     round(mean(aimain[which(aimain$race.cat == "white" & aimain$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(aimain[which(aimain$race.cat == "white" & aimain$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(aimain[which(aimain$race.cat == "white" & aimain$partracecat == "white"),]$duration, na.rm = TRUE), 1)),

                     round(mean(aicas[which(aicas$race.cat == "white" & aicas$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(aicas[which(aicas$race.cat == "white" & aicas$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(aicas[which(aicas$race.cat == "white" & aicas$partracecat == "white"),]$duration, na.rm = TRUE), 1)),

                     round(mean(oimain[which(oimain$race.cat == "white" & oimain$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(oimain[which(oimain$race.cat == "white" & oimain$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(oimain[which(oimain$race.cat == "white" & oimain$partracecat == "white"),]$duration, na.rm = TRUE), 1)),

                     round(mean(oicas[which(oicas$race.cat == "white" & oicas$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                     paste0(round(sd(oicas[which(oicas$race.cat == "white" & oicas$partracecat == "white"),]$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(oicas[which(oicas$race.cat == "white" & oicas$partracecat == "white"),]$duration, na.rm = TRUE), 1)))


# HIV Status


#neg-neg
neg_neg <- cbind("neg-neg", paste0(nrow(extant[which(extant$hivcomb == "Neg-Neg"), ]), " (",
                                   round(100 * nrow(extant[which(extant$hivcomb == "Neg-Neg"), ]) / nrow(extant), 1), ")"),

                round(mean(bothmain$duration[bothmain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$duration[bothmain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$duration[bothmain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1)),

                round(mean(bothcas$duration[bothcas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$duration[bothcas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$duration[bothcas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1)),

                round(mean(aimain$duration[aimain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                paste0(round(sd(aimain$duration[aimain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                       ", ",
                       round(median(aimain$duration[aimain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1)),

                round(mean(aicas$duration[aicas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                paste0(round(sd(aicas$duration[aicas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                       ", ",
                       round(median(aicas$duration[aicas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1)),

                round(mean(oimain$duration[oimain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                paste0(round(sd(oimain$duration[oimain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oimain$duration[oimain$hivcomb == "Neg-Neg"], na.rm = TRUE), 1)),

                round(mean(oicas$duration[oicas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                paste0(round(sd(oicas$duration[oicas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1),
                       ", ",
                       round(median(oicas$duration[oicas$hivcomb == "Neg-Neg"], na.rm = TRUE), 1)))


#neg-pos
neg_pos <- cbind("neg-pos", paste0(nrow(extant[which(extant$hivcomb == "Neg-Pos" | extant$hivcomb == "Pos-Neg"), ]), " (",
                                   round(100 * nrow(extant[which(extant$hivcomb == "Neg-Pos" | extant$hivcomb == "Pos-Neg"), ]) / nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$hivcomb == "Neg-Pos" | bothmain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$hivcomb == "Neg-Pos" | bothmain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$hivcomb == "Neg-Pos" | bothmain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$hivcomb == "Neg-Pos" | bothcas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$hivcomb == "Neg-Pos" | bothcas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$hivcomb == "Neg-Pos" | bothcas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1)),

                 round(mean(aimain$duration[aimain$hivcomb == "Neg-Pos" | aimain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$hivcomb == "Neg-Pos" | aimain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$hivcomb == "Neg-Pos" | aimain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1)),

                 round(mean(aicas$duration[aicas$hivcomb == "Neg-Pos" | aicas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$hivcomb == "Neg-Pos" | aicas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$hivcomb == "Neg-Pos" | aicas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$hivcomb == "Neg-Pos" | oimain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$hivcomb == "Neg-Pos" | oimain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$hivcomb == "Neg-Pos" | oimain$hivcomb == "Pos-Neg"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$hivcomb == "Neg-Pos" | oicas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$hivcomb == "Neg-Pos" | oicas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$hivcomb == "Neg-Pos" | oicas$hivcomb == "Pos-Neg"], na.rm = TRUE), 1)))


#neg-unk
neg_unk <- cbind("neg-unk", paste0(nrow(extant[which(extant$hivcomb == "Neg-Unknown"), ]), " (",
                                   round(100 * nrow(extant[which(extant$hivcomb == "Neg-Unknown"), ]) / nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1)),

                 round(mean(aimain$duration[aimain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1)),

                 round(mean(aicas$duration[aicas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$hivcomb == "Neg-Unknown"], na.rm = TRUE), 1)))


#pos-pos
pos_pos <- cbind("pos-pos", paste0(nrow(extant[which(extant$hivcomb == "Pos-Pos"), ]), " (",
                                   round(100 * nrow(extant[which(extant$hivcomb == "Pos-Pos"), ]) / nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1)),

                 round(mean(aimain$duration[aimain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1)),

                 round(mean(aicas$duration[aicas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$hivcomb == "Pos-Pos"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$hivcomb == "Pos-Pos"], na.rm = TRUE), 1)))

#pos-unk
pos_unk <- cbind("pos-unk", paste0(nrow(extant[which(extant$hivcomb == "Pos-Unknown"), ]), " (",
                                   round(100 * nrow(extant[which(extant$hivcomb == "Pos-Unknown"), ]) / nrow(extant), 1), ")"),

                 round(mean(bothmain$duration[bothmain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(bothmain$duration[bothmain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothmain$duration[bothmain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1)),

                 round(mean(bothcas$duration[bothcas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(bothcas$duration[bothcas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(bothcas$duration[bothcas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1)),

                 round(mean(aimain$duration[aimain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(aimain$duration[aimain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aimain$duration[aimain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1)),

                 round(mean(aicas$duration[aicas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(aicas$duration[aicas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(aicas$duration[aicas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1)),

                 round(mean(oimain$duration[oimain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(oimain$duration[oimain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oimain$duration[oimain$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1)),

                 round(mean(oicas$duration[oicas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                 paste0(round(sd(oicas$duration[oicas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1),
                        ", ",
                        round(median(oicas$duration[oicas$hivcomb == "Pos-Unknown"], na.rm = TRUE), 1)))


table_s3 <- rbind(tot, black_black, black_hisp, black_other, black_white, hisp_hisp,
                 hispanic_other, hispanic_white, other_other, other_white, white_white,
                 neg_neg, neg_pos, neg_unk, pos_pos, pos_unk)
colnames(table_s3) <- c("Category", "N (%)", "Ongoing Either Main Mean", "Ongoing Either Main SD, Med",
                      "Ongoing Either Cas Mean", "Ongoing Either Cas SD, Med",
                      "Ongoing AI Main Mean", "Ongoing AI Main SD, Med",
                      "Ongoing AI Cas Mean", "Ongoing AI Cas SD, Med",
                      "Ongoing OI Main Mean", "Ongoing OI Main SD, Med",
                      "Ongoing OI Cas Mean", "Ongoing OI Cas SD, Med")
write.csv(table_s3, file = "stable3.csv")
