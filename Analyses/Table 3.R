## ART-Net Study 2018   ##
## Table 3 Duration     ##
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

# Table 3 - Duration of ongoing partnerships --------------

#TODO: Imputed or given duration?

# Set up data frames
extant <- artnetLong[which(artnetLong$ONGOING == 1 & artnetLong$duration < 2150 & artnetLong$ptype %in% c(1, 2)), ]

# Subset to those active
extant <- extant[which(extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1), ]

# Number of rows with no reported activity
nrow(extant[which(extant$RAI == 0 & extant$IAI == 0 & extant$ROI == 0 & extant$IOI == 0), ])

bothmain <- extant[which((extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 1), ]
bothcas <- extant[which((extant$RAI == 1 | extant$IAI == 1 | extant$ROI == 1 | extant$IOI == 1) & extant$ptype == 2), ]


# Total number of ongoing partnerships
total <- cbind("Total", paste0(nrow(extant), " (", 100 * nrow(extant) / nrow(extant), ")"),

               round(mean(extant$duration, na.rm = TRUE), 1),
               paste0(round(sd(extant$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration, na.rm = TRUE), 1)),

               # Calculate mean, 95% CI
               round(mean(bothmain$duration, na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration, na.rm = TRUE), 1)),

               round(mean(bothcas$duration, na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration, na.rm = TRUE), 1)))

# Race/ethnicity
black <- cbind("black",
               paste0(nrow(extant[which(extant$race.cat == "black"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "black"), ]) /
                              nrow(extant), 1), ")"),

               round(mean(extant$duration[which(extant$race.cat == "black")], na.rm = TRUE), 1),
               paste0(round(sd(extant$duration[which(extant$race.cat == "black")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration[which(extant$race.cat == "black")], na.rm = TRUE), 1)),

               round(mean(bothmain$duration[bothmain$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration[bothmain$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration[bothmain$race.cat == "black"], na.rm = TRUE), 1)),

               round(mean(bothcas$duration[bothcas$race.cat == "black"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration[bothcas$race.cat == "black"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration[bothcas$race.cat == "black"], na.rm = TRUE), 1)))


white <- cbind("white",
               paste0(nrow(extant[which(extant$race.cat == "white"), ]),
                      " (",
                      round(100 * nrow(extant[which(extant$race.cat == "white"), ]) /
                              nrow(extant), 1), ")"),
               round(mean(extant$duration[which(extant$race.cat == "white")], na.rm = TRUE), 1),
               paste0(round(sd(extant$duration[which(extant$race.cat == "white")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration[which(extant$race.cat == "white")], na.rm = TRUE), 1)),
               round(mean(bothmain$duration[bothmain$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration[bothmain$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration[bothmain$race.cat == "white"], na.rm = TRUE), 1)),

               round(mean(bothcas$duration[bothcas$race.cat == "white"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration[bothcas$race.cat == "white"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration[bothcas$race.cat == "white"], na.rm = TRUE), 1)))

hispanic <- cbind("hispanic",
                  paste0(nrow(extant[which(extant$race.cat == "hispanic"), ]),
                         " (",
                         round(100 * nrow(extant[which(extant$race.cat == "hispanic"), ]) /
                                 nrow(extant), 1), ")"),
                  round(mean(extant$duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1),
                  paste0(round(sd(extant$duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1),
                         ", ",
                         round(median(extant$duration[which(extant$race.cat == "hispanic")], na.rm = TRUE), 1)),
                  round(mean(bothmain$duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(bothmain$duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothmain$duration[bothmain$race.cat == "hispanic"], na.rm = TRUE), 1)),

                  round(mean(bothcas$duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1),
                  paste0(round(sd(bothcas$duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1),
                         ", ",
                         round(median(bothcas$duration[bothcas$race.cat == "hispanic"], na.rm = TRUE), 1)))

other <- cbind("other",
               paste0(nrow(extant[which(extant$race.cat == "other"), ]),
                      " (", round(100 * nrow(extant[which(extant$race.cat == "other"), ]) /
                                    nrow(extant), 1), ")"),
               round(mean(extant$duration[which(extant$race.cat == "other")], na.rm = TRUE), 1),
               paste0(round(sd(extant$duration[which(extant$race.cat == "other")], na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration[which(extant$race.cat == "other")], na.rm = TRUE), 1)),
               round(mean(bothmain$duration[bothmain$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration[bothmain$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration[bothmain$race.cat == "other"], na.rm = TRUE), 1)),

               round(mean(bothcas$duration[bothcas$race.cat == "other"], na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration[bothcas$race.cat == "other"], na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration[bothcas$race.cat == "other"], na.rm = TRUE), 1)))


# Age
fifteen24 <- cbind("15-24",
                   paste0(nrow(extant[which(extant$age.cat == "15-24"), ]),
                          " (",
                          round(100 * nrow(extant[which(extant$age.cat == "15-24"), ]) /
                                  nrow(extant), 1), ")"),
                   round(mean(extant$duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1),
                   paste0(round(sd(extant$duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1),
                          ", ",
                          round(median(extant$duration[which(extant$age.cat == "15-24")], na.rm = TRUE), 1)),
                   round(mean(bothmain$duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(bothmain$duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(bothmain$duration[bothmain$age.cat == "15-24"], na.rm = TRUE), 1)),

                   round(mean(bothcas$duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1),
                   paste0(round(sd(bothcas$duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1),
                          ", ",
                          round(median(bothcas$duration[bothcas$age.cat == "15-24"], na.rm = TRUE), 1)))

twentyfive34 <- cbind("25-34",
                      paste0(nrow(extant[which(extant$age.cat == "25-34"), ]),
                             " (",
                             round(100 * nrow(extant[which(extant$age.cat == "25-34"), ]) /
                                     nrow(extant), 1), ")"),
                      round(mean(extant$duration[which(extant$age.cat == "25-34")], na.rm = TRUE), 1),
                      paste0(round(sd(extant$duration[which(extant$age.cat == "25-34")], na.rm = TRUE), 1),
                             ", ",
                             round(median(extant$duration[which(extant$age.cat == "25-34")], na.rm = TRUE), 1)),
                      round(mean(bothmain$duration[bothmain$age.cat == "25-34"], na.rm = TRUE), 1),
                      paste0(round(sd(bothmain$duration[bothmain$age.cat == "25-34"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothmain$duration[bothmain$age.cat == "25-34"], na.rm = TRUE), 1)),

                      round(mean(bothcas$duration[bothcas$age.cat == "25-34"], na.rm = TRUE), 1),
                      paste0(round(sd(bothcas$duration[bothcas$age.cat == "25-34"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothcas$duration[bothcas$age.cat == "25-34"], na.rm = TRUE), 1)))

thirtyfive44 <- cbind("35-44",
                      paste0(nrow(extant[which(extant$age.cat == "35-44"), ]),
                             " (",
                             round(100 * nrow(extant[which(extant$age.cat == "35-44"), ]) /
                                     nrow(extant), 1), ")"),
                      round(mean(extant$duration[which(extant$age.cat == "35-44")], na.rm = TRUE), 1),
                      paste0(round(sd(extant$duration[which(extant$age.cat == "35-44")], na.rm = TRUE), 1),
                             ", ",
                             round(median(extant$duration[which(extant$age.cat == "35-44")], na.rm = TRUE), 1)),
                      round(mean(bothmain$duration[bothmain$age.cat == "35-44"], na.rm = TRUE), 1),
                      paste0(round(sd(bothmain$duration[bothmain$age.cat == "35-44"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothmain$duration[bothmain$age.cat == "35-44"], na.rm = TRUE), 1)),

                      round(mean(bothcas$duration[bothcas$age.cat == "35-44"], na.rm = TRUE), 1),
                      paste0(round(sd(bothcas$duration[bothcas$age.cat == "35-44"], na.rm = TRUE), 1),
                             ", ",
                             round(median(bothcas$duration[bothcas$age.cat == "35-44"], na.rm = TRUE), 1)))

fortyfive54 <- cbind("45-54",
                     paste0(nrow(extant[which(extant$age.cat == "45-54"), ]),
                            " (",
                            round(100 * nrow(extant[which(extant$age.cat == "45-54"), ]) /
                                    nrow(extant), 1), ")"),
                     round(mean(extant$duration[which(extant$age.cat == "45-54")], na.rm = TRUE), 1),
                     paste0(round(sd(extant$duration[which(extant$age.cat == "45-54")], na.rm = TRUE), 1),
                            ", ",
                            round(median(extant$duration[which(extant$age.cat == "45-54")], na.rm = TRUE), 1)),
                     round(mean(bothmain$duration[bothmain$age.cat == "45-54"], na.rm = TRUE), 1),
                     paste0(round(sd(bothmain$duration[bothmain$age.cat == "45-54"], na.rm = TRUE), 1),
                            ", ",
                            round(median(bothmain$duration[bothmain$age.cat == "45-54"], na.rm = TRUE), 1)),

                     round(mean(bothcas$duration[bothcas$age.cat == "45-54"], na.rm = TRUE), 1),
                     paste0(round(sd(bothcas$duration[bothcas$age.cat == "45-54"], na.rm = TRUE), 1),
                            ", ",
                            round(median(bothcas$duration[bothcas$age.cat == "45-54"], na.rm = TRUE), 1)))


fiftyfive65 <- cbind("55-65",
                     paste0(nrow(extant[which(extant$age.cat == "55-65"), ]),
                            " (",
                            round(100 * nrow(extant[which(extant$age.cat == "55-65"), ]) /
                                    nrow(extant), 1), ")"),
                     round(mean(extant$duration[which(extant$age.cat == "55-65")], na.rm = TRUE), 1),
                     paste0(round(sd(extant$duration[which(extant$age.cat == "55-65")], na.rm = TRUE), 1),
                            ", ",
                            round(median(extant$duration[which(extant$age.cat == "55-65")], na.rm = TRUE), 1)),
                     round(mean(bothmain$duration[bothmain$age.cat == "55-65"], na.rm = TRUE), 1),
                     paste0(round(sd(bothmain$duration[bothmain$age.cat == "55-65"], na.rm = TRUE), 1),
                            ", ",
                            round(median(bothmain$duration[bothmain$age.cat == "55-65"], na.rm = TRUE), 1)),

                     round(mean(bothcas$duration[bothcas$age.cat == "55-65"], na.rm = TRUE), 1),
                     paste0(round(sd(bothcas$duration[bothcas$age.cat == "55-65"], na.rm = TRUE), 1),
                            ", ",
                            round(median(bothcas$duration[bothcas$age.cat == "55-65"], na.rm = TRUE), 1)))

# HIV Status
HIVPos <- cbind("HIV Pos",
                paste0(nrow(extant[which(extant$hiv == 1), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == 1), ]) /
                               nrow(extant), 1), ")"),
                round(mean(extant$duration[which(extant$hiv == 1)], na.rm = TRUE), 1),
                paste0(round(sd(extant$duration[which(extant$hiv == 1)], na.rm = TRUE), 1),
                       ", ",
                       round(median(extant$duration[which(extant$hiv == 1)], na.rm = TRUE), 1)),
                round(mean(bothmain$duration[bothmain$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$duration[bothmain$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$duration[bothmain$hiv == 1], na.rm = TRUE), 1)),

                round(mean(bothcas$duration[bothcas$hiv == 1], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$duration[bothcas$hiv == 1], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$duration[bothcas$hiv == 1], na.rm = TRUE), 1)))

HIVNeg <- cbind("HIV Neg",
                paste0(nrow(extant[which(extant$hiv == 0), ]),
                       " (",
                       round(100 * nrow(extant[which(extant$hiv == 0), ]) /
                               nrow(extant), 1), ")"),
                round(mean(extant$duration[which(extant$hiv == 0)], na.rm = TRUE), 1),
                paste0(round(sd(extant$duration[which(extant$hiv == 0)], na.rm = TRUE), 1),
                       ", ",
                       round(median(extant$duration[which(extant$hiv == 0)], na.rm = TRUE), 1)),
                round(mean(bothmain$duration[bothmain$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(bothmain$duration[bothmain$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothmain$duration[bothmain$hiv == 0], na.rm = TRUE), 1)),

                round(mean(bothcas$duration[bothcas$hiv == 0], na.rm = TRUE), 1),
                paste0(round(sd(bothcas$duration[bothcas$hiv == 0], na.rm = TRUE), 1),
                       ", ",
                       round(median(bothcas$duration[bothcas$hiv == 0], na.rm = TRUE), 1)))

# Output table
table3 <- rbind(total, black, white, hispanic, other,
                fifteen24, twentyfive34, thirtyfive44, fortyfive54, fiftyfive65,
                HIVNeg, HIVPos)
colnames(table3) <- c("Category", "N (%)", "Total Mean", "Total SD, Med",
                      "Main Degree Mean", "Main Degree SD, Med", "Cas Degree Mean",
                      "Cas Degree SD, Med")
write.csv(table3, file = "Output/table3.csv")


# Table 3b - Duration by partner matching --------------

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

# Main
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
               round(mean(extant$duration, na.rm = TRUE), 1),
               paste0(round(sd(extant$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(extant$duration, na.rm = TRUE), 1)),

               # Main
               round(mean(bothmain$duration, na.rm = TRUE), 1),
               paste0(round(sd(bothmain$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothmain$duration, na.rm = TRUE), 1)),

               # Cas
               round(mean(bothcas$duration, na.rm = TRUE), 1),
               paste0(round(sd(bothcas$duration, na.rm = TRUE), 1),
                      ", ",
                      round(median(bothcas$duration, na.rm = TRUE), 1)))

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
                    round(mean(bb$duration, na.rm = TRUE), 1),
                    paste0(round(sd(bb$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(bb$duration, na.rm = TRUE), 1)),

                    # Main
                    round(mean(bbm$duration, na.rm = TRUE), 1),
                    paste0(round(sd(bbm$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(bbm$duration, na.rm = TRUE), 1)),

                    # Cas
                    round(mean(bbc$duration, na.rm = TRUE), 1),
                    paste0(round(sd(bbc$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(bbc$duration, na.rm = TRUE), 1)))


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
                    round(mean(ww$duration, na.rm = TRUE), 1),
                    paste0(round(sd(ww$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(ww$duration, na.rm = TRUE), 1)),

                    # Main
                    round(mean(wwm$duration, na.rm = TRUE), 1),
                    paste0(round(sd(wwm$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(wwm$duration, na.rm = TRUE), 1)),

                    # Cas
                    round(mean(wwc$duration, na.rm = TRUE), 1),
                    paste0(round(sd(wwc$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(wwc$duration, na.rm = TRUE), 1)))

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
                  round(mean(hh$duration, na.rm = TRUE), 1),
                  paste0(round(sd(hh$duration, na.rm = TRUE), 1),
                         ", ",
                         round(median(hh$duration, na.rm = TRUE), 1)),

                  # Main
                  round(mean(hhm$duration, na.rm = TRUE), 1),
                  paste0(round(sd(hhm$duration, na.rm = TRUE), 1),
                         ", ",
                         round(median(hhm$duration, na.rm = TRUE), 1)),

                  # Cas
                  round(mean(hhc$duration, na.rm = TRUE), 1),
                  paste0(round(sd(hhc$duration, na.rm = TRUE), 1),
                         ", ",
                         round(median(hhc$duration, na.rm = TRUE), 1)))

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
                    round(mean(oo$duration, na.rm = TRUE), 1),
                    paste0(round(sd(oo$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(oo$duration, na.rm = TRUE), 1)),

                    # Main
                    round(mean(oom$duration, na.rm = TRUE), 1),
                    paste0(round(sd(oom$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(oom$duration, na.rm = TRUE), 1)),

                    # Cas
                    round(mean(ooc$duration, na.rm = TRUE), 1),
                    paste0(round(sd(ooc$duration, na.rm = TRUE), 1),
                           ", ",
                           round(median(ooc$duration, na.rm = TRUE), 1)))
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
                       round(mean(unmatchrace$duration, na.rm = TRUE), 1),
                       paste0(round(sd(unmatchrace$duration, na.rm = TRUE), 1),
                              ", ",
                              round(median(unmatchrace$duration, na.rm = TRUE), 1)),

                       # Main
                       round(mean(unmatchracem$duration, na.rm = TRUE), 1),
                       paste0(round(sd(unmatchracem$duration, na.rm = TRUE), 1),
                              ", ",
                              round(median(unmatchracem$duration, na.rm = TRUE), 1)),

                       # Cas
                       round(mean(unmatchracec$duration, na.rm = TRUE), 1),
                       paste0(round(sd(unmatchracec$duration, na.rm = TRUE), 1),
                              ", ",
                              round(median(unmatchracec$duration, na.rm = TRUE), 1)))



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
                round(mean(nn$duration, na.rm = TRUE), 1),
                paste0(round(sd(nn$duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(nn$duration, na.rm = TRUE), 1)),

                # Main
                round(mean(nnm$duration, na.rm = TRUE), 1),
                paste0(round(sd(nnm$duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(nnm$duration, na.rm = TRUE), 1)),

                # Cas
                round(mean(nnc$duration, na.rm = TRUE), 1),
                paste0(round(sd(nnc$duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(nnc$duration, na.rm = TRUE), 1)))

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
                round(mean(pp$duration, na.rm = TRUE), 1),
                paste0(round(sd(pp$duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(pp$duration, na.rm = TRUE), 1)),

                # Main
                round(mean(ppm$duration, na.rm = TRUE), 1),
                paste0(round(sd(ppm$duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(ppm$duration, na.rm = TRUE), 1)),

                # Cas
                round(mean(ppc$duration, na.rm = TRUE), 1),
                paste0(round(sd(ppc$duration, na.rm = TRUE), 1),
                       ", ",
                       round(median(ppc$duration, na.rm = TRUE), 1)))
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
                      round(mean(unmatchhiv$duration, na.rm = TRUE), 1),
                      paste0(round(sd(unmatchhiv$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(unmatchhiv$duration, na.rm = TRUE), 1)),

                      # Main
                      round(mean(unmatchhivm$duration, na.rm = TRUE), 1),
                      paste0(round(sd(unmatchhivm$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(unmatchhivm$duration, na.rm = TRUE), 1)),

                      # Cas
                      round(mean(unmatchhivc$duration, na.rm = TRUE), 1),
                      paste0(round(sd(unmatchhivc$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(unmatchhivc$duration, na.rm = TRUE), 1)))

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
                   round(mean(fifteen24$duration, na.rm = TRUE), 1),
                   paste0(round(sd(fifteen24$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(fifteen24$duration, na.rm = TRUE), 1)),

                   # Main
                   round(mean(fifteen24m$duration, na.rm = TRUE), 1),
                   paste0(round(sd(fifteen24m$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(fifteen24m$duration, na.rm = TRUE), 1)),

                   # Cas
                   round(mean(fifteen24c$duration, na.rm = TRUE), 1),
                   paste0(round(sd(fifteen24c$duration, na.rm = TRUE), 1),
                          ", ",
                          round(median(fifteen24c$duration, na.rm = TRUE), 1)))

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
                      round(mean(twentyfive34$duration, na.rm = TRUE), 1),
                      paste0(round(sd(twentyfive34$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(twentyfive34$duration, na.rm = TRUE), 1)),

                      # Main
                      round(mean(twentyfive34m$duration, na.rm = TRUE), 1),
                      paste0(round(sd(twentyfive34m$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(twentyfive34m$duration, na.rm = TRUE), 1)),

                      # Cas
                      round(mean(twentyfive34c$duration, na.rm = TRUE), 1),
                      paste0(round(sd(twentyfive34c$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(twentyfive34c$duration, na.rm = TRUE), 1)))

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
                      round(mean(thirtyfive44$duration, na.rm = TRUE), 1),
                      paste0(round(sd(thirtyfive44$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(thirtyfive44$duration, na.rm = TRUE), 1)),

                      # Main
                      round(mean(thirtyfive44m$duration, na.rm = TRUE), 1),
                      paste0(round(sd(thirtyfive44m$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(thirtyfive44m$duration, na.rm = TRUE), 1)),

                      # Cas
                      round(mean(thirtyfive44c$duration, na.rm = TRUE), 1),
                      paste0(round(sd(thirtyfive44c$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(thirtyfive44c$duration, na.rm = TRUE), 1)))

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
                     round(mean(fortyfive54$duration, na.rm = TRUE), 1),
                     paste0(round(sd(fortyfive54$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fortyfive54$duration, na.rm = TRUE), 1)),

                     # Main
                     round(mean(fortyfive54m$duration, na.rm = TRUE), 1),
                     paste0(round(sd(fortyfive54m$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fortyfive54m$duration, na.rm = TRUE), 1)),

                     # Cas
                     round(mean(fortyfive54c$duration, na.rm = TRUE), 1),
                     paste0(round(sd(fortyfive54c$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fortyfive54c$duration, na.rm = TRUE), 1)))


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
                     round(mean(fiftyfive65$duration, na.rm = TRUE), 1),
                     paste0(round(sd(fiftyfive65$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fiftyfive65$duration, na.rm = TRUE), 1)),

                     # Main
                     round(mean(fiftyfive65m$duration, na.rm = TRUE), 1),
                     paste0(round(sd(fiftyfive65m$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fiftyfive65m$duration, na.rm = TRUE), 1)),

                     # Cas
                     round(mean(fiftyfive65c$duration, na.rm = TRUE), 1),
                     paste0(round(sd(fiftyfive65c$duration, na.rm = TRUE), 1),
                            ", ",
                            round(median(fiftyfive65c$duration, na.rm = TRUE), 1)))

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
                      round(mean(unmatchage$duration, na.rm = TRUE), 1),
                      paste0(round(sd(unmatchage$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(unmatchage$duration, na.rm = TRUE), 1)),

                      # Main
                      round(mean(unmatchagem$duration, na.rm = TRUE), 1),
                      paste0(round(sd(unmatchagem$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(unmatchagem$duration, na.rm = TRUE), 1)),

                      # Cas
                      round(mean(unmatchagec$duration, na.rm = TRUE), 1),
                      paste0(round(sd(unmatchagec$duration, na.rm = TRUE), 1),
                             ", ",
                             round(median(unmatchagec$duration, na.rm = TRUE), 1)))

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
