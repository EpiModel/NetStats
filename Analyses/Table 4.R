## ART-Net Study 2018   ##
## Table 4 Mixing       ##
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
tot <- artnetLong %>% count(partraces)
a <- main %>% count(partraces)
# Black-black, black-hisp, black-other, black-white, hisp-hisp, hisp-other,
# hisp-white, other-other, other-white, white-white
racemain <- cbind(cbind(c(tot$partraces[2], tot$partraces[3], tot$partraces[4],
                  tot$partraces[5], tot$partraces[6], tot$partraces[7],
                  tot$partraces[8], tot$partraces[9], tot$partraces[10],
                  tot$partraces[11], NA)),
                  cbind(c(sum(tot$n[2]), sum(tot$n[3]), sum(tot$n[4]),
                          sum(tot$n[5]), sum(tot$n[6]), sum(tot$n[7]),
                          sum(tot$n[8]), sum(tot$n[9]),
                          sum(tot$n[10]), sum(tot$n[11]), sum(tot$n[1]))),
                  rbind((100 * tot$n[2] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[3] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[4] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[5] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[6] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[7] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[8] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[9] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[10] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[11] / sum(tot$n[c(2:11)])),
                        (100 * tot$n[1] / sum(tot$n))),
                  cbind(c(sum(a$n[2]), sum(a$n[3]), sum(a$n[4]),
                          sum(a$n[5]), sum(a$n[6]), sum(a$n[7]),
                          sum(a$n[8]), sum(a$n[9]),
                          sum(a$n[10]), sum(a$n[11]), sum(a$n[1]))),
                  rbind((100 * a$n[2] / sum(a$n[c(2:11)])),
                        (100 * a$n[3] / sum(a$n[c(2:11)])),
                        (100 * a$n[4] / sum(a$n[c(2:11)])),
                        (100 * a$n[5] / sum(a$n[c(2:11)])),
                        (100 * a$n[6] / sum(a$n[c(2:11)])),
                        (100 * a$n[7] / sum(a$n[c(2:11)])),
                        (100 * a$n[8] / sum(a$n[c(2:11)])),
                        (100 * a$n[9] / sum(a$n[c(2:11)])),
                        (100 * a$n[10] / sum(a$n[c(2:11)])),
                        (100 * a$n[11] / sum(a$n[c(2:11)])),
                        (100 * a$n[1] / sum(a$n))))

b <- cas %>% count(partraces)
racecas <- cbind(cbind(c(sum(b$n[2]), sum(b$n[3]), sum(b$n[4]),
                         sum(b$n[5]), sum(b$n[6]), sum(b$n[7]),
                         sum(b$n[8]), sum(b$n[9]),
                         sum(b$n[10]), sum(b$n[11]), sum(b$n[1]))),
                 rbind((100 * b$n[2] / sum(b$n[c(2:11)])),
                       (100 * b$n[3] / sum(b$n[c(2:11)])),
                       (100 * b$n[4] / sum(b$n[c(2:11)])),
                       (100 * b$n[5] / sum(b$n[c(2:11)])),
                       (100 * b$n[6] / sum(b$n[c(2:11)])),
                       (100 * b$n[7] / sum(b$n[c(2:11)])),
                       (100 * b$n[8] / sum(b$n[c(2:11)])),
                       (100 * b$n[9] / sum(b$n[c(2:11)])),
                       (100 * b$n[10] / sum(b$n[c(2:11)])),
                       (100 * b$n[11] / sum(b$n[c(2:11)])),
                       (100 * b$n[1] / sum(b$n))))

c <- inst %>% count(partraces)
raceinst <- cbind(cbind(c(sum(c$n[2]), sum(c$n[3]), sum(c$n[4]),
                          sum(c$n[5]), sum(c$n[6]), sum(c$n[7]),
                          sum(c$n[8]), sum(c$n[9]),
                          sum(c$n[10]), sum(c$n[11]), sum(c$n[1]))),
                  rbind((100 * c$n[2] / sum(c$n[c(2:11)])),
                        (100 * c$n[3] / sum(c$n[c(2:11)])),
                        (100 * c$n[4] / sum(c$n[c(2:11)])),
                        (100 * c$n[5] / sum(c$n[c(2:11)])),
                        (100 * c$n[6] / sum(c$n[c(2:11)])),
                        (100 * c$n[7] / sum(c$n[c(2:11)])),
                        (100 * c$n[8] / sum(c$n[c(2:11)])),
                        (100 * c$n[9] / sum(c$n[c(2:11)])),
                        (100 * c$n[10] / sum(c$n[c(2:11)])),
                        (100 * c$n[11] / sum(c$n[c(2:11)])),
                        (100 * c$n[1] / sum(c$n))))

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

hivmain <- cbind(cbind(c(tot$partstatuses[c(2:6, 1)])),
                 cbind(c(tot$n[c(2:6, 1)])),
                 cbind(rbind((100 * tot$n[2] / sum(tot$n[2:6])),
                             (100 * tot$n[3] / sum(tot$n[2:6])),
                             (100 * tot$n[4] / sum(tot$n[2:6])),
                             (100 * tot$n[5] / sum(tot$n[2:6])),
                             (100 * tot$n[6] / sum(tot$n[2:6])),
                             (100 * tot$n[1] / sum(tot$n)))),
                 cbind(c(a$n[c(2:6, 1)])),
                 cbind(rbind((100 * a$n[2] / sum(a$n[2:6])),
                             (100 * a$n[3] / sum(a$n[2:6])),
                             (100 * a$n[4] / sum(a$n[2:6])),
                             (100 * a$n[5] / sum(a$n[2:6])),
                             (100 * a$n[6] / sum(a$n[2:6])),
                             (100 * a$n[1] / sum(a$n)))))
hivcas <- cbind(cbind(c(b$n[c(2:6, 1)])),
                cbind(rbind((100 * b$n[2] / sum(b$n[2:6])),
                            (100 * b$n[3] / sum(b$n[2:6])),
                            (100 * b$n[4] / sum(b$n[2:6])),
                            (100 * b$n[5] / sum(b$n[2:6])),
                            (100 * b$n[6] / sum(b$n[2:6])),
                            (100 * b$n[1] / sum(b$n)))))

hivinst <- cbind(cbind(c(c$n[c(2:6, 1)])),
                 cbind(rbind((100 * c$n[2] / sum(c$n[2:6])),
                             (100 * c$n[3] / sum(c$n[2:6])),
                             (100 * c$n[4] / sum(c$n[2:6])),
                             (100 * c$n[5] / sum(c$n[2:6])),
                             (100 * c$n[6] / sum(c$n[2:6])),
                             (100 * c$n[1] / sum(c$n)))))


# Age (categorical)
tot <- artnetLong %>% count(partages)
a <- main %>% count(partages)

agemain <- cbind(cbind(c("15-24", "25-34", "35-44", "45-54", "55-65", "Unmatched", NA)),
                 cbind(c(sum(tot$n[2]), sum(tot$n[3]), sum(tot$n[4]),
                         sum(tot$n[5]), sum(tot$n[6]),
                         sum(tot$n[7]), sum(tot$n[1]))),
                 cbind(rbind((100 * tot$n[2] / sum(tot$n[c(2:7)])),
                             (100 * tot$n[3] / sum(tot$n[c(2:7)])),
                             (100 * tot$n[4] / sum(tot$n[c(2:7)])),
                             (100 * tot$n[5] / sum(tot$n[c(2:7)])),
                             (100 * tot$n[6] / sum(tot$n[c(2:7)])),
                             (100 * tot$n[7] / sum(tot$n[c(2:7)])),
                             (100 * tot$n[1] / sum(tot$n)))),
                 cbind(c(sum(a$n[2]), sum(a$n[3]), sum(a$n[4]),
                         sum(a$n[5]), sum(a$n[6]),
                         sum(a$n[7]), sum(a$n[1]))),
                 cbind(rbind((100 * a$n[2] / sum(a$n[c(2:7)])),
                             (100 * a$n[3] / sum(a$n[c(2:7)])),
                             (100 * a$n[4] / sum(a$n[c(2:7)])),
                             (100 * a$n[5] / sum(a$n[c(2:7)])),
                             (100 * a$n[6] / sum(a$n[c(2:7)])),
                             (100 * a$n[7] / sum(a$n[c(2:7)])),
                             (100 * a$n[1] / sum(a$n)))))
b <- cas %>% count(partages)
agecas <- cbind(cbind(c(sum(b$n[2]), sum(b$n[3]), sum(b$n[4]),
                        sum(b$n[5]), sum(b$n[6]),
                        sum(b$n[7]), sum(b$n[1]))),
                cbind(rbind((100 * b$n[2] / sum(b$n[c(2:7)])),
                            (100 * b$n[3] / sum(b$n[c(2:7)])),
                            (100 * b$n[4] / sum(b$n[c(2:7)])),
                            (100 * b$n[5] / sum(b$n[c(2:7)])),
                            (100 * b$n[6] / sum(b$n[c(2:7)])),
                            (100 * b$n[7] / sum(b$n[c(2:7)])),
                            (100 * b$n[1] / sum(b$n)))))
c <- inst %>% count(partages)
ageinst <- cbind(cbind(c(sum(c$n[2]), sum(c$n[3]), sum(c$n[4]),
                         sum(c$n[5]), sum(c$n[6]),
                         sum(c$n[7]), sum(c$n[1]))),
                 cbind(rbind((100 * c$n[2] / sum(c$n[c(2:7)])),
                             (100 * c$n[3] / sum(c$n[c(2:7)])),
                             (100 * c$n[4] / sum(c$n[c(2:7)])),
                             (100 * c$n[5] / sum(c$n[c(2:7)])),
                             (100 * c$n[6] / sum(c$n[c(2:7)])),
                             (100 * c$n[7] / sum(c$n[c(2:7)])),
                             (100 * c$n[1] / sum(c$n)))))

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
