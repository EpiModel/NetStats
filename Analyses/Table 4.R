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
tot <- artnetLong %>% count(race.cat, partracecat)
a <- main %>% count(race.cat, partracecat)
# Black-black, black-hisp, black-other, black-white, hisp-hisp, hisp-other,
# hisp-white, other-other, other-white, white-white
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
