## ART-Net Study 2018   ##
## PrEP Analysis Script ##
## 2018-09-18           ##

# Package and setup -----------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(haven)
library(tidyverse)

# Read in datasets
artnet <- readRDS("Shiny Output/artnet4shiny.rda")
artnetLong <- readRDS("Shiny Output/artnetlong4shiny.rda")
artnet2 <- artnet

# Set up individual components of indications
artnet2$prep_part6mo <- rep(0, nrow(artnet))
artnet2$prep_hiv <- rep(0, nrow(artnet))
artnet2$prep_nonmonog <- rep(0, nrow(artnet))
artnet2$prep_sti <- rep(0, nrow(artnet))
artnet2$prep_uai <- rep(0, nrow(artnet))

# Set up actual indications
artnet2$prepind_uai <- rep(0, nrow(artnet))
artnet2$prepind_sti <- rep(0, nrow(artnet))
artnet2$prepind_any <- rep(0, nrow(artnet))

# Denominator: not known to be positive
sum(artnet$hiv == 0) # 1951

# artnetLong$part6mo <- table(difftime(artnetLong$SUB_DATE, artnetLong$p_end.date) > 182)


# CDC Guidelines  ---------
# Adult man
# Without acute or established HIV infection
# Any male sex partners in past 6 months (if also has sex with women, see Box B2)
# Not in a monogamous partnership with a recently tested, HIV-negative man
# AND at least one of the following
# Any anal sex without condoms (receptive or insertive) in past 6 months
# A bacterial STI (syphilis, gonorrhea, or chlamydia) diagnosed or reported in past 6
# months
#
# Indications
# Common: HIV-negative, sexually active in past 6 months
# Common: multiple ongoing partners | (partner # = 1 & partner is not negative)
# Indication 1: Any CAI
# Indication 2: Any STI in past 6 months


# Subsetting to factors ------------------------------------------
# Condition 1: Age > 18
adults <- as.numeric(artnet2[which(artnet2$age >= 18), "AMIS_ID"]) #2141 men
artnet2$prep_adult[which(artnet2$AMIS_ID %in% adults)] <- 1

# Condition 2: HIV-negative man
negativeids <- as.numeric(artnet2[which(artnet2$hiv == 0), "AMIS_ID"]) # 1951 men
artnet2$prep_hiv[which(artnet2$AMIS_ID %in% negativeids)] <- 1

# Condition 3: IDs of those who had a male partner in past 6 months
sixmonthids <- as.numeric(unique(artnetLong[which(difftime(artnetLong$SUB_DATE, artnetLong$p_end.date, units = "days") <= 182), "AMIS_ID"])) #1951
artnet2$prep_part6mo[which(artnet2$AMIS_ID %in% sixmonthids)] <- 1


# Condition 4: Not in monogamous partnership with recently tested, HIV-negative man
### Version A: More than one ongoing partner
gt2partids <- as.numeric(unique(artnet$AMIS_ID[which(artnet$totdegree > 1)])) #608 men with >1 partners

### Version B: Partner number = 1 and partner is not negative
onepartids <- as.numeric(unique(artnet$AMIS_ID[which(artnet$totdegree == 1)])) #933 men with one partner
onepartids2 <- as.numeric(unique(artnetLong[which((artnetLong$AMIS_ID %in% onepartids) &
                       (artnetLong$partstatus == "Unknown" |
                          artnetLong$partstatus == "Positive" |
                          is.na(artnetLong$partstatus))), "AMIS_ID"])) # 446 men with one partner who is not HIV-negative
### Combine Version A and B
partners <- unique(c(gt2partids, onepartids2)) # 1054 men not in monog
artnet2$prep_nonmonog[which(artnet2$AMIS_ID %in% partners)] <- 1

# CAI in past 6 months
df <- artnetLong %>%
  # filter(AMIS_ID %in% prepeligv3) %>%
  filter(difftime(SUB_DATE, p_end.date) <= 182) %>%
  filter(p_anal.acts.week > 0) %>%
  filter(p_protected.anal.acts.week < p_anal.acts.week) %>%
  group_by(AMIS_ID)
cai <- as.numeric(unique(df$AMIS_ID)) #739 men who meet other criteria
artnet2$prep_uai[which(artnet2$AMIS_ID %in% cai)] <- 1

# Recent STI
recentsti <- unique(as.numeric(artnet2$AMIS_ID[which(artnet2$BSTIA == 1 | artnet2$BSTIB == 1 | artnet2$BSTIC == 1)])) #260 men
artnet2$prep_sti[which(artnet2$AMIS_ID %in% recentsti)] <- 1

# Common factors
prepeligv1 <- intersect(adults, negativeids) #1916 men >18 and negative
prepeligv2 <- intersect(prepeligv1, sixmonthids) #1725 men >18, negative, and active in last 6 months
prepeligv3 <- intersect(prepeligv2, partners) # 903 men >18, negative, active in last 6 months, non-monog
prepeligv4 <- intersect(prepeligv3, cai) # 903 men >18, negative, active in last 6 months, non-monog, CAI
prepeligv5 <- intersect(prepeligv3, recentsti) # 903 men >18, negative, active in last 6 months, non-monog, STI
prepeligv6 <- intersect(prepeligv3, c(cai, recentsti)) # 903 men >18, negative, active in last 6 months, non-monog, CAI or STI

# PrEP Indications
artnet2$prepind_uai[which(artnet2$AMIS_ID %in% prepeligv4)] <- 1
artnet2$prepind_sti[which(artnet2$AMIS_ID %in% prepeligv5)] <- 1
artnet2$prepind_any[which(artnet2$AMIS_ID %in% prepeligv6)] <- 1


# Summary Table ---------------
respondents <- rbind(nrow(artnet2), length(which(artnet2$race.cat == "white")),
                     length(which(artnet2$race.cat == "black")),
                     length(which(artnet2$race.cat == "hispanic")),
                     length(which(artnet2$race.cat == "other")),
                     length(which(artnet2$region == "Northeast")),
                     length(which(artnet2$region == "Midwest")),
                     length(which(artnet2$region == "South")),
                     length(which(artnet2$region == "West")),
                     length(which(artnet2$age.cat == "15-24")),
                     length(which(artnet2$age.cat == "25-34")),
                     length(which(artnet2$age.cat == "35-44")),
                     length(which(artnet2$age.cat == "45-54")),
                     length(which(artnet2$age.cat == "55-65")),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white")),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white")),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white")),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white")),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white")),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black")),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black")),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black")),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black")),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black")),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic")),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic")),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic")),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic")),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic")),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other")),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other")),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other")),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other")),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other")))

adultmen <- rbind(length(which(artnet2$prep_adult == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_adult == 1)),
                     length(which(artnet2$race.cat == "black" & artnet2$prep_adult == 1)),
                     length(which(artnet2$race.cat == "hispanic" & artnet2$prep_adult == 1)),
                     length(which(artnet2$race.cat == "other" & artnet2$prep_adult == 1)),
                  length(which(artnet2$region == "Northeast" & artnet2$prep_adult == 1)),
                  length(which(artnet2$region == "Midwest" & artnet2$prep_adult == 1)),
                  length(which(artnet2$region == "South" & artnet2$prep_adult == 1)),
                  length(which(artnet2$region == "West" & artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "15-24" & artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "25-34" & artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "35-44" & artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "45-54" & artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "55-65" & artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                    artnet2$prep_adult == 1)),
                     length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                    artnet2$prep_adult == 1)))

hivneg <- rbind(length(which(artnet2$prep_hiv == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$race.cat == "black" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$race.cat == "hispanic" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$race.cat == "other" & artnet2$prep_hiv == 1)),
                length(which(artnet2$region == "Northeast" & artnet2$prep_hiv == 1)),
                length(which(artnet2$region == "Midwest" & artnet2$prep_hiv == 1)),
                length(which(artnet2$region == "South" & artnet2$prep_hiv == 1)),
                length(which(artnet2$region == "West" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                 artnet2$prep_hiv == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                 artnet2$prep_hiv == 1)))

past6mos <- rbind(length(which(artnet2$prep_part6mo == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$race.cat == "black" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$race.cat == "hispanic" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$race.cat == "other" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$region == "Northeast" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$region == "Midwest" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$region == "South" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$region == "West" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                 artnet2$prep_part6mo == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                 artnet2$prep_part6mo == 1)))

nonmonog <- rbind(length(which(artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "black" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "hispanic" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "other" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "Northeast" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "Midwest" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "South" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "West" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                 artnet2$prep_nonmonog == 1)))
behav1to4 <- rbind(length(which(artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "black" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "hispanic" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$race.cat == "other" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "Northeast" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "Midwest" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "South" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$region == "West" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$prep_adult == 1 &
                                 artnet2$prep_hiv == 1 & artnet2$prep_part6mo &
                                 artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo &  artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                 artnet2$prep_adult == 1 &  artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                 artnet2$prep_adult == 1 & artnet2$prep_hiv == 1 &
                                 artnet2$prep_part6mo & artnet2$prep_nonmonog == 1)))

CAI <- rbind(length(which(artnet2$prep_uai == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_uai == 1)),
                  length(which(artnet2$race.cat == "black" & artnet2$prep_uai == 1)),
                  length(which(artnet2$race.cat == "hispanic" & artnet2$prep_uai == 1)),
                  length(which(artnet2$race.cat == "other" & artnet2$prep_uai == 1)),
             length(which(artnet2$region == "Northeast" & artnet2$prep_uai == 1)),
             length(which(artnet2$region == "Midwest" & artnet2$prep_uai == 1)),
             length(which(artnet2$region == "South" & artnet2$prep_uai == 1)),
             length(which(artnet2$region == "West" & artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                 artnet2$prep_uai == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                 artnet2$prep_uai == 1)))

sti <- rbind(length(which(artnet2$prep_sti == 1)),
                  length(which(artnet2$race.cat == "white" & artnet2$prep_sti == 1)),
                  length(which(artnet2$race.cat == "black" & artnet2$prep_sti == 1)),
                  length(which(artnet2$race.cat == "hispanic" & artnet2$prep_sti == 1)),
                  length(which(artnet2$race.cat == "other" & artnet2$prep_sti == 1)),
             length(which(artnet2$region == "Northeast" & artnet2$prep_sti == 1)),
             length(which(artnet2$region == "Midwest" & artnet2$prep_sti == 1)),
             length(which(artnet2$region == "South" & artnet2$prep_sti == 1)),
             length(which(artnet2$region == "West" & artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                 artnet2$prep_sti == 1)),
                  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                 artnet2$prep_sti == 1)))

commoncai <- rbind(length(which(artnet2$prepind_uai == 1)),
                   length(which(artnet2$race.cat == "white" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$race.cat == "black" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$race.cat == "hispanic" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$race.cat == "other" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$region == "Northeast" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$region == "Midwest" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$region == "South" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$region == "West" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                  artnet2$prepind_uai == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                  artnet2$prepind_uai == 1)))

commonsti <- rbind(length(which(artnet2$prepind_sti == 1)),
                   length(which(artnet2$race.cat == "white" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$race.cat == "black" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$race.cat == "hispanic" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$race.cat == "other" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$region == "Northeast" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$region == "Midwest" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$region == "South" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$region == "West" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                  artnet2$prepind_sti == 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                  artnet2$prepind_sti == 1)))

commonany <- rbind(length(which(artnet2$prepind_any == 1)),
                length(which(artnet2$race.cat == "white" & artnet2$prepind_any == 1)),
                length(which(artnet2$race.cat == "black" & artnet2$prepind_any == 1)),
                length(which(artnet2$race.cat == "hispanic" & artnet2$prepind_any == 1)),
                length(which(artnet2$race.cat == "other" & artnet2$prepind_any == 1)),
                length(which(artnet2$region == "Northeast" & artnet2$prepind_any == 1)),
                length(which(artnet2$region == "Midwest" & artnet2$prepind_any == 1)),
                length(which(artnet2$region == "South" & artnet2$prepind_any == 1)),
                length(which(artnet2$region == "West" & artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "15-24" & artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "25-34" & artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "35-44" & artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "45-54" & artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "55-65" & artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                               artnet2$prepind_any == 1)),
                length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                               artnet2$prepind_any == 1)))

dawndenom <- rbind(length(which(artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$race.cat == "white" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$race.cat == "black" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$race.cat == "hispanic" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$race.cat == "other" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$region == "Northeast" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$region == "Midwest" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$region == "South" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$region == "West" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$prep_hiv == 1 &
                                  artnet2$prep_adult == 1 & artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "white" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "black" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "hispanic" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "15-24" & artnet2$race.cat == "other" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)),
                   length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                                  artnet2$prep_hiv == 1 & artnet2$prep_adult == 1 &
                                  artnet2$ai.part >= 1)))

Indications <- cbind(respondents, adultmen, hivneg, past6mos, nonmonog, CAI,
                     sti, behav1to4, commoncai, commonsti, commonany, dawndenom)

colnames(Indications) <- c("Respondents", "1 - Adult Men", "2 - HIV-negative",
                       "3 - Male P6MO", "4 - Not monog w/ HIV-",
                       "5- CAI 6 months", "6 - Bacterial STI 6 months", "All Base (1-4)",
                       "1-4 + CAI", "1-4 + STI", "1-4 + Any", "Dawn denom")
rownames(Indications) <- c("All", "White", "Black", "Hisp", "Other",
                           "Northeast", "Midwest", "South", "West",
                           "15-24", "25-34", "35-44", "45-54", "55-65",
                           "W 15-24", "W 25-34", "W 35-44", "W 45-54", "W 55-65",
                           "B 15-24", "B 25-34", "B 35-44", "B 45-54", "B 55-65",
                           "H 15-24", "H 25-34", "H 35-44", "H 45-54", "H 55-65",
                           "O 15-24", "O 25-34", "O 35-44", "O 45-54", "O 55-65")

write.csv(Indications, file = "Tables/PrEPElig.csv")
