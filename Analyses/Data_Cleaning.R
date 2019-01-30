## ART-Net Study 2018   ##
## Data Cleaning Script ##
## 2019-01-29           ##


# De-duplicate AMIS by ID and EMAIL
# De-duplicate INTERMEDIATE by ID and EMAIL
# Add IP address de-duplication?


# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)

# Read in datasets ---------
artnet <- readRDS("Cleaned/ARTNet-vars-full.rda")
artnetLong <- readRDS("Cleaned/ARTNet-long-full.rda")
amis.2017 <- readRDS("Cleaned/AMIS-2017.rda")
amis.2018 <- readRDS("Cleaned/AMIS-2018.rda")
intermed.2017 <- readRDS("Cleaned/ARTNet-intermediate-2017.rda")
intermed.2018 <- readRDS("Cleaned/ARTNet-intermediate-2018.rda")

# Create merged AMIS dataset ---------------
# Subset to main variables
amis1 <- amis.2017 %>%
  select(subID, subID_CHAR, EMAIL, HISPANIC, RACEA, RACEB, RACEC, RACED, RACEE, RACEF, RACEG,
         RACEH, AGE, RCNTRSLT, EVRPOS, HLEDUCAT, zip_combined, EVERTEST, TEST2YRS)
amis2 <- amis.2018 %>%
  select(subID, subIDchar, amis_EMAIL, HISPANIC, RACEA, RACEB, RACEC, RACED, RACEE, RACEF, RACEG,
         RACEH, AGE, RCNTRSLT, EVRPOS, HLEDUCAT, zip_combined, EVERTEST, TEST2YRS) %>%
  rename(subID_CHAR = subIDchar, EMAIL = amis_EMAIL)
amis1$survey.year <- "2017"
amis2$survey.year <- "2018"
amis <- rbind(amis1, amis2) # 22382

# De-duplicate AMIS by ID
table(duplicated(amis.2017$subID))
table(duplicated(amis.2018$subID))
table(amis.2017$subID %in% amis.2018$subID)
table(amis.2018$subID %in% amis.2017$subID)


# Deduplicate AMIS dataset by email
blankemail <- amis[which(amis$EMAIL == ""), ] #13882
notblankemail <- amis[which(amis$EMAIL != ""), ] #8500

# Quantifying across-year duplicates
email.count <- as.data.frame(table(notblankemail[, c("EMAIL")])) #7640
email.count2 <- email.count[email.count$Freq > 1, ] # 849 duplicates

# Take the first record for each duplicate
a3 <- notblankemail[which(!duplicated(notblankemail$EMAIL, fromLast = FALSE)),] #7640

# Bind blank and de-duplicated non-blanks
amis <- rbind(blankemail, a3) #21522

# Create merged Intermediate dataset (with AMIS) ---------------
# Subset to main variables
intermed1 <- intermed.2017 %>%
  select(ID2017, ARTNETINTEMAIL2017) %>%
  rename(subID = ID2017, INTEMAIL = ARTNETINTEMAIL2017)
intermed2 <- intermed.2018 %>%
  select(ID2018, ARTNETINTEMAIL2018) %>%
  rename(subID = ID2018, INTEMAIL = ARTNETINTEMAIL2018)

intermed1$survey.year <- "2017"
intermed2$survey.year <- "2018"
amis <- rbind(amis1, amis2) # 22382


# Cross year ID issues?




# De-duplicate AMIS by ID
table(duplicated(intermed.2017$subID))
table(duplicated(intermed.2018$subID))
table(intermed.2017$subID %in% intermed.2018$subID)
table(intermed.2018$subID %in% intermed.2017$subID)

# Deduplicate AMIS dataset by email
blankemail <- amis[which(amis$EMAIL == ""), ] #13882
notblankemail <- amis[which(amis$EMAIL != ""), ] #8500

# Quantifying across-year duplicates
email.count <- as.data.frame(table(notblankemail[, c("EMAIL")])) #7640
email.count2 <- email.count[email.count$Freq > 1, ] # 849 duplicates

# Take the first record for each duplicate
a3 <- notblankemail[which(!duplicated(notblankemail$EMAIL, fromLast = FALSE)),] #7640

# Bind blank and de-duplicated non-blanks
amis <- rbind(blankemail, a3) #21522


# Join on subID_char
left_join


# Geomatching ------------------------------------
# Read in Rural/Urban urbanicity codes (https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/)
all_geoc <- read_excel("Input/ruralurbancodes2013.xls")
all_geoc <- all_geoc[, c("FIPS", "State", "County_Name", "RUCC_2013", "RUCC_Description")]

# Read ZIPtoCounty data (https://www.huduser.gov/portal/datasets/usps_crosswalk.html)
ZIPCounty <- read_xlsx("Input/HUD_ZIP_COUNTY_092017.xlsx")
length(unique(ZIPCounty$ZIP)) #52,894 rows, but 39,454 unique ZIPs, others are in multiple FIPS codes
length(unique(ZIPCounty$COUNTYFIPS)) # 3,225 unique counties

# Join ZIP Code by county FIPS code
merged <- inner_join(ZIPCounty, all_geoc, by = c("COUNTYFIPS" = "FIPS")) #52,876 rows
nocountymatch <- anti_join(ZIPCounty, all_geoc, by = c("COUNTYFIPS" = "FIPS")) #18 with no match to county FIPS
table(nocountymatch$STATEFIPS) # 4 geographies with no match,
#2 in Alaska (02), 4 in Guam (66), 7 in South Dakota (46), 5 in US Virgin Islands (78)

# Merge NCHS Coding
# #(ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/OAE/urbanrural/NCHSURCodes2013.txt)
#https://www.cdc.gov/nchs/data/oae/NCHSUrbruralFileDocumentation.pdf
NCHS <- read_xlsx("Input/NCHSData.xlsx")
NCHS <- NCHS[, c("GEOID_CHAR", "NCHS_2013")]
NCHS$NCHSCHAR <- rep(NA, nrow(NCHS))
NCHS$NCHSCHARc[NCHS$NCHS_2013 == 1] <- "Large Central Metro"
NCHS$NCHSCHAR[NCHS$NCHS_2013 == 2] <- "Large Fringe Metro"
NCHS$NCHSCHAR[NCHS$NCHS_2013 == 3] <- "Medium Metro"
NCHS$NCHSCHAR[NCHS$NCHS_2013 == 4] <- "Small Metro"
NCHS$NCHSCHAR[NCHS$NCHS_2013 == 5] <- "Micropolitan"
NCHS$NCHSCHAR[NCHS$NCHS_2013 == 6] <- "Noncore"
merged2 <- inner_join(merged, NCHS, by = c("COUNTYFIPS" = "GEOID_CHAR")) #52,567 rows
nonchs <- anti_join(merged, NCHS, by = c("COUNTYFIPS" = "GEOID_CHAR")) # All PR

# Merge Reg/Division code (https://www2.census.gov/geo/docs/maps-data/maps/reg_div.txt,
regdiv <- read_xlsx("Input/RegDiv.xlsx")
merged3 <- inner_join(merged2, regdiv, by = c("STATEFIPS" = "STATEFIPS")) #52,567 rows
anti_join(merged3, regdiv, by = c("STATEFIPS" = "STATEFIPS")) #0 rows

# Merge Geodata with Participant data ----------------
# Add on leading zeroes - New England issue
amis$ZIP_CHAR <- as.character(amis$zip_combined) # 837 with less than 5
amis$ZIP_CHAR[which(nchar(amis$ZIP_CHAR) == 4)] <- paste0("0", amis$ZIP_CHAR[which(nchar(amis$ZIP_CHAR) == 4)])
amis$ZIP_CHAR[which(nchar(amis$ZIP_CHAR) == 3)] <- paste0("00", amis$ZIP_CHAR[which(nchar(amis$ZIP_CHAR) == 3)])

intermed$ZIP_CHAR <- as.character(intermed$zip_combined) # 837 with less than 5
intermed$ZIP_CHAR[which(nchar(intermed$ZIP_CHAR) == 4)] <- paste0("0", intermed$ZIP_CHAR[which(nchar(intermed$ZIP_CHAR) == 4)])
intermed$ZIP_CHAR[which(nchar(intermed$ZIP_CHAR) == 3)] <- paste0("00", intermed$ZIP_CHAR[which(nchar(intermed$ZIP_CHAR) == 3)])

leftamis <- left_join(amis, merged3,
                  by = c("ZIP_CHAR" = "ZIP")) # 14,241 rows, up from 11,065

inneramis <- inner_join(amis, merged3,
                    by = c("ZIP_CHAR" = "ZIP")) # 14,139 rows, up from 11065

antiamis <- anti_join(amis, merged3,
                        by = c("ZIP_CHAR" = "ZIP")) # 102 rows, up from 11065
#View(antiamis[, c("ZIP_CHAR", "zip_combined", "ZIPCODE", "sg_zip")])

length(unique(inneramis$subID_CHAR)) #10963


intermed$SUBID_CHAR <- as.character(intermed$subID)
leftintermed <- left_join(intermed, merged3,
                      by = c("ZIP_CHAR" = "ZIP")) # 4633 rows, up from 3776

innerintermed <- inner_join(intermed, merged3,
                        by = c("ZIP_CHAR" = "ZIP")) # 4622 rows, up from 3776
antiiintermed <- anti_join(intermed, merged3,
                            by = c("ZIP_CHAR" = "ZIP")) # 11 rows, up from 3776
#View(antiiintermed[, c("ZIP_CHAR", "zip_combined", "ZIPCODE", "sg_zip")])
length(unique(innerintermed$subID)) #3764

# look for unmatched ZIPs (possibly junk ZIPS?)
antiamis <- anti_join(amis, merged3,
                    by = c("ZIP_CHAR" = "ZIP")) # 102 rows

antiintermed <- anti_join(intermed, merged3,
                      by = c("ZIP_CHAR" = "ZIP")) # 11 rows
table(antiamis$ZIP_CHAR)
table(antiintermed$ZIP_CHAR)
# Compare to ZIPs (https://m.usps.com/m/ZipLookupAction?search=zip)

# correcting for the above issue - making ZIP_CHAR blank
antiamis$ZIP_CHAR <- intermed$ZIP_CHAR <- ""
antiamis_joined <- left_join(antiamis, merged3,
                         by = c("ZIP_CHAR" = "ZIP"))
antiintermed_joined <- left_join(antiintermed, merged3,
                             by = c("ZIP_CHAR" = "ZIP"))


amis <- rbind(inneramis, antiamis_joined) # Now 14241, up from 11065
intermed <- rbind(innerintermed, antiintermed_joined) # Now 4633, up from 3776

# Deduplicate ZIPS
dupesamis <- amis[which(duplicated(amis$AMIS_ID)), ]
dupedamis <- amis[which(amis$AMIS_ID %in%
                          dupesamis$AMIS_ID), ]
undupedamis <- amis[which(!(amis$AMIS_ID %in%
                              dupesamis$AMIS_ID)), ]

dupesintermed <- intermed[which(duplicated(intermed$AMIS_ID)), ]
dupedintermed <- intermed[which(intermed$AMIS_ID %in%
                          dupesintermed$AMIS_ID), ]
undupedintermed <- intermed[which(!(intermed$AMIS_ID %in%
                              dupesintermed$AMIS_ID)), ]

length(unique(dupesamis$AMIS_ID)) # 2492 unique ids for 5688 observations (duplicated ZIPs)
length(unique(dupesintermed$AMIS_ID)) # 689 unique ids for 1547 observations (duplicated ZIPs)

# Take 1st county if in multiple counties
amis <- rbind(undupedamis, dupedamis[which(!(duplicated(dupedamis$AMIS_ID))), ]) # get back to 11065
intermed <- rbind(undupedintermed, dupedintermed[which(!(duplicated(dupedintermed$AMIS_ID))), ]) # get back to 3775


# Clean ART-Net ------------

# Age category
artnet$age.cat <- rep(NA, nrow(artnet))
artnet$age.cat[artnet$AGE2 >= 15 & artnet$AGE2 <= 24] <- "15-24"
artnet$age.cat[artnet$AGE2 >= 25 & artnet$AGE2 <= 34] <- "25-34"
artnet$age.cat[artnet$AGE2 >= 35 & artnet$AGE2 <= 44] <- "35-44"
artnet$age.cat[artnet$AGE2 >= 45 & artnet$AGE2 <= 54] <- "45-54"
artnet$age.cat[artnet$AGE2 >= 55 & artnet$AGE2 <= 65] <- "55-65"
artnet$age.cat[artnet$AGE2 > 65] <- "66+"

artnet$old[artnet$AGE2 <= median(artnet$AGE2)] <- 0
artnet$old[artnet$AGE2 > median(artnet$AGE2)] <- 1

# Region/Divisions
#Div: 1-New England, 2-Middle Atlantic, 3-East North Central, 4-West North Central
#5-South Atlantic, 6-East South Central, 7- West South Central, 8-Mountain
#9 - Pacific
# Region: 1-Northeast, 2-Midwest, 3-South, 4-West
artnet$region <- rep(NA, nrow(artnet))
artnet$region[artnet$REGCODE == 1] <- "Northeast"
artnet$region[artnet$REGCODE == 2] <- "Midwest"
artnet$region[artnet$REGCODE == 3] <- "South"
artnet$region[artnet$REGCODE == 4] <- "West"
artnet$division <- rep(NA, nrow(artnet))
artnet$division[artnet$DIVCODE == 1] <- "New England"
artnet$division[artnet$DIVCODE == 2] <- "Middle Atlantic"
artnet$division[artnet$DIVCODE == 3] <- "East North Central"
artnet$division[artnet$DIVCODE == 4] <- "West North Central"
artnet$division[artnet$DIVCODE == 5] <- "South Atlantic"
artnet$division[artnet$DIVCODE == 6] <- "East South Central"
artnet$division[artnet$DIVCODE == 7] <- "West South Central"
artnet$division[artnet$DIVCODE == 8] <- "Mountain"
artnet$division[artnet$DIVCODE == 9] <- "Pacific"

# Education
artnet$education <- rep(NA, nrow(artnet))
artnet$education[artnet$HLEDUCAT %in% c(0, 1, 2)] <- "Less than High School"
artnet$education[artnet$HLEDUCAT == 3] <- "High School Graduate"
artnet$education[artnet$HLEDUCAT == 4] <- "Some College or Associates/Technical"
artnet$education[artnet$HLEDUCAT == 5] <- "College or Greater"

# Urbanicity
artnet$NCHSCHAR <- rep(NA, nrow(artnet))
artnet$NCHSCHAR[artnet$NCHS_2013 == 1] <- "Large Central Metro"
artnet$NCHSCHAR[artnet$NCHS_2013 == 2] <- "Large Fringe Metro"
artnet$NCHSCHAR[artnet$NCHS_2013 == 3] <- "Medium Metro"
artnet$NCHSCHAR[artnet$NCHS_2013 == 4] <- "Small Metro"
artnet$NCHSCHAR[artnet$NCHS_2013 == 5] <- "Micropolitan"
artnet$NCHSCHAR[artnet$NCHS_2013 == 6] <- "Noncore"

# HIV Testing
artnet$hivtest <- rep(NA, nrow(artnet))
artnet$hivtest[artnet$EVERTEST == 1] <- "Have Tested"
artnet$hivtest[artnet$EVERTEST == 0] <- "Have Never Tested"

# Sexual Role
artnet$roletype  <- rep(NA, nrow(artnet))
recept <- which(artnet$PART1RAI == 1 | artnet$PART2RAI == 1 |
                  artnet$PART3RAI == 1 | artnet$PART4RAI == 1 |
                  artnet$PART5RAI == 1)
insert <- which(artnet$PART1IAI == 1 | artnet$PART2IAI == 1 |
                  artnet$PART3IAI == 1 | artnet$PART4IAI == 1 |
                  artnet$PART5IAI == 1)
vers <- intersect(recept, insert)
receptonly <- setdiff(recept, vers)
insertonly <- setdiff(insert, vers)
artnet$roletype[receptonly] <- "Receptive Only"
artnet$roletype[insertonly] <- "Insertive Only"
artnet$roletype[vers] <- "Versatile"

# Clean Intermediate ------------------------

## Race/Ethnicity
intermed$hispan <- ifelse(intermed$HISPANIC == 1, 1, 0)
intermed$race.mult <- ifelse(intermed$RACEA + intermed$RACEB + intermed$RACEC + intermed$RACED + intermed$RACEE > 1, 1, 0)
intermed$race <- rep(NA, nrow(intermed))
intermed$race[intermed$race.mult == 1] <- "mult"
intermed$race[intermed$race.mult == 0 & intermed$RACEA == 1] <- "ai/an"
intermed$race[intermed$race.mult == 0 & intermed$RACEB == 1] <- "asian"
intermed$race[intermed$race.mult == 0 & intermed$RACEC == 1] <- "black"
intermed$race[intermed$race.mult == 0 & intermed$RACED == 1] <- "nh/pi"
intermed$race[intermed$race.mult == 0 & intermed$RACEE == 1] <- "white"
intermed$race[intermed$race.mult == 0 & (intermed$RACEF == 1 | intermed$RACEG == 1 | intermed$RACEH == 1)] <- "other"
intermed$race.cat <- rep("other", nrow(intermed))
intermed$race.cat[intermed$hispan == 1] <- "hispanic"
intermed$race.cat[intermed$hispan == 0 & intermed$race == "black"] <- "black"
intermed$race.cat[intermed$hispan == 0 & intermed$race == "white"] <- "white"
table(intermed$race.cat)

## HIV Status - Can be edited
intermed$hiv <- 0
intermed$hiv[intermed$RCNTRSLT %in% c(7, 9)] <- NA
intermed$hiv[intermed$RCNTRSLT == 2 | intermed$EVRPOS == 1] <- 1
table(intermed$hiv)

## Age cat
intermed$age.cat <- rep(NA, nrow(intermed))
intermed$age.cat[intermed$AGE >= 15 & intermed$AGE <= 24] <- "15-24"
intermed$age.cat[intermed$AGE >= 25 & intermed$AGE <= 34] <- "25-34"
intermed$age.cat[intermed$AGE >= 35 & intermed$AGE <= 44] <- "35-44"
intermed$age.cat[intermed$AGE >= 45 & intermed$AGE <= 54] <- "45-54"
intermed$age.cat[intermed$AGE >= 55 & intermed$AGE <= 65] <- "55-65"
intermed$age.cat[intermed$AGE > 65] <- "66+"

## Region/Division
intermed$region <- rep(NA, nrow(intermed))
intermed$region[intermed$REGCODE == 1] <- "Northeast"
intermed$region[intermed$REGCODE == 2] <- "Midwest"
intermed$region[intermed$REGCODE == 3] <- "South"
intermed$region[intermed$REGCODE == 4] <- "West"
intermed$division <- rep(NA, nrow(intermed))
intermed$division[intermed$DIVCODE == 1] <- "New England"
intermed$division[intermed$DIVCODE == 2] <- "Middle Atlantic"
intermed$division[intermed$DIVCODE == 3] <- "East North Central"
intermed$division[intermed$DIVCODE == 4] <- "West North Central"
intermed$division[intermed$DIVCODE == 5] <- "South Atlantic"
intermed$division[intermed$DIVCODE == 6] <- "East South Central"
intermed$division[intermed$DIVCODE == 7] <- "West South Central"
intermed$division[intermed$DIVCODE == 8] <- "Mountain"
intermed$division[intermed$DIVCODE == 9] <- "Pacific"

# Education
intermed$education <- rep(NA, nrow(intermed))
intermed$education[intermed$HLEDUCAT %in% c(0, 1, 2)] <- "Less than High School"
intermed$education[intermed$HLEDUCAT == 3] <- "High School Graduate"
intermed$education[intermed$HLEDUCAT == 4] <- "Some College or Associates/Technical"
intermed$education[intermed$HLEDUCAT == 5] <- "College or Greater"

# HIV Testing
intermed$hivtest <- rep(NA, nrow(intermed))
intermed$hivtest[intermed$EVERTEST == 1] <- "Have Tested"
intermed$hivtest[intermed$EVERTEST == 0] <- "Have Never Tested"


# Clean AMIS ------------------------

## Race/Ethnicity
amis$hispan <- ifelse(amis$HISPANIC == 1, 1, 0)
amis$race.mult <- ifelse(amis$RACEA + amis$RACEB + amis$RACEC + amis$RACED + amis$RACEE > 1, 1, 0)
amis$race <- rep(NA, nrow(amis))
amis$race[amis$race.mult == 1] <- "mult"
amis$race[amis$race.mult == 0 & amis$RACEA == 1] <- "ai/an"
amis$race[amis$race.mult == 0 & amis$RACEB == 1] <- "asian"
amis$race[amis$race.mult == 0 & amis$RACEC == 1] <- "black"
amis$race[amis$race.mult == 0 & amis$RACED == 1] <- "nh/pi"
amis$race[amis$race.mult == 0 & amis$RACEE == 1] <- "white"
amis$race[amis$race.mult == 0 & (amis$RACEF == 1 | amis$RACEG == 1 | amis$RACEH == 1)] <- "other"
amis$race.cat <- rep("other", nrow(amis))
amis$race.cat[amis$hispan == 1] <- "hispanic"
amis$race.cat[amis$hispan == 0 & amis$race == "black"] <- "black"
amis$race.cat[amis$hispan == 0 & amis$race == "white"] <- "white"
table(amis$race.cat)

## HIV Status - Can be edited
amis$hiv <- 0
amis$hiv[amis$RCNTRSLT %in% c(7, 9)] <- NA
amis$hiv[amis$RCNTRSLT == 2 | amis$EVRPOS == 1] <- 1
table(amis$hiv)

## Age cat
amis$age.cat <- rep(NA, nrow(amis))
amis$age.cat[amis$AGE >= 15 & amis$AGE <= 24] <- "15-24"
amis$age.cat[amis$AGE >= 25 & amis$AGE <= 34] <- "25-34"
amis$age.cat[amis$AGE >= 35 & amis$AGE <= 44] <- "35-44"
amis$age.cat[amis$AGE >= 45 & amis$AGE <= 54] <- "45-54"
amis$age.cat[amis$AGE >= 55 & amis$AGE <= 65] <- "55-65"
amis$age.cat[amis$AGE > 65] <- "66+"

## Region/Division
amis$region <- rep(NA, nrow(amis))
amis$region[amis$REGCODE == 1] <- "Northeast"
amis$region[amis$REGCODE == 2] <- "Midwest"
amis$region[amis$REGCODE == 3] <- "South"
amis$region[amis$REGCODE == 4] <- "West"
amis$division <- rep(NA, nrow(amis))
amis$division[amis$DIVCODE == 1] <- "New England"
amis$division[amis$DIVCODE == 2] <- "Middle Atlantic"
amis$division[amis$DIVCODE == 3] <- "East North Central"
amis$division[amis$DIVCODE == 4] <- "West North Central"
amis$division[amis$DIVCODE == 5] <- "South Atlantic"
amis$division[amis$DIVCODE == 6] <- "East South Central"
amis$division[amis$DIVCODE == 7] <- "West South Central"
amis$division[amis$DIVCODE == 8] <- "Mountain"
amis$division[amis$DIVCODE == 9] <- "Pacific"

# Education
amis$education <- rep(NA, nrow(amis))
amis$education[amis$HLEDUCAT %in% c(0, 1, 2)] <- "Less than High School"
amis$education[amis$HLEDUCAT == 3] <- "High School Graduate"
amis$education[amis$HLEDUCAT == 4] <- "Some College or Associates/Technical"
amis$education[amis$HLEDUCAT == 5] <- "College or Greater"

# HIV Testing
amis$hivtest <- rep(NA, nrow(amis))
amis$hivtest[amis$EVERTEST == 1] <- "Have Tested"
amis$hivtest[amis$EVERTEST == 0] <- "Have Never Tested"

## Check for NA Values ------------------
# NAs
table(artnet$race.cat, useNA = "always") # 0 NA
table(artnet$AGE2, useNA = "always") # 0 NA
table(artnet$age.cat, useNA = "always") # 0 NA
table(artnet$old, useNA = "always") # 0 NA
table(artnet$region, useNA = "always") # 0 NA
table(artnet$division, useNA = "always") # 0 NA
table(artnet$education, useNA = "always") # 12 NA
table(artnet$roletype, useNA = "always") # 377 NA
table(artnet$hivtest, useNA = "always") # 24 NA

table(amis$hiv, useNA = "always") # 47 NA
table(amis$race.cat, useNA = "always") # 0 NA
table(amis$age.cat, useNA = "always") # 0 NA
table(amis$region, useNA = "always") # 102 NA
table(amis$division, useNA = "always") # 102 NA
table(amis$education, useNA = "always") # 171 NA
table(amis$hivtest, useNA = "always") # 172 NA

table(intermed$hiv, useNA = "always") # 16 NA
table(intermed$race.cat, useNA = "always") # 0 NA
table(intermed$age.cat, useNA = "always") # 0 NA
table(intermed$region, useNA = "always") # 11 NA
table(intermed$division, useNA = "always") # 11 NA
table(intermed$education, useNA = "always") # 32 NA
table(intermed$hivtest, useNA = "always") # 43 NA


# Clean ART-Net Long ------------

# HIV status
artnetLong$partstatus <- rep(NA, nrow(artnetLong))
artnetLong$partstatus[artnetLong$p_HIV == 2] <- "Negative"
artnetLong$partstatus[artnetLong$p_HIV == 1] <- "Positive"
artnetLong$partstatus[artnetLong$p_HIV %in% c(3, 4, 88, 99)] <- "Unknown"


artnetLong$partstatuses <- rep(NA, nrow(artnetLong))
artnetLong$partstatuses[artnetLong$hiv == 0 & artnetLong$p_HIV == 2] <- "Negative - Negative"
artnetLong$partstatuses[(artnetLong$hiv == 0 & artnetLong$p_HIV == 1) |
                                (artnetLong$hiv == 1 & artnetLong$p_HIV == 2)] <- "Negative - Positive"
artnetLong$partstatuses[artnetLong$hiv == 0 & artnetLong$p_HIV %in% c(3, 4, 88, 99)] <- "Negative - Unknown"
artnetLong$partstatuses[artnetLong$hiv == 1 & artnetLong$p_HIV == 1] <- "Positive - Positive"
artnetLong$partstatuses[artnetLong$hiv == 1 & artnetLong$p_HIV %in% c(3, 4, 88, 99)] <- "Positive - Unknown"
table(artnetLong$partstatuses, useNA = "always")


# Age category (ego)
artnetLong$old <- rep(NA, nrow(artnetLong))
artnetLong$old[artnetLong$age <= median(artnetLong$age)] <- "Young"
artnetLong$old[artnetLong$age > median(artnetLong$age)] <- "Old"

# Clean up partner age
artnetLong$p_AGE <- as.numeric(artnetLong$p_AGE)
artnetLong$p_AGE[is.na(artnetLong$p_AGE) | artnetLong$p_AGE == 0] <- NA

# Age category
artnetLong$age.cat <- rep(NA, nrow(artnetLong))
artnetLong$age.cat[artnetLong$age >= 15 & artnetLong$age <= 24] <- "15-24"
artnetLong$age.cat[artnetLong$age >= 25 & artnetLong$age <= 34] <- "25-34"
artnetLong$age.cat[artnetLong$age >= 35 & artnetLong$age <= 44] <- "35-44"
artnetLong$age.cat[artnetLong$age >= 45 & artnetLong$age <= 54] <- "45-54"
artnetLong$age.cat[artnetLong$age >= 55 & artnetLong$age <= 65] <- "55-65"
artnetLong$age.cat[artnetLong$age > 65] <- "66+"

# Partner age category
artnetLong$partage.cat <- rep(NA, nrow(artnetLong))
artnetLong$partage.cat[artnetLong$p_AGE >= 15 & artnetLong$p_AGE <= 24] <- "15-24"
artnetLong$partage.cat[artnetLong$p_AGE >= 25 & artnetLong$p_AGE <= 34] <- "25-34"
artnetLong$partage.cat[artnetLong$p_AGE >= 35 & artnetLong$p_AGE <= 44] <- "35-44"
artnetLong$partage.cat[artnetLong$p_AGE >= 45 & artnetLong$p_AGE <= 54] <- "45-54"
artnetLong$partage.cat[artnetLong$p_AGE >= 55 & artnetLong$p_AGE <= 65] <- "55-65"
artnetLong$partage.cat[artnetLong$p_AGE > 65] <- "66+"

# Young/Old Classification
artnetLong$edgeage <- rep(NA, nrow(artnetLong))
artnetLong$edgeage[artnetLong$age < median(artnetLong$age) & artnetLong$p_AGE >= median(artnetLong$age)] <- "Young-Old"
artnetLong$edgeage[artnetLong$age < median(artnetLong$age) & artnetLong$p_AGE < median(artnetLong$age)] <- "Young-Young"
artnetLong$edgeage[artnetLong$age >= median(artnetLong$age) & artnetLong$p_AGE >= median(artnetLong$age)] <- "Old-Old"

# Age difference
artnetLong$edgeagediff <- rep(NA, nrow(artnetLong))
artnetLong$edgeagediff <- abs(artnetLong$age - artnetLong$p_AGE)
artnetLong$sqrtedgeagediff <- rep(NA, nrow(artnetLong))
artnetLong$sqrtedgeagediff <- abs(sqrt(artnetLong$age) - sqrt(artnetLong$p_AGE))

# Age combinations
artnetLong$partages <- rep(NA, nrow(artnetLong))
artnetLong$partages[!(is.na(artnetLong$age)) & !(is.na(artnetLong$partage.cat))] <- "Unmatched"
artnetLong$partages[artnetLong$age.cat == "15-24" & artnetLong$partage.cat == "15-24"] <- "15-24"
artnetLong$partages[artnetLong$age.cat == "25-34" & artnetLong$partage.cat == "25-34"] <- "25-34"
artnetLong$partages[artnetLong$age.cat == "35-44" & artnetLong$partage.cat == "35-44"] <- "35-44"
artnetLong$partages[artnetLong$age.cat == "45-54" & artnetLong$partage.cat == "45-54"] <- "45-54"
artnetLong$partages[artnetLong$age.cat == "55-65" & artnetLong$partage.cat == "55-65"] <- "55-65"


# Race of partner
artnetLong$parthisp <- rep(NA, nrow(artnetLong))
artnetLong$parthisp <- ifelse(artnetLong$p_HISP == 1, 1, 0)
table(artnetLong$parthisp)

artnetLong$partrace <- rep(NA, nrow(artnetLong))
artnetLong$partrace[artnetLong$p_RACE %in% c(1, 4, 5, 6, 7)] <- "other"
artnetLong$partrace[artnetLong$p_RACE == 2] <- "black"
artnetLong$partrace[artnetLong$p_RACE == 3] <- "white"

artnetLong$partracecat <- rep(NA, nrow(artnetLong))
artnetLong$partracecat[artnetLong$parthisp == 1] <- "hispanic"
artnetLong$partracecat[artnetLong$parthisp == 0 & artnetLong$partrace == "black"] <- "black"
artnetLong$partracecat[artnetLong$parthisp == 0 & artnetLong$partrace == "white"] <- "white"
artnetLong$partracecat[artnetLong$parthisp == 0 & artnetLong$partrace == "other"] <- "other"

# Dyad Race Combination
artnetLong$partraces <- rep(NA, nrow(artnetLong))
artnetLong$partraces[which(artnetLong$race.cat == "black" &
                             artnetLong$partracecat == "black")] <- "Black - Black"
artnetLong$partraces[(artnetLong$race.cat == "black" &
                        artnetLong$partracecat == "hispanic") |
                             (artnetLong$race.cat == "hispanic" &
                                artnetLong$partracecat == "black")] <- "Black - Hispanic"
artnetLong$partraces[(artnetLong$race.cat == "black" &
                        artnetLong$partracecat == "other") |
                             (artnetLong$race.cat == "other" &
                                artnetLong$partracecat == "black")] <- "Black - Other"
artnetLong$partraces[(artnetLong$race.cat == "black" &
                        artnetLong$partracecat == "white") |
                             (artnetLong$race.cat == "white" &
                                artnetLong$partracecat == "black")] <- "Black - White"
artnetLong$partraces[(artnetLong$race.cat == "hispanic" &
                        artnetLong$partracecat == "hispanic")] <- "Hispanic - Hispanic"
artnetLong$partraces[(artnetLong$race.cat == "hispanic" &
                        artnetLong$partracecat == "other") |
                             (artnetLong$race.cat == "other" &
                                artnetLong$partracecat == "hispanic")] <- "Hispanic - Other"
artnetLong$partraces[(artnetLong$race.cat == "hispanic" &
                        artnetLong$partracecat == "white") |
                             (artnetLong$race.cat == "white" &
                                artnetLong$partracecat == "hispanic")] <- "Hispanic - White"
artnetLong$partraces[(artnetLong$race.cat == "other" &
                        artnetLong$partracecat == "other")] <- "Other - Other"
artnetLong$partraces[(artnetLong$race.cat == "other" &
                        artnetLong$partracecat == "white") |
                             (artnetLong$race.cat == "white" &
                                artnetLong$partracecat == "other")] <- "Other - White"
artnetLong$partraces[(artnetLong$race.cat == "white" &
                        artnetLong$partracecat == "white")] <- "White - White"
table(artnetLong$partraces, useNA = "always")


table(artnetLong$partrace, artnetLong$parthisp)

# Region/Divisions
#Div: 1-New England, 2-Middle Atlantic, 3-East North Central, 4-West North Central
#5-South Atlantic, 6-East South Central, 7- West South Central, 8-Mountain
#9 - Pacific
# Region: 1-Northeast, 2-Midwest, 3-South, 4-West
artnetLong$region <- rep(NA, nrow(artnetLong))
artnetLong$region[artnetLong$DIVCODE %in% c(1, 2)] <- "Northeast"
artnetLong$region[artnetLong$DIVCODE %in% c(3, 4)] <- "Midwest"
artnetLong$region[artnetLong$DIVCODE %in% c(5, 6, 7)] <- "South"
artnetLong$region[artnetLong$DIVCODE %in% c(8, 9)] <- "West"
artnetLong$division <- rep(NA, nrow(artnetLong))
artnetLong$division[artnetLong$DIVCODE == 1] <- "New England"
artnetLong$division[artnetLong$DIVCODE == 2] <- "Middle Atlantic"
artnetLong$division[artnetLong$DIVCODE == 3] <- "East North Central"
artnetLong$division[artnetLong$DIVCODE == 4] <- "West North Central"
artnetLong$division[artnetLong$DIVCODE == 5] <- "South Atlantic"
artnetLong$division[artnetLong$DIVCODE == 6] <- "East South Central"
artnetLong$division[artnetLong$DIVCODE == 7] <- "West South Central"
artnetLong$division[artnetLong$DIVCODE == 8] <- "Mountain"
artnetLong$division[artnetLong$DIVCODE == 9] <- "Pacific"

# NAs
# table(artnetLong$race.cat, useNA = "always") # 0 NA
# table(artnetLong$AGE2, useNA = "always") # 0 NA
# table(artnetLong$age.cat, useNA = "always") # 0 NA
# table(artnetLong$old, useNA = "always") # 0 NA
table(artnetLong$edgeagediff, useNA = "always") #683 NA

# Dyad-level variables

