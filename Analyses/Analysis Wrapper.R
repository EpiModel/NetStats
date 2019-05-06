## Analysis Wrapper
## Install ART-Net Data Package
remotes::install_github("EpiModel/ARTnetData", ref = "f07ba02")

## Demographic characteristics
source("Analyses/Table 1.R", echo = TRUE)

## Partnership Type-Stratified Sexual Network Degree by Demographics and HIV Status
source("Analyses/Table 2.R", echo = TRUE)

## Duration (in Weeks) of Ongoing Sexual Partnerships by Demographics and HIV Status
source("Analyses/Table 3.R", echo = TRUE)

## Assortative Mixing of Sexual Partnerships by Race/Ethnicity, Age, and HIV Status, Stratified by Partnership Type
source("Analyses/Table 4.R", echo = TRUE)

## Matrix of Ongoing Partnerships
source("Analyses/Table 5.R", echo = TRUE)

## Supplementary Tables
source("Analyses/Supp Tables.R", echo = TRUE)

## Create Shiny Dataset
source("Analyses/Create Shiny.R", echo = TRUE)

## Figures
source("Analyses/Figures.R", echo = TRUE)

## Run PrEP eligibility analysis
source("Analyses/PrEP_Eligibility_Analysis.R", echo = TRUE)


