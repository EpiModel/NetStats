artnetvars <- readRDS("C:/Users/ramya/Box Sync/ART-Net/Data/Cleaned/output/ARTNet-vars.rda")

artnetvars$age.cat <- rep(NA, nrow(artnetvars))
artnetvars$age.cat[artnetvars$age >= 15 & artnetvars$age <= 24] <- "15-24"
artnetvars$age.cat[artnetvars$age >= 25 & artnetvars$age <= 34] <- "25-34"
artnetvars$age.cat[artnetvars$age >= 35 & artnetvars$age <= 44] <- "35-44"
artnetvars$age.cat[artnetvars$age >= 45 & artnetvars$age <= 54] <- "45-54"
artnetvars$age.cat[artnetvars$age >= 55 & artnetvars$age <= 65] <- "55-65"
artnetvars$age.cat[artnetvars$age > 65] <- "66+"


dim(artnetvars)
str(artnetvars)

#SUMMARY MEASURES

#NUMBER OF TIMES TESTED IN THE PAST TWO YEARS
meantest2years<-mean(artnetvars$STITEST_2YR, na.rm = TRUE)
meantest2years

mediantest2years<-median(artnetvars$STITEST_2YR, na.rm = TRUE)
mediantest2years

sdtest2years<-sd(artnetvars$STITEST_2YR, na.rm=TRUE)
sdtest2years

q3test2years<-quantile(artnetvars$STITEST_2YR,probs=.75,na.rm=TRUE)
q3test2years

q1test2years<-quantile(artnetvars$STITEST_2YR,probs=.25,na.rm=TRUE)
q1test2years

IQRtest2years<-q3test2years-q1test2years
IQRtest2years

lconfidenceinterval<-mean(artnetvars$STITEST_2YR,na.rm = TRUE) - (qnorm(.975)*(sd(artnetvars$STITEST_2YR,na.rm =TRUE)/sqrt(n())))
lconfidenceinterval

uconfidenceinterval<-mean(artnetvars$STITEST_2YR,na.rm = TRUE) + (qnorm(.975)*(sd(artnetvars$STITEST_2YR,na.rm =TRUE)/sqrt(n())))
uconfidenceinterval

#Predictor variables

#Number of partners in past year
partnernumber<-mean(artnetvars$cuml.pnum, na.rm = TRUE)
partnernumber

medianpartnumber<-median(artnetvars$cuml.pnum, na.rm = TRUE)
medianpartnumber

sdpartnumber<-sd(artnetvars$cuml.pnum, na.rm =TRUE)
sdpartnumber

q3partnumber<-quantile(artnetvars$cuml.pnum,probs=.75,na.rm=TRUE)
q3partnumber

q1partnumber<-quantile(artnetvars$cuml.pnum,probs=.25,na.rm=TRUE)
q1partnumber

IQRpartnumber<-q3partnumber-q1partnumber
IQRpartnumber


#Total Partners
meantotalpartnernumber<-mean(artnetvars$M_MP12OANUM, na.rm = TRUE)
meantotalpartnernumber

mediantotalpartnumber<-median(artnetvars$M_MP12OANUM, na.rm = TRUE)
mediantotalpartnumber

sdtotalpartnumber<-sd(artnetvars$M_MP12OANUM, na.rm =TRUE)
sdtotalpartnumber

q3totalpartnumber<-quantile(artnetvars$M_MP12OANUM,probs=.75,na.rm=TRUE)
q3totalpartnumber

q1totalpartnumber<-quantile(artnetvars$M_MP12OANUM,probs=.25,na.rm=TRUE)
q1totalpartnumber

IQRtotalpartnumber<-q3totalpartnumber-q1totalpartnumber
IQRtotalpartnumber

#Oral Partners
meanoipartnernumber<-mean(artnetvars$M_MP12ONUM, na.rm = TRUE)
meanoipartnernumber

medianoipartnumber<-median(artnetvars$M_MP12ONUM, na.rm = TRUE)
medianoipartnumber

sdoipartnumber<-sd(artnetvars$M_MP12ONUM, na.rm =TRUE)
sdoipartnumber

q3oipartnumber<-quantile(artnetvars$M_MP12ONUM,probs=.75,na.rm=TRUE)
q3oipartnumber

q1oipartnumber<-quantile(artnetvars$M_MP12ONUM,probs=.25,na.rm=TRUE)
q1oipartnumber

IQRoipartnumber<-q3oipartnumber-q1oipartnumber
IQRoipartnumber

#Anal Parnters

meanaipartners<-mean(artnetvars$ai.part, na.rm = TRUE)
meanaipartners

medianaipartners<-median(artnetvars$ai.part, na.rm = TRUE)
medianaipartners

sdaipartners<-sd(artnetvars$ai.part, na.rm =TRUE)
sdaipartners

q3aipartners<-quantile(artnetvars$ai.part,probs=.75,na.rm=TRUE)
q3aipartners

q1aipartners<-quantile(artnetvars$ai.part,probs=.25,na.rm=TRUE)
q1aipartners

IQRaipartners<-q3partnumber-q1partnumber
IQRaipartners

#Stratifications of 2 Year STI Test Rate by demographic factors--------

#Stratification by Age
artnetvars %>%
  group_by(age.cat) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#Stratification by Race Cat
artnetvars %>%
  group_by(race.cat) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#Stratification by City
artnetvars %>%
  group_by(city) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#Stratification by HIV Status
artnetvars %>%
  group_by(hiv) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#Testing rates with previous positive HIV result
artnetvars %>%
  filter(EVRPOS==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#Testing rates with previous negative HIV result
artnetvars %>%
  filter(EVRPOS==0) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )


#Stratifications of TOTAL PARTNERS by demographic factors--------

#Stratification by Age Cat
artnetvars %>%
  group_by(age.cat) %>%
  summarise(
  n = n(),
  mean = mean(M_MP12OANUM, na.rm = TRUE),
  median=median(M_MP12OANUM, na.rm = TRUE),
  sd=sd(M_MP12OANUM, na.rm = TRUE)
)

#Stratification by Race Cat
artnetvars %>%
  group_by(race.cat) %>%
  filter()
  summarise(
    n = n(),
    mean = mean(M_MP12OANUM, na.rm = TRUE),
    median=median(M_MP12OANUM, na.rm = TRUE),
    sd=sd(M_MP12OANUM, na.rm = TRUE)
  )

#Stratification by City
artnetvars %>%
  group_by(city) %>%
  #filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12OANUM, na.rm = TRUE),
    median=median(M_MP12OANUM, na.rm = TRUE),
    sd=sd(M_MP12OANUM, na.rm = TRUE)
  )

#Stratification by HIV Status
artnetvars %>%
  group_by(hiv) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12OANUM, na.rm = TRUE),
    median=median(M_MP12OANUM, na.rm = TRUE),
    sd=sd(M_MP12OANUM, na.rm = TRUE)
  )

#Number of total partners when ego had previous positive HIV test (on most recent test)
artnetvars %>%
  filter(RCNTRSLT==2) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12OANUM, na.rm = TRUE),
    median=median(M_MP12OANUM, na.rm = TRUE),
    sd=sd(M_MP12OANUM, na.rm = TRUE)
  )

#Number of total partners when ego had previous negative HIV test (on most recent test)
artnetvars %>%
  filter(RCNTRSLT==1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12OANUM, na.rm = TRUE),
    median=median(M_MP12OANUM, na.rm = TRUE),
    sd=sd(M_MP12OANUM, na.rm = TRUE)
  )


#Stratifications of ORAL Partners by demographic factors when number of STI tests in last 2 years is at least 1--------
#Stratification by Race Cat
artnetvars %>%
  group_by(race.cat) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12ONUM, na.rm = TRUE),
    median=median(M_MP12ONUM, na.rm = TRUE),
    sd=sd(M_MP12ONUM, na.rm = TRUE)
  )

#Stratification by City
artnetvars %>%
  group_by(city) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12ONUM, na.rm = TRUE),
    median=median(M_MP12ONUM, na.rm = TRUE),
    sd=sd(M_MP12ONUM, na.rm = TRUE)
  )

#Stratification by HIV Status
artnetvars %>%
  group_by(hiv) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12ONUM, na.rm = TRUE),
    median=median(M_MP12ONUM, na.rm = TRUE),
    sd=sd(M_MP12ONUM, na.rm = TRUE)
  )

#Number of OI partners if ego had previous positive HIV test (on most recent test)
artnetvars %>%
  filter(RCNTRSLT==2) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12ONUM, na.rm = TRUE),
    median=median(M_MP12ONUM, na.rm = TRUE),
    sd=sd(M_MP12ONUM, na.rm = TRUE)
  )

#Number of OI partners if ego had previous negative HIV test (on most recent test)
artnetvars %>%
  filter(RCNTRSLT==1) %>%
  summarise(
    n = n(),
    mean = mean(M_MP12ONUM, na.rm = TRUE),
    median=median(M_MP12ONUM, na.rm = TRUE),
    sd=sd(M_MP12ONUM, na.rm = TRUE)
  )


#Stratifications of ANAL partners of ego by demographic factors when number of tests in last 2 years is at least 1--------
#Stratification by Race Cat
artnetvars %>%
  group_by(race.cat) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(ai.part, na.rm = TRUE),
    median=median(ai.part, na.rm = TRUE),
    sd=sd(ai.part, na.rm = TRUE)
  )

#Stratification by City
artnetvars %>%
  group_by(city) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(ai.part, na.rm = TRUE),
    median=median(ai.part, na.rm = TRUE),
    sd=sd(ai.part, na.rm = TRUE)
  )


#Stratification by HIV Status
artnetvars %>%
  group_by(hiv) %>%
  filter(STITEST_2YR >=1) %>%
  summarise(
    n = n(),
    mean = mean(ai.part, na.rm = TRUE),
    median=median(ai.part, na.rm = TRUE),
    sd=sd(ai.part, na.rm = TRUE)
  )


#Number of AI partners when ego had previous positive HIV test (on most recent test)
artnetvars %>%
  group_by(hiv) %>%
  filter(RCNTRSLT==2) %>%
  summarise(
    n = n(),
    mean = mean(ai.part, na.rm = TRUE),
    median=median(ai.part, na.rm = TRUE),
    sd=sd(ai.part, na.rm = TRUE)
  )

#Number of AI partners when ego had previous negative HIV test (on most recent test)
artnetvars %>%
  group_by(hiv) %>%
  filter(RCNTRSLT==1) %>%
  summarise(
    n = n(),
    mean = mean(ai.part, na.rm = TRUE),
    median=median(ai.part, na.rm = TRUE),
    sd=sd(ai.part, na.rm = TRUE)
  )

#STI Testing rate when having anal sex without condom--------
#AGE
artnetvars %>%
  group_by(age.cat) %>%
  filter(M_M1UAS==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#RACE
artnetvars %>%
  group_by(race.cat) %>%
  filter(M_M1UAS==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#CITY
artnetvars %>%
  group_by(city) %>%
  filter(M_M1UAS==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#HIV STATUS
artnetvars %>%
  group_by(hiv) %>%
  filter(M_M1UAS==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#STI TESTING RATE WHEN EGO WAS ALSO TESTED FOR HIV
#BY AGE
artnetvars %>%
  group_by(age.cat) %>%
  filter(ANYSTI_TEST==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#BY RACE
artnetvars %>%
  group_by(race.cat) %>%
  filter(ANYSTI_TEST==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#BY CITY
artnetvars %>%
  group_by(city) %>%
  filter(ANYSTI_TEST==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )


#BY HIV STATUS
artnetvars %>%
  group_by(hiv) %>%
  filter(ANYSTI_TEST==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

# STI TESTING RATE IF CURRENTLY TAKING PREP
artnetvars %>%
  group_by(age.cat) %>%
  filter(PREP_CURRENT==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#BY RACE
artnetvars %>%
  group_by(race.cat) %>%
  filter(PREP_CURRENT==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

#BY CITY
artnetvars %>%
  group_by(city) %>%
  filter(PREP_CURRENT==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )


#BY HIV STATUS
artnetvars %>%
  group_by(hiv) %>%
  filter(PREP_CURRENT==1) %>%
  summarise(
    n = n(),
    mean = mean(STITEST_2YR, na.rm = TRUE),
    median=median(STITEST_2YR, na.rm = TRUE),
    sd=sd(STITEST_2YR, na.rm = TRUE)
  )

