#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/Glendon X/Desktop/UofT/STA304/PS3/Data/")
raw_datac <- read_dta("usa_00002.dta")


# Add the labels
raw_datac <- labelled::to_factor(raw_datac)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_datac <- 
  raw_datac %>% 
  select(perwt,
         region,
         stateicp,
         sex, 
         age, 
         race, 
         citizen,
         educd,
         hhincome)

#Change data types
reduced_datac$age<-as.numeric(reduced_datac$age)


clean_datac<-reduced_datac %>% 
  filter(age>=18 & (citizen!="not a citizen"|citizen!="citizenship status not reported"|citizen!="n/a"))

#remove NA
clean_datac$hhincome<-ifelse(clean_datac$hhincome==9999999,
                                      NaN,clean_datac$hhincome)

clean_datac<-na.omit(clean_datac)
#ageinterval
clean_datac<-clean_datac %>% 
  mutate(age_int = case_when(age <=20 ~ '18-20',
                             age >20  & age <= 30 ~ '21-30',
                             age >30  & age <= 40 ~ '31-40',
                             age >40  & age <= 50 ~ '41-50',
                             age >50  & age <= 60 ~ '51-60',
                             age >60  & age <= 70 ~ '61-70',
                             age >70  & age <= 80 ~ '71-80',
                             age >80 ~ '80up'
  )) 

#sex
clean_datac$sex<-ifelse(clean_datac$sex=="female","Female","Male")

#education
clean_datac$educd<-ifelse(clean_datac$educd=="grade 1"|
                            clean_datac$educd=="grade 2"|
                            clean_datac$educd=="grade 3"|
                            clean_datac$educd=="grade 4"|
                            clean_datac$educd=="grade 5"|
                            clean_datac$educd=="grade 6"|
                            clean_datac$educd=="grade 7"|
                            clean_datac$educd=="grade 8"|
                            clean_datac$educd=="grade 9"|
                            clean_datac$educd=="grade 10"|
                            clean_datac$educd=="grade 10"|
                            clean_datac$educd=="g12th grade, no diploma"|
                            clean_datac$educd=="no schooling completed"|
                            clean_datac$educd=="kindergarten"|
                            clean_datac$educd=="nursery school, preschool"|
                            clean_datac$educd=="nursery school to grade 4"|
                            clean_datac$educd=="grade 1, 2, 3, or 4"|
                            clean_datac$educd=="grade 5, 6, 7, or 8"|
                            clean_datac$educd=="grade 5 or 6"|
                            clean_datac$educd=="grade 7 or 8"|
                            clean_datac$educd=="grade 12","Grade12&under",
                          ifelse(clean_datac$educd=="regular high school diploma"|
                                   clean_datac$educd=="high school graduate or GED"|
                                   clean_datac$educd=="1 year of college"|
                                   clean_datac$educd=="2 years of college"|
                                   clean_datac$educd=="3 years of college"|
                                   clean_datac$educd=="4 years of college"|
                                   clean_datac$educd=="5+ years of college"|
                                   clean_datac$educd=="6 years of college (6+ in 1960-1970)"|
                                   clean_datac$educd=="7 years of college"|
                                   clean_datac$educd=="8+ years of college"|
                                   clean_datac$educd=="ged or alternative credential"|
                                   clean_datac$educd=="1 or more years of college credit, no degree"|
                                   clean_datac$educd=="some college, but less than 1 year","Highschool",
                                 ifelse(clean_datac$educd=="associate's degree, type not specified"|
                                          clean_datac$educd=="associate's degree, occupational program"|
                                          clean_datac$educd=="associate's degree, academic program","Associate Degree",
                                        ifelse(clean_datac$educd=="bachelor's degree","College Degree",
                                               ifelse(clean_datac$educd=="n/a or no schooling"|
                                                        clean_datac$educd=="n/a"|
                                                        clean_datac$educd=="missing","m","Masters&Doc")))))
#rename educd
clean_datac<-rename(clean_datac,education=educd)

#stateicp
clean_datac$stateicp<-ifelse(clean_datac$stateicp=="alabama","AL",
                             ifelse(clean_datac$stateicp=="alaska","AK",
                             ifelse(clean_datac$stateicp=="arizona","AZ",
                             ifelse(clean_datac$stateicp=="arkansas","AR",
                             ifelse(clean_datac$stateicp=="california","CA",
                             ifelse(clean_datac$stateicp=="colorado","CO",
                             ifelse(clean_datac$stateicp=="connecticut","CT",
                             ifelse(clean_datac$stateicp=="delaware","DE",
                             ifelse(clean_datac$stateicp=="district of columbia","DC",
                             ifelse(clean_datac$stateicp=="florida","FL",
                             ifelse(clean_datac$stateicp=="georgia","GA",
                             ifelse(clean_datac$stateicp=="hawaii","HI",
                             ifelse(clean_datac$stateicp=="idaho","ID",
                             ifelse(clean_datac$stateicp=="illinois","IL",
                             ifelse(clean_datac$stateicp=="indiana","IN",
                             ifelse(clean_datac$stateicp=="iowa","IA",
                             ifelse(clean_datac$stateicp=="kansas","KS",
                             ifelse(clean_datac$stateicp=="kentucky","KY",
                             ifelse(clean_datac$stateicp=="louisiana","LA",
                             ifelse(clean_datac$stateicp=="maine","ME",
                             ifelse(clean_datac$stateicp=="maryland","MD",
                             ifelse(clean_datac$stateicp=="massachusetts","MA",
                             ifelse(clean_datac$stateicp=="michigan","MI",
                             ifelse(clean_datac$stateicp=="minnesota","MN",
                             ifelse(clean_datac$stateicp=="mississippi","MS",
                             ifelse(clean_datac$stateicp=="missouri","MO",
                             ifelse(clean_datac$stateicp=="montana","MT",
                             ifelse(clean_datac$stateicp=="nebraska","NE",
                             ifelse(clean_datac$stateicp=="nevada","NV",
                             ifelse(clean_datac$stateicp=="new hampshire","NH",
                             ifelse(clean_datac$stateicp=="new jersey","NJ",
                             ifelse(clean_datac$stateicp=="new mexico","NM",
                             ifelse(clean_datac$stateicp=="new york","NY",
                             ifelse(clean_datac$stateicp=="north carolina","NC",
                             ifelse(clean_datac$stateicp=="north dakota","ND",
                             ifelse(clean_datac$stateicp=="ohio","OH",
                             ifelse(clean_datac$stateicp=="oklahoma","OK",
                             ifelse(clean_datac$stateicp=="oregon","OR",
                             ifelse(clean_datac$stateicp=="pennsylvania","PA",
                             ifelse(clean_datac$stateicp=="rhode island","RI",
                             ifelse(clean_datac$stateicp=="south carolina","SC",
                             ifelse(clean_datac$stateicp=="south dakota","SD",
                             ifelse(clean_datac$stateicp=="tennessee","TN",
                             ifelse(clean_datac$stateicp=="texas","TX",
                             ifelse(clean_datac$stateicp=="utah","UT",
                             ifelse(clean_datac$stateicp=="vermont","VT",
                             ifelse(clean_datac$stateicp=="virginia","VA",
                             ifelse(clean_datac$stateicp=="washington","WA",
                             ifelse(clean_datac$stateicp=="west virginia","WV",
                             ifelse(clean_datac$stateicp=="wisconsin","WI","WY")
                             )))))))))))))))))))))))))))))))))))))))))))))))))
                             


unique(clean_datac$stateicp)

#rename state
clean_datac<-rename(clean_datac,state=stateicp)


#household income
clean_datac<-clean_datac %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 


clean_datac<-clean_datac%>%select(-hhincome)


#race
clean_datac$race<-ifelse(clean_datac$race=="white","White",
                         ifelse(clean_datac$race=="black/african american/negro","Black/African American",
                                ifelse(clean_datac$race=="american indian or alaska native","American Indian or Alaska Native",
                                       ifelse(clean_datac$race=="other asian or pacific islander","other asian or pacific islander",
                                              ifelse(clean_datac$race=="other race, nec"|
                                                       clean_datac$race=="two major races"|
                                                       clean_datac=="three or more major races","Other race",
                                                     ifelse(clean_datac$race=="chinese","Chinese","Japanese"))))))



#data for modelling
final_datac <- clean_datac[,c(1,3,4,6,8,9,10)]

#create cells
final_datac$cell <- paste(final_datac$race, final_datac$education,final_datac$household_income)

final_datac$age_int <- factor(final_datac$age_int)
final_datac$race <- factor(final_datac$race)
final_datac$education <- factor(final_datac$education)
final_datac$state <- factor(final_datac$state)
final_datac$cell <- factor(final_datac$cell)



# Saving the census data as a csv file in my
# working directory
write_csv(final_datac, "C:/Users/Glendon X/Desktop/UofT/STA304/PS3/Data/census_data.csv")


         