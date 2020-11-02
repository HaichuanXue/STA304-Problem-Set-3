#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Wenzhuo Zeng
# Data: 22 October 2020
# Contact: wenzhuo.zeng@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/Glendon X/Desktop/UofT/STA304/PS3/Data/")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_datas <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_datas <- labelled::to_factor(raw_datas)
# Just keep some variables
reduced_datas <- 
  raw_datas %>% 
  select(interest,
         registration,
         vote_intention,
         vote_2020,
         employment,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age)


#change type
reduced_datas$age<-as.numeric(reduced_datas$age)


#filter data
clean_datas<-reduced_datas %>% 
  filter(registration=="Registered"&
           vote_intention!="No, I am not eligible to vote"&
           vote_intention!="No, I will not vote but I am eligible"&
           vote_intention!="Not sure"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
  )

#remove NA
clean_datas<-na.omit(clean_datas)

#ageinterval
clean_datas<-clean_datas %>% 
  mutate(age_int = case_when(age <=20 ~ '18-20',
                              age >20  & age <= 30 ~ '21-30',
                              age >30  & age <= 40 ~ '31-40',
                              age >40  & age <= 50 ~ '41-50',
                              age >50  & age <= 60 ~ '51-60',
                              age >60  & age <= 70 ~ '61-70',
                              age >70  & age <= 80 ~ '71-80',
                              age >80 ~ '80up'
  )) 

#rename column gender to sex
clean_datas<-rename(clean_datas,sex=gender)


#education
clean_datas$education<-ifelse(clean_datas$education=="3rd Grade or less"|
                                clean_datas$education=="Middle School - Grades 4 - 8"|
                                clean_datas$education=="ompleted some high school","Grade12&under",
                              ifelse(clean_datas$education=="Completed some college, but no degree"|
                                       clean_datas$education=="Other post high school vocational training"|
                                       clean_datas$education=="High school graduate","Highschool",
                                     ifelse(clean_datas$education=="Associate Degree","Associate Degree",
                                            ifelse(clean_datas$education=="Completed some graduate, but no degree"|
                                                     clean_datas$education== "College Degree (such as B.A., B.S.)",
                                                   "College Degree","Masters&Doc"))))



#race
clean_datas<-rename(clean_datas,race=race_ethnicity)
clean_datas$race<-ifelse(clean_datas$race=="White","White",
                         ifelse(clean_datas$race=="Black, or African American","Black/African American",
                                ifelse(clean_datas$race=="Asian (Chinese)","Chinese",
                                       ifelse(clean_datas$race=="Asian (Japanese)","Japanese",
                                              ifelse(clean_datas$race=="American Indian or Alaska Native","American Indian or Alaska Native",
                                                     ifelse(clean_datas$race=="Some other race","Other race","other asian or pacific islander"))))))

clean_datas$vote_trump <- ifelse(clean_datas$vote_2020=="Donald Trump", 1, 0)

#data for modelling
final_datas <- clean_datas[,c(6,7,8,9,10,12,13)]

#create cells
final_datas$cell <- paste(final_datas$race, final_datas$education,
                          final_datas$household_income)

final_datas$age_int <- factor(final_datas$age_int)
final_datas$race <- factor(final_datas$race)
final_datas$education <- factor(final_datas$education)
final_datas$state <- factor(final_datas$state)
final_datas$cell <- factor(final_datas$cell)


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(final_datas, "C:/Users/Glendon X/Desktop/UofT/STA304/PS3/Data/survey_data.csv")

