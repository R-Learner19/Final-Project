#Data Cleaning
#
#Load in packages
library(tidyverse)
library(GGally)
library(gtsummary)
library(ggplot2)
library(haven)

# Load in data and set up working directory 
setwd("/Users/kellynevaudreuil/Documents/Public Health/R/Final_Project/Raw Data")
brfss2024<-read_xpt("LLCP2024.XPT ") 

#Clean Data According to Hypothesis 
#Hypothesis: Do different types of disabilities influence marijuana use among respondents of the 2024 BRFSS? 

#Exposure: Disability Type (DEAF, BLIND, DECIDE, DIFFWALK, DIFFDRES, DIFFALON)
#Outcome: Marijuana usage (States and territories who answered the module)


#1. Make a data frame with states that answered both the marijuana usage module and disability questions (exclusionary criteria)
marijuana2024<- subset(brfss2024, brfss2024$`_STATE`==6|brfss2024$`_STATE`==9|
                         brfss2024$`_STATE`==10|brfss2024$`_STATE`==11| brfss2024$`_STATE`==66|
                         brfss2024$`_STATE`==15|brfss2024$`_STATE`==17|brfss2024$`_STATE`==18|
                         brfss2024$`_STATE`==21|brfss2024$`_STATE`==23|brfss2024$`_STATE`==27|
                         brfss2024$`_STATE`==28|brfss2024$`_STATE`==29|brfss2024$`_STATE`==30|
                         brfss2024$`_STATE`==32|brfss2024$`_STATE`==33|brfss2024$`_STATE`==35|
                         brfss2024$`_STATE`==41|brfss2024$`_STATE`==44|brfss2024$`_STATE`==45|
                         brfss2024$`_STATE`==50|brfss2024$`_STATE`==78|brfss2024$`_STATE`==51|
                         brfss2024$`_STATE`==54) 



#2. Make variables into factors and recode 77 and 99 as missing/NA, recode 88 as no days
#Recode 1 as "Yes", 2 as "No" and 7 and 9 as missing/NA

#STATE
marijuana2024[["_STATE"]]

marijuana2024<- marijuana2024 %>%
  mutate(state= case_when(
    `_STATE`== 6~ "California" ,
    `_STATE`== 9~ "Connecticut",
    `_STATE`== 10 ~ "Delaware", 
    `_STATE`== 11~ "District of Columbia",
    `_STATE`== 66~  "Guam", 
    `_STATE`== 15 ~ "Hawaii",
    `_STATE`== 17~ "Illinois",
    `_STATE`== 18~ "Indiana",
    `_STATE`== 21 ~ "Kentucky", 
    `_STATE`== 23~ "Maine", 
    `_STATE`==27~ "Minnesota", 
    `_STATE`==28~ "Mississippi", 
    `_STATE`==29~"Missouri",
    `_STATE`==30~"Montana",
    `_STATE`==32~"Nevada", 
    `_STATE`==33~"New Hampshire", 
    `_STATE`==35~"New Mexico", 
    `_STATE`==41~"Oregon", 
    `_STATE`==44~"Rhode Island", 
    `_STATE`==45~"South Carolina", 
    `_STATE`==50~"Vermont", 
    `_STATE`==78~"Virgin Islands", 
    `_STATE`==51~"Virginia", 
    `_STATE`==54~"West Virginia"
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    state= factor(
      state, 
      levels= c("California", "Connecticut", 
                "Delaware", 
                "District of Columbia",
                "Guam", 
                "Hawaii",
                "Illinois",
                "Indiana",
                "Kentucky", 
                "Maine", 
                "Minnesota", 
                "Mississippi", 
                "Missouri",
                "Montana",
                "Nevada", 
                "New Hampshire", 
                "New Mexico", 
                "Oregon", 
                "Rhode Island", 
                "South Carolina", 
                "Vermont", 
                "Virgin Islands", 
                "Virginia", 
                "West Virginia")
    ))


table(marijuana2024$state)
unique(marijuana2024$state)

#MARIJAN1
table(marijuana2024$MARIJAN1) #seeing table of how many people smoked in that day
marijuana2024<-marijuana2024 %>% 
  mutate(marij_num_days= case_when( #must use mutate when using case_when to attach to dataframe
    MARIJAN1==77| MARIJAN1==99 ~ NA_character_, #if var is equal to 77 or 99 set it equal to na 
    MARIJAN1==88 ~ "No usage", #if var is equal to 88 set it equal to 0 days
    MARIJAN1 >=1 & MARIJAN1 <=10 ~ "Low Usage", #if var is between to 1-10 days set it equal to low usage
    MARIJAN1>=11 & MARIJAN1 <=20 ~ "Medium Usage", #if var is between 11-20 days set it equal to medium days
    MARIJAN1>=21 & MARIJAN1 <=30~ "High Usage" #if var is between 21-30 days set it equal to high usage 
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    marij_num_days= factor(
      marij_num_days, 
      levels= c("No usage", "Low Usage", "Medium Usage", "High Usage")
    )) #make variable a factor with four levels 

sum(is.na(marijuana2024$marij_num_days)) #more used way of seeing any missing/NA values

marijuana2024<-marijuana2024 %>% 
  filter(!is.na(marij_num_days)) #keep everything but na values for this variable

table(marijuana2024$marij_num_days)
unique(marijuana2024$MARIJAN1)
marijuana2024$marij_num_days
#MARIJAN1 is now marij_num_days

#MARJSMOK
marijuana2024<- marijuana2024 %>%
  mutate(marij_smoke= case_when(
    MARJSMOK==1 ~ "Yes",
    MARJSMOK==2 ~ "No",
    MARJSMOK==7| MARJSMOK==9 ~ NA_character_
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    marij_smoke= factor(
      marij_smoke, 
      levels= c("Yes", "No")
    ))

marijuana2024<-marijuana2024 %>%
  mutate(marij_smoke= case_when(
    marij_smoke=="Yes"~ 1,
    marij_smoke=="No"~ 0, 
    TRUE~ NA_real_))

table(marijuana2024$marij_smoke)
unique(marijuana2024$marij_smoke)

#MARJSMOK is now marij_smoke


#MARJEAT
marijuana2024<- marijuana2024 %>%
  mutate(marij_eat= case_when(
    MARJEAT==1 ~ "Yes",
    MARJEAT==2 ~ "No",
    MARJEAT==7| MARJEAT==9 ~ NA_character_
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    marij_eat= factor(
      marij_eat, 
      levels= c("Yes", "No")
    ))

marijuana2024<-marijuana2024 %>%
  mutate(marij_eat= case_when(
    marij_eat=="Yes"~ 1,
    marij_eat=="No"~ 0, 
    TRUE~ NA_real_))

unique(marijuana2024$marij_eat)
#MARJEAT is now marij_eat

#MARJVAPE
marijuana2024<- marijuana2024 %>%
  mutate(marij_vape= case_when(
    MARJVAPE==1 ~ "Yes",
    MARJVAPE==2 ~ "No",
    MARJVAPE==7| MARJVAPE==9 ~ NA_character_
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    marij_vape= factor(
      marij_vape, 
      levels= c("Yes", "No")
    ))

marijuana2024<-marijuana2024 %>%
  mutate(marij_vape= case_when(
    marij_vape=="Yes"~ 1,
    marij_vape=="No"~ 0, 
    TRUE~ NA_real_))

unique(marijuana2024$marij_vape)
#MARJVAPE is now marij_vape

#MARJDAB
marijuana2024<- marijuana2024 %>%
  mutate(marij_dab= case_when(
    MARJDAB==1 ~ "Yes",
    MARJDAB==2 ~ "No",
    MARJDAB==7| MARJDAB==9 ~ NA_character_
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    marij_dab= factor(
      marij_dab, 
      levels= c("Yes", "No")
    ))

marijuana2024<-marijuana2024 %>%
  mutate(marij_dab= case_when(
    marij_dab=="Yes"~ 1,
    marij_dab=="No"~ 0, 
    TRUE~ NA_real_))

unique(marijuana2024$marij_dab)
#MARJDAB is now marij_dab

#MARJOTHR
marijuana2024<- marijuana2024 %>%
  mutate(marij_oth= case_when(
    MARJOTHR==1 ~ "Yes",
    MARJOTHR==2 ~ "No",
    MARJOTHR==7| MARJOTHR==9 ~ NA_character_
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    marij_oth= factor(
      marij_oth, 
      levels= c("Yes", "No")
    ))

marijuana2024<-marijuana2024 %>%
  mutate(marij_oth= case_when(
    marij_oth=="Yes"~ 1,
    marij_oth=="No"~ 0, 
    TRUE~ NA_real_))

unique(marijuana2024$marij_oth)
#MARJOTHR is now marij_oth

#DEAF
marijuana2024<- marijuana2024 %>%
  mutate(deaf= case_when(
    DEAF==1 ~ "Yes",
    DEAF==2 ~ "No",
    DEAF==7| DEAF==9 ~ NA_character_
  ))%>%
  mutate(deaf=factor(deaf, levels=c("Yes", "No")))

#BLIND
marijuana2024<- marijuana2024 %>%
  mutate(blind= case_when(
    BLIND==1 ~ "Yes",
    BLIND==2 ~ "No",
    BLIND==7| BLIND==9 ~ NA_character_
  )) %>%
  mutate(blind=factor(blind, levels=c("Yes", "No")))


#DECIDE
marijuana2024<- marijuana2024 %>%
  mutate(decide= case_when(
    DECIDE==1 ~ "Yes",
    DECIDE==2 ~ "No",
    DECIDE==7| DECIDE==9 ~ NA_character_
  )) %>%
  mutate(decide=factor(decide, levels=c("Yes", "No")))


#DIFFWALK
marijuana2024<- marijuana2024 %>%
  mutate(diffwalk= case_when(
    DIFFWALK==1 ~ "Yes",
    DIFFWALK==2 ~ "No",
    DIFFWALK==7| DIFFWALK==9 ~ NA_character_
  )) %>%
  mutate(diffwalk=factor(diffwalk, levels=c("Yes", "No")))

#DIFFDRES
marijuana2024<- marijuana2024 %>%
  mutate(diffdress= case_when(
    DIFFDRES==1 ~ "Yes",
    DIFFDRES==2 ~ "No",
    DIFFDRES==7| DIFFDRES==9 ~ NA_character_
  )) %>%
  mutate(diffdress=factor(diffdress, levels=c("Yes", "No")))

#DIFFALON
marijuana2024<- marijuana2024 %>%
  mutate(diffalone= case_when(
    DIFFALON==1 ~ "Yes",
    DIFFALON==2 ~ "No",
    DIFFALON==7| DIFFALON==9 ~ NA_character_
  )) %>%
  mutate(diffalone=factor(diffalone, levels=c("Yes", "No")))

#Multiple Disabilities

class(marijuana2024$deaf)

marijuana2024<- marijuana2024 %>%
  mutate(
    deaf= ifelse(deaf=="Yes", 1, 0), 
    blind= ifelse(blind=="Yes", 1, 0), 
    decide= ifelse(decide=="Yes", 1, 0), 
    diffalone= ifelse(diffalone=="Yes", 1, 0),
    diffwalk= ifelse(diffwalk=="Yes", 1, 0),
    diffdress= ifelse(diffdress=="Yes", 1, 0)
  )

marijuana2024<- marijuana2024%>%
  mutate(multi_dis= 
           deaf+ blind+ decide+ diffwalk+ diffdress+ diffalone)

marijuana2024<- marijuana2024%>%
  mutate(multi_dis= case_when(
    multi_dis >=2 ~ "Multiple Disabilities",
    multi_dis==0 ~ "No Disabilities", 
    multi_dis==1 ~ "One Disability", 
    TRUE ~ NA_character_
  ))

table(marijuana2024$multi_dis)

#3. Clean Confounding variables
#Confounder Requirements: 
#it must be associated with the exposure (disability), 
#it must be an independent risk factor for the outcome (marijuana),
#and it must not be on the causal pathway between the exposure and the outcome
  #MENTHLTH
  #Age  (_AGE_G)
  #Sex  (SEXVAR)
  #Employment Status (EMPLOY1)


#SEXVAR
marijuana2024[["SEXVAR"]]
unique(marijuana2024[["SEXVAR"]])

marijuana2024<-marijuana2024 %>%
  mutate(sex= case_when(
    SEXVAR==1 ~ "Male", 
    SEXVAR==2 ~ "Female"
  )) %>%
  mutate(sex=factor(sex, levels= c("Male", "Female")))
                     
table(marijuana2024[["sex"]])
class(marijuana2024$sex)


#AGE
marijuana2024<- marijuana2024 %>%
  mutate(age= case_when(
    `_AGE_G`== 1 ~ "18-24",
    `_AGE_G`== 2 ~ "25-34",
    `_AGE_G`== 3 ~ "35-44",
    `_AGE_G`== 4 ~ "45-54",
    `_AGE_G`== 5 ~ "55-64",
    `_AGE_G`== 6 ~ "65+"
  )) #variable in codebook, levels determined by BRFSS

marijuana2024<- marijuana2024 %>%
  mutate(
    age= factor(
      age, 
      levels= c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ))

table(marijuana2024$age)
sum(complete.cases(marijuana2024$age))

#EMPLOYMENT
marijuana2024<- marijuana2024 %>%
  mutate(employment= case_when(
    EMPLOY1==1| EMPLOY1==2| EMPLOY1==5 ~ "Employed",
    EMPLOY1==3| EMPLOY1==4 ~ "Unemployed",
    EMPLOY1==6 ~ "Student",
    EMPLOY1==7 ~ "Retired",
    EMPLOY1==8 ~ "Unable to Work",
    EMPLOY1==9 ~ NA_character_
  ))

marijuana2024<- marijuana2024 %>%
  mutate(
    employment= factor(
      employment, 
      levels= c( "Employed",
                 "Unemployed",
                  "Student",
                   "Retired",
                  "Unable to Work")
    ))

table(marijuana2024$employment)


#MENTAL HEALTH
marijuana2024<- marijuana2024 %>%
  mutate(mental_health=case_when(
    MENTHLTH==88 ~ "No days",
    MENTHLTH>=1 & MENTHLTH<=15  ~ "Half Days",
    MENTHLTH>= 16 & MENTHLTH<=30 ~ "More than half",
    MENTHLTH==99 ~ NA_character_
  ))


marijuana2024<- marijuana2024 %>%
  mutate(
    mental_health= factor(
      mental_health, 
      levels= c("No days",
                 "Half Days",
                "More than half")
    ))

table(marijuana2024$mental_health)

