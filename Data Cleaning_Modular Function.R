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
marijuana2024m<- subset(brfss2024, brfss2024$`_STATE`==6|brfss2024$`_STATE`==9|
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
marijuana2024m[["_STATE"]]

marijuana2024m<- marijuana2024m %>%
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

marijuana2024m<- marijuana2024m %>%
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


table(marijuana2024m$state)
unique(marijuana202m$state)

#MARIJAN1
table(marijuana2024m$MARIJAN1) #seeing table of how many people smoked in that day
marijuana2024m<-marijuana2024m %>% 
  mutate(marij_num_days= case_when( #must use mutate when using case_when to attach to dataframe
    MARIJAN1==77| MARIJAN1==99 ~ NA_character_, #if var is equal to 77 or 99 set it equal to na 
    MARIJAN1==88 ~ "No usage", #if var is equal to 88 set it equal to 0 days
    MARIJAN1 >=1 & MARIJAN1 <=10 ~ "Low Usage", #if var is between to 1-10 days set it equal to low usage
    MARIJAN1>=11 & MARIJAN1 <=20 ~ "Medium Usage", #if var is between 11-20 days set it equal to medium days
    MARIJAN1>=21 & MARIJAN1 <=30~ "High Usage" #if var is between 21-30 days set it equal to high usage 
  ))

marijuana2024m<- marijuana2024m %>%
  mutate(
    marij_num_days= factor(
      marij_num_days, 
      levels= c("No usage", "Low Usage", "Medium Usage", "High Usage")
    )) #make variable a factor with four levels 

sum(is.na(marijuana2024m$marij_num_days)) #more used way of seeing any missing/NA values

marijuana2024m<-marijuana2024m %>% 
  filter(!is.na(marij_num_days)) #keep everything but na values for this variable

table(marijuana2024m$marij_num_days)
unique(marijuana2024m$MARIJAN1)
marijuana2024m$marij_num_days
#MARIJAN1 is now marij_num_days

#Modular Function for recoding 1, 2, 7, 9 variables
#' @description Creating a function to clean BRFSS variables with 1,2,7,9 coding scheme
#' @param x==1 when participant answered yes
#' @param x==2 when participant answered no
#' @param x==7 or x==9 when participant had missing data 
clean_var<-function(x){
  result<- case_when(
    x==1 ~ "Yes", 
    x==2 ~ "No", 
    x==7| x==9 ~ NA_character_, 
    TRUE~ NA_character_
  )
  print(result)
  return(result)
} 

marijuana2024m$smoke<-clean_var(marijuana2024m$MARJSMOK)
marijuana2024m$dab<-clean_var(marijuana2024m$MARJDAB)
marijuana2024m$vape<-clean_var(marijuana2024m$MARJVAPE)
marijuana2024m$eat<-clean_var(marijuana2024m$MARJEAT)
marijuana2024m$oth<-clean_var(marijuana2024m$MARJOTHR)

#compare variables from marijuana2024(no modular function) to marijuana2024m (includes a modular function) to see if it worked
sum(is.na(marijuana2024$marij_smoke))
sum(is.na(marijuana2024m$smoke))


