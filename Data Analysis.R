library(tidyverse)
library(GGally)
library(gtsummary)
library(ggplot2)
library(haven)
library(tidyr)
install.packages("nnet")
library(nnet)
install.packages("emmeans")
library(emmeans) #load in all possible packages

#All Important Variables
#marij_num_days
#marij_smoke
#marij_eat
#marij_vape
#marij_dab
#marij_oth
#deaf
#blind
#decide
#diffwalk
#diffdress
#diffalone
#multi_dis
#age
#sex
#employment
#mental_health

#1. Explore distribution of variables through table 1

sapply(marijuana2024[c("blind", "deaf", "decide", "diffwalk", "diffdress", "diffalone", "multi_dis",
                       "age", "sex", "employment", "mental_health")], class) #var must be character or factor when getting rid of unknowns in table 1

marijuana2024<- marijuana2024%>%
  mutate(across(
    c("blind", "deaf", "decide", "diffwalk", "diffdress", "diffalone", "multi_dis",
      "age", "sex", "employment", "mental_health"),
    ~fct_recode(.x, NULL="Unknown")))

table1test<- tbl_summary(marijuana2024, 
            include= c("blind", "deaf", "decide", "diffwalk", "diffdress", "diffalone", "multi_dis",
                       "age", "sex", "employment", "mental_health"), #include exposures and covariates
            by= marij_num_days, #group by outcome variable
            statistic= all_categorical() ~ "{n} ({p}%)", 
            label= list( 
             blind= "Blind", 
             deaf= "Deaf",
             decide= "Difficulty Concentrating, Remembering, or Deciding",
             diffwalk= "Difficulty Walking",
             diffdress= "Difficulty Dressing",
             diffalone= "Difficulty Doing Errands Alone",
             multi_dis= "Multiple Disabilities",
             age= "Age",
             sex= "Sex",
             employment= "Employment Status",
             mental_health= "Days Mental Health Not Good"), 
            missing_text = "Missing" #renaming variables in the table
            
                         )
table1test
table1test<-table1test %>%
  as_gt() %>%
  tab_header(title= md("Table 1. Demographics of Adults by Marijuana Usage")) #adding a title


#Create tables of the type of marijuana usage (vape, dab, eat, smoke) by disability type
#BLIND
table_marijtype_blind<- tbl_summary(marijuana2024, 
                         include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                         by= blind, 
                         statistic= all_categorical() ~ "{n} ({p}%)", #include counts(percentages)
                         label= list(
                           marij_vape= "Vaping",
                           marij_dab= "Dab pen, rig, knife",
                           marij_eat= "Ate it",
                           marij_smoke= "Smoke it", 
                           marij_oth= "Other way"), 
                         missing_text = "Missing"
                         )

table_marijtype_blind<-table_marijtype_blind %>%
  as_gt() %>%
  tab_header(title= md("Table 3. Type of Marijuana Usage in Blind Adults"))

table_marijtype_blind

#DEAF
table_marijtype_deaf<- tbl_summary(marijuana2024, 
                                    include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                                    by= deaf, 
                                    statistic= all_categorical() ~ "{n} ({p}%)", 
                                    label= list(
                                      marij_vape= "Vaping",
                                      marij_dab= "Dab pen, rig, knife",
                                      marij_eat= "Ate it",
                                      marij_smoke= "Smoke it", 
                                      marij_oth= "Other way"), 
                                    missing_text = "Missing"
)
table_marijtype_deaf<-table_marijtype_deaf %>%
  as_gt() %>%
  tab_header(title= md("Table 4. Type of Marijuana Usage in Deaf Adults"))

table_marijtype_deaf


#DECIDE
table_marijtype_decide<- tbl_summary(marijuana2024, 
                                   include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                                   by= decide, 
                                   statistic= all_categorical() ~ "{n} ({p}%)", 
                                   label= list(
                                     marij_vape= "Vaping",
                                     marij_dab= "Dab pen, rig, knife",
                                     marij_eat= "Ate it",
                                     marij_smoke= "Smoke it", 
                                     marij_oth= "Other way"), 
                                   missing_text = "Missing"
)

table_marijtype_decide<-table_marijtype_decide %>%
  as_gt() %>%
  tab_header(title= md("Table 5. Type of Marijuana Usage in Adults w/ Difficulty Concentrating, Remembering, or Deciding"))

table_marijtype_decide

#DIFFWALK
table_marijtype_diffwalk<- tbl_summary(marijuana2024, 
                                   include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                                   by= diffwalk, 
                                   statistic= all_categorical() ~ "{n} ({p}%)", 
                                   label= list(
                                     marij_vape= "Vaping",
                                     marij_dab= "Dab pen, rig, knife",
                                     marij_eat= "Ate it",
                                     marij_smoke= "Smoke it", 
                                     marij_oth= "Other way"), 
                                   missing_text = "Missing"
)

table_marijtype_diffwalk<-table_marijtype_diffwalk %>%
  as_gt() %>%
  tab_header(title= md("Table 6. Type of Marijuana Usage in Adults w/ Difficulty Walking"))

table_marijtype_diffwalk

#DIFFDRESS
table_marijtype_diffdress<- tbl_summary(marijuana2024, 
                                   include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                                   by= diffdress, 
                                   statistic= all_categorical() ~ "{n} ({p}%)", 
                                   label= list(
                                     marij_vape= "Vaping",
                                     marij_dab= "Dab pen, rig, knife",
                                     marij_eat= "Ate it",
                                     marij_smoke= "Smoke it", 
                                     marij_oth= "Other way"), 
                                   missing_text = "Missing"
)

table_marijtype_diffdress<-table_marijtype_diffdress %>%
  as_gt() %>%
  tab_header(title= md("Table 7. Type of Marijuana Usage in Adults w/ Difficulty Dressing"))

table_marijtype_diffdress

#DIFFALONE
table_marijtype_diffalone<- tbl_summary(marijuana2024, 
                                   include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                                   by= diffalone, 
                                   statistic= all_categorical() ~ "{n} ({p}%)", 
                                   label= list(
                                     marij_vape= "Vaping",
                                     marij_dab= "Dab pen, rig, knife",
                                     marij_eat= "Ate it",
                                     marij_smoke= "Smoke it", 
                                     marij_oth= "Other way"), 
                                   missing_text = "Missing"
)

table_marijtype_diffalone<-table_marijtype_diffalone %>%
  as_gt() %>%
  tab_header(title= md("Table 8. Type of Marijuana Usage in Adults w/ Difficulty Doing Errands Alone"))

table_marijtype_diffalone

#MULTI_DIS
table_marijtype_multidis<- tbl_summary(marijuana2024, 
                                   include= c("marij_vape", "marij_dab", "marij_eat", "marij_smoke", "marij_oth"), 
                                   by= multi_dis, 
                                   statistic= all_categorical() ~ "{n} ({p}%)", 
                                   label= list(
                                     marij_vape= "Vaping",
                                     marij_dab= "Dab pen, rig, knife",
                                     marij_eat= "Ate it",
                                     marij_smoke= "Smoke it", 
                                     marij_oth= "Other way"), 
                                   missing_text = "Missing"
)

table_marijtype_multidis<-table_marijtype_multidis %>%
  as_gt() %>%
  tab_header(title= md("Table 9. Type of Marijuana Usage in Adults w/ Multiple Disabilities"))

table_marijtype_multidis

#Visualization of Marijuana Usage by Disability Type

#Pivoting Longer to make a data frame of my one combined disability variable to use for ggplot
dis_combined_long<-marijuana2024 %>%
  pivot_longer(
    cols= c(deaf,
            blind,
            decide,
            diffwalk,
            diffdress,
            diffalone), 
    names_to= "disability_type",
    values_to= "disability_value")


#Bar Plot of Marijuana Use by Disability Type
distype_marij_plot<- ggplot(dis_combined_long) +
  geom_bar(aes(x=disability_type, fill=marij_num_days), position= "fill") +
  labs(
    x= "Disability Type",
    y= "Count",
    title= "Figure 1. Marijuana Usage by Disability Type") +
  scale_x_discrete(labels= c(
    "blind"= "Blind", 
    "deaf"= "Deaf",
    "decide"= 
    "Difficulty 
    Concentrating, 
    Remembering, 
    or Deciding",
    "diffwalk"= 
    "Difficulty 
     Walking",
    "diffdress"= 
    "Difficulty 
     Dressing",
    "diffalone"= 
    "Difficulty 
     Doing 
     Errands 
     Alone"
  )) + 
  scale_fill_manual(
    values= c("purple", "pink", "lightblue","lightgreen"),
    name= "Marijuana Usage") +
  theme_minimal()
  
#Bar Plot of Marijuana Use by Disability Type ommitting the No Usage Category 
dis_combined_filter<- dis_combined_long %>%
  filter(marij_num_days != "No usage")

distype_filter_plot<- ggplot(dis_combined_filter) +
  geom_bar(aes(x=disability_type, fill=marij_num_days), position= "fill") +
  labs(
    x= "Disability Type",
    y= "Count",
    title= "Figure 2. Marijuana Usage by Disability Type") +
  scale_x_discrete(labels= c(
    "blind"= "Blind", 
    "deaf"= "Deaf",
    "decide"= 
      "Difficulty 
    Concentrating, 
    Remembering, 
    or Deciding",
    "diffwalk"= 
      "Difficulty 
     Walking",
    "diffdress"= 
      "Difficulty 
     Dressing",
    "diffalone"= 
      "Difficulty 
     Doing 
     Errands 
     Alone"
  )) + 
  scale_fill_manual(
    values= c("pink", "lightblue","lightgreen"),
    name= "Marijuana Usage") +
  theme_minimal()

distype_filter_plot

#Modeling
#Multinomial Logistic Regression (used for all four outcome categories: No usage, low usage, medium usage, high usage)

marijuana2024<- marijuana2024%>%
  mutate(multi_dis_model= case_when(
    multi_dis=="Multiple Disabilities" ~ 2, 
    multi_dis=="No Disabilities" ~ 0, 
    multi_dis=="One Disability" ~ 1, 
    TRUE ~ NA_real_
  )) #turning my multiple disability variable numeric so I can create a combined disability variable for the model

marijuana2024<- marijuana2024%>%
  mutate(
    blind_model= ifelse(blind== "Yes", 1, 0),
    deaf_model= ifelse(deaf=="Yes", 1, 0), 
    decide_model= ifelse(decide=="Yes", 1, 0), 
    diffwalk_model= ifelse(diffwalk=="Yes", 1, 0), 
    diffdress_model= ifelse(diffdress=="Yes", 1, 0), 
    diffalone_model= ifelse(diffalone=="Yes", 1, 0)
  ) #turning my other disability variables numeric so I can create a combined disability variable for the model


marijuana2024<- marijuana2024%>%
  mutate(dis_model= case_when(
    multi_dis_model>=2~ "Multiple Disabilities", 
    multi_dis_model==1 & blind_model==1 ~ "Blind",
    multi_dis_model==1 & deaf_model==1~ "Deaf",
    multi_dis_model==1 & decide_model==1~ "Difficulty Concentrating, Remembering, or Deciding",
    multi_dis_model==1 & diffwalk_model==1~ "Difficulty Walking",
    multi_dis_model==1 & diffdress_model==1~ "Difficulty Dressing",
    multi_dis_model==1 & diffalone_model==1~ "Difficulty Doing Errands Alone",
    TRUE~NA_character_
  )) #create one disability variable to include in regression model

marijuana2024<- marijuana2024%>%
  mutate(dis_model_f= factor(dis_model, 
                      levels= c("Blind",
    "Multiple Disabilities", 
    "Deaf",
    "Difficulty Concentrating, Remembering, or Deciding",
    "Difficulty Walking",
    "Difficulty Dressing",
    "Difficulty Doing Errands Alone")
  )) #making a factorized duplicate of my model variable so the original var stays intact
  
logistic_model<-multinom(marij_num_days ~ dis_model_f, data = marijuana2024) #running my logistic regression
summary(logistic_model) #getting the log_odds
exp(coef(logistic_model)) #getting the odds ratio

?emmeans #used to assess relationship between disability pairs

emm<-emmeans(logistic_model, ~ dis_model_f) #assessing the adjusted means 
pairs(emm, type= "response") #getting the odds ratio

table(marijuana2024$marij_num_days, marijuana2024$dis_model_f)

#The output for the pairwise emm showed negative estimates,
#null values for the standard error, and negative variance estimates indicating an issue with the model choice

#Better Model 
#Binomial Logistic Regression (Creating a binomial marijuana usage category in order increase sample size within categories)

marijuana2024<-marijuana2024%>%
  mutate(num_days_marij= case_when(
marij_num_days=="Low Usage" | marij_num_days=="Medium Usage" |  marij_num_days=="High Usage" ~ "Any use", 
    marij_num_days=="No usage" ~ "No usage"
  )) #creating a binary marijuana usage category

marijuana2024<-marijuana2024%>%
  mutate(num_days_marij= factor(num_days_marij,
    levels = c("Any use", "No usage"))) #factorizing my variable
    
levels(marijuana2024$num_days_marij)  

marijuana2024<- marijuana2024%>%
  mutate(dis_model2= case_when(
    multi_dis_model>=2~ "Multiple Disabilities", 
    multi_dis_model==1 & blind_model==1 ~ "Blind",
    multi_dis_model==1 & deaf_model==1~ "Deaf",
    multi_dis_model==1 & decide_model==1~ "Difficulty Concentrating, Remembering, or Deciding",
    TRUE~NA_character_
  )) #recoding my disability variable to omit categories with low sample sizes

marijuana2024<- marijuana2024%>%
  mutate(dis_model_2f= factor(dis_model2, 
                             levels= c("Blind",
                                       "Multiple Disabilities", 
                                       "Deaf",
                                       "Difficulty Concentrating, Remembering, or Deciding")
  ))

model<-glm(num_days_marij ~ dis_model_2f, data = marijuana2024, family=binomial) #running my regression
summary(model) #getting log odds
exp(coef(model)) #getting odds ratio

?emmeans #used to assess relationship between disability pairs

emm<-emmeans(model, ~ dis_model_2f)
pairs(emm, type= "response", infer=c(TRUE, TRUE))

table(marijuana2024$num_days_marij, marijuana2024$dis_model_2f)

#Table 2a. Unadjusted Odds Ratio
model_dataframe<-pairs(emm, type= "response", infer=c(TRUE, TRUE)) %>%
  as.data.frame() #creating a data frame of my binomial logistic regression model including odds ratio and their CI 

table2a<-model_dataframe %>%
  gt() %>%
  cols_label(
  contrast= "Comparison Categories", 
  odds.ratio= "Odds Ratio", 
  asymp.LCL= "Lower CI (95%)", 
  asymp.UCL= "Upper CI (95%)"
  ) %>%
  tab_header(
    title= "Table 2a. Unadjusted Odds Ratio between Pairwise Disability Groups"
  )

table2a

#Table 2b. Adjusted Binary Logistic Regression
model_adj<-glm(num_days_marij ~ dis_model_2f + age + sex + employment + mental_health, 
               data = marijuana2024, family=binomial)

summary(model_adj)
exp(coef(model_adj))

emm_adj<-emmeans(model_adj, ~ dis_model_2f)

pairs(emm_adj, type= "response", infer=c(TRUE, TRUE))

model_adj_dataframe<-pairs(emm_adj, type= "response", infer=c(TRUE, TRUE)) %>%
  as.data.frame()

table2b<-model_adj_dataframe %>%
  gt() %>%
  cols_label(
    contrast= "Comparison Categories", 
    odds.ratio= "Odds Ratio", 
    asymp.LCL= "Lower CI (95%)", 
    asymp.UCL= "Upper CI (95%)"
  ) %>%
  tab_header(
    title= "Table 2b. Adjusted Odds Ratio"
  )

table2b



