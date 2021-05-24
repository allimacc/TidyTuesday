library(tidyverse)
library(tidytuesdayR)
library(stringr)
library(GGally)
library(caret)

#load the information
survey <- tidytuesdayR::tt_load(2021, week = 21)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')


head(survey)
str(survey)

#what types of salaries are we looking at?


survey_clean<- survey %>%
  filter(annual_salary>15000 & annual_salary<200000 & currency == "USD" & how_old_are_you != "under 18") %>%
  select(how_old_are_you, annual_salary,overall_years_of_professional_experience, highest_level_of_education_completed,
          gender, race) %>%
  rename(years_experience = overall_years_of_professional_experience ,  
         education_level = highest_level_of_education_completed,
         age = how_old_are_you) %>%
  mutate(gender= if_else(gender == "Other or prefer not to answer", "No Answer", 
                 if_else(gender == "Prefer not to answer", "No Answer",gender))) %>%
  mutate(education_level = if_else(education_level %in% c("High School","Some college"), "Highschool", 
                            if_else(education_level %in% c("Master's degree","PhD", 
                                                        "Professional degree (MD, JD, etc.)"),"Post Bachelor Degree",
                            education_level))) %>%
 # mutate(state = ifelse(str_detect(state, ","), "Multi-state", state)) %>%
  mutate(race = ifelse(str_detect(race, ","), "Bi/Multi Racial", race)) %>%
  mutate(race2 = ifelse(race == "White", "White", "Non-White"))

#having trouble getting under 18 out so just adding again here
survey_clean <- filter(survey_clean, (!str_detect(age, "under")))

#cleaning up answers above using these fields         
table(survey_clean$gender)  
table(survey_clean$age)
table(survey_clean$years_experience)
table(survey_clean$education_level)
table(survey_clean$race)
table(survey_clean$race2)
table(survey_clean$state)

#adding factors to some of the info
survey_clean$years_experience <- factor(x=survey_clean$years_experience, levels= c("1 year or less",
                                                                                  "2 - 4 years ",
                                                                                  "5-7 years",
                                                                                 "8 - 10 years",
                                                                                 "11 - 20 years",
                                                                                 "21 - 30 years",
                                                                                 "31 - 40 years",
                                                                                 "41 years or more"))


survey_clean$education_level <-factor(x = survey_clean$education_level, levels = c("High School",
                                                                                   "College degree",
                                                                                   "Post Bachelor Degree"))



survey_clean$age <- factor(x= survey_clean$age, levels = c("under 18",
                                                           "18-24", 
                                                           "25-34", 
                                                           "35-44", 
                                                           "45-54", 
                                                           "55-64",
                                                           "65 or over"))

#clean up nas
survey_clean <- na.omit(survey_clean)


#Checking to see what I see

ggplot(data=survey_clean, aes(x=age, y=annual_salary))+
  geom_boxplot()

ggplot(data=survey_clean, aes(x=years_experience, y=annual_salary))+
  geom_boxplot()


ggplot(data=survey_clean, aes(x=years_experience, y=annual_salary))+
  geom_boxplot()

ggplot(data=survey_clean, aes(x=education_level, y=annual_salary))+
  geom_boxplot()

ggplot(data= survey_clean, aes(annual_salary))+
  geom_area(stat="bin")




#tried a poisson regresssion, maybe I'm not ready for that. Let's spend longer with binomial
# Loading caret library
require(caret)
# Splitting the data into train and test
index <- createDataPartition(survey_clean$annual_salary, p = .70, list = FALSE)
train <- survey_clean[index, ]
test <- survey_clean[-index, ]
# Training the model
logistic_model <- glm(annual_salary ~ ., family = poisson(), train)
# Checking the model
summary(logistic_model)



#prepping for binomial regression- output is is salary above 80k

survey_80 <- survey_clean %>%
              mutate(above_80k = if_else(annual_salary> 80000, 1, 0)) %>%
              select(-annual_salary)

head(survey_80)

# Splitting the data into train and test
index2 <- createDataPartition(survey80$above_80k, p = .70, list = FALSE)
train2 <- survey_80[index, ]
test2 <- survey_80[-index, ]
# Training the model
logistic_model <- glm(above_80k ~ *, family = binomial(), train2)
# Checking the model
summary(logistic_model)




train2$pred_class <- ifelse(logistic_model$fitted.values >= .5, "1", "0")
# Generating the classification table
ctab_train <- table(train2$above_80k, train2$pred_class)
ctab_train




test2$pred_class <- ifelse(pred_prob >= .5, "1", "0")
# Generating the classification table
ctab_test <- table(train2$above_80k, train2$pred_class)
ctab_test



accuracy_train <- sum(diag(ctab_train))/sum(ctab_train)*100
accuracy_train

#Misclassification Rate
#Misclassification Rate indicates how often is our predicted values are False.
#Misclassification Rate = (FP+FN)/(TN + FP + FN + TP)

misclass_test<- sum(diag(ctab_test))
misclass_test


# Recall in Train dataset
Recall <- (ctab_train[2, 2]/sum(ctab_train[2, ]))*100
Recall



#precision
#Precision indicates how often does your predicted TRUE values are actually TRUE.
#Precision = TP/FP + TP
Precision <- (ctab_train[2, 2]/sum(ctab_train[, 2]))*100
Precision



#fscore
#F-Score is a harmonic mean of recall and precision. The score value lies between 0 and 1. 
#The value of 1 represents perfect precision & recall. The value 0 represents the worst case.
F_Score <- (2 * Precision * Recall / (Precision + Recall))/100
F_Score

