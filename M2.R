#Logistic Regression Code
#Loading the required libraries:
#Please install these libraries in case they are not installed using the install.packages() function
library(dplyr)
library(tidyverse)
library(caTools)
library(ggplot2)
library(gmodels)
library(glmnet)
library(aod)
library(mlogit)

#Importing the data. (Please select job_postings.csv file after running the read.csv function)
dataset <- read.csv(file.choose(), header = TRUE)
set.seed(1)
attach(dataset)

View(dataset)
dataset$class_applies <- as.factor(dataset$class_applies)
dataset$views <- as.integer(dataset$views)
dataset$formatted_experience_level <- as.factor(dataset$formatted_experience_level)
dataset$clean_salary <- as.integer(dataset$clean_salary)

#Creating the dataframe of the two columns for the research, for convenience.
dataset_R1 <- data.frame(applies,views,clean_salary, formatted_experience_level, class_applies,class_salary, class_views)
view(dataset_R1)
#Removing the rows with NA values
dataset_R1 <-drop_na(dataset_R1)
view(dataset_R1)

#Converting class_applies to binomial
dataset_R1 <- dataset_R1 %>%
  mutate(class_applies = ifelse(class_applies %in% c("Unspecified", "Low"), 0, 1))
 
#Splitting the data into training and testing sets based on 70/30 split
split <- sample.split(dataset_R1$applies, SplitRatio= 0.7)
training_set <-subset(dataset_R1, split_RQ1 == TRUE)
testing_set <-subset(dataset_R1, split_RQ1 == FALSE)


#Question 2: Logistic Regression and NB
#Research Q1:	Can we predict the number of applicants a job post will receive given the number of views that it receives? 
classifier <- glm(class_applies~views,family = "binomial", data = training_set)
classifier
#output class probabilities
prob_pred <-predict(classifier,type='response', data = testing_set)
prob_pred
classifier2 <- glm(class_applies~views+formatted_experience_level, data = training_set, family = "binomial")
classifier2
ggplot(dataset_R1, aes(x=class_applies,y=views))+geom_line(color=class_applies)


#Research Q2: Can we predict the number of applicants based on the job's salary information?
#Research Q3: Can we predict a job's salary based on the industry the job is from and where the job is located?
#Research Q4: Can we predict the number of views a job advertisement will receive based on the size of the company posting the ad?


