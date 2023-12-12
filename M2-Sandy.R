setwd("G:/My Drive/MIM/INST 737")
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
View(dataset)
attach(dataset)
#Using Multivariate Regression 1:   The multivariate regression #1 from 1b above included the following variables:
#Dependent Variable - Number of applicants (we need to use clean_applies adjusted into binomial values)
#Independent Variables - Number of views, salary, work type

dataset$class_applies <- as.factor(dataset$class_applies)
dataset$views <- as.integer(dataset$views)
#Creating the dataframe of the relevant columns for the research, for convenience.
dataset_R1 <- data.frame(views, class_applies)
View(dataset_R1)
#Removing the rows with NA values
dataset_R1 <-drop_na(dataset_R1)
View(dataset_R1)
#Converting class_applies to binomial
dataset_R1 <- dataset_R1 %>%
  mutate(class_applies = ifelse(class_applies %in% c("Unspecified", "Low"), 0, 1))
View(dataset_R1)
#Splitting the data into training and testing sets based on 70/30 split
split1 <- sample.split(dataset_R1$class_applies, SplitRatio= 0.7)
training_set <-subset(dataset_R1, split1 == TRUE)
testing_set <-subset(dataset_R1, split1 == FALSE)
classifier <- glm(class_applies~views,family = "binomial", data = training_set)
classifier
prob_pred <-predict(classifier,type='response', newdata = testing_set)
prob_pred
y_pred <- ifelse(prob_pred>.5, 1,0)
y_pred  
library(gmodels)
CrossTable(testing_set[,2],y_pred, prop.chisq = FALSE,prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))
#new dataframe with additional fields 
dataset_R2 <- data.frame(views, class_applies, clean_salary, formatted_work_type, formatted_experience_level)
dataset_R2 <-drop_na(dataset_R2)
View(dataset_R2)
#Converting class_applies to binomial
dataset_R2 <- dataset_R2 %>%
  mutate(class_applies = ifelse(class_applies %in% c("Unspecified", "Low"), 0, 1))
attach(dataset_R2)
mylogit <- glm(class_applies~clean_salary+formatted_work_type,family = "binomial")
mylogit
summary(mylogit)

#Using Multivariate Regression 2: The multivariate regression #2 from 1b above included the following variables:
#Dependent Variable - Number of applicants ( (we need to use clean_applies adjusted into binomial values))
#Independent variables are views and clean_salary (i.e. the numeric variables)
dataset_R3 <- data.frame(views, class_applies, clean_salary)
dataset_R3 <-drop_na(dataset_R3)
View(dataset_R3)
#Converting class_applies to binomial
dataset_R3 <- dataset_R3 %>%
  mutate(class_applies = ifelse(class_applies %in% c("Unspecified", "Low"), 0, 1))
attach(dataset_R3)
#Splitting the data into training and testing sets based on 70/30 split
split1 <- sample.split(dataset_R3$class_applies, SplitRatio= 0.7)
training_set2 <-subset(dataset_R3, split1 == TRUE)
testing_set2 <-subset(dataset_R3, split1 == FALSE)
classifier2 <- glm(class_applies~.,family = "binomial", data = training_set2)
classifier2
prob_pred2 <-predict(classifier2,type='response', newdata = testing_set2)
prob_pred2
y_pred <- ifelse(prob_pred>.5, 1,0)
y_pred  
CrossTable(testing_set2[,5],y_pred, prop.chisq = FALSE,prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))
mylogit2 <- glm(class_applies~clean_salary+views,family = "binomial")
mylogit2
summary(mylogit2)
#Naive Bayes 
#Splitting the data into training and testing sets based on 70/30 split
split1 <- sample.split(dataset_R3$class_applies, SplitRatio= 0.7)
training_set2 <-subset(dataset_R3, split1 == TRUE)
testing_set2 <-subset(dataset_R3, split1 == FALSE)
library(e1071)
Classifier3 <-naiveBayes(training_set2, training_set2$class_applies)
Classifier3
predict2 <-predict(Classifier3, training_set2)
predict2
Classifier4 <-naiveBayes(training_set2, training_set2$class_applies,laplace = 1)
Classifier4

