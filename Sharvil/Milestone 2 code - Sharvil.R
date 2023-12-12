#Note: We skipped the code to plot confidence bands whenever categorical independent variables were involved
#Loading the required libraries:
#Please install these libraries in case they are not installed using the install.packages() function
library(dplyr)
library(tidyverse)
library(caTools)
library(ggplot2)
library(gmodels)
library(glmnet)


#Importing the data. (Please select job_postings.csv file after running the read.csv function)
dataset <- read.csv(file.choose(), header = TRUE)

set.seed(1234)
attach(dataset)


#Question 1: Linear Regression:

#Variable 1.	Can we predict the number of applicants a job post will receive given the number of views that it receives? 

#Creating the dataframe of the two columns for the research, for convenience.
dataset_R1 <- data.frame(applies,views)

#Removing the rows with NA values
dataset_R1 <- drop_na(dataset_R1)

#Splitting and creating the sample data. And creating the Training and Testing set for our model and predictor
split_R1 <- sample.split(dataset_R1$applies, SplitRatio = 0.75)
training_set_R1 <- subset(dataset_R1, split_R1 == TRUE)
testing_set_R1 <- subset(dataset_R1, split_R1 == FALSE)

#Creating the linear regression model
lm_R1 <- lm(applies ~ views, data <- training_set_R1)
lm_R1
summary(lm_R1)

#Plotting the testing set. And removing the outliers by using the ylim and xlim attributes
plot(testing_set_R1$applies ~ testing_set_R1$views, xlab="Number of Views", ylab = "Number of Applicants", ylim =c(0,150), xlim= c(0,500))
abline(lm_R1, col = "red", lwd = 3)


#Check for Normal Distribution
qqnorm(resid(lm_R1))
qqline(resid(lm_R1))
#Specifying the residuals and fitted values into the training set
R1_residual_fitted_df <- data.frame(training_set_R1, fittd.value = fitted(lm_R1), residual = resid(lm_R1))

#Using the trained model to predict the testing data
fitted_values_R1 <- predict(lm_R1, newdata = testing_set_R1)
fitted_values_R1
#Creating the dataset with the predicted values
final_dataset_R1 <- data.frame(testing_set_R1, fitted_values_R1)

#Confidence and Prediction Bands
#THIS IS THE CODE I'M RUNNING FOR CONFIDENCE BAND. OUTLIERS ARE REMOVED IN THIS
plot(testing_set_R1$views,testing_set_R1$applies, xlab="Number of Views", ylab = "Number of Applicants", ylim = c(0,150), xlim = c(0,800))

predictor_R1_confidence_band <- predict(lm_R1,int="c", newdata = testing_set_R1)
predictor_R1_prediction_band <- predict(lm_R1,int="p", newdata = testing_set_R1)
matlines(testing_set_R1$views, predictor_R1_confidence_band)
matlines(testing_set_R1$views, predictor_R1_prediction_band)

#Reporting Prediction Accuracy:
# See how well it correlates with the actual test applications
cor(fitted_values_R1,testing_set_R1$applies)
summary(lm_R1)

#-----------------------------------------------------------------------------------------------------------------------------#

#2.	 Can we predict the number of applicants based on the job's salary information?
#The clean_salary column and applies column will be used in this.

dataset_R2 <- data.frame(applies, clean_salary)

#Removing the NA values
dataset_R2 <- drop_na(dataset_R2)

#Splitting the values into 70-30 proportion
split_R2 <- sample.split(dataset_R2$applies, SplitRatio = 0.7)

#Creating the Training and Testing Datasets
training_set_R2 <- subset(dataset_R2, split_R2 == TRUE)
testing_set_R2 <- subset(dataset_R2, split_R2 == FALSE)

#Creating the linear regression model
lm_R2 <- lm(applies ~ clean_salary, data = training_set_R2)
lm_R2
summary(lm_R2)


options(scipen = 999)
plot(testing_set_R2$applies ~ testing_set_R2$clean_salary, xlab="Salary", ylab = "Number of Applicants")
abline(lm_R2, col="red", lwd = 3)



#Specifying the residuals and fitted values into the training set
R2_residual_fitted_df <- data.frame(training_set_R2, fittd.value = fitted(lm_R2), residual = resid(lm_R2))


#Using the trained model to predict the testing data
fitted_values_R2 <- predict(lm_R2, newdata = testing_set_R2)
fitted_values_R2
#Creating the dataset with the predicted values
final_dataset_R2 <- data.frame(testing_set_R2, fitted_values_R2)


#Confidence and Prediction Bands
#THIS IS THE CODE I'M RUNNING FOR CONFIDENCE BAND. OUTLIERS ARE REMOVED IN THIS
plot(testing_set_R2$clean_salary, testing_set_R2$applies, xlab="Salary", ylab = "Number of Applicants")


predictor_R2_confidence_band <- predict(lm_R2,int="c", newdata = testing_set_R2)
predictor_R2_prediction_band <- predict(lm_R2,int="p", newdata = testing_set_R2)
matlines(testing_set_R2$clean_salary, predictor_R2_confidence_band)
matlines(testing_set_R2$clean_salary, predictor_R2_prediction_band)

#Check the residuals for Normal Distribution
qqnorm(resid(lm_R2))
qqline(resid(lm_R2))


#Reporting Prediction Accuracy:
#See how well it correlates with the actual test applications
summary(lm_R2)
cor(fitted_values_R2,testing_set_R2$applies)



#---------------------------------------------------------------------------------------------#


#Can we predict a job's salary based on the experience level, and also the industry the job is from?

#Predicting salary based on the experience level

#importing Another file. Please choose company_industries file
company_industries <- read.csv(file.choose())
#Joining the two csv files
merged_data1 <- merge(x=dataset, y=company_industries, by = "company_id")


#separating the columns
dataset_R3a <- data.frame(merged_data1$clean_salary, merged_data1$formatted_experience_level)

#Changing the column names
colnames(dataset_R3a) <- c("clean_salary", "experience_level")

#removing NA values
dataset_R3a$experience_level[dataset_R3a$experience_level==""] <- NA
dataset_R3a <- drop_na(dataset_R3a)

#Changing datatype for salary column and converting it to int
dataset_R3a$clean_salary <- as.integer(dataset_R3a$clean_salary)

#Splitting the dataset, 70-30
split_R3a <- sample.split(dataset_R3a$clean_salary, SplitRatio = 0.8)

#Creating the Training & Testing sets
training_set_R3a <- subset(dataset_R3a, split_R3a == TRUE)
testing_set_R3a <- subset(dataset_R3a, split_R3a == FALSE)

#Creating the linear regression model
lm_R3a <- lm(clean_salary ~ experience_level, data=training_set_R3a)
lm_R3a
summary(lm_R3a)


#Taking care of scientific notation
options(scipen=999)




#Specifying the residuals and fitted values into the training set
R3a_residual_fitted_df <- data.frame(training_set_R3a, fittd.value = fitted(lm_R3a), residual = resid(lm_R3a))


#Using the trained model to predict the testing data
fitted_values_R3a <- predict(lm_R3a, newdata = testing_set_R3a)
fitted_values_R3a
#Creating the dataset with the predicted values
final_dataset_R3a <- data.frame(testing_set_R3a, fitted_values_R3a)


#THIS IS THE CODE I'M RUNNING FOR CONFIDENCE BAND.
plot(factor(testing_set_R3a$experience_level), testing_set_R3a$clean_salary, xlab = "Experience Level", ylab = "Salary")
abline(lm_R3a, col = "red", lwd = 3)

#Check the residuals for Normal Distribution
qqnorm(resid(lm_R3a))
qqline(resid(lm_R3a))

#Reporting Prediction Accuracy:
#See how well it correlates with the actual test applications
summary(lm_R3a)
cor(fitted_values_R3a,testing_set_R3a$clean_salary)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Predicting salary based on the industry

#separating the columns. Using the merged_data dataframe we created in last variable work
dataset_R3b <- data.frame(merged_data1$clean_salary, merged_data1$industry)

#Changing the column names
colnames(dataset_R3b) <- c("clean_salary", "industry")
dataset_R3b$industry <- as.factor(dataset_R3b$industry)
#removing NA values
dataset_R3b <- drop_na(dataset_R3b)

#Changing datatype for salary column and converting it to int
dataset_R3b$clean_salary <- as.integer(dataset_R3b$clean_salary)
#Splitting the dataset, 95-10
#95 - 10 because it would give an error Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) :factor industry has new levels 
split_R3b <- sample.split(dataset_R3b$clean_salary, SplitRatio = 0.95)

#Creating the Training & Testing sets
training_set_R3b <- subset(dataset_R3b, split_R3b == TRUE)
testing_set_R3b <- subset(dataset_R3b, split_R3b == FALSE)
#Creating the linear regression model
lm_R3b <- lm(clean_salary ~ industry, data=training_set_R3b)
lm_R3b
summary(lm_R3b)

#Taking care of scientific notation
options(scipen=999)

#Specifying the residuals and fitted values into the training set
R3b_residual_fitted_df <- data.frame(training_set_R3b, fittd.value = fitted(lm_R3b), residual = resid(lm_R3b))

#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R3b again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R3b again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored when creating it. So that error occurs. That's why we have taken split ratio of 0.95
#Using the trained model to predict the testing data
fitted_values_R3b <- predict(lm_R3b, newdata = testing_set_R3b, na.action = na.omit)
fitted_values_R3b
#Creating the dataset with the predicted values
final_dataset_R3b <- data.frame(testing_set_R3b, fitted_values_R3b)




#Confidence and Prediction Bands
#THIS IS THE CODE I'M RUNNING FOR CONFIDENCE BAND. OUTLIERS ARE REMOVED IN THIS
plot(factor(testing_set_R3b$industry), testing_set_R3b$clean_salary, xlab = "Industry", ylab = "Salary")
abline(lm_R3b, col = "red", lwd = 3)


#Check the residuals for Normal Distribution
qqnorm(resid(lm_R3b))
qqline(resid(lm_R3b))


#Reporting Prediction Accuracy:
#See how well it correlates with the actual test applications
summary(lm_R3b)
cor(fitted_values_R3b,testing_set_R3b$clean_salary)


#---------------------------------------------------------------------------
#Can we predict the number of views a job advertisement will receive based on the size of the company posting the ad?

#Please import the companies.csv file in this:
company_data <- read.csv(file.choose())

merged_data2 <- merge(x = dataset, y=company_data, by = "company_id")
dataset_R4 <- data.frame(merged_data2$views, merged_data2$company_size)

#changing column names:
colnames(dataset_R4) <- c("views","company_size")
set.seed(123)
#dropping the NA values
dataset_R4 <- drop_na(dataset_R4)
dataset_R4$company_size <- as.numeric(dataset_R4$company_size)
#Splitting the dataset into 70-30 proportion
split_R4 <- sample.split(dataset_R4$company_size, SplitRatio = 0.7)

#Creating the Training and Testing sets
training_set_R4 <- subset(dataset_R4, split_R4 == TRUE)
testing_set_R4 <- subset(dataset_R4, split_R4 == FALSE)


#Creating the linear regression model
lm_R4 <- lm(views ~ company_size, data =training_set_R4 )
lm_R4

summary(lm_R4)
#Plotting the testing set. And removing the outliers by using the ylim and xlim attributes
plot(testing_set_R4$views ~ testing_set_R4$company_size, xlab = "Company Size", ylab = "Number of Views")
abline(lm_R4, col = "red", lwd = 3)

R4_residual_fitted_df <- data.frame(training_set_R4, fittd.value = fitted(lm_R4), residual = resid(lm_R4))


#Check for Normal Distribution
qqnorm(resid(lm_R4))
qqline(resid(lm_R4))

#Using the trained model to predict the testing data
fitted_values_R4 <- predict(lm_R4, newdata = testing_set_R4)
fitted_values_R4
#Creating the dataset with the predicted values
final_dataset_R4 <- data.frame(testing_set_R4, fitted_values_R4)


#Confidence and Prediction Bands
plot(testing_set_R4$company_size,testing_set_R4$views, xlab="Company Size", ylab = "Number of Views", ylim = c(0,800))


#Reporting Prediction Accuracy:
# See how well it correlates with the actual test applications
cor(fitted_values_R4,testing_set_R4$views)
summary(lm_R4)

#Predicting the values:
#Using the trained model to predict the testing data
fitted_values_R4 <- predict(lm_R4, newdata = testing_set_R4)
fitted_values_R4
#Creating the dataset with the predicted values
final_dataset_R4 <- data.frame(testing_set_R4, fitted_values_R4)



#------------------------------------------------------------------------------------------------------------------------------------------------------

#Can we predict the number of applicants a job post will receive when the independent variable is the Experience Level 

#Creating the dataframe of the two columns for the research, for convenience.
dataset_R5 <- data.frame(applies,formatted_experience_level)
colnames(dataset_R5) <- c("applies", "experience_level")

#Removing the rows with NA values

dataset_R5$experience_level[dataset_R5$experience_level==""] <- NA
dataset_R5 <- drop_na(dataset_R5)
#Splitting and creating the sample data. And creating the Training and Testing set for our model and predictor
split_R5 <- sample.split(dataset_R5$applies, SplitRatio = 0.7)

training_set_R5 <- subset(dataset_R5, split_R5 == TRUE)

testing_set_R5 <- subset(dataset_R5, split_R5 == FALSE)

#Creating the linear regression model
lm_R5 <- lm(applies ~ experience_level, data <- training_set_R5)
lm_R5
summary(lm_R5)

#Plotting the testing set. And removing the outliers by using the ylim and xlim attributes
plot(testing_set_R5$applies ~ factor(testing_set_R5$experience_level), xlab="Experience Level", ylab = "Number of Applicants")
abline(lm_R1, col = "red", lwd = 3)

#Check for Normal Distribution
qqnorm(resid(lm_R5))
qqline(resid(lm_R5))

#Specifying the residuals and fitted values into the training set
R5_residual_fitted_df <- data.frame(training_set_R5, fittd.value = fitted(lm_R5), residual = resid(lm_R5))

#Using the trained model to predict the testing data
fitted_values_R5 <- predict(lm_R5, newdata = testing_set_R5)
fitted_values_R5
#Creating the dataset with the predicted values
final_dataset_R5 <- data.frame(testing_set_R5, fitted_values_R5)

#Reporting Prediction Accuracy:
# See how well it correlates with the actual test applications
cor(fitted_values_R5,testing_set_R5$applies)
summary(lm_R5)

#Confidence Interval for the model
confint(lm_R5)
#------------------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------------------

#Can we predict the number of applicants when the independent variable is the work type?
#Creating the dataframe of the two columns for the research, for convenience.
dataset_R6 <- data.frame(applies,formatted_work_type)
colnames(dataset_R6) <- c("applies", "work_type")

#Removing the rows with NA values

dataset_R6 <- drop_na(dataset_R6)
#Splitting and creating the sample data. And creating the Training and Testing set for our model and predictor
split_R6 <- sample.split(dataset_R6$applies, SplitRatio = 0.7)

training_set_R6 <- subset(dataset_R6, split_R6 == TRUE)

testing_set_R6 <- subset(dataset_R6, split_R6 == FALSE)

#Creating the linear regression model
lm_R6 <- lm(applies ~ work_type, data <- training_set_R6)
lm_R6
summary(lm_R6)

#Plotting the testing set. And removing the outliers by using the ylim and xlim attributes
plot(testing_set_R6$applies ~ factor(testing_set_R6$work_type), xlab="Work Type", ylab = "Number of Applicants")
abline(lm_R1, col = "red", lwd = 3)

#Check for Normal Distribution
qqnorm(resid(lm_R6))
qqline(resid(lm_R6))

#Specifying the residuals and fitted values into the training set
R6_residual_fitted_df <- data.frame(training_set_R6, fittd.value = fitted(lm_R6), residual = resid(lm_R6))

#Using the trained model to predict the testing data
fitted_values_R6 <- predict(lm_R6, newdata = testing_set_R6)
fitted_values_R6
#Creating the dataset with the predicted values
final_dataset_R6 <- data.frame(testing_set_R6, fitted_values_R6)

#THIS IS THE CODE I'M RUNNING FOR CONFIDENCE BAND. OUTLIERS ARE REMOVED IN THIS
plot(testing_set_R6$applies ~ factor(testing_set_R6$work_type), xlab="Work Type", ylab = "Number of Applicants", pch=16)
abline(lm_R6, col= "red", lwd = 3)

#Reporting Prediction Accuracy:
# See how well it correlates with the actual test applications
cor(fitted_values_R6,testing_set_R6$applies)
summary(lm_R6)
#------------------------------------------------------------------------------------------------------------------------------------------------------


#Can we predict the number of applicants when the independent variable is the job's state location?
#Creating the dataframe of the two columns for the research, for convenience.

dataset_R7 <- data.frame(applies,state)
colnames(dataset_R7) <- c("applies", "state")

#Removing the rows with NA values

dataset_R7 <- drop_na(dataset_R7)
#Splitting and creating the sample data. And creating the Training and Testing set for our model and predictor
#Taking SplitRation = 0.95 because since one variable is categorical, when we try to predict, using the model on the testing data, then, some categories/state locations do not get added into the lm model when 0.7 is taken


#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R7 again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R7 again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored when creating it. So that error occurs. That's why we have taken split ratio of 0.95

split_R7 <- sample.split(dataset_R7$applies, SplitRatio = 0.95)

training_set_R7 <- subset(dataset_R7, split_R7 == TRUE)

testing_set_R7 <- subset(dataset_R7, split_R7 == FALSE)

#Creating the linear regression model
lm_R7 <- lm(applies ~ state, data <- training_set_R7)
lm_R7
summary(lm_R7)

#Plotting the testing set. And removing the outliers by using the ylim and xlim attributes
plot(testing_set_R7$applies ~ factor(testing_set_R7$state), xlab="Work Type", ylab = "Number of Applicants")
abline(lm_R1, col = "red", lwd = 3)

#Check for Normal Distribution
qqnorm(resid(lm_R7))
qqline(resid(lm_R7))

#Specifying the residuals and fitted values into the training set
R7_residual_fitted_df <- data.frame(training_set_R7, fittd.value = fitted(lm_R7), residual = resid(lm_R7))

#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R7 again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R7 again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored when creating it. So that error occurs. That's why we have taken split ratio of 0.95
#Using the trained model to predict the testing data
fitted_values_R7 <- predict(lm_R7, newdata = testing_set_R7)
fitted_values_R7
#Creating the dataset with the predicted values
final_dataset_R7 <- data.frame(testing_set_R7, fitted_values_R7)


#Confidence and Prediction Bands
#THIS IS THE CODE I'M RUNNING FOR CONFIDENCE BAND
plot(testing_set_R7$applies ~ factor(testing_set_R7$state), xlab="Location", ylab = "Number of Applicants")
abline(lm_R7, col="red", lwd = 3)


#Reporting Prediction Accuracy:
# See how well it correlates with the actual test applications
cor(fitted_values_R7,testing_set_R7$applies)
summary(lm_R7)
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------

#Multivariate Regressions:
#Considering all independent variables, and DV as the number of applications
#Creating the dataset
dataset_R8 <- data.frame(applies,views,clean_salary,formatted_experience_level,formatted_work_type,state)


#Cleaning up the data
dataset_R8$formatted_experience_level[dataset_R8$formatted_experience_level==""] <- NA
dataset_R8$state[dataset_R8$state==""] <- NA
dataset_R8$clean_salary <- as.integer(dataset_R8$clean_salary)
dataset_R8  <- drop_na(dataset_R8 )
#Splitting the data and creating training and testing sets
#SplitRatio of 0.95 is because
set.seed(4622313)
split_R8 <- sample.split(dataset_R8$applies, SplitRatio = 0.95)

training_set_R8 <- subset(dataset_R8, split_R8 == TRUE)

testing_set_R8 <- subset(dataset_R8, split_R8 == FALSE)


#Creating the linear regression Model
lm_R8 <- lm(applies ~., data <- training_set_R8)
lm_R8
summary(lm_R8)


#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R8 again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R8 again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored and when creating the testing data, different categories get stored, So that error occurs. That's why we have taken split ratio of 0.95
#Using the trained model to predict the testing data
fitted_values_R8 <- predict(lm_R8, newdata = testing_set_R8)
fitted_values_R8
#Creating the dataset with the predicted values
final_dataset_R8 <- data.frame(testing_set_R8, fitted_values_R8)

#Prediction Accuracy
cor(fitted_values_R8,testing_set_R8$applies)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------

#Considering IV as a combination of number of views and salary

dataset_R9 <- data.frame(applies,views,clean_salary)
dataset_R9  <- drop_na(dataset_R9)

#Cleaning up the data
dataset_R9$clean_salary <- as.integer(dataset_R9$clean_salary)

#Splitting the data and creating training and testing sets
#SplitRatio of 0.95 is because
set.seed(4622313)
split_R9 <- sample.split(dataset_R9$applies, SplitRatio = 0.8)

training_set_R9 <- subset(dataset_R9, split_R9 == TRUE)

testing_set_R9 <- subset(dataset_R9, split_R9 == FALSE)


#Creating the linear regression Model
lm_R9 <- lm(applies ~., data <- training_set_R9)
lm_R9
summary(lm_R9)



#Using the trained model to predict the testing data
fitted_values_R9 <- predict(lm_R9, newdata = testing_set_R9)
fitted_values_R9
#Creating the dataset with the predicted values
final_dataset_R9 <- data.frame(testing_set_R9, fitted_values_R9)

#Prediction Accuracy
cor(fitted_values_R9,testing_set_R9$applies)

#------------------------------------------------------------------------------------------------------------------------------------------------------

#Considering IV as a combination of experience level, work type and state

dataset_R10 <- data.frame(applies,formatted_experience_level, formatted_work_type, state)

#Cleaning up the data
dataset_R10$formatted_experience_level[dataset_R10$formatted_experience_level==""] <- NA
dataset_R10$state[dataset_R10$state==""] <- NA
dataset_R10  <- drop_na(dataset_R10)


#Splitting the data and creating training and testing sets
#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R10 again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R10 again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored when creating it. So that error occurs. That's why we have taken split ratio of 0.95

set.seed(1)
split_R10 <- sample.split(dataset_R10$applies, SplitRatio = 0.95)

training_set_R10 <- subset(dataset_R10, split_R10 == TRUE)

testing_set_R10 <- subset(dataset_R10, split_R10 == FALSE)


#Creating the linear regression Model
lm_R10 <- lm(applies ~., data <- training_set_R10)
lm_R10
summary(lm_R10)

#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R10 again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R10 again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored when creating it. So that error occurs. That's why we have taken split ratio of 0.95


#Using the trained model to predict the testing data
fitted_values_R10 <- predict(lm_R10, newdata = testing_set_R10)
fitted_values_R10
#Creating the dataset with the predicted values
final_dataset_R10 <- data.frame(testing_set_R10, fitted_values_R10)

#Prediction Accuracy
cor(fitted_values_R10,testing_set_R10$applies)

#------------------------------------------------------------------------------------------------------------------------------------------------------

#Considering IV as a combination of experience level and industry. DV is salary

#Importing the data:
#importing Another file. Please choose company_industries file in this
#Joining the two csv files
merged_data3 <- merge(x=dataset, y=company_industries, by = "company_id")
dataset_R11 <- data.frame(merged_data3$clean_salary,merged_data3$formatted_experience_level, merged_data3$industry)

#Changing Column names
colnames(dataset_R11) <- c("clean_salary","experience_level","industry")
#Cleaning up the data
dataset_R11$experience_level[dataset_R11$experience_level==""] <- NA
dataset_R11  <- drop_na(dataset_R11)


#Splitting the data and creating training and testing sets
set.seed(123456)
split_R11 <- sample.split(dataset_R11$clean_salary, SplitRatio = 0.95)

training_set_R11 <- subset(dataset_R11, split_R11 == TRUE)

testing_set_R11 <- subset(dataset_R11, split_R11 == FALSE)



#IMPORTANT NOTE: If the predict() function gives an error, please run the code for split_R11 again and then try running the all the code till predict function again. Thank you.
#REPEATING AGAIN: If the predict() function gives an error, please run the code for split_R11 again and then try running the all the code till predict function line again. Thank you.
#The error occurs because the the IV contains categorical data. And in the Testing set, not all of the categories get stored when creating it. So that error occurs. That's why we have taken split ratio of 0.95


#Creating the linear regression Model
lm_R11 <- lm(clean_salary ~., data <- training_set_R11)
lm_R11
summary(lm_R11)



#Using the trained model to predict the testing data
fitted_values_R11 <- predict(lm_R11, newdata = testing_set_R11)
fitted_values_R11
#Creating the dataset with the predicted values
final_dataset_R11 <- data.frame(testing_set_R11, fitted_values_R11)

#Prediction Accuracy
cor(fitted_values_R11,testing_set_R11$clean_salary)


#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#Regularization
#Regularization won't work if the data isn't numeric


regularization_dataset_R12 <- data.frame(applies,clean_salary,views)

#Cleaning up the data
regularization_dataset_R12 <- drop_na(regularization_dataset_R12)

#Splitting the data
regularization_split_R12 <- sample.split(regularization_dataset_R12$applies, SplitRatio = 0.8)

training_set_R12 <- subset(regularization_dataset_R12, regularization_split_R12 == TRUE)

testing_set_R12 <- subset(regularization_dataset_R12, regularization_split_R12 == FALSE)


#USING cv.glmnet function to create the regularization fit
regularization_fit_R12 <- cv.glmnet(as.matrix(training_set_R12[,-1]), as.vector(training_set_R12[,1]), alpha =1)

#Creating the plot
plot(regularization_fit_R12) 

#Getting the lambda min value
regularization_fit_R12$lambda.min

#Getting the Coefficients
coef(regularization_fit_R12)


regularization_prediction_R12 <- predict(regularization_fit_R12, newx = as.matrix((testing_set_R12[,c(-1)])) )


#Checking the corelation for regularization.
cor(regularization_prediction_R12, as.vector(testing_set_R12[,1]))
#This is original prediction accuracy
cor(fitted_values_R9,testing_set_R9$applies)


#------------------------------------------------------------------------------------------------------

#Q1 d.
#Running test only for the most predictive variables. Here, it is views
#x value can be changed
#IV is views, DV is applies
for (x in 1:5) {
  set.seed(x)
  split_R1 <- sample.split(dataset_R1$applies, SplitRatio = 0.75)
  training_set_R1 <- subset(dataset_R1, split_R1 == TRUE)
  testing_set_R1 <- subset(dataset_R1, split_R1 == FALSE)
  lm_R1 <- lm(applies ~ views, data <- training_set_R1)
  cat("Run no", x,"\n")
  cat("Coefficients:", lm_R1$coefficients, "\n")
  cat("correlation between fitted values and testing dataset:", cor(fitted_values_R1,testing_set_R1$applies), " \n")
  cat("R-squared:", summary(lm_R1)$r.squared, "\n\n")
}

#Multiple tests for Multivate Regression test 2: IV is clean_salary and views. DV is applies
for (x in 1:5) {
  set.seed(4622313)
  split_R9 <- sample.split(dataset_R9$applies, SplitRatio = 0.8)
  training_set_R9 <- subset(dataset_R9, split_R9 == TRUE)
  testing_set_R9 <- subset(dataset_R9, split_R9 == FALSE)
  lm_R9 <- lm(applies ~., data <- training_set_R9)
  #Using the trained model to predict the testing data
  fitted_values_R9 <- predict(lm_R9, newdata = testing_set_R9)
  cat("Run no", x,"\n")
  cat("Intercept:", lm_R9$coefficients[1], "\n")
  cat("Coefficients:", lm_R9$coefficients, "\n")
  cat("correlation between fitted values and testing dataset:", cor(fitted_values_R9,testing_set_R9$applies), " \n")
  cat("R-squared:", summary(lm_R9)$r.squared, "\n")
  cat("\n\n")

}

#There will be lots of coefficients for this. That is because the IV's are categorical here and hence split ratio chosen is 0.95
for (x in 1:5) {
  set.seed(4622313)
  split_R10 <- sample.split(dataset_R10$applies, SplitRatio = 0.95)
  training_set_R10 <- subset(dataset_R10, split_R10 == TRUE)
  testing_set_R10 <- subset(dataset_R10, split_R10 == FALSE)
  lm_R10 <- lm(applies ~., data <- training_set_R10)
  #Using the trained model to predict the testing data
  fitted_values_R10 <- predict(lm_R10, newdata = testing_set_R10)
  cat("Run no", x,"\n")
  cat("Intercept:", lm_R10$coefficients[1], "\n")
  cat("Coefficients:", lm_R10$coefficients, sep = "\n")
  cat("Correlation between fitted values and testing dataset:", cor(fitted_values_R10,testing_set_R10$applies), " \n")
  cat("R-squared:", summary(lm_R10)$r.squared, "\n")
  cat("\n\n")
  
}


#IV = salary, DV = applies
for (x in 1:5) {
  set.seed(46241)
  split_R2 <- sample.split(dataset_R2$applies, SplitRatio = 0.95)
  training_set_R2 <- subset(dataset_R2, split_R2 == TRUE)
  testing_set_R2 <- subset(dataset_R2, split_R2 == FALSE)
  lm_R2 <- lm(applies ~., data <- training_set_R2)
  #Using the trained model to predict the testing data
  fitted_values_R2 <- predict(lm_R2, newdata = testing_set_R2)
  cat("Run no", x,"\n")
  cat("Intercept:", lm_R2$coefficients[1], "\n")
  cat("Coefficients:", lm_R2$coefficients, sep = "\n")
  cat("Correlation between fitted values and testing dataset:", cor(fitted_values_R2,testing_set_R2$applies), " \n")
  cat("R-squared:", summary(lm_R2)$r.squared, "\n")
  cat("\n\n")
}

#IV = experience_level, DV = applies
for (x in 1:5) {
  set.seed(46241)
  split_R5 <- sample.split(dataset_R5$applies, SplitRatio = 0.95)
  training_set_R5 <- subset(dataset_R5, split_R5 == TRUE)
  testing_set_R5 <- subset(dataset_R5, split_R5 == FALSE)
  lm_R5 <- lm(applies ~., data <- training_set_R5)
  #Using the trained model to predict the testing data
  fitted_values_R5 <- predict(lm_R5, newdata = testing_set_R5)
  cat("Run no", x,"\n")
  cat("Intercept:", lm_R5$coefficients[1], "\n")
  cat("Coefficients:", lm_R5$coefficients, sep = "\n")
  cat("Correlation between fitted values and testing dataset:", cor(fitted_values_R5,testing_set_R5$applies), " \n")
  cat("R-squared:", summary(lm_R5)$r.squared, "\n")
  cat("\n\n")
}

#IV = work_type, DV = applies
for (x in 1:5) {
  set.seed(x)
  split_R6 <- sample.split(dataset_R6$applies, SplitRatio = 0.95)
  training_set_R6 <- subset(dataset_R6, split_R6 == TRUE)
  testing_set_R6 <- subset(dataset_R6, split_R6 == FALSE)
  lm_R6 <- lm(applies ~., data <- training_set_R6)
  #Using the trained model to predict the testing data
  fitted_values_R6 <- predict(lm_R6, newdata = testing_set_R6)
  cat("Run no", x,"\n")
  cat("Intercept:", lm_R6$coefficients[1], "\n")
  cat("Coefficients:", lm_R6$coefficients, sep = "\n")
  cat("Correlation between fitted values and testing dataset:", cor(fitted_values_R6,testing_set_R6$applies), " \n")
  cat("R-squared:", summary(lm_R6)$r.squared, "\n")
  cat("\n\n")
}


#Multiple runs for Regularization
for (x in 1:5) {
  set.seed(x+sample.int(100,10))
  regularization_split_R12 <- sample.split(regularization_dataset_R12$applies, SplitRatio = 0.8)
  training_set_R12 <- subset(regularization_dataset_R12, regularization_split_R12 == TRUE)
  testing_set_R12 <- subset(regularization_dataset_R12, regularization_split_R12 == FALSE)
  regularization_fit_R12 <- cv.glmnet(as.matrix(training_set_R12[,-1]), as.vector(training_set_R12[,1]), alpha =1)
  regularization_prediction_R12 <- predict(regularization_fit_R12, newx = as.matrix((testing_set_R12[,c(-1)])) )
  cat("Run no", x,"\n")
  cat("Intercept:", regularization_fit_R12$coefficients[1], "\n")
 # cat("Coefficients:", coef(regularization_fit_R12), sep = "\n")
  cat("lambda value:",regularization_fit_R12$lambda.min,"\n")
  cat("Correlation between fitted values and testing dataset:\n")
  cat("Original:" ,cor(fitted_values_R9,testing_set_R9$applies),"\n")
  cat("After Regularization:", cor(regularization_prediction_R12, as.vector(testing_set_R12[,1])),"\n")
  cat("\n\n")
}




