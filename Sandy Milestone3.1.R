#Loading the required libraries:
#Please install these libraries in case they are not installed using the install.packages() function
library(e1071) #for SVM
library(tidyverse) #for Drop_NA
library(kernlab) # For Kernel

#Set the working directory
setwd("G:/My Drive/MIM/INST 737")
#Read and attach the dataset for convenience in referencing variables
jobpostings <- read.csv("job_postings.csv")
attach(jobpostings)

#View the data just to make sure it looks right
View(jobpostings)
#create dataframe for Question 1 for only the variables we are using, to simplify
jobpostings_Q1 <- data.frame(views,applies)
View(jobpostings_Q1)

#drop na's so the arguments have the same length
jobpostings_Q1 <- drop_na(jobpostings_Q1)
#Just checking to make sure it worked
View(jobpostings_Q1)

#separate the data into training and testing sets (80/20)
jobpostings_Q1_train <- jobpostings_Q1[1:6455,]
jobpostings_Q1_test <- jobpostings_Q1[6456:8069,]
View(jobpostings_Q1_train)
View(jobpostings_Q1_test)

#Fit the SVM regression  model on the training data and run prediction
svm_model <- svm(applies~views, data = jobpostings_Q1_train, kernel = "linear", epsilon = 0.1)
predictions_Q1_train <- predict(svm_model, jobpostings_Q1_train)

# Evaluate the performance (Mean Squared Error) of the training data
mse <- mean((jobpostings_Q1_train$applies - predictions_Q1_train)^2)
cat("Mean Squared Error on Training Data:", mse, "\n")

# Make predictions on the testing data
predictions_Q1 <- predict(svm_model, jobpostings_Q1_test)

# Evaluate the performance (Mean Squared Error) of the testing data
mse <- mean((jobpostings_Q1_test$applies - predictions_Q1)^2)
cat("Mean Squared Error on Testing Data:", mse, "\n")

# Plotting actual vs. predicted values for training data
predicted_applies_train <- predict(svm_model, jobpostings_Q1_train)
plot(jobpostings_Q1_train$views, jobpostings_Q1_train$applies, col = "blue", pch = 16, 
     main = "SVM Regression - Training Data")
points(jobpostings_Q1_train$views, predicted_applies_train, col="red", pch=4)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), pch = c(16, 4))


# Plotting actual vs. predicted values for testing data
predicted_applies_test <- predict(svm_model, jobpostings_Q1_test)
plot(jobpostings_Q1_test$views, jobpostings_Q1_test$applies, col = "blue", pch = 16, 
     main = "SVM Regression - Testing Data")
points(jobpostings_Q1_test$views, predicted_applies_test, col = "red", pch = 4)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), pch = c(16, 4))

#Using the same data frame and the same training and testing data sets
# Create SVM regression model with an RBF kernel
svm_model_rbf <- ksvm(applies ~ ., data = jobpostings_Q1_train, type = "eps-svr", kernel = "rbfdot")

# Make predictions on the training and testing data
predictions_train_Q1 <- predict(svm_model_rbf, jobpostings_Q1_train)
predictions_test_Q1 <- predict(svm_model_rbf, jobpostings_Q1_test)

# Evaluate the model performance on training data using MSE
mse_train_Q1 <- mean((jobpostings_Q1_train$applies - predictions_train_Q1)^2)
cat("MSE on Training Data (RBF Kernel):", mse_train_Q1, "\n")

# Evaluate the model performance on testing data using MSE
mse_test_Q1 <- mean((jobpostings_Q1_test$applies - predictions_test_Q1)^2)
cat("MSE on Testing Data (RBF Kernel):", mse_test_Q1, "\n")

#convert MSE to RMSE just to see how they compare
rmse_value_Q1_test <- sqrt(mse_test_Q1)
print(rmse_value_Q1_test)
rmse_value_Q1_train <- sqrt(mse_train_Q1)
print(rmse_value_Q1_train)


#####QUESTION 2########
#create dataframe for Question 2 for only the variables we are using, to simplify
jobpostings_Q2 <- data.frame(clean_salary,applies)
View(jobpostings_Q2)

#drop na's so the arguments have the same length
jobpostings_Q2 <- drop_na(jobpostings_Q2)
#Just checking to make sure it worked
View(jobpostings_Q2)
#separate the data into training and testing sets (80/20)
jobpostings_Q2_train <- jobpostings_Q2[1:2653,]
jobpostings_Q2_test <- jobpostings_Q2[2654:3316,]
View(jobpostings_Q2_train)
View(jobpostings_Q2_test)

#Fit the SVM regression  model on the training data and run prediction
svm_model_Q2 <- svm(applies~clean_salary, data = jobpostings_Q2_train, kernel = "linear", epsilon = 0.1)
predictions_Q2_train <- predict(svm_model_Q2, jobpostings_Q2_train)

# Evaluate the performance (Mean Squared Error) of the training data
mse_Q2 <- mean((jobpostings_Q2_train$applies - predictions_Q2_train)^2)
cat("Mean Squared Error on Training Data:", mse_Q2, "\n")

# Make predictions on the testing data
predictions_Q2 <- predict(svm_model_Q2, jobpostings_Q2_test)

# Evaluate the performance (Mean Squared Error) of the testing data
mse_Q2_test <- mean((jobpostings_Q2_test$applies - predictions_Q2)^2)
cat("Mean Squared Error on Testing Data:", mse_Q2_test, "\n")

# Plotting actual vs. predicted values for training data
predicted_applies_train_Q2 <- predict(svm_model_Q2, jobpostings_Q2_train)
plot(jobpostings_Q2_train$clean_salary, jobpostings_Q2_train$applies, col = "blue", pch = 16, 
     main = "SVM Regression - Training Data")
points(jobpostings_Q2_train$clean_salary, predicted_applies_train_Q2, col="red", pch=4)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), pch = c(16, 4))


# Plotting actual vs. predicted values for testing data
predicted_applies_test_Q2 <- predict(svm_model_Q2, jobpostings_Q2_test)
plot(jobpostings_Q2_test$clean_salary, jobpostings_Q2_test$applies, col = "blue", pch = 16, 
     main = "SVM Regression - Testing Data")
points(jobpostings_Q2_test$clean_salary, predicted_applies_test_Q2, col = "red", pch = 4)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), pch = c(16, 4))
