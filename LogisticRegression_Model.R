# Loading required libraries
library(tidyr)
library(dplyr)
library(pROC)
library(ggplot2)
library(caret)
library(corrplot)
library(gridExtra)
library(caTools)

# Setting the random seed based on student number
set.seed(22200878)

# Reading the dataset into the variable cardiac_data
cardiac_data <- read.csv("cardiac.csv")
head(cardiac_data)

# Descriptive Statistics
print(summary(cardiac_data))
str(cardiac_data)

# Checking the class of each variable for level of Measurement 
variable_class <- sapply(cardiac_data, class)

# Print class_variable
print(variable_class)

# Checking missing or Null values
any(is.na(cardiac_data))
# Display missing value counts for each variables
colSums(is.na(cardiac_data))

# Histograms for continuous numerical variables
hist(cardiac_data$age, col = "lightblue", main = "Age Distribution", xlab = "Age")
hist(cardiac_data$weight, col = "coral", main = "Weight Distribution", xlab = "Weight")
hist(cardiac_data$fitness_score, col = "darkgreen", main = "Fitness Score Distribution", xlab = "Fitness Score")

# Gender Distribution Pie Chart
gender_counts <- table(cardiac_data$gender)
colors_gender <- c("blue", "orange")
names(gender_counts) <- c("Female", "Male")
gender_labels <- paste(names(gender_counts), "\n", gender_counts)

# Plotting the pie chart for Gender Distribution
pie(gender_counts, labels = gender_labels, col = colors_gender, main = "Gender Distribution")

# Cardiac Condition Distribution Pie Chart
condition_counts <- table(cardiac_data$cardiac_condition)
colors_condition <- c("darkgreen", "red")
names(condition_counts) <- c("Absent", "Present")
condition_labels <- paste(names(condition_counts), "\n", condition_counts)

# Plotting the pie chart for Cardiac Condition Distribution
pie(condition_counts, labels = condition_labels, col = colors_condition, main = "Cardiac Condition Distribution")

# Converting gender to a numerical variable via one-hot encoding
# Converting gender to factor
cardiac_data$gender <- as.factor(cardiac_data$gender)
encoded_gender <- model.matrix(~ gender - 1, data = cardiac_data)

# Adding the 2 new encoded columns to the dataset
cardiac_data <- cbind(cardiac_data, encoded_gender)

# Dropping the original 'gender' column
cardiac_data <- cardiac_data[, -which(names(cardiac_data) %in% c("gender"))]

# View the updated dataset
str(cardiac_data)

# dropping caseno variable
cardiac_data <- select(cardiac_data,-caseno)
head(cardiac_data)

#Applying Logistic Regression Model
#Initial Model
set.seed(22200878)
# Converting "Present" to 1 and "Absent" to 0
cardiac_data$cardiac_condition_binary <- ifelse(cardiac_data$cardiac_condition == "Present", 1, 0)

# Removing 'cardiac_condition' variable 
cardiac_data <- subset(cardiac_data, select = -c(cardiac_condition))

# Splitting dataset into train and test for modelling
sample <- sample.split(cardiac_data$cardiac_condition_binary, SplitRatio = 0.70) 
train <- subset(cardiac_data, sample == TRUE)
test <- subset(cardiac_data, sample == FALSE)

# Training the logistic regression model
model <- glm(cardiac_condition_binary ~ ., family = binomial(logit), data = train)
summary(model)

# Making predictions on the test data
test$predicted.cardiac <- predict(model, newdata = test, type = "response")

# Converting to binary predictions
test$predicted.cardiac_binary <- ifelse(test$predicted.cardiac > 0.5, 1, 0)
test$predicted.cardiac_binary <- factor(test$predicted.cardiac_binary, levels = c("0", "1"))
test$cardiac_condition_binary<- factor(test$cardiac_condition_binary, levels = c("0", "1"))

# Evaluating the model
conf_matrix_lr <- confusionMatrix(test$predicted.cardiac_binary, test$cardiac_condition_binary)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)

conf_matrix <- confusionMatrix(test$predicted.cardiac_binary, test$cardiac_condition_binary)
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
accuracy <- conf_matrix$overall["Accuracy"]
roc_curve <- roc(as.numeric(test$cardiac_condition_binary) - 1, test$predicted.cardiac)
roc_auc_lr <- auc(roc_curve)
f1 <- conf_matrix$byClass["F1"]

# printing results
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("Accuracy:", accuracy))
cat("ROC AUC (Logistic Regression):", roc_auc_lr, "\n")
print(paste("F1 Score:", f1))

# Generatung ROC value for Logistic Regression 
roc_curve <- roc(as.numeric(test$cardiac_condition_binary) - 1, test$predicted.cardiac)
cat("AUC:", round(roc_curve$auc, 4), "\n")
# Plotting ROC curve for Logistics Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)

#Applying Logistic Regression Model
#Intermediate Model

# Calculate median and IQR for Age variable
median_age <- median(cardiac_data$age)
iqr_age <- IQR(cardiac_data$age)

#Removing outliers 
# Identify outliers using median and IQR
outliers <- which(cardiac_data$age > median_age + 1.5 * iqr_age | cardiac_data$age < median_age - 1.5 * iqr_age)
cardiac_data_NoOutliers <- cardiac_data[-outliers, ]

#  After removing outliers Plotting boxplot
boxplot(cardiac_data_NoOutliers$age, main = "Boxplot of Age (No Outliers)", col = "Red", border = "black")


# Selecting the numeric columns to normalize (excluding the target variable)
numeric_columns <- c("age", "weight", "fitness_score")

# Applying normalization to selected columns
cardiac_data <- cardiac_data_NoOutliers
cardiac_data[numeric_columns] <- scale(cardiac_data_NoOutliers[numeric_columns])

# normalized dataset
head(cardiac_data)

# Splitting the normalized data into training and testing sets for Intermediate model
spl_normalized <- sample.split(cardiac_data$cardiac_condition_binary, 0.7)
train_data_normalized <- subset(cardiac_data, spl_normalized == TRUE)
test_data_normalized <- subset(cardiac_data, spl_normalized == FALSE)
str(train_data_normalized)
str(test_data_normalized)


# Building the logistic regression model on the normalized data
model <- glm(
  cardiac_condition_binary ~ .,
  data = train_data_normalized,
  family = binomial(logit)
)
# summary
summary(model)

# Make predictions on the test data
test_data_normalized$predicted.cardiac <- predict(model, newdata = test_data_normalized, type = "response")

# Converting to binary predictions
test_data_normalized$predicted.cardiac_binary <- ifelse(test_data_normalized$predicted.cardiac > 0.5, 1, 0)
test_data_normalized$predicted.cardiac_binary <- factor(test_data_normalized$predicted.cardiac_binary, levels = c("0", "1"))
test_data_normalized$cardiac_condition_binary <- factor(test_data_normalized$cardiac_condition_binary, levels = c("0", "1"))


# Evaluate model
conf_matrix_lr <- confusionMatrix(test_data_normalized$predicted.cardiac_binary, test_data_normalized$cardiac_condition_binary)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)
# Calculating evaluation metrics
accuracy_lr <- sum(diag(as.matrix(conf_matrix_lr))) / sum(as.matrix(conf_matrix_lr))
precision_lr <- conf_matrix_lr$byClass["Pos Pred Value"]
recall_lr <- conf_matrix_lr$byClass["Sensitivity"]
roc_curve <- roc(as.numeric(test$cardiac_condition_binary) - 1, test$predicted.cardiac)
roc_auc_lr <- auc(roc_curve)
f1_score_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)

# Results
cat("Accuracy (Logistic Regression):", accuracy_lr, "\n")
cat("Precision (Logistic Regression):", precision_lr, "\n")
cat("Recall (Logistic Regression):", recall_lr, "\n")
cat("ROC AUC (Logistic Regression):", roc_auc_lr, "\n")
cat("F1 Score (Logistic Regression):", f1_score_lr, "\n")


# Generatung ROC value for Logistic Regression 
roc_curve <- roc(as.numeric(test_data_normalized$cardiac_condition_binary) - 1, test_data_normalized$predicted.cardiac)
cat("AUC:", round(roc_curve$auc, 4), "\n")

# Plotting ROC curve for Logistic Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)


# Calculating the correlation matrix for the specified variables
cor_matrix <- cor(train_data_normalized[, c("age", "weight", "fitness_score", "genderFemale", "genderMale")])
# Printing the correlation matrix
print(cor_matrix)
# Calculating the correlation matrix for all numeric column
corrplot(cor_matrix, method = "color",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, addCoef.col = "black", number.cex = 0.7)

# Checking for aliased coefficients
alias_summary <- alias(model)
print(alias_summary)

model <- glm(
  cardiac_condition_binary ~ age + weight + genderFemale + fitness_score,
  data = train_data_normalized,
  family = "binomial"
)
# Checking for multicollinearity using variance inflation factor (VIF)
vif_values <- car::vif(model)
# Printing VIF values
print("VIF values:")
print(vif_values)


# Splitting the normalized data into training and testing sets
set.seed(22200878)
spl_cardiac_data_final <- sample.split(cardiac_data$cardiac_condition_binary, 0.7)
train_data_final <- subset(cardiac_data, spl_cardiac_data_final == TRUE)
test_data_final <- subset(cardiac_data, spl_cardiac_data_final == FALSE)
str(train_data_final)
str(test_data_final)


#Final model 
final_model <- glm(
  cardiac_condition_binary ~ age + weight + genderFemale + fitness_score,
  data = train_data_final,
  family = "binomial"
)
# Summary
summary(final_model)


logit_prob <- predict(final_model, newdata = test_data_final, type = "response")

# Creating predicted.cardiac based on probabilities
test_data_final$predicted.cardiac <- logit_prob

# Verifying that the column exists and is not NULL
str(test_data_final)

# Converting to binary predictions
test_data_final$predicted.cardiac_binary <- ifelse(test_data_final$predicted.cardiac > 0.5, 1, 0)
test_data_final$predicted.cardiac_binary <- factor(test_data_final$predicted.cardiac_binary, levels = c("0", "1"))
test_data_final$cardiac_condition_binary <- factor(test_data_final$cardiac_condition_binary, levels = c("0", "1"))

# Evaluating model
conf_matrix_lr <- confusionMatrix(test_data_final$predicted.cardiac_binary, test_data_final$cardiac_condition_binary)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)

# Calculating evaluation metrics
accuracy_lr <- sum(diag(as.matrix(conf_matrix_lr))) / sum(as.matrix(conf_matrix_lr))
precision_lr <- conf_matrix_lr$byClass["Pos Pred Value"]
recall_lr <- conf_matrix_lr$byClass["Sensitivity"]
roc_curve <- roc(as.numeric(test_data_final$cardiac_condition_binary) - 1, test_data_final$predicted.cardiac)
roc_auc_lr <- auc(roc_curve)
f1_score_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)

# Results
cat("Accuracy (Logistic Regression):", accuracy_lr, "\n")
cat("Precision (Logistic Regression):", precision_lr, "\n")
cat("Recall (Logistic Regression):", recall_lr, "\n")
cat("ROC AUC (Logistic Regression):", roc_auc_lr, "\n")
cat("F1 Score (Logistic Regression):", f1_score_lr, "\n")

# Generatung ROC value for Logistic Regression 
roc_curve <- roc(as.numeric(test_data_final$cardiac_condition_binary) - 1, test_data_final$predicted.cardiac)
cat("AUC:", round(roc_curve$auc, 4), "\n")

# Plotting ROC curve for Logistic Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)
