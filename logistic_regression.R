## Aprendnedo machine learning no software R ##

## Logistic regression

## Building the model
# m <- glm(y ~ x1 + x2 + x3, data = my_dataset, family = "binomial")
# prob <- predict(m, test_dataet, type = "response")
# prob <- ifelse(prob>0.5,1,0)

## Define the path
my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.path)

## Read the data
donors<-read.table("donors.txt",h=T,sep=",")

# Examine the dataset to identify potential independent variables
str(donors)

# Explore the dependent variable
table(donors$donated)

# Build the donation model
donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)

# Estimate the donation probability
donors$donation_prob <- predict(donation_model,donors, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)

# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

# making Dummy coding methodology in R
# my_data$gender <- factor(my_data_gender,
#                          levels = c(0,1,2),
#                          labels = c("Male", "Female", "Other"))

# Interaction effects
# glm(disease ~ obesity*smoking,
#     data = health,
#     family = "binomial")

# Convert the wealth rating to a factor
donors$wealth_levels <- factor(donors$wealth_rating, levels = c(0,1,2,3), labels = c("Unknown","Low","Medium","High"))

# Use relevel() to change reference category
donors$wealth_levels <- relevel(donors$wealth_levels, ref = "Medium")

# See how our factor coding impacts the model
model <- glm(donated ~ wealth_levels, donors, family="binomial")
summary(model)

# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age),round(mean(donors$age,na.rm=T),2),donors$age)
summary(donors$imputed_age)
# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age),1,0)

# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ money + recency*frequency, donors, family="binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model,donors,type="response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(rfm_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated,rfm_prob)
plot(ROC, col = "red")
auc(ROC)

# Forward stepwise
# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)

## Exerc�cio sobre logistic regression
## Baseado no arquivo utilizado durante a aula monte um modelo
## de regress�o logistica utilizando os conceitos que voc� aprendeu aqui.
## Por�m, agora divida a popula��o em popula��o de treinamento (75%
## dos dados) e popula��o de valida��o (25% dos dados)

## Define the path
setwd("C:/Users/hp/OneDrive - Experimental Analytics Corporation/Short course/Machine learning no software R - m�dulo 1")

## Read the data
dados<-read.table("donors.txt",h=T,sep=",")

## Montando a popula��o de treinamento e popula��o de teste
smp_size<-nrow(dados)*0.75
train_ind <- sample(seq_len(nrow(dados)), size = smp_size)

train <- dados[train_ind, ]
test <- dados[-train_ind, ]

# Build the donation model
donation_model <- glm(donated ~ ., data = train, family = "binomial")

# Summarize the model results
summary(donation_model)

# Forward stepwise
# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = train, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = train, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the donation probability
train$donation_prob <- predict(step_model,train, type = "response")
test$donation_prob <- predict(step_model,test, type = "response")

# Find the donation probability of the average prospect
mean(train$donated)

# Predict a donation if probability of donation is greater than average (0.0504)
train$donation_pred <- ifelse(train$donation_prob > 0.0501, 1, 0)
test$donation_pred <- ifelse(test$donation_prob > 0.0501, 1, 0)

# Calculate the model's accuracy
mean(train$donation_pred == train$donated, na.rm = T)
mean(test$donation_pred == test$donated, na.rm = T)

# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(test$donated, test$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)
