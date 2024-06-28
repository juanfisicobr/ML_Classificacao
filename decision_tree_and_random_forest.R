## Aprendnedo machine learning no software R ##

## Decision tree
# Load the rpart package
library(rpart)
# m <- rpart(outcome ~ loan_amount + creadit_score, data = loans, method="class")

# making prediction from an rpart tree
# p <- predict(m, test_data, type="class")

## Define the path
my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.path)
## Read the data
loans<-read.table("loans.txt",h=T,sep=",")
loans$Outcome <- ifelse(loans$rand<0.5,"Repaid","Default")

# Build a lending model predicting loan outcome versus loan amount and credit score
loan_model <- rpart(Outcome ~ loan_amount + credit_score, data = loans, method = "class", control = rpart.control(cp = 0))

# Make a prediction for someone with good credit
good_credit <- loans[loans$Outcome=="Repaid",]
good_credit <- good_credit[1,]
predict(loan_model, good_credit, type = "class")

# Make a prediction for someone with bad credit
bad_credit <- loans[loans$Outcome=="Default",]
bad_credit <- bad_credit[1,]
predict(loan_model, bad_credit, type = "class")

# Examine the loan_model object
loan_model

# Load the rpart.plot package
library(rpart.plot)

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# testing the model
# Determine the number of rows for training
nrow(loans)

# Create a random sample of row IDs
sample_rows <- sample(nrow(loans), nrow(loans)*0.75)

# Create the training dataset
loans_train <- loans[sample_rows,-c(2)]

# Create the test dataset
loans_test <- loans[-sample_rows,-c(2)]

# Grow a tree using all of the available applicant data
loan_model <- rpart(Outcome ~., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model, loans_test, type = "class")

# Examine the confusion matrix
table(loans_test$pred, loans_test$Outcome)

# Compute the accuracy on the test dataset
mean(loans_test$Outcome==loans_test$pred)


# pre-pruning with repair
# prune_control=rpart.control(maxDepth = 30, minsplit = 20)
# m <- rpart(repaid - credit_score + request_amt,
#            data = loans,
#            method="class",
#            control = prune_control)
#
# post-pruning with repair
# m <- rpart(repaid - credit_score + request_amt,
#            data = loans,
#            method="class")
# 
# plotcp(m)
# m_pruned <- prune(m,cp=0.20)

# Grow a tree with maxdepth of 6
loan_model <- rpart(Outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0, maxdepth = 6))

# Make a class prediction on the test set
loans_test$pred <- predict(loan_model, loans_test, type = "class")

# Compute the accuracy of the simpler tree
mean(loans_test$pred == loans_test$Outcome)

# Swap maxdepth for a minimum split of 500 
loan_model <- rpart(Outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0, minsplit = 500))

# Run this. How does the accuracy change?
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$pred == loans_test$Outcome)

# Grow an overly complex tree
loan_model <- rpart(Outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned,loans_test,type="class")
mean(loans_test$pred==loans_test$Outcome)

# building a simple random forest
library(randomForest)
# m <- randomForest(repaid ~ creadit_card + request_amt, data = loans,
#                   ntree = 500, #number pf trees in the forest
#                   mtry = sqrt(p)) #number of predictors (p) per tree

# making prediction from a random forest
# predict(m,test_data)

# Load the randomForest package
library(randomForest)

# Build a random forest model
loans_train$Outcome<-factor(loans_train$Outcome)
loan_model <- randomForest(Outcome ~ ., data = loans_train)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model, loans_test, type="class")
mean(loans_test$pred== loans_test$Outcome)

## Exerc?cio
## Monte um modelo de Random Forest para o arquivo iris (que j? existe dentro
## do software R). Divida o arquivo de dados em 75% dos dados para popula??o
## de treinamento e 25% dos dados para a popula??o de teste. Calcule a 
## acur?cia e a matriz de confus?o.

dados<-iris

## Montando a popula??o de treinamento e popula??o de teste
smp_size<-nrow(dados)*0.75
train_ind <- sample(seq_len(nrow(dados)), size = smp_size)

train <- dados[train_ind, ]
test <- dados[-train_ind, ]

# Load the randomForest package
library(randomForest)

# Build a random forest model
train$Species<-factor(train$Species)
model <- randomForest(Species ~ ., data = train)

# Compute the accuracy of the random forest
test$pred <- predict(model, test, type="class")
mean(test$pred==test$Species)

# Compute the confusion matrix
table(test$pred,test$Species)
