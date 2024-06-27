## Aprendnedo machine learning no software R ##

## Nearest neignboor
library(class)
pred <- knn(train = , test = , training_label)

## Define the path
setwd("C:/Users/juanf/OneDrive/Documentos/curriculo/moocs/machine learning com R/ML Clasificação/modulo1")

## Read the data
signs<-read.table("signs.txt",h=T,sep=",")

# Examine the structure of the signs dataset
str(signs)

# Count the number of signs of each type
table(signs$sign_type)

# Check r10's average red level by sign type
aggregate(r10 ~ sign_type, data = signs, mean)

# Create train and test data
library(data.table)
smp_size<-150
train_ind <- sample(seq_len(nrow(signs)), size = smp_size)

train_signs <- signs[train_ind, ]
test_signs <- signs[-train_ind, ]

# Use kNN to identify the test road signs
sign_types <- train_signs$sign_type
signs_pred <- knn(train = train_signs[,-c(1:3)], test = test_signs[,-c(1:3)], cl = sign_types)

# Create a confusion matrix of the predicted versus actual values
signs_actual <- test_signs$sign_type
table(signs_pred, signs_actual)

# Compute the accuracy
mean(signs_pred == signs_actual)

# Testing different k values
# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = train_signs[,-c(1:3)], test = test_signs[,-c(1:3)], cl = sign_types)
mean(k_1==signs_actual)
table(k_1, signs_actual)

# Modify the above to set k = 7
k_7 <- knn(train = train_signs[,-c(1:3)], test = test_signs[,-c(1:3)], cl = sign_types, k=7)
mean(k_7==signs_actual)
table(k_7, signs_actual)

# Set k = 15 and compare to the above
k_15 <- knn(train = train_signs[,-c(1:3)], test = test_signs[,-c(1:3)], cl = sign_types, k=15)
mean(k_15==signs_actual)
table(k_15, signs_actual)

# Verify the votes proportion
# Use the prob parameter to get the proportion of votes for the winning class
sign_pred <- knn(train = train_signs[,-c(1:3)], test = test_signs[,-c(1:3)], cl = sign_types, k=15, prob=TRUE)

# Get the "prob" attribute from the predicted classes
sign_prob <- attr(sign_pred, "prob")

# Examine the first several predictions
head(sign_pred)

# Examine the proportion of votes for the winning class
head(sign_prob)

# Normalize tha data to be used in the kNN model
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
new_data_train<-normalize(train_signs[,-c(1:3)])
new_data_test<-normalize(test_signs[,-c(1:3)])

k_norm <- knn(train = new_data_train, test = new_data_test, cl = sign_types)
mean(k_norm==signs_actual)

k_norm_7 <- knn(train = new_data_train, test = new_data_test, cl = sign_types, k=7)
mean(k_norm_7==signs_actual)
