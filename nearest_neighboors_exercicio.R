## Exerc?cio nearest neighboors

## Utilize o arquivo exemplo do R "iris" e monte uma popula??o de treinamento com
## 75% dos dados e a popula??o de teste com 25% dos dados. Depois rode o modelo knn
## e verifique os resultados para diferentes valores de k

dados<-iris

## Montando a popula??o de treinamento e popula??o de teste
smp_size<-nrow(dados)*0.75
train_ind <- sample(seq_len(nrow(dados)), size = smp_size)

train <- dados[train_ind, ]
test <- dados[-train_ind, ]

# Use kNN to identify the test 
especie <- train$Species
pred <- knn(train = train[,-c(5)], test = test[,-c(5)], cl = especie)

# Create a confusion matrix of the predicted versus actual values
actual <- test$Species
table(pred, actual)

# Compute the accuracy
mean(pred == actual)

# Testing different k values
# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = train[,-c(5)], test = test[,-c(5)], cl = especie)
mean(k_1==actual)
table(k_1, actual)

# Modify the above to set k = 5
k_5 <- knn(train = train[,-c(5)], test = test[,-c(5)], cl = especie, k=5)
mean(k_5==actual)
table(k_5, actual)

# Set k = 10 and compare to the above
k_10 <- knn(train = train[,-c(5)], test = test[,-c(5)], cl = especie, k=10)
mean(k_10==actual)
table(k_10, actual)

# Normalize tha data to be used in the kNN model
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
new_data_train<-normalize(train[,-c(5)])
new_data_test<-normalize(test[,-c(5)])

k_norm <- knn(train = new_data_train, test = new_data_test, cl = especie)
table(k_norm, actual)
mean(k_norm==actual)
