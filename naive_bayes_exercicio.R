## Exercício naive bayes

## Utilize o arquivo exemplo do R "iris" e monte uma população de treinamento com
## 75% dos dados e a população de teste com 25% dos dados. Depois rode o modelo naives bayes
## e verifique os resultados

library(naivebayes)
library(caret)

dados<-iris

## Montando a população de treinamento e população de teste
smp_size<-nrow(dados)*0.75
train_ind <- sample(seq_len(nrow(dados)), size = smp_size)

train <- dados[train_ind, ]
test <- dados[-train_ind, ]

# Build the prediction model
model <- naive_bayes(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = train)

# Making prediction
predict(model, test)

# Obtain the predicted probabilities 
predict(model, test , type = "prob")

# Calculating the accuracy
mean(predict(model, test)==test$Species)

# Confusion table
table(predict(model, test),test$Species)
