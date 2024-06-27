## Aprendendo machine learning no software R ##
lista.de.pacotes <- c("data.table","naivebayes")
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in% installed.packages()[,"Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes)
lapply(lista.de.pacotes, require, character.only = TRUE)
## Aplicando metodologia bayesiana para fazer predi??o

# Building a Naive Bayes model
library(naivebayes)
# m <- naive_bayes(location ~ time_of_day, data=location_history)

# Making predictions with Naive Bayes
# future_location <- predict(m,future_conditions)

## Define the path
my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.path)
## Read the data
locations<-read.table("location.txt",h=T,sep=",")

#Create one file with just 9 am hour
where9am <- locations[locations$hour==9,]

# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, location == "office" & daytype == "weekday")) / nrow(where9am)

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

# Build the location prediction model
locmodel <- naive_bayes(location~daytype, data = where9am)

# Create test file
thursday9am <- where9am[where9am$weekday=="thursday"&where9am$hour==9,]
saturday9am <- where9am[where9am$weekday=="saturday"&where9am$hour==9,]

# Predict Thursday's 9am location
predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)

# Examine the location prediction model
print(locmodel)

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am , type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am , type = "prob")


# Build a more complex NB model of location
locmodel <- naive_bayes(location ~ daytype + hourtype, locations)

# Create test file
weekday_afternoon <- locations[locations$hourtype == "afternoon" & locations$daytype=="weekday",]
weekday_evening <- locations[locations$hourtype == "evening" & locations$daytype=="weekday",]

# Predict Brett's location on a weekday afternoon
predict(locmodel,weekday_afternoon)

# Predict Brett's location on a weekday evening
predict(locmodel,weekday_evening)

# Create a new file 
weekend_afternoon <- locations[locations$hourtype=="afternoon" & locations$daytype=="weekend",]

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel,weekend_afternoon, type = "prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, locations, laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2,weekend_afternoon, type = "prob")
