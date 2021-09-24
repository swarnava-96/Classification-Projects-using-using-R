######### WINE QUALITY PREDICTION USING RANDOM FOREST CLASSIFIER#########

# Installing the packages
install.packages("randomForest")

# Importing the libraries
library(randomForest)

# Importing the data
dataset <- read.csv("winequality-red.csv")

# Lets see the data
head(dataset)

# Feature Engineering -> converting the target to a factor
dataset$quality = as.factor(dataset$quality)

# Train and test data
data_set_size = floor(nrow(dataset)*0.80)
index <- sample(1:nrow(dataset),size = data_set_size)
training <- dataset[index,]
testing <- dataset[-index,]

# Model training
rf <- randomForest(quality~.,data = training,mtry=4,ntree=2001,importance=TRUE )

rf

# Plotting
plot(rf)

# Prediction
result <- data.frame(testing$quality,predict(rf,testing[,1:11],type = "response"))
result