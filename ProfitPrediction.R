############# PROFIT PREDICTION USING LINEAR REGRESSION ################

# Loading the data set
library("readxl")
data <- read_excel("revenue.xlsx")
data

# Lets see the summary of the data set
summary(data)

# Lets see the first six rows
head(data)

# Lets visualize the data
plot(data)

# Splitting the data into train and test data
set.seed(2)
install.packages("caTools") # Lets install the caTools package 
library(caTools) # This library helps in splitting data
split <- sample.split(data,SplitRatio = 0.7) # 70% --> train set, 30% -->test set
split # Returns True and Falses

train <- subset(data,split = TRUE) 
test <- subset(data,split = FALSE)

# Model training
model <- lm(Profit~.,data = train)

# Lets see the model summary
summary(model)

# Prediction
pred <- predict(model,test)
pred

# Predicted vs Actual
plot(test$Profit,type = "l",lty=1.8,col="red") # Test data
lines(pred,type="l",col="blue") # Predicted value vs actual
plot(pred,type="l",lty=1.8,col="blue") # Predicted

# Finding the Accuracy
rmse <- sqrt(mean(pred-data$Profit)^2)
rmse
