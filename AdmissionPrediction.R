######### COLLEGE ADMISSION PREDICTION USING LOGISTIC REGRESSION ###############

# Lets import the dataset
data <- read.csv("binary.csv")
data
View(data)

# Lets split the data
library(caTools)

split <- sample.split(data,SplitRatio = 0.8)
split

train <- subset(data,split == "TRUE")
test <- subset(data,split == "FALSE")

# Data Munging
# These are categorical variables so,we need to convert them into factors
data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)

# Model Training
# We will train the model using the training data
# Use glm, the general linear model function
# Dependent variable is admit;independent variables are gpa and rank
# The family argument should be binomial to indicate Logistic Regression
logreg <- glm(admit~gpa+rank,data = train,family = "binomial")

# Lets see the model summary
summary(logreg)

# Prediction
pred_log <- predict(logreg,test,type = "response")
pred_log

# Lets see with the train set as well
pred_train <- predict(logreg,train,type = "response")
pred_train

# Lets validate the model
## Confusion Matrix
confmatrix <- table(Actual_Value = test$admit, Predicted_Value = pred_log>0.5)
confmatrix

## Accuracy score
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)