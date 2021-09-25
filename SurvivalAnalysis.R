######### SURVIVAL ANALYSIS USING DECISION TREE CLASSIFIER ###########

# Loading packages
install.packages("FSelector")
install.packages("rpart")
install.packages("caret",dependencies=TRUE)
install.packages("dplyr")
install.packages("xlsx")
install.packages("rpart.plot")
install.packages("data.tree")
install.packages("caTools")
install.packages("excel")

library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)

# Loading the data set
add <- "titanic.xls"
data <- read_xls(add,sheet="titanic3")
data
View(data)

# Feature Selection
data <- select(data,survived,pclass,sex,age)

# Handling data types
data <- mutate(data,survived=factor(survived),pclass=as.numeric(pclass),age=as.numeric(age))

# Splitting into training and testing data
set.seed(123)
sample = sample.split(data$survived,SplitRatio = 0.70)
train = subset(data,sample == TRUE)
test = subset(data,sample == FALSE)

# Model Training
tree <- rpart(survived~.,data = train)

# Prediction
tree.survived.predicted <- predict(tree,test,type = "class")

# Model Evaluation
confusionMatrix(tree.survived.predicted,test$survived)

# Visualizing the tree
prp(tree)
