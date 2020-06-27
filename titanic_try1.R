# libraries
library(tidyverse)
library(randomForest)

# reading data
train = read.csv(file.choose(), header = T)
test = read.csv(file.choose(), header = T)

# binding two datasets into one
train$isTrainSet = T
test$isTrainSet = F
test$Survived = NA
titanic = rbind(train, test)
str(titanic)

# working with variables
titanic$Pclass = as.factor(titanic$Pclass)
titanic$Sex = as.factor(titanic$Sex)
titanic$Embarked = as.factor(titanic$Embarked)

# data cleaning
titanic$Embarked[titanic$Embarked == ""] = "S"
age.mean = mean(titanic$Age, na.rm = T)
titanic$Age[is.na(titanic$Age)] = age.mean
titanic$Fare[1044] = 8.25

# partitioning
train1 = titanic[titanic$isTrainSet == T,]
test1 = titanic[titanic$isTrainSet == F,]


train1$Survived = as.factor(train1$Survived)

# random forest model
model = randomForest(Survived~Pclass+Sex+Age+SibSp+Embarked,
                     data = train1, ntree = 500)

# prediction
s = predict(model, train1)
table(s, train1$Survived)
Survived = predict(model, test1)

# submission file
PassengerId = test1$PassengerId
output = data.frame(PassengerId, Survived)
write.csv(output, file = "gender_submission.csv", row.names = F)

### score = 0.79904
