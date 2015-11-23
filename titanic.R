## load libraries
library(rpart)
library(rpart.plot)
library(Amelia)
library(randomForest)
library(missForest)


## read in data
train <- read.csv("~/R/titanic/train.csv", na.strings = c("NA", ""), strip.white = TRUE)


## drop some of the columns
train <- train[,c(-1,-4,-9,-11)]
train$Survived <- as.factor(train$Survived)


## build simple classification tree
cart_tree <- rpart(Survived ~ ., data = train)


## view tree
prp(cart_tree
    , extra = 2
    , box.col = c("pink","lightgreen")
    , main = "Titanic Survival Classification Tree")


## count missing values
sum(is.na(train))


## view missing values map
missmap(train
        , main = "Titanic Missing Data Map"
        , col = c("yellow", "black")
        , legend = FALSE)


## fill in missing values
train_imputed <- missForest(train
                            , maxiter = 10
                            , ntree = 100)$ximp


## count missing values on imputed
sum(is.na(train_imputed))


## train random forest model
rf <- randomForest(Survived ~ .
                   , data = train_imputed
                   , n.tree = 2000
                   , importance = TRUE
                   , proximity = TRUE)


## view ntree / error plot
plot(rf, main="Random Forest Error Plot by Number of Trees")


## view feature importance
varImpPlot(rf, main = "Variable Importance", type = 1)