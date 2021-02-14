
library(data.table)
library(dplyr)
library(ggplot2)
library(glmnet)
library(randomForest)
library(rpart)
library(gbm)
library(caret)
install.packages("DMwR")
library(DMwR)
install.packages("party")
library(party)

#Setting the working directory
setwd('/Users/user/Documents/AYCADoc/BOUN/IE582/Project582')

## Importing train and test data

data = read.table("IE582_Fall20_ProjectTrain.csv", sep = ",")
colnames(data) = data[1,]
data = data[-1,]


test = read.table("IE582_Fall20_ProjectTest.csv", sep = ",")
colnames(test) = test[1,]
test = test[-1,]




# Converting binary features to factors and others to numeric

for(i in 1:ncol(data)){
    if(nlevels(as.factor(data[,i])) == 2){
        data[,i] = as.factor(data[,i])
        test[,i] = as.factor(test[,i])
         }
    else{
        data[,i] = as.numeric(data[,i])
        test[,i] = as.numeric(test[,i])
    }
}




# Defining normalization function

norm <- function(x){(x-min(x))/(max(x)-min(x))}

normIndex <- c(9,10,11,14,27,30,32,36,42) # features with wide ranges

for(i in normIndex){
    if(class(data[,i])=="numeric"){
        data[,i] <- norm(data[,i])
        test[,i] <- norm(test[,i])
        
    }
}

# removing sparse and low variance features from test and train set

data = data[,c(-37,-50,-52,-55)]
test = test[,c(-37,-50,-52,-55)]



# Creating custom training sets

set.seed(121)

train_data_down<- downSample(data, as.factor(data$y), list = FALSE,yname = "y")
train_data_up <- upSample(data, as.factor(data$y), list = FALSE, yname = "y")
train_data_smote <- SMOTE(y~., data, k=6)


#Stochastic Gradient Boosting

myGrid <- expand.grid(shrinkage = c(0.01,0.05,0.1),interaction.depth=c(4:6),
                        n.trees=c(100,125,150),n.minobsinnode=10)

boost_down <- train(y~., data=train_data_down, method="gbm", 
                 trControl=myControl,verbose=FALSE)

boost_data <- train(y~., data=data, method="gbm",trControl=myControl,verbose=FALSE)


boost_up <- train(y~., data=train_data_up, method="gbm",trControl=myControl,verbose=FALSE,tuneGrid = myGrid)

boost_smote <- train(y~., data=train_data_smote, method="gbm",trControl=myControl,verbose=FALSE)


# Test data
predictions <- predict(boost_up,test)
predictions <- as.numeric(as.list(predictions))-1


## ATTEMPTED BUT DISCARDED METHODS

#Lasso 

lasso <- train(y~., data=train_data_smote, method="glmnet",trControl=myControl)
print(lasso)

# Random Forest
myControl = trainControl(method = "repeatedcv", number = 10, repeats = 2)
forest_feature = randomForest(y~., data=train_data_up,trControl=myControl,type="class",importance = T, ntree = 500)

# For determining feature importance
varImpPlot(forest_feature)


#SVM
svm <- train(y ~., data = train_data_smote, method = "svmLinear", type="one-classification",kernel="sigmoid",trControl = myControl)


##### Train error
error_test <- confusionMatrix(prediction_test_RF,test_data$y)
prediction_train <- predict(boost_up,data)
conf_train <- confusionMatrix(prediction_train,data$y)

