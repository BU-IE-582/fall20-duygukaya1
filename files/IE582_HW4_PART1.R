# HOMEWORK 4

library(tidyverse)
library(dplyr)
library(stringr)
library(glmnet)
library(stats)
library(base)
library(caret)
library(rpart)
library(rattle)
library(readxl)
library(Metrics)
library(penalized)
library(randomForest)
library(gbm)

# SPORTS ARTICLES for OBJECTIVITY ANALYSIS

sports <- read_xlsx("C:\\Users\\kayad\\Desktop\\sports_data.xlsx")
sports <- subset(sports,select=-c(TextID,URL,NNP,ellipsis,WRB))

# train and test data

# 80% of the sample size
smp_size <- floor(0.8 * nrow(sports))

set.seed(123)
train_ind <- sample(seq_len(nrow(sports)), size = smp_size)

sport_training <-sports[train_ind, ]
sport_test <- sports[-train_ind, ]

# cv data

sport_training<-sport_training[sample(nrow(sport_training)),]

folds <- cut(seq(1,nrow(sport_training)),breaks=10,labels=FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  sport_cv <- sport_training[testIndexes, ]
  sport_train <- sport_training[-testIndexes, ]
}

prop.table(table(sport_train$Label))
prop.table(table(sport_cv$Label))
prop.table(table(sport_test$Label))


# PENALIZED REGRESSION METHOD
label  <- sport_train$Label
label2 <- sport_test$Label
label3 <- sport_cv$Label
label  <- ifelse(label == "subjective", 1, 0)
label2 <- ifelse(label2 == "subjective", 1, 0)
label3 <- ifelse(label3 == "subjective", 1, 0)
label  <- as.matrix(label)
label2 <- as.matrix(label2)
label3 <- as.matrix(label3)
label_test <-label2
sport_train_lasso <- sport_train[,2:57]
sport_test_lasso <- sport_test[,2:57]
sport_cv_lasso <- sport_cv[,2:57]

#L1 penalty with different lamddas (0.01,0.1)

cv_sport <- cv.glmnet(as.matrix(sport_train_lasso),label, family = "gaussian")
lambda_min <- cv_sport$lambda.min
plot(cv_sport)


lasso1 <- glmnet( as.matrix(sport_train_lasso),label, family='gaussian',lambda = lambda_min)
predict1<- predict(lasso1,as.matrix(sport_test_lasso))
predict_cv1 <- predict(lasso1,as.matrix(sport_cv_lasso))
mse_cv_lasso1 <- mse(predict_cv1,label3)
mse_lasso1 <- mse(predict1,label2)

lasso2 <- glmnet( as.matrix(sport_train_lasso),label, family='gaussian',lambda = 0.01)
predict2<- predict(lasso2,as.matrix(sport_test_lasso))
predict_cv2 <- predict(lasso2,as.matrix(sport_cv_lasso))
mse_cv_lasso2<- mse(predict_cv2,label3)
mse_lasso2 <- mse(predict2,label2)


lasso3 <- glmnet( as.matrix(sport_train_lasso),label, family='gaussian',lambda = 0.1)
predict3<- predict(lasso3,as.matrix(sport_test_lasso))
predict_cv3 <- predict(lasso3,as.matrix(sport_cv_lasso))
mse_cv_lasso3 <- mse(predict_cv3,label3)
mse_lasso3 <- mse(predict3,label2)


# Table
lambda_005<-c("0.05","0.154","0.135")
lambda_001<-c("0.01","0.147","0.132")
lambda_01<-c("0.1","0.166","0.187")
PRA_TABLE <-rbind(lambda_005,lambda_001,lambda_01)
PRA_TABLE<- as.data.frame(PRA_TABLE)
colnames(PRA_TABLE)<-c("lambda","cv error","test error")
PRA_TABLE

# DECISION TEREE METHOD

sport_train_DT <- sport_train[,2:57]
sport_train_DT <- cbind(label,sport_train_DT)
sport_test_DT <- sport_test[,2:57]
sport_cv_DT <- sport_cv[,2:57]

cla_tree_1 = rpart(label~.,data = sport_train_DT,method = 'class')

plotcp(cla_tree_1)
printcp(cla_tree_1)
cla_tree_1$variable.importance
fancyRpartPlot(cla_tree_1)


mse_DT<-c()
mse_cv_DT<-c()
DT_TABLE <-matrix(0,4,12)
test_pred <- matrix(0,200,12)
cv_pred<- matrix(0,80,12)
cp <- c(0.01,0.05,0.1)
minsplit <- c(10,20,50,100)


i=1
   for(k in 1:length(minsplit)){
        for(l in 1:length(cp)){
 cla_tree <- rpart(label~.,data = sport_train_DT, method='class', control = rpart.control(cp = cp[l],minsplit = minsplit[k]))
 cla_tree_cv <- rpart(label~.,data = sport_train_DT, method='class', control = rpart.control(cp = cp[l],minsplit = minsplit[k]))
 testp <-predict(cla_tree, sport_test_DT, type = 'class')
 cv_p  <- predict(cla_tree_cv, sport_cv_DT, type = 'class')
 cv_pred[,i] <- as.numeric(as.character(cv_p))
 test_pred[,i] <- as.numeric(as.character(testp))
 test_label <- label2
 cv_label <- label3
 mse_cv_DT[i]<- mse(cv_pred[,i],cv_label)
 mse_DT[i] <- mse(test_pred[,i],test_label)
 DT_TABLE <- cbind(mse_DT,mse_cv_DT, minsplit,cp)
 i = i+1
        }
   }
   
DT_TABLE

# RANDOM FOREST MODEL 

sport_train_DT$label <- as.factor(as.character(sport_train_DT$label))
sport_test <- cbind(label2,sport_test)
sport_test$label2 <- as.factor(as.character(sport_test$label2))
sport_test<-  subset(sport_test,select=-c(Label))


# mtry 10
rforest1<- randomForest(sport_train_DT[,2:57],sport_train_DT$label,ntree = 500 , nodesize = 5, mtry = 10) 
rforest1_cv<-randomForest(sport_train_DT[,2:57],sport_train_DT$label,ntree = 500 , nodesize = 5, mtry = 10)
pred_cv_RF1<-predict(rforest1_cv,sport_cv[,2:57])
pred_cv_RF1<-as.numeric(as.character(pred_cv_RF1))
pred_RF1<-predict(rforest1,sport_test[,2:57])
pred_RF1<-as.numeric(as.character(pred_RF1))
mse_cv_RF1<- mse(pred_cv_RF1,cv_label)
mse_RF1 <- mse(pred_RF1,test_label)


# mtry 30

rforest2<- randomForest(sport_train_DT[,2:57],sport_train_DT$label,ntree = 500 , nodesize = 5, mtry = 30) 
rforest2_cv<-randomForest(sport_train_DT[,2:57],sport_train_DT$label,ntree = 500 , nodesize = 5, mtry = 30)
pred_cv_RF2<-predict(rforest2_cv,sport_cv[,2:57])
pred_cv_RF2<-as.numeric(as.character(pred_cv_RF2))
pred_RF2<-predict(rforest2,sport_test[,2:57])
pred_RF2<-as.numeric(as.character(pred_RF2))
mse_cv_RF2<- mse(pred_cv_RF2,cv_label)
mse_RF2 <- mse(pred_RF2,test_label)

# mtry 5 

rforest3<- randomForest(sport_train_DT[,2:57],sport_train_DT$label,ntree = 500 , nodesize = 5, mtry = 5) 
rforest3_cv<-randomForest(sport_train_DT[,2:57],sport_train_DT$label,ntree = 500 , nodesize = 5, mtry = 5)
pred_cv_RF3<-predict(rforest3_cv,sport_cv[,2:57])
pred_cv_RF3<-as.numeric(as.character(pred_cv_RF3))
pred_RF3<-predict(rforest3,sport_test[,2:57])
pred_RF3<-as.numeric(as.character(pred_RF3))
mse_cv_RF3<- mse(pred_cv_RF3,cv_label)
mse_RF3 <- mse(pred_RF3,test_label)


# Table
mtry5<-c("5","0.187","0.155")
mtry10<-c("10","0.175","0.150")
mtry30<-c("30","0.15","0.145")
RF_TABLE <-rbind(mtry5,mtry10,mtry30)
RF_TABLE<- as.data.frame(RF_TABLE)
colnames(RF_TABLE)<-c("mtry","cv error","test error")
RF_TABLE


# STOCHASTIC GRADIENT BOOSTING


mse_cv_sgb <- c()
mse_sgb <- c()
pred_sgb <-matrix(0,200,27)
predcv_sgb<-matrix(0,80,27)
SGB_TABLE <- matrix(0,5,27)
shrinkage= c(0.01,0.1,0.5)
ntree=c(100,500,1000)
interaction_depth =c(1,2,3)

j=1
 for(m in 1:length(ntree)){
  for(n in 1:length(shrinkage)){
    for(p in 1:length(interaction_depth)){

 sgb <- gbm(label~.,data = sport_train_DT[,2:57],distribution ="bernoulli",n.trees = ntree[m],shrinkage = shrinkage[n], interaction.depth = interaction_depth[p])
 sgb_cv <- gbm(label~.,data = sport_train_DT[,2:57],distribution ="bernoulli",n.trees = ntree[m],shrinkage = shrinkage[n], interaction.depth = interaction_depth[p])
 pred_cv_sgb <-predict(sgb_cv,sport_cv[,2:57])
 pred_cv_sgb<-ifelse(pred_cv_sgb>0.50,"1","0")
 predcv_sgb[,j] <- as.numeric(as.character(pred_cv_sgb)) 
 pred_test_sgb <- predict(sgb,sport_test[,2:57])
 pred_test_sgb<-ifelse(pred_test_sgb>0.50,"1","0")
 pred_sgb[,j]<- as.numeric(as.character(pred_test_sgb)) 
 mse_cv_sgb[j] <-mse(predcv_sgb[,j],cv_label)
 mse_sgb[j]<- mse(pred_sgb[,j],test_label)
 SGB_TABLE <- cbind(mse_DT,mse_cv_DT, ntree,shrinkage,interaction_depth)
 j=j+1
    }
  }
 }

SGB_TABLE 
