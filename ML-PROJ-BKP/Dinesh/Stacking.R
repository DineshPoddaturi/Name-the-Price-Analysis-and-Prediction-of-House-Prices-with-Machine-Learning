require(psych)
require(corrplot)
require(caret)
require(tidyverse)
require(pls)
require(glmnet)
require(gdata)
require(Matrix)
require(graphics)
require(rpart)
require(randomForest)
require(gbm)
require(earth)
require(caretEnsemble)


train_data <- read.csv("../Data/train.csv")

test_data <- read.csv("../Data/test.csv")

##### Changing training dataset
house_train <- train_data[,c(-1,-2,-18,-19)]

house_train$zipcode <- as.factor(house_train$zipcode)
house_train$waterfront <- as.factor(house_train$waterfront)
house_train$condition <- as.factor(house_train$condition)

findLinearCombos(house_train)

house_train <- house_train[,-12]

##### Replicating  the same changes in the test data set
house_test <- test_data[,c(-1,-2,-3,-18,-19)]

house_test$zipcode <- as.factor(house_test$zipcode)
house_test$waterfront <- as.factor(house_test$waterfront)
house_test$condition <- as.factor(house_test$condition)

findLinearCombos(house_test)

house_test <- house_test[,-11]




method<- c("knn","rpart","rf")

parameter <- list(data.frame(k=4),
                   data.frame(cp=0.0001),
                   data.frame(mtry=47))

pred1<- list()
pred2<- list()

pred11<- list()
pred21<- list()

train<- house_train
test<- house_test
pred1<- matrix(NA,nrow=dim(test)[1],ncol=5)
pred2<- matrix(NA,nrow=dim(train)[1],ncol=3)

pred11<- matrix(NA,nrow=dim(train)[1],ncol=5)
pred21<- matrix(NA,nrow=dim(train)[1],ncol=3)

for (j in 1:3){
    modelfit<- train(price~.,
                     data=train,
                     method=method[j],
                     preProc=c("center","scale"),
                     trControl=trainControl(method="none"),
                     tuneGrid=parameter[[j]])
  
  pred1[,j]<- predict(modelfit,newdata=test)
  pred2[,j]<- predict(modelfit,newdata=train[,-1])
  
  pred11[,j]<- predict(modelfit,newdata=train[,-1])
  pred21[,j]<- predict(modelfit,newdata=train[,-1])
}

emfitLM<- lm(log(train$price)~as.matrix(log(pred2)))

emfitRF<- train(y=log(train$price),
                x=as.matrix(log(as.data.frame(pred2))),
                method="rf", 
                tuneGrid=data.frame(mtry=47))

emfitRF$results


emfit11LM<- lm(log(train$price)~as.matrix(log(pred21)))

emfit21RF<- train(y=log(train$price),
                  x=as.matrix(log(as.data.frame(pred21))),
                  method="rf",
                  tuneGrid=data.frame(mtry=47))
  
  pred1[,4]<- cbind(matrix(1,nrow=dim(test)[1],ncol=1),pred1[,-c(4,5)])%*%coef(emfitLM)
  pred1[,5]<- exp(predict(emfitRF,as.data.frame(log(pred1[,-c(4,5)]))))
  
  
  pred11[,4]<- cbind(matrix(1,nrow=dim(train)[1],ncol=1),pred11[,-c(4,5)])%*%coef(emfit11LM)
  pred11[,5]<- exp(predict(emfit21RF,as.data.frame(log(pred11[,-c(4,5)]))))
  
  
  predictionsTest<- as.data.frame(pred1)
  colnames(predictionsTest)[1:3]<- method
  colnames(predictionsTest)[4:5]<- c("enOLS","enFOR")
  
  
  
  
  predictionsTrain<- as.data.frame(pred11)
  colnames(predictionsTrain)[1:3]<- method
  colnames(predictionsTrain)[4:5]<- c("enOLS","enFOR")
  train_housePrice <- train$price
  predictionsTrain$trainPrice <- train_housePrice


  filenamefitRF <- paste("emfitRF.rds")
  saveRDS(emfitRF,file=filenamefitRF)
  
  filenamefitLM <- paste("emfitLM.rds")
  saveRDS(emfitLM,file=filenamefitLM)
  
  filenamefit11LM <- paste("emfit11LM.rds")
  saveRDS(emfit11LM,file=filenamefit11LM)
  
  filenamefit21RF <- paste("emfit21RF.rds")
  saveRDS(emfit21RF,file=filenamefit21RF)
  
  predictionsFileNameTest <- paste("PredictionsTest.rds")
  saveRDS(predictionsTest,file=predictionsFileNameTest)
  
  predictionsFileNameTrain <- paste("PredictionsTrain.rds")
  saveRDS(predictionsTrain,file=predictionsFileNameTrain)
  

  
predictionsLM <- readRDS("emfitLM.rds")

predictionsRF <- readRDS("emfitRF.rds")
predictionsRF$bestTune
predictionsRF$results%>%filter(mtry==2)

    
predictionsTest <- readRDS("PredictionsTest.rds")
predictionsTrain <- readRDS("PredictionsTrain.rds")  
  
lm(predictionsTrain$trainPrice ~ predictionsTrain$enFOR)
  
ggplot(data=predictionsTrain,aes(x=predictionsTrain$trainPrice,
                              y=predictionsTrain$enFOR))+geom_jitter()+geom_smooth(method = loess)




Stacking_Prediction_housePrice_RF <- predictionsTest$enFOR %>%as.data.frame()

nrow(Stacking_Prediction_housePrice_RF)

names(Stacking_Prediction_housePrice_RF) <- "price"

write.csv(Stacking_Prediction_housePrice_RF,file="StackingRF.csv")

Stacking_Prediction_housePrice_OLS <- predictionsTest$enOLS %>% as.data.frame()

names(Stacking_Prediction_housePrice_OLS) <- "price"

write.csv(Stacking_Prediction_housePrice_OLS,file="StackingOLS.csv")



