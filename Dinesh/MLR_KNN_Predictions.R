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

train_data <- read.csv("../Data/train.csv")
test_data <- read.csv("../Data/test.csv")

##### Changing training dataset
house_train <- train_data[,c(-1,-2,-18,-19)]

house_train$zipcode <- as.factor(house_train$zipcode)
house_train$waterfront <- as.factor(house_train$waterfront)
house_train$condition <- as.factor(house_train$condition)



##### Replicating  the same changes in the test data set
house_test <- test_data[,c(-1,-2,-3,-18,-19)]

house_test$zipcode <- as.factor(house_test$zipcode)
house_test$waterfront <- as.factor(house_test$waterfront)
house_test$condition <- as.factor(house_test$condition)

################### Using multiple linear regression

lmTune_housePrice <- train(price~.,data=house_train,
                           method="lm",
                           trControl=trainControl(method="repeatedcv",
                                                  repeats=100,number=10))
lmTune_housePrice$bestTune
lmTune_housePrice$results

lmPrediction_housePrice <- predict(lmTune_housePrice,house_test)

lmPrediction_housePrice<-round(as.data.frame(lmPrediction_housePrice),3)

names(lmPrediction_housePrice) <- "price"

write.csv(lmPrediction_housePrice,file="Prediction_regression.csv")



############## kNN Prediction
knnTune_housePrice<- train(y=house_train[,1],x=house_train[,2:17],
                           method="knn",
                           preProcess = c("center","scale"),
                           tuneGrid=data.frame(.k=1:20),
                           trControl=trainControl(method="repeatedcv",repeats=100,number=10))
knnTune_housePrice$bestTune
knnTune_housePrice$results

knnPrediction_housePrice<- predict(knnTune_housePrice,house_test)

knnPrediction_housePrice<-round(as.data.frame(knnPrediction_housePrice),3)

names(knnPrediction_housePrice) <- "price"

write.csv(knnPrediction_housePrice,file="Prediction_knn.csv")


#train_housePrice <- house_train$housePrice

#lm_housePrice <- predict(lmTune_housePrice,house_train)
#lmPrediction <- as.data.frame(cbind(train_housePrice,lm_housePrice))
#lm_prediction_plot <- ggplot(data=lmPrediction,aes(x=train_housePrice,
#                                                   y=lm_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "lm_prediction_plot.png",width = 1000, height = 1000)
#lm_prediction_plot
#dev.off()

#KNN_housePrice <- predict(knnTune_housePrice,house_train)
#KNNPrediction <- as.data.frame(cbind(train_housePrice,KNN_housePrice))
#KNN_prediction_plot <- ggplot(data=KNNPrediction,aes(x=train_housePrice,
 #                                                    y=KNN_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "KNN_prediction_plot.png",width = 1000, height = 1000)
#KNN_prediction_plot
#dev.off()



