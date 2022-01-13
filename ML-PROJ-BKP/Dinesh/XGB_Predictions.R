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

############## XG boosting
require(xgboost)

dummyMat <- model.matrix(lm(price~.,data=house_train))
dummyMat <- dummyMat[,-1]

output_vector <- house_train$price
bst <- xgboost(data = dummyMat, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10)

importance <- xgb.importance(feature_names = colnames(dummyMat), model = bst)

xgbTune_housePrice<- train(y=house_train[,1],x=dummyMat,
                           method="xgbTree",
                           trControl=trainControl(method="repeatedcv",repeats=100,number=10))

xgbTune_housePrice$bestTune

xgbTune_housePrice$results

xgBoostPrediction_housePrice<- predict(xgbTune_housePrice,house_test)

xgBoostPrediction_housePrice<-round(as.data.frame(xgBoostPrediction_housePrice),3)

names(xgBoostPrediction_housePrice) <- "price"

write.csv(xgBoostPrediction_housePrice,file="Prediction_xgboost.csv")



#train_housePrice <- house_train$housePrice


#XGB_housePrice <- predict(xgbTune_housePrice,house_train)
#XGBPrediction <- as.data.frame(cbind(train_housePrice,XGB_housePrice))
#XGB_prediction_plot <- ggplot(data=XGBPrediction,aes(x=train_housePrice,
#                                                     y=XGB_housePrice))+geom_jitter()+geom_smooth(method = loess)
#png(file = "XGB_prediction_plot.png",width = 1000, height = 1000)
#XGB_prediction_plot
#dev.off()



