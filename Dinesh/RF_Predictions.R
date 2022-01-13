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


######################## Random forests

dummyMat <- model.matrix(lm(price~.,data=house_train))
dummyMat <- dummyMat[,-1]


ForestTune_housePrice<-train(y=house_train[,1],x=dummyMat,
                             tuneGrid=data.frame(mtry=1:100),
                             method="rf",ntree=500,
                             trControl=trainControl(method="oob"))

ForestTune_housePrice$bestTune
ForestTune_housePrice$results

test1 <- house_test
y <-1:nrow(test1)
test1 <- cbind(y,test1)

dummyMatTest <- model.matrix(lm(y~.,data=test1))
dummyMatTest <- dummyMatTest[,-1]

forestPrediction_housePrice<- predict(ForestTune_housePrice,dummyMatTest)

forestPrediction_housePrice<-round(as.data.frame(forestPrediction_housePrice),3)

names(forestPrediction_housePrice) <- "price"

write.csv(forestPrediction_housePrice,file="Prediction_randomForest.csv")



#train_housePrice <- house_train$housePrice

#RF_housePrice <- predict(ForestTune_housePrice,dummyMat)
#RFPrediction <-as.data.frame(cbind(train_housePrice,RF_housePrice))
#RF_prediction_plot <- ggplot(data=RFPrediction,aes(x=train_housePrice,
#                                                   y=RF_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "RF_prediction_plot.png",width = 1000, height = 1000)
#RF_prediction_plot
#dev.off()


