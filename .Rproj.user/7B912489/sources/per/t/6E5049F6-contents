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



############# Using GBM to predict

gbm.grid <- expand.grid(n.trees = seq(120,160,length.out=3),
                        interaction.depth = seq(1,5),
                        shrinkage = seq(.05,0.2,length.out=3),
                        n.minobsinnode = seq(7,12,length.out=3))

cv.control_house <- trainControl(method = "repeatedcv", repeats = 100,number = 10)

GbmTune_housePricing<-train(y=house_train[,1],x=house_train[,2:17],tuneGrid=gbm.grid,
                            method="gbm",
                            trControl=cv.control_house)

GbmTune_housePricing$bestTune
GbmTune_housePricing$results

housePricePredict_Gbm <- predict(GbmTune_housePricing,house_test)

housePricePredict_Gbm <- round(as.data.frame(housePricePredict_Gbm),3)

names(housePricePredict_Gbm) <- "price"

write.csv(housePricePredict_Gbm,file="Prediction_Gbm.csv")


#train_housePrice <- house_train$housePrice
#GBM_housePrice <- predict(GbmTune_housePricing,house_train)
#GBMPrediction <- as.data.frame(cbind(train_housePrice,GBM_housePrice))
#GBM_prediction_plot <- ggplot(data=GBMPrediction,aes(x=train_housePrice,
#                                                     y=GBM_housePrice))+geom_jitter()+geom_smooth(method = loess)
#png(file = "GBM_prediction_plot.png",width = 1000, height = 1000)
#GBM_prediction_plot
#dev.off()


