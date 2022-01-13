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



#####Predictions


######### TREE

TreeTune_housePricing<-train(y=house_train[,1],x=house_train[,2:17],
                             method="rpart",tuneGrid=data.frame(cp=seq(.0001,.1,length.out = 50)),
                             trControl=trainControl(method="repeatedcv",repeats=100,number=10))


TreeTune_housePricing$bestTune

TreeTune_housePricing$results


housePricePredict_Tree <- predict(TreeTune_housePricing,house_test)

housePricePredict_Tree <- round(as.data.frame(housePricePredict_Tree),3)

names(housePricePredict_Tree) <- "price"

write.csv(housePricePredict_Tree,file="Prediction_Tree.csv")


##### Lets check if our models are predicting properly by using our training data to predict new values

#train_housePrice <- house_train$housePrice

#Tree_housePrice <- predict(TreeTune_housePricing,house_train)
#TreePrediction <- as.data.frame(cbind(train_housePrice,Tree_housePrice))
#Tree_prediction_plot <- ggplot(data=TreePrediction,aes(x=train_housePrice,
#                                                       y=Tree_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "Tree_prediction_plot.png",width = 1000, height = 1000)
#Tree_prediction_plot
#dev.off()







