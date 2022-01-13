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





########## Elastic net
dummyMat <- model.matrix(lm(price~.,data=house_train))
dummyMat <- dummyMat[,-1]
Enet.grid_house <- expand.grid(alpha = seq(0,.5,length.out=15),
                               lambda=seq(10,500,length.out=15))

ENetTune_housePrice<-train(y=house_train[,1],x=dummyMat,
                           method="glmnet",tuneGrid = Enet.grid_house,
                           trControl=trainControl(method="repeatedcv",
                                                  repeats=100,number=10))
ENetTune_housePrice$bestTune

ENetTune_housePrice$results

test1 <- house_test
y <-1:nrow(test1)
test1 <- cbind(y,test1)

dummyMatTest <- model.matrix(lm(y~.,data=test1))
dummyMatTest <- dummyMatTest[,-1]

ENetPrediction_housePrice <- predict(ENetTune_housePrice,dummyMatTest)

names(ENetPrediction_housePrice) <- "price"

write.csv(ENetPrediction_housePrice,file="Prediction_enet.csv")


########## PCR
pcrTune_housePrice <- train(y=house_train[,1],x=house_train[,2:17],
                            method="pcr",
                            preProcess = c("center","scale"),
                            tuneGrid=data.frame(ncomp=1:50),
                            trControl=trainControl(method="repeatedcv",repeats=100,number=10))
pcrTune_housePrice$bestTune
pcrTune_housePrice$results

PcrPrediction_housePrice <- predict(pcrTune_housePrice,house_test)

names(PcrPrediction_housePrice) <- "price"

write.csv(PcrPrediction_housePrice,file="Prediction_pcr.csv")


############### PLS
plsTune_houseprice<-train(y=house_train[,1],x=house_train[,2:17],
                          method="pls",
                          preProcess = c("center","scale"),
                          tuneGrid=data.frame(ncomp=1:50),
                          trControl=trainControl(method="repeatedcv",repeats=100,number=10))
plsTune_houseprice$bestTune
plsTune_houseprice$results

PlsPrediction_housePrice <- predict(plsTune_houseprice,house_test)

names(PlsPrediction_housePrice) <- "price"

write.csv(PlsPrediction_housePrice,file="Prediction_pls.csv")





#train_housePrice <- house_train$housePrice

#enet_housePrice <- predict(ENetTune_housePrice,dummyMat)
#enetPrediction <- as.data.frame(cbind(train_housePrice,enet_housePrice))
#enet_prediction_plot <- ggplot(data=enetPrediction,aes(x=train_housePrice,
#                                                       y=enet_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "enet_prediction_plot.png",width = 1000, height = 1000)
#enet_prediction_plot
#dev.off()


#PCR_housePrice <- predict(pcrTune_housePrice,house_train)
#PCRPrediction <- as.data.frame(cbind(train_housePrice,PCR_housePrice))
#PCR_prediction_plot <- ggplot(data=PCRPrediction,aes(x=train_housePrice,
 #                                                    y=PCR_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "PCR_prediction_plot.png",width = 1000, height = 1000)
#PCR_prediction_plot
#dev.off()



#PLS_housePrice <- predict(plsTune_houseprice,house_train)
#PLSPrediction <-as.data.frame(cbind(train_housePrice,PLS_housePrice))
#PLS_prediction_plot <- ggplot(data=PLSPrediction,aes(x=train_housePrice,
  #                                                   y=PLS_housePrice))+geom_jitter()+geom_smooth(method = loess)

#png(file = "PLS_prediction_plot.png",width = 1000, height = 1000)
#PLS_prediction_plot
#dev.off()



