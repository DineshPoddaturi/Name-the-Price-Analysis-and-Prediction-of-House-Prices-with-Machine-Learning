setwd("/home/xiyuansu/stat502_project/Xiyuan")


library(psych)
library(caret)
library(rpart)
library(randomForest)
library(xgboost)
library(pls)
library(party)
library(readr)
require(glmnet)
require(caretEnsemble)

#read in the csv files for training and output
ks_train <- read.csv(file='ks_train.csv')
ks_test <- read.csv(file='ks_test.csv')

set.seed(20180422)


#kNN
ks_knnTune<-train(y=ks_train[,1],
                  x=ks_train[,2:18],
                  method="knn",
                  preProcess = c("center","scale"),
                  tuneGrid=data.frame(.k=1:10),
                  trControl=trainControl(method="repeatedcv",repeats=100,number=10))

ks_knnTune$results

saveRDS(ks_knnTune, "ks_knnTune.rds")


ks_knn_predict <- predict(ks_knnTune, ks_test)
ks_knn_predict <- round(as.data.frame(ks_knn_predict),3)
names(ks_knn_predict) <- "price"

write.csv(ks_knn_predict, file="ks_knn_predict.csv")

#gbm

gbm.grid <- expand.grid(n.trees = seq(120,160,length.out=3),
                        interaction.depth = seq(1,5),
                        shrinkage = seq(.05,0.2,length.out=3),
                        n.minobsinnode = seq(7,12,length.out=3))

cv.control <- trainControl(method = "repeatedcv", repeats =100,number = 10)

ks_gbmTune<-train(y=ks_train[,1],
                  x=ks_train[,2:18],
                  tuneGrid=gbm.grid,
                  method="gbm",
                  trControl=cv.control)

ks_gbmTune$bestTune

saveRDS(ks_gbmTune, "ks_gbmTune.rds")

ks_gbm_predict <- predict(ks_gbmTune, ks_test)
ks_gbm_predict <- round(as.data.frame(ks_gbm_predict),3)
names(ks_gbm_predict) <- "price"

write.csv(ks_gbm_predict, file="ks_gbm_predict.csv")




#Now do some Random Forest fitting
#Load the randomForest package

AmesRf<-randomForest(price~.,data=ks_train, 
                     type="regression",ntree=50000,mtry=4) 

sqrt(AmesRf$mse[50000])

#caret will tune random forests on OOB error over mtry
#It is alternatively not hard to simply make the kind of
#exhaustive search that led to the table in the short course 
#slides comparing all pairs of mtry and n-min (nodesize)
#load the caret package and begin

#task 1 failed - "Can not handle categorical predictors with more than 53 categories."
ks_train$zipcode <- as.numeric(ks_train$zipcode)

ks_forestTune<-train(y=ks_train[,1],
                     x=ks_train[,2:18],
                     tuneGrid=data.frame(mtry=1:20),
                     method="rf",ntree=5000,
                     trControl=trainControl(method="oob"))
ks_forestTune$results
saveRDS(ks_forestTune, file="ks_forestTune.rds")

ks_rf_predict <- predict(ks_forestTune, ks_test)
ks_rf_predict <- round(as.data.frame(ks_rf_predict),3)
names(ks_rf_predict) <- "price"

write.csv(ks_rf_predict, file="ks_rf_predict.csv")

#xgboost
library(xgboost)

xgbGrid <- expand.grid(nrounds = 2000,
                       max_depth = 4,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .5,
                       min_child_weight = 1,
                       subsample = 1)

cv.control <- trainControl(method="repeatedcv",repeats=100,number=10)

ks_xgbTune <- train(y=ks_train[,1],
                    x=ks_train[,2:18],
                    method = "xgbTree",
                    trControl = cv.control,
                    tuneGrid = xgbGrid)

ks_xgbTune$bestTune

saveRDS(ks_xgbTune, file="ks_xgbTune.rds")

ks_xgb_predict <- predict(ks_xgbTune, ks_test)

ks_xgb_predict <- round(as.data.frame(ks_xgb_predict),3)
names(ks_xgb_predict) <- "price"

write.csv(ks_xgb_predict, file="ks_xgb_predict.csv")


#enet

enet.grid <- expand.grid(alpha = seq(0,.5,length.out=15),
                         lambda=seq(10,500,length.out=15))

ks_enetTune<-train(y=ks_train[,1],
                   x=ks_train[,2:18],
                   method="glmnet",
                   tuneGrid = enet.grid,
                   trControl=cv.control)

ks_enetTune$bestTune

saveRDS(ks_enetTune, file="ks_enetTune.rds")

ks_enet_predict <- predict(ks_enetTune, ks_test)

ks_enet_predict <- round(as.data.frame(ks_enet_predict),3)
names(ks_enet_predict) <- "price"

write.csv(ks_enet_predict, file="ks_enet_predict.csv")


#######################################################################################

# Tune control.
my_tc <- trainControl(method = "repeatedcv", savePredictions = "final", 
                      number = 100, repeats = 10)


xgb.grid = expand.grid(nrounds = 2000,
                       eta = 0.05,
                       max_depth = 4,
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 1,
                       gamma = 0)


my_tl <- list(
  gl = caretModelSpec(method = "glmnet", preProcess = c("center","scale"), 
                      tuneGrid = expand.grid(.alpha = seq(0, 0.3, by = 0.05),
                                             .lambda = seq(0.001, 0.1, by = 0.01))),
  pls = caretModelSpec(method = "pls", preProcess = c("center","scale"), 
                       tuneGrid = data.frame(.ncomp=seq(1:10))),
  xg = caretModelSpec(method = "xgbTree", preProcess = c("center","scale"), tuneGrid = xgb.grid)
)


model_list <- caretList(y = log(ks_train[,1]),
                        x = ks_train[,2:18],
                        trControl = my_tc,
                        tuneList = my_tl)


set.seed(2018502)
fit <- caretEnsemble(model_list, trControl = my_tc)
summary(fit)
fit$error

saveRDS(fit, file="fit.rds")

newd <- data.frame(gl = predict(fit$models$gl, ks_test),
                   pls = predict(fit$models$pls, ks_test),
                   xg = predict(fit$models$xg, newdata = as.matrix(ks_test)))
cor(newd)

beta <- coef(fit$ens_model$finalModel)
beta
hat <- as.matrix(cbind(1, newd)) %*% beta

out <- data.frame(Id = 1:length(hat), price = exp(hat))














