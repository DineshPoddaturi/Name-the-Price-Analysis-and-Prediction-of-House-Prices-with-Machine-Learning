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

write.csv(out, "pred2_out.csv")





