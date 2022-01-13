#loading libraries
library(psych)
library(corrplot)
library(caret)
library(tidyverse)
library(lubridate)
# library(MASS)
require(pls)
library(glmnet)
# library(fields)
library(gdata)
library(Matrix)
# library(cubature)
library(graphics)
library(rpart)
library(randomForest)
library(gbm)
library(earth)


train_data <- read.csv("../Data/train.csv")
test_data <- read.csv("../Data/test.csv")

train_data$date <- ymd(train_data$date)

train_data %>% glimpse()

test_data %>% glimpse()



train_data%>%glimpse()

trainData <- train_data %>%select(-property,-date)
trainData$waterfront <- as.factor(trainData$waterfront)
trainData$condition <- as.factor(trainData$condition)

library(ggcorrplot)


trainData%>%glimpse()
ggcorrplot(cor(trainData), p.mat = cor_pmat(trainData), hc.order=TRUE, type='lower')


detach("package:MASS", unload=TRUE)


train_data <- train_data%>%select(property,price,date,bedrooms:sqft_lot15)%>%as.data.frame()
train_data <- train_data %>% select(price:sqft_lot15)




priceplot <- ggplot(data = train_data,aes(x=price))+geom_histogram(fill="Green")+
  scale_x_continuous(breaks= seq(min(train_data$price), max(train_data$price), by=1000000))
#from the plot we can see that most of the houses are between 75000 and 1075000.

#Lets see the correlation between the predictors and the price

housePrice <- train_data$price
correlationMatrix <- cor(cbind(housePrice,train_in))
correlationMatrix_sorted <- as.matrix(sort(correlationMatrix[,'housePrice'], decreasing = TRUE))

#From the correlation matrix the predictors sqft_living, grade, sqft_above, sqft_living15, bathrooms are highly correlated 
#with the price where as zipcode, longitude, condition, yr_built are less correlated.

#Lets see how the house prices are affected by the year the houses are built
price_YearBuilt <- train_data %>% ggplot(aes(x=yr_built,y=price))+geom_point()+
                      scale_y_continuous(breaks= seq(min(train_data$price), max(train_data$price), by=1000000))


#Fitting a multiple linear regression to find significant explanatory variables
mreg <- lm(price ~ . , data=train_data)

an <- anova(mreg)


# Analysis of Variance Table
# 
# Response: price
#                    Df     Sum Sq    Mean Sq   F value    Pr(>F)    
#   date             1 4.6142e+11 4.6142e+11   10.7926  0.001023 ** 
#   bedrooms         1 1.3050e+14 1.3050e+14 3052.3730 < 2.2e-16 ***
#   bathrooms        1 2.7594e+14 2.7594e+14 6454.2090 < 2.2e-16 ***
#   sqft_living      1 3.3042e+14 3.3042e+14 7728.3782 < 2.2e-16 ***
#   sqft_lot         1 2.3902e+12 2.3902e+12   55.9054 8.244e-14 ***
#   floors           1 2.6707e+09 2.6707e+09    0.0625  0.802643    
# waterfront         1 5.2917e+13 5.2917e+13 1237.7278 < 2.2e-16 ***
#   view             1 2.3373e+13 2.3373e+13  546.6865 < 2.2e-16 ***
#   condition        1 1.1827e+13 1.1827e+13  276.6238 < 2.2e-16 ***
#   grade            1 4.6525e+13 4.6525e+13 1088.2065 < 2.2e-16 ***
#   sqft_above       1 1.0268e+12 1.0268e+12   24.0159 9.705e-07 ***
#   yr_built         1 6.5894e+13 6.5894e+13 1541.2582 < 2.2e-16 ***
#   yr_renovated     1 2.8626e+11 2.8626e+11    6.6955  0.009680 ** 
#   zipcode          1 1.7674e+10 1.7674e+10    0.4134  0.520263    
#   lat              1 5.9773e+13 5.9773e+13 1398.0842 < 2.2e-16 ***
#   long             1 5.0728e+12 5.0728e+12  118.6512 < 2.2e-16 ***
#   sqft_living15    1 7.8880e+10 7.8880e+10    1.8450  0.174398    
#   sqft_lot15       1 4.4232e+11 4.4232e+11   10.3458  0.001302 ** 
#   Residuals     9981 4.2672e+14 4.2754e+10                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The ANOVA table above concludes that almost all the explanatory variables are significant explaining the price of the house




impVar_lm <- varImp(mreg)

#                 Overall
# date           5.316075
# bedrooms      12.878415
# bathrooms      9.268720
# sqft_living   24.228047
# sqft_lot       1.855466
# floors         2.047728
# waterfront    24.436059
# view          15.825967
# condition      9.193686
# grade         29.778625
# sqft_above     5.180917
# yr_built      24.627081
# yr_renovated   4.449300
# zipcode       11.909202
# lat           37.991541
# long          10.582156
# sqft_living15  1.536770
# sqft_lot15     3.216488






####################### PREDICTION OF PRICES ########################

#Here we are removing date from the data since the houses sold are in the same year. 
train_in <- train_data%>%select(bedrooms:sqft_lot15)


train_in%>%glimpse()
#Creating a new variable for nelwy built houses (>2000) 
# yrBuilt_Recent <- (train_in$yr_built>2000)*1
# 
# train_in <- train_in%>%mutate(yrBuilt_Recent)
# 
# train_in$yrBuilt_Recent <- as.factor(train_in$yrBuilt_Recent)
# 
# #indicator for renovated building
# yrRenovated_recent <- (train_in$yr_renovated>1990)*1
# train_in <- train_in%>%mutate(yrRenovated_recent)
# train_in$yrRenovated_recent <- as.factor(train_in$yrRenovated_recent)


house_train <- cbind(housePrice,train_in)%>%as.data.frame()


#changing zipcode to factor variable since it is not an integer/numeric variable
house_train$zipcode <- as.factor(house_train$zipcode)


#Removing latitude and longitude from the data. Since it adds extraneous information to our data. 
#Another reson for removing the latitide and longitude is because we are least interested in spatial analysis
#We are also dropping sqft_living, sqft_lot,sqft_above, and sqft_basement since we have sqft_living15 and sqft_lot15
#and those variables adds unnecessary information
house_train <- house_train%>%select(-lat,-long,-sqft_above, -sqft_basement, -sqft_living15, -sqft_lot15)

#changing the waterfront to factor since it is an indicator 
house_train$waterfront <- as.factor(house_train$waterfront)

#changing the condition to a factor since it is a rating
house_train$condition <- as.factor(house_train$condition)

house_train%>%glimpse()

################# replicating the same changes for test data set

house_test <- test_data %>% select(bedrooms:sqft_lot15) 

# #Creating a new variable for nelwy built houses (>2000) 
# yrBuilt_Recent <- (house_test$yr_built>2000)*1
# 
# house_test <- house_test%>%mutate(yrBuilt_Recent)
# 
# house_test$yrBuilt_Recent <- as.factor(house_test$yrBuilt_Recent)
# 
# #indicator for renovated building
# yrRenovated_recent <- (house_test$yr_renovated>1990)*1
# house_test <- house_test%>%mutate(yrRenovated_recent)
# house_test$yrRenovated_recent <- as.factor(house_test$yrRenovated_recent)



house_test <- house_test %>% select(-lat,-long,-sqft_above, -sqft_basement, -sqft_living15, -sqft_lot15)

house_test$zipcode <- as.factor(house_test$zipcode)

house_test$waterfront <- as.factor(house_test$waterfront)

house_test$condition <- as.factor(house_test$condition)

house_test %>% glimpse()



################ Using Tree to predict

# tuneGrid=data.frame(cp=seq(.001,.01,length.out = 10)),
TreeTune_housePricing<-train(y=house_train[,1],x=house_train[,2:13],
                         method="rpart",tuneGrid=data.frame(cp=seq(.0001,.1,length.out = 50)),
                         trControl=trainControl(method="repeatedcv",repeats=100,number=10))


TreeTune_housePricing$bestTune
# cp
# 0.0001

TreeTune_housePricing$results
#           cp     RMSE  Rsquared       MAE   RMSESD RsquaredSD    MAESD
# 11  0.000100000 183945.6 0.7682293  92455.98 27481.36 0.04425470 4482.587


#predicting the price

housePricePredict_Tree <- predict(TreeTune_housePricing,house_test)

housePricePredict_Tree <- round(as.data.frame(housePricePredict_Tree),3)

names(housePricePredict_Tree) <- "price"

write.csv(housePricePredict_Tree,file="Prediction_Tree.csv")




############# Using GBM to predict

gbm.grid <- expand.grid(n.trees = seq(120,160,length.out=3),
                        interaction.depth = seq(1,5),
                        shrinkage = seq(.05,0.2,length.out=3),
                        n.minobsinnode = seq(7,12,length.out=3))

cv.control_house <- trainControl(method = "repeatedcv", repeats = 1,number = 10)

GbmTune_housePricing<-train(y=house_train[,1],x=house_train[,2:13],tuneGrid=gbm.grid,
                        method="gbm",
                        trControl=cv.control_house)

GbmTune_housePricing$bestTune
GbmTune_housePricing$results%>%filter(shrinkage==0.05 & n.trees==160 & n.minobsinnode==7 & interaction.depth==5)

#   shrinkage interaction.depth n.minobsinnode n.trees     RMSE  Rsquared      MAE   RMSESD
# 1      0.05                 5              7     160 138241.2 0.8674524 76983.51 16841.08
#      RsquaredSD    MAESD
# 1 0.02161843 3150.249




#predicting the price

housePricePredict_Gbm <- predict(GbmTune_housePricing,house_test)

housePricePredict_Gbm <- round(as.data.frame(housePricePredict_Gbm),3)

names(housePricePredict_Gbm) <- "price"

write.csv(housePricePredict_Gbm,file="Prediction_Gbm.csv")



############### Using Neural nets to predict

nnet.grid <- expand.grid(size= seq(1,5,length.out=5),
                         decay= seq(.3,.8,length.out=6))

nnetTune_housePrice<-train(y=house_train[,1],x=house_train[,2:13],
                            method="nnet", trace=FALSE,
                            preProc = c("center","scale"),
                            linout=TRUE,tuneGrid=nnet.grid,
                            maxit=50,
                            trControl=cv.control_house)
nnetTune_housePrice$bestTune
nnetTune_housePrice$results%>%filter(size==10,decay==0.38)
#     size decay     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#     10  0.38 280029.6 0.4513237 170516.6 35221.78 0.06552164 9497.666

nnetPrediction_housePrice <- predict(nnetTune_housePrice,house_test)
nnetPrediction_housePrice<-round(as.data.frame(nnetPrediction_housePrice),3)

names(nnetPrediction_housePrice) <- "price"

write.csv(nnetPrediction_housePrice,file="Prediction_neural.csv")



################### Using multiple linear regression

lmTune_housePrice <- train(housePrice~.,data=house_train,
                            method="lm",
                            trControl=trainControl(method="repeatedcv",
                                                   repeats=100,number=10))
lmTune_housePrice$results
#   intercept     RMSE  Rsquared     MAE   RMSESD RsquaredSD    MAESD
# 1      TRUE 169968.5 0.8004974 98518.5 23833.69 0.02564948 3733.016

lmPrediction_housePrice <- predict(lmTune_housePrice,house_test)

lmPrediction_housePrice<-round(as.data.frame(lmPrediction_housePrice),3)

names(lmPrediction_housePrice) <- "price"

write.csv(lmPrediction_housePrice,file="Prediction_regression.csv")



############## kNN Prediction
knnTune_housePrice<- train(y=house_train[,1],x=house_train[,2:13],
                             method="knn",
                             preProcess = c("center","scale"),
                             tuneGrid=data.frame(.k=1:20),
                             trControl=trainControl(method="repeatedcv",repeats=100,number=10))
knnTune_housePrice$bestTune
knnTune_housePrice$results
#     k     RMSE  Rsquared       MAE   RMSESD RsquaredSD    MAESD
# 4   4 193332.3 0.7483432  94518.80 29906.35 0.03708109 4919.976



knnPrediction_housePrice<- predict(knnTune_housePrice,house_test)

knnPrediction_housePrice<-round(as.data.frame(knnPrediction_housePrice),3)

names(knnPrediction_housePrice) <- "price"

write.csv(knnPrediction_housePrice,file="Prediction_knn.csv")



############## XG boosting
library(xgboost)

sparse_matrix <- sparse.model.matrix(housePrice~.-1,data = house_train)

str(sparse_matrix)
house_train%>%glimpse()

dummyMat <- model.matrix(lm(housePrice~.,data=house_train))
dummyMat <- dummyMat[,-1]

output_vector <- house_train$housePrice
bst <- xgboost(data = dummyMat, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10)

importance <- xgb.importance(feature_names = colnames(dummyMat), model = bst)

xgbTune_housePrice<- train(y=house_train[,1],x=dummyMat,
                           method="xgbTree",
                           trControl=trainControl(method="repeatedcv",repeats=100,number=10))

xgbTune_housePrice$bestTune

xgbTune_housePrice$results%>%filter(nrounds==150,max_depth==3,gamma==0,min_child_weight==1,subsample==0.75,eta==0.3,
                                    colsample_bytree==0.8)
#   eta max_depth gamma colsample_bytree min_child_weight subsample nrounds     RMSE  Rsquared
# 1 0.3         3     0              0.8                1      0.75     150 154796.7 0.8341774
#     MAE   RMSESD RsquaredSD    MAESD
# 1 87655.12 20523.45 0.03420622 3704.675

xgBoostPrediction_housePrice<- predict(xgbTune_housePrice,house_test)

xgBoostPrediction_housePrice<-round(as.data.frame(xgBoostPrediction_housePrice),3)

names(xgBoostPrediction_housePrice) <- "price"

write.csv(xgBoostPrediction_housePrice,file="Prediction_xgboost.csv")

######################## Random forests

dummyMat <- model.matrix(lm(housePrice~.,data=house_train))
dummyMat <- dummyMat[,-1]


ForestTune_housePrice<-train(y=house_train[,1],x=dummyMat,
                             tuneGrid=data.frame(mtry=1:100),
                           method="rf",ntree=500,
                           trControl=trainControl(method="oob"))

ForestTune_housePrice$bestTune
ForestTune_housePrice$results%>%filter(mtry==26)
ForestTune_housePrice

#   RMSE  Rsquared mtry
# 161072.8 0.8190347   26

test1 <- house_test
y <-1:nrow(test1)
test1 <- cbind(y,test1)
test1 %>% glimpse()

dummyMatTest <- model.matrix(lm(y~.,data=test1))
dummyMatTest <- dummyMatTest[,-1]

forestPrediction_housePrice<- predict(ForestTune_housePrice,dummyMatTest)

forestPrediction_housePrice<-round(as.data.frame(forestPrediction_housePrice),3)

names(forestPrediction_housePrice) <- "price"

write.csv(forestPrediction_housePrice,file="Prediction_randomForest.csv")



########## Elastic net

Enet.grid_house <- expand.grid(alpha = seq(0,.5,length.out=15),
                         lambda=seq(10,500,length.out=15))

ENetTune_housePrice<-train(y=house_train[,1],x=dummyMat,
                            method="glmnet",tuneGrid = Enet.grid_house,
                            trControl=cv.control_house)
ENetTune_housePrice$bestTune
#         alpha lambda
# 41 0.07142857    325

ENetTune_housePrice$results%>%filter(lambda==325)
# alpha       lambda     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
# 0.07142857    325 170114.9 0.8002947 98508.57 23553.94 0.02532276 3733.543

ENetPrediction_housePrice <- predict(ENetTune_housePrice,dummyMatTest)

names(ENetPrediction_housePrice) <- "price"

write.csv(ENetPrediction_housePrice,file="Prediction_enet.csv")


# PCR prediction:
# tuneGrid=data.frame(ncomp=1:20),
pcrTune_housePrice <- train(y=house_train[,1],x=house_train[,2:13],
                             method="pcr",
                             preProcess = c("center","scale"),
                            tuneGrid=data.frame(ncomp=1:50),
                             trControl=trainControl(method="repeatedcv",repeats=100,number=10))
pcrTune_housePrice$bestTune
pcrTune_housePrice$results%>%filter(ncomp==49)
#     ncomp     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
# 1    49 211394.4 0.6897637 129662.8 24314.89 0.02221704 4411.102

PcrPrediction_housePrice <- predict(pcrTune_housePrice,house_test)

names(PcrPrediction_housePrice) <- "price"

write.csv(PcrPrediction_housePrice,file="Prediction_pcr.csv")


#PLS prediction:
plsTune_houseprice<-train(y=house_train[,1],x=house_train[,2:13],
                           method="pls",
                           preProcess = c("center","scale"),
                           tuneGrid=data.frame(ncomp=1:50),
                           trControl=trainControl(method="repeatedcv",repeats=100,number=10))
plsTune_houseprice$bestTune
plsTune_houseprice$results%>%filter(ncomp==41)
#     ncomp     RMSE Rsquared      MAE   RMSESD RsquaredSD    MAESD
# 1    41 170069.3 0.800278 98529.16 23717.39 0.02516047 3796.101

PlsPrediction_housePrice <- predict(plsTune_houseprice,house_test)

names(PlsPrediction_housePrice) <- "price"

write.csv(PlsPrediction_housePrice,file="Prediction_pls.csv")







##### Lets check if our models are predicting properly by using our training data to predict new values

train_housePrice <- house_train$housePrice
GBM_housePrice <- predict(GbmTune_housePricing,house_train)
GBMPrediction <- cbind(train_housePrice,GBM_housePrice)%>%as.data.frame()
GBM_prediction_plot <- ggplot(data=GBMPrediction,aes(x=train_housePrice,
                                                     y=GBM_housePrice))+geom_jitter()+geom_smooth(method = loess)

XGB_housePrice <- predict(xgbTune_housePrice,house_train)
XGBPrediction <- cbind(train_housePrice,XGB_housePrice)%>%as.data.frame()
XGB_prediction_plot <- ggplot(data=XGBPrediction,aes(x=train_housePrice,
                                                     y=XGB_housePrice))+geom_jitter()+geom_smooth(method = loess)

RF_housePrice <- predict(ForestTune_housePrice,dummyMat)
RFPrediction <- cbind(train_housePrice,RF_housePrice)%>%as.data.frame()
RF_prediction_plot <- ggplot(data=RFPrediction,aes(x=train_housePrice,
                                                     y=RF_housePrice))+geom_jitter()+geom_smooth(method = loess)

lm_housePrice <- predict(lmTune_housePrice,house_train)
lmPrediction <- cbind(train_housePrice,lm_housePrice)%>%as.data.frame()
lm_prediction_plot <- ggplot(data=lmPrediction,aes(x=train_housePrice,
                                                   y=lm_housePrice))+geom_jitter()+geom_smooth(method = loess)

enet_housePrice <- predict(ENetTune_housePrice,dummyMat)
enetPrediction <- cbind(train_housePrice,enet_housePrice)%>%as.data.frame()
enet_prediction_plot <- ggplot(data=enetPrediction,aes(x=train_housePrice,
                                                   y=enet_housePrice))+geom_jitter()+geom_smooth(method = loess)

Tree_housePrice <- predict(TreeTune_housePricing,house_train)
TreePrediction <- cbind(train_housePrice,Tree_housePrice)%>%as.data.frame()
Tree_prediction_plot <- ggplot(data=TreePrediction,aes(x=train_housePrice,
                                                       y=Tree_housePrice))+geom_jitter()+geom_smooth(method = loess)

KNN_housePrice <- predict(knnTune_housePrice,house_train)
KNNPrediction <- cbind(train_housePrice,KNN_housePrice)%>%as.data.frame()
KNN_prediction_plot <- ggplot(data=KNNPrediction,aes(x=train_housePrice,
                                                       y=KNN_housePrice))+geom_jitter()+geom_smooth(method = loess)

PCR_housePrice <- predict(pcrTune_housePrice,house_train)
PCRPrediction <- cbind(train_housePrice,PCR_housePrice)%>%as.data.frame()
PCR_prediction_plot <- ggplot(data=PCRPrediction,aes(x=train_housePrice,
                                                     y=PCR_housePrice))+geom_jitter()+geom_smooth(method = loess)

PLS_housePrice <- predict(plsTune_houseprice,house_train)
PLSPrediction <- cbind(train_housePrice,PLS_housePrice)%>%as.data.frame()
PLS_prediction_plot <- ggplot(data=PLSPrediction,aes(x=train_housePrice,
                                                     y=PLS_housePrice))+geom_jitter()+geom_smooth(method = loess)



NNET_housePrice <- predict(nnetTune_housePrice,house_train)
NNETPrediction <- cbind(train_housePrice,NNET_housePrice)%>%as.data.frame()
NNET_prediction_plot <- ggplot(data=NNETPrediction,aes(x=train_housePrice,
                                                     y=NNET_housePrice))+geom_jitter()+geom_smooth(method = loess)



####Stacking
#By looking at the prediction plots we can see that random forest, GBM , KNN predictions looks linear. 
#Lets stack them together using OLS and use the weights


# randomly break sample into 10 folds
shuffle<- sample(dim(house_train)[1])
folds<- list()
for (i in 1:10){
  if (i != 10){
    folds[[i]]<- shuffle[(490*(i-1)+1):(490*i)]
  } else {folds[[i]]<- shuffle[(490*(i-1)+1):dim(house_train)[1]]}
}

house_train[c(folds[[1]]),]

method<- c("knn","gbm","rf")
parameter<- list(data.frame(k=4),
                 data.frame(n.trees=160, interaction.depth=5, shrinkage=0.05, n.minobsinnode=7),
                 data.frame(mtry=26))
pred1<- list()
pred2<- list()

pred11<- list()
pred21<- list()

# for (i in 1:10){
  train<- house_train
  test<- house_test
  pred1<- matrix(NA,nrow=dim(test)[1],ncol=5)
  pred2<- matrix(NA,nrow=dim(train)[1],ncol=3)
  
  pred11<- matrix(NA,nrow=dim(train)[1],ncol=5)
  pred21<- matrix(NA,nrow=dim(train)[1],ncol=3)
  for (j in 1:3){
      modelfit<- train(housePrice~.,
                       data=train,
                       method=method[j],
                       preProc=c("center","scale"),
                       trControl=trainControl(method="none"),
                       tuneGrid=parameter[[j]])
      
    pred1[,j]<- predict(modelfit,newdata=test)
    pred2[,j]<- predict(modelfit,newdata=train)
    
    pred11[,j]<- predict(modelfit,newdata=train)
    pred21[,j]<- predict(modelfit,newdata=train)
  }
  emfit1<- lm(train$housePrice~pred2)
  emfit2<- train(y=train$housePrice,
                 x=as.data.frame(pred2),
                 method="rf",
                 trControl=trainControl(method="none"))
  
  emfit11<- lm(train$housePrice~pred21)
  emfit21<- train(y=train$housePrice,
                 x=as.data.frame(pred21),
                 method="rf",
                 trControl=trainControl(method="none"))
  
  pred1[,4]<- cbind(matrix(1,nrow=dim(test)[1],ncol=1),pred1[,-c(4,5)])%*%coef(emfit1)
  pred1[,5]<- predict(emfit2,as.data.frame(pred1))
  
  
  pred11[,4]<- cbind(matrix(1,nrow=dim(train)[1],ncol=1),pred11[,-c(4,5)])%*%coef(emfit11)
  pred11[,5]<- predict(emfit21,as.data.frame(pred11))
  
  
# }

feature<- as.data.frame(pred1)
colnames(feature)[1:3]<- method
colnames(feature)[4:5]<- c("enOLS","enFOR")

feature1<- as.data.frame(pred11)
colnames(feature1)[1:3]<- method
colnames(feature1)[4:5]<- c("enOLS","enFOR")

RF_housePriceStack<- feature1$enFOR
RF_housePriceStack <- cbind(train_housePrice,RF_housePriceStack)%>%as.data.frame()
RF_prediction_plot_stack <- ggplot(data=RF_housePriceStack,aes(x=train_housePrice,
                                                       y=RF_housePriceStack))+geom_jitter()+geom_smooth(method = loess)

OLS_housePriceStack<- feature1$enOLS
OLS_housePriceStack <- cbind(train_housePrice,OLS_housePriceStack)%>%as.data.frame()
OLS_prediction_plot_stack <- ggplot(data=OLS_housePriceStack,aes(x=train_housePrice,
                                                               y=OLS_housePriceStack))+geom_jitter()+geom_smooth(method = loess)


Stacking_Prediction_housePrice_RF <- feature$enFOR %>% as.data.frame()

names(Stacking_Prediction_housePrice_RF) <- "price"

write.csv(Stacking_Prediction_housePrice_RF,file="Prediction_StackingRF.csv")

Stacking_Prediction_housePrice_OLS <- feature$enOLS %>% as.data.frame()

names(Stacking_Prediction_housePrice_OLS) <- "price"

write.csv(Stacking_Prediction_housePrice_OLS,file="Prediction_StackingOLS.csv")




#Bylooking at the RMSE and Rsquared values we can choose predictors with low RMSE and high Rsquared values.
#From the above methods, I would pick Random forest, xgb, gbm, linear regression, Knn, and tree to build an ensamble model.


#GBM best results
#   RMSE      Rsquared    
# 138569.5 0.8666611

#XGB best results
#     RMSE  Rsquared
# 154853.1 0.8339858

#Random Forest best results
#     RMSE  Rsquared  
# 161460 0.8181637 

#Regression
#   RMSE    Rsquared     
# 170052.2 0.8005786 

#Enet
#      RMSE  Rsquared     
#   170143.6  0.7999568

#TREE
#  RMSE     Rsquared   
# 183698.4 0.7693686

#KNN
#     RMSE  Rsquared        
# 193293.7 0.7483630 




