# require(stringr) #extracting string patterns
# require(Matrix) # matrix transformations
# require(glmnet) # ridge, lasso & elastinet
# require(xgboost) # gbm
# require(randomForest)
# require(Metrics) # rmse
# require(caret) # one hot encoding
# require(scales) # plotting $$
# require(e1071) # skewness
# require(corrplot) # correlation plot
# require(psych)
# require(tidyverse)
# require(lubridate)
# require(pls)
# require(gdata)
# require(graphics)
# require(rpart)
# require(gbm)
# require(earth)
# require(Boruta)

### Here instead of loading each package seperately, I load all of them at the same time. 
packagesToLoad <- c("stringr", "Matrix", "glmnet", "xgboost", "randomForest", "Metrics", "caret", "scales",
                    "e1071", "corrplot", "psych", "tidyverse", "lubridate", "pls", "gdata", "graphics", "rpart",
                    "gbm", "earth", "Boruta", "ggcorrplot", "rpart.plot")
lapply(packagesToLoad, require, character.only = TRUE)


# Reading training and testing data
train_data <- read.csv("./Data/train.csv")
test_data <- read.csv("./Data/test.csv")

### Converting date to ymd format
train_data$date <- ymd(train_data$date)

train_data <- train_data %>% select(price:sqft_lot15)

### This plot simply plots the density of house prices as histogram. 
priceplot <- ggplot(data = train_data,aes(x=price))+geom_histogram(fill="Grey")+
  scale_x_continuous(breaks= seq(min(train_data$price), max(train_data$price), by=1000000))
priceplot

### Here we perform a simple correlation analysis
housePrice <- train_data$price
correlationMatrix <- cor(cbind(housePrice,train_data %>% select(-price)))
correlationMatrix_sorted <- as.matrix(sort(correlationMatrix[,'housePrice'], decreasing = TRUE))
correlationMatrix_sorted

#From the correlation matrix the predictors sqft_living, grade, sqft_above, sqft_living15, and bathrooms are highly correlated 
# and zipcode, longitude, condition, and yr_built are less correlated with the price

#Lets see how the house prices are affected by the year the houses are built
price_YearBuilt <- train_data %>% ggplot(aes(x=yr_built,y=price))+geom_point()+
  scale_y_continuous(breaks= seq(min(train_data$price), max(train_data$price), by=1000000))

## Fitting a multiple linear regression to find significant explanatory variables
mreg <- lm(price ~ . , data=train_data)

an <- anova(mreg)
#The ANOVA table above concludes that almost all the explanatory variables are significant explaining the price of the house
impVar_lm <- varImp(mreg)
impVar_lm
### Variable importance above concludes that bedrooms, sqft_living, waterfront, grade, yr_built, lat are the most 
### important variables. However, all the other variables seem important too. 

# I am using XG boost to further analyze variable importance 
# designX <- train_data %>% select(-price)
# xgBoost_fit <- xgboost(data.matrix(designX), train_data$price, nrounds=800)
# xgBoost_Imp <- xgb.importance(model = xgBoost_fit, feature_names = names(designX))
# xgBoost_Imp
### The XG boost selectd sqft_living, grade, lat, long, sqft_living15, yr_built, and waterfront as the 
### most important variables
# xgBoost_Imp
# Feature        Gain       Cover   Frequency
# 1:   sqft_living 0.342766579 0.080952848 0.105720569
# 2:         grade 0.281494710 0.016340344 0.020216740
# 3:           lat 0.154047844 0.161911073 0.121803874
# 4:          long 0.059350082 0.090955113 0.086749159
# 5: sqft_living15 0.028514173 0.093267398 0.083808060
# 6:      yr_built 0.028293888 0.065392241 0.068811107
# 7:    waterfront 0.026511502 0.002898349 0.000900877
# 8:          view 0.016122752 0.005884459 0.008505339
# 9:    sqft_above 0.015441194 0.084794264 0.066664900
# 10:      sqft_lot 0.010513967 0.135272838 0.125486871
# 11:       zipcode 0.010247276 0.028953751 0.044566917
# 12:    sqft_lot15 0.007177084 0.135922821 0.089663761
# 13: sqft_basement 0.004845246 0.035933456 0.031530696
# 14:      bedrooms 0.004534543 0.011543024 0.052621817
# 15:     bathrooms 0.004318820 0.017579433 0.061259638
# 16:     condition 0.003319069 0.012581875 0.015712355
# 17:  yr_renovated 0.001492976 0.014274313 0.005325773
# 18:        floors 0.001008294 0.005542399 0.010651546

#### I am also using Boruta feature selection algorithm to determine the variable importance
boruta_fit <- Boruta(price~. , data = train_data , doTrace = 2)

print(boruta_fit)
# Boruta performed 68 iterations in 15.85281 mins.
# 18 attributes confirmed important: bathrooms,
# bedrooms, condition, floors, grade and 13 more;
# No attributes deemed unimportant.
plot(boruta_fit)

colMeans(boruta_fit$ImpHistory)

# bedrooms     bathrooms   sqft_living 
# 7.49151943   10.31067030   25.42566323 
# sqft_lot        floors    waterfront 
# 14.55496223   10.57595428   14.84058240 
# view          condition         grade 
# 10.36141918   14.05250969   27.91921900 
# sqft_above    sqft_basement      yr_built 
# 18.56574317    9.11120221   19.00066069 
# yr_renovated       zipcode           lat 
# 2.62159209      22.49199270   69.54239009 
# long        sqft_living15    sqft_lot15 
# 33.74248399   25.26492590   15.47102339 
# shadowMax    shadowMean     shadowMin 
# 1.98228316   -0.02308776   -1.98540693 

###### From all the tests for variable importance above, I conclude that almost all the variables are 
###### important. I am now reading the description of each variable and then decide whether to drop a variable 
###### or not and changing the data type of the variable.


########################################################################################################
####################################### PRE-PROCESSING THE DATA ########################################
########################################################################################################

# Here I am  removing date from the data since the houses sold are in the same year. 
train_in <- train_data %>% select(bedrooms:sqft_lot15)
house_train <- cbind(housePrice,train_in) %>% as.data.frame()

# Changing zipcode to factor variable since it is not an integer/numeric variable
house_train$zipcode <- as.factor(house_train$zipcode)

#### I change the waterfront and condition variable to factor variable. 
## Waterfront is a binary variable and condition is the rating variable. So we change them to factor.
house_train$waterfront <- as.factor(house_train$waterfront)
house_train$condition <- as.factor(house_train$condition)

# I am dropping sqft_living, sqft_lot,sqft_above, and sqft_basement.
# sqft_living15 and sqft_lot15 contain most recent information about the sqft information 
house_train <- house_train %>% select(-sqft_above, -sqft_basement, -sqft_living, -sqft_lot)

################# Replicating the same changes in test data set
house_test <- test_data %>% select(bedrooms:sqft_lot15)
house_test$zipcode <- as.factor(house_test$zipcode)
house_test$waterfront <- as.factor(house_test$waterfront)
house_test$condition <- as.factor(house_test$condition)
house_test <- house_test %>% select(-sqft_above, -sqft_basement, -sqft_living, -sqft_lot)






########################################################################################################
####################################### USING K NEAREST NEIGHBOURS #####################################
########################################################################################################

knnTune_housePrice <- train(y = house_train[,1], x = house_train[,2:13],
                            method = "knn",
                            preProcess = c("center","scale"),
                            tuneGrid = data.frame(.k=1:20),
                            trControl = trainControl(method = "repeatedcv", repeats = 20, number = 10))

##### I standardize the data and perform repeated cross validation. Basically I perform 10-fold cross validation
##### with repetition (Note: As the number of repetitions increase, the execution time also increases). 
##### I am also asking the algorithm to try values of k from 1 to 20.

plot(knnTune_housePrice)
knnBestTune <- knnTune_housePrice$bestTune
#### From the plot and bestTune above, the model selected k = 4
knnTune_housePrice$results


########################################################################################################
####################################### USING NEURAL NETS ##############################################
########################################################################################################

nnet.grid <- expand.grid(size = seq(from = 1, to = 5, length.out = 5),
                         decay = seq(from = .3, to = .8, length.out = 6))

# Size is the number of units in hidden layer (nnet fit a single hidden layer neural network) 
# and decay is the regularization parameter to avoid over-fitting.

nnetTune_housePrice <- train(y = house_train[,1], x = house_train[,2:13],
                           method = "nnet", trace = FALSE,
                           preProc = c("center","scale"),
                           linout = TRUE, tuneGrid = nnet.grid,
                           maxit = 50,
                           trControl = trainControl(method = "repeatedcv", repeats = 2, number = 10) )

plot(nnetTune_housePrice)

nnetBestTune <- nnetTune_housePrice$bestTune
# size decay
# 27    5   0.5

nnetResults <- nnetTune_housePrice$results


########################################################################################################
####################################### USING MULTIPLE LINEAR REGRESSION ###############################
########################################################################################################

lmTune_housePrice <- train(housePrice~., data = house_train,
                           method = "lm",
                           trControl = trainControl(method = "repeatedcv",
                                                  repeats = 2, number = 10))

lmBestTune <- lmTune_housePrice$bestTune

lmResults <- lmTune_housePrice$results


########################################################################################################
####################################### USING RANDOM FOREST ############################################
########################################################################################################


designMat <- model.matrix(lm(housePrice~.,data=house_train))
designMatRF <- designMat[,-1]

forestTune_housePrice <- train(y = house_train[,1], x = designMatRF,
                             tuneGrid = data.frame(mtry=1:50),
                             method = "rf", ntree = 150,
                             trControl = trainControl(method="oob"))

# In the Random Forest model, the original training data is randomly sampled-with-replacement generating small subsets of data.
# These subsets are also known as bootstrap samples.
# mtry: Number of variables randomly sampled as candidates at each split.
# ntree: Number of trees to grow.
# oob: Out Of Bag is a method to validate the ranndom forest model. 
# At each bootstrap, the algorithm leaves out some rows and trains with kept data. Once the training is finished, the model is 
# fed the left out rows and asks to predict the outcome. This is repeated multiple times and oob score is computed by simply adding
# up the number of correctly predicted rows. 
# We can also use repeated CV as well. However, OOB is a well known method and is recognized well. 

plot(forestTune_housePrice)

forestResults <- forestTune_housePrice
# Random Forest 
# 
# 10000 samples
# 85 predictor
# 
# No pre-processing
# Resampling results across tuning parameters:
#   
#   mtry  RMSE      Rsquared 
# 1    310202.1  0.3288181
# 2    240160.1  0.5976978
# 3    210520.7  0.6908705
# 4    192123.4  0.7425391
# 5    182811.1  0.7668927
# 6    175636.4  0.7848309
# 7    168967.7  0.8008603
# 8    164429.1  0.8114147
# 9    162614.4  0.8155541
# 10    159146.9  0.8233364
# 11    161349.2  0.8184131
# 12    157665.6  0.8266097
# 13    157255.1  0.8275116
# 14    156379.6  0.8294267
# 15    155123.8  0.8321553
# 16    154627.6  0.8332274
# 17    155400.4  0.8315563
# 18    153610.1  0.8354149
# 19    152152.2  0.8385243
# 20    151776.8  0.8393202
# 21    153545.8  0.8355527
# 22    151808.8  0.8392523
# 23    153067.8  0.8365751
# 24    153282.6  0.8361160
# 25    150461.3  0.8420935
# 26    151271.3  0.8403886
# 27    151569.5  0.8397586
# 28    152608.6  0.8375542
# 29    150119.1  0.8428108
# 30    150416.8  0.8421867
# 31    148686.7  0.8457963
# 32    151615.5  0.8396614
# 33    151846.9  0.8391717
# 34    150312.1  0.8424064
# 35    151660.1  0.8395671
# 36    152666.3  0.8374313
# 37    149208.4  0.8447122
# 38    148171.3  0.8468633
# 39    150219.5  0.8426005
# 40    149958.4  0.8431472
# 41    151272.2  0.8403867
# 42    150159.3  0.8427267
# 43    150927.0  0.8411143
# 44    150592.5  0.8418178
# 45    150772.8  0.8414389
# 46    150083.7  0.8428850
# 47    150755.1  0.8414761
# 48    153218.2  0.8362538
# 49    148974.5  0.8451987
# 50    152212.0  0.8383974
# 
# RMSE was used to select the optimal model
# using the smallest value.
# The final value used for the model was mtry
# = 38.

forestBestTune <- forestTune_housePrice$bestTune
#     mtry
# 38   38



########################################################################################################
####################################### USING TREE #####################################################
########################################################################################################

TreeTune_housePricing <- train(y = house_train[,1], x = house_train[,2:13],
                             method = "rpart", tuneGrid = data.frame(cp = seq(from = .0001, to = .1, length.out = 50)),
                             trControl = trainControl(method = "repeatedcv",repeats = 50, number = 10))


plot(TreeTune_housePricing)

treeBestTune <- TreeTune_housePricing$bestTune
# cp (complexity parameter)
# 1 1e-04

fit.model <- rpart(house_train[,1]~., data = house_train, cp = treeBestTune)

fittedModelPlot <- rpart.plot(fit.model, main = "Fitted Model")


########################################################################################################
####################################### USING Gradient Boosting Machine ################################
########################################################################################################
gbm.grid <- expand.grid(n.trees = seq(from = 120, to = 180, length.out = 6),
                        interaction.depth = seq(1,5),
                        shrinkage = seq(from = .05, to = 0.2, length.out = 3),
                        n.minobsinnode = seq(from = 7, to = 12, length.out = 3))

cv.control_house <- trainControl(method = "repeatedcv", repeats = 2, number = 10)

GbmTune_housePricing <- train(y = house_train[,1], x = house_train[,2:13], tuneGrid = gbm.grid,
                            method = "gbm",
                            trControl = cv.control_house)

gbmBestTune <- GbmTune_housePricing$bestTune
# n.trees interaction.depth shrinkage n.minobsinnode
# 180                 5      0.05              7

plot(GbmTune_housePricing)

### from the plot it is clear that the tree depths 2, 3, 4, and 5 perform fairly similar with 5 being the dominant one
### Shrinkage 0.050 is the dominant one to give minimum RMSE
### I believe the more the number of trees the more RMSE decrease. However when I closely look at the RMSE after 160 trees they all are converging.


########################################################################################################
####################################### USING eXtreme Gradient BOOSTing ################################
########################################################################################################

designMatXGB <- model.matrix(lm(housePrice~.,data=house_train))
designMatXGB <- designMatXGB[,-1]

# set up the cross-validated hyper-parameter search
xgb_grid <- expand.grid(eta = seq(from = 0.01, to = 0.2, length.out = 3),
                        max_depth = seq(from = 3, to = 10, length.out = 3),
                        colsample_bytree = seq (from = 0.5, to = 1, length.out = 3),
                        nrounds = seq(from = 100, to = 500, length.out = 3),
                        gamma = 0,  min_child_weight = 1,
                        subsample = seq(from = 0.5, to = 1, length.out = 3))

xgbTune_housePrice <- train(y = house_train[,1], x = designMatXGB,
                           method = "xgbTree",
                           trControl = trainControl(method="repeatedcv", repeats = 2, number = 10),
                           tuneGrid = xgb_grid)

xgbTune_housePrice <- train(y = house_train[,1], x = designMatXGB,
                            method = "xgbTree",
                            trControl = trainControl(method="repeatedcv", repeats = 2, number = 10))

xgbBestTune <- xgbTune_housePrice$bestTune

xgbresults <- xgbTune_housePrice$results

plot(xgbTune_housePrice)


########################################################################################################
####################################### USING Elastic Net ##############################################
########################################################################################################

designMatENET <- model.matrix(lm(housePrice~.,data=house_train))
designMatENET <- designMat[,-1]

Enet_grid <- expand.grid(alpha = seq(0,.5,length.out=15),
                               lambda = seq(10,500,length.out=15))

ENetTune_housePrice <- train(y = house_train[,1], x = designMatENET,
                           method = "glmnet",
                           tuneGrid = Enet_grid,
                           trControl = trainControl(method="repeatedcv", repeats = 2, number = 10))

ENetBestTune <- ENetTune_housePrice$bestTune
#         alpha lambda
# 30 0.03571429    500

ENetResults <- ENetTune_housePrice$results

plot(ENetTune_housePrice)


########################################################################################################
####################################### USING Principal Component Regression ###########################
########################################################################################################
# NOTE: PCR is based on principal component analysis (PCA)

designMatPCR <- model.matrix(lm(housePrice~.,data=house_train))
designMatPCR <- designMat[,-1]

pcrTune_housePrice <- train(y = house_train[,1], x = designMatPCR,
                            method = "pcr",
                            preProcess = c("center","scale"),
                            trControl = trainControl(method = "repeatedcv", repeats = 2, number = 10),
                            tuneLength = 120)

pcrBestTune <- pcrTune_housePrice$bestTune
# ncomp
# 83    83
pcrResults <- pcrTune_housePrice$results

plot(pcrTune_housePrice)

########################################################################################################
####################################### USING Partial Least Squares ####################################
########################################################################################################

designMatPLS <- model.matrix(lm(housePrice~.,data=house_train))
designMatPLS <- designMat[,-1]

plsTune_housePrice <- train(y = house_train[,1], x = designMatPLS,
                            method = "pls",
                            preProcess = c("center","scale"),
                            trControl = trainControl(method = "repeatedcv", repeats = 2, number = 10),
                            tuneLength = 120)

plsBestTune <- plsTune_housePrice$bestTune
# ncomp
# 22    22

plsResults <- plsTune_housePrice$results

plot(plsTune_housePrice)

########################################################################################################
######### Here I use the best tunes from above to predict the training set #############################
########################################################################################################

train_housePrice <- house_train$housePrice

### Using Neural Net best tune to predict the training data house price
NNET_housePrice <- predict(nnetTune_housePrice, house_train)
nnetPrediction <- cbind(train_housePrice, NNET_housePrice)%>%as.data.frame()

nnetPrediction <- nnetPrediction/1000000

NNET_prediction_plot <- ggplot(data = nnetPrediction, aes(x = train_housePrice,
                                                     y = NNET_housePrice)) + geom_jitter() + geom_smooth(method = loess) + 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="NNet Predicted House Price (in Million $)")


### Using k Nearest Neighbor best tune to predict the training data house price
KNN_housePrice <- predict(knnTune_housePrice,house_train)
knnPrediction <- cbind(train_housePrice,KNN_housePrice)%>%as.data.frame()
knnPrediction <- knnPrediction/1000000
KNN_prediction_plot <- ggplot(data = knnPrediction, aes(x = train_housePrice,
                                                         y = KNN_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="kNN Predicted House Price (in Million $)")


### Using multiple linear regression best tune to predict the training data house price
LM_housePrice <- predict(lmTune_housePrice,house_train)
lmPrediction <- cbind(train_housePrice,LM_housePrice) %>% as.data.frame()
lmPrediction <- lmPrediction/1000000
LM_prediction_plot <- ggplot(data = lmPrediction, aes(x = train_housePrice,
                                                        y = LM_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="MLR Predicted House Price (in Million $)")


### Using random forest best tune to predict the training data house price
RF_housePrice <- predict(forestTune_housePrice,designMatRF)
rfPrediction <- cbind(train_housePrice,RF_housePrice) %>% as.data.frame()
rfPrediction <- rfPrediction/1000000
RF_prediction_plot <- ggplot(data = rfPrediction, aes(x = train_housePrice,
                                                      y = RF_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="RF Predicted House Price (in Million $)")


### Using tree best tune to predict the training data house price
TREE_housePrice <- predict(TreeTune_housePricing,house_train)
treePrediction <- cbind(train_housePrice,TREE_housePrice) %>% as.data.frame()
treePrediction <- treePrediction/1000000
TREE_prediction_plot <- ggplot(data = treePrediction, aes(x = train_housePrice,
                                                      y = TREE_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="Decision Tree Predicted House Price (in Million $)")


### Using Gradient Boosting Machine best tune to predict the training data house price
GBM_housePrice <- predict(GbmTune_housePricing,house_train)
gbmPrediction <- cbind(train_housePrice,GBM_housePrice) %>% as.data.frame()
gbmPrediction <- gbmPrediction/1000000
GBM_prediction_plot <- ggplot(data = gbmPrediction, aes(x = train_housePrice,
                                                          y = GBM_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="GBM Predicted House Price (in Million $)")

### Using eXtreme Gradient Boosting Machine best tune to predict the training data house price
XGB_housePrice <- predict(xgbTune_housePrice,designMatXGB)
xgbPrediction <- cbind(train_housePrice,XGB_housePrice) %>% as.data.frame()
xgbPrediction <- xgbPrediction/1000000
XGB_prediction_plot <- ggplot(data = xgbPrediction, aes(x = train_housePrice,
                                                        y = XGB_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="XGB Predicted House Price (in Million $)")

### Using Elastic Net best tune to predict the training data house price
ENET_housePrice <- predict(ENetTune_housePrice,designMatENET)
enetPrediction <- cbind(train_housePrice,ENET_housePrice) %>% as.data.frame()
enetPrediction <- enetPrediction/1000000
ENET_prediction_plot <- ggplot(data = enetPrediction, aes(x = train_housePrice,
                                                        y = ENET_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="ENET Predicted House Price (in Million $)")

### Using Principal Component Regression best tune to predict the training data house price
PCR_housePrice <- predict(pcrTune_housePrice,designMatPCR)
pcrPrediction <- cbind(train_housePrice,PCR_housePrice) %>% as.data.frame()
pcrPrediction <- pcrPrediction/1000000
PCR_prediction_plot <- ggplot(data = pcrPrediction, aes(x = train_housePrice,
                                                          y = PCR_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="PCR Predicted House Price (in Million $)")

### Using Partial Least Squares best tune to predict the training data house price
PLS_housePrice <- predict(plsTune_housePrice,designMatPLS)
plsPrediction <- cbind(train_housePrice,PLS_housePrice) %>% as.data.frame()
plsPrediction <- plsPrediction/1000000
PLS_prediction_plot <- ggplot(data = plsPrediction, aes(x = train_housePrice,
                                                        y = PLS_housePrice)) + geom_jitter() + geom_smooth(method = loess)+ 
  scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="PLS Predicted House Price (in Million $)")


########################################################################################################
############################################# STACKING #################################################
########################################################################################################

### It is clear from the above plots eXtreme Gradiant Boosting (XGB), Random Forest, 
### Gradiant Boosting Method (GBM), and K Nearest Neighbours Perform well in predicting the house prices
### in that order. 

#### Now for better performance I am going to use ensamble methods to combine all the models to create 
#### a meta-model. Specifically I use Stacking to create a meta-model.

### First I get the best tune from the above mentioned models

xgbBestTune_meta <- xgbBestTune
forestBestTune_meta <- forestBestTune
gbmBestTune_meta <- gbm_BestTune
knnBestTune_meta <- knnBestTune

### I arrange all the best tune and the models for stacking
method_meta <- c("xgbTree", "rf", "gbm", "knn")
parametersTuned_meta <- list(xgbBestTune_meta, forestBestTune_meta, gbmBestTune_meta, knnBestTune_meta)


metaPred1<- matrix( NA, nrow = dim(house_test)[1], ncol = length(method_meta) + 2)
metaPred2<- matrix( NA, nrow = dim(house_train)[1], ncol = length(method_meta))

metaPred11<- matrix( NA, nrow = dim(house_train)[1], ncol = length(method_meta) + 2)
metaPred21<- matrix( NA, nrow = dim(house_train)[1], ncol = length(method_meta))

train <- house_train
test <- house_test

for (j in 1:length(method_meta)){
  
  # j <- 1
  
  # if(method_meta[j] %in% c("xgbTree", "rf")){
  #   
  #   designMat <- model.matrix(lm(house_train$housePrice~.,data=house_train))
  #   designMat <- designMat[,-1]
  #   
  # } else if (method_meta[j] %in% c("gbm", "knn")){
  #   
  #   designMat <- house_train[,-1]
  #   
  # }
  
  modelfit <- train(housePrice~.,
                    data  = house_train,
                    method = method_meta[j],
                    preProc = c("center","scale"),
                    trControl = trainControl(method="none"),
                    tuneGrid = parametersTuned_meta[[j]])
  
  
  metaPred1[,j] <- predict(modelfit, newdata = house_test)
  metaPred2[,j] <- predict(modelfit, newdata = house_train)
  
  metaPred11[,j] <- predict(modelfit, newdata = house_train)
  metaPred21[,j] <- predict(modelfit, newdata = house_train)
  
}


emfit1 <- lm(train$housePrice ~ metaPred2)

emfit2_RF <- train(y = train$housePrice,
               x = as.data.frame(metaPred2),
               tuneGrid = data.frame(mtry=1:50),
               method = "rf", ntree = 150,
               trControl = trainControl(method="oob"))

plot(emfit2_RF)
# emfit11 <- lm(train$housePrice ~ metaPred21)
# 
# emfit21 <- train(y = train$housePrice,
#                  x = as.data.frame(metaPred21),
#                 tuneGrid = data.frame(mtry=1:50),
#                 method = "rf", ntree = 150,
#                 trControl = trainControl(method="oob"))
# 
# plot(emfit21)

nnet.gridMeta <- expand.grid(size = seq(from = 1, to = 6, length.out = 6),
                         decay = seq(from = .3, to = .8, length.out = 6))

emfit2_kNN <- train(y = train$housePrice,
                 x = as.data.frame(metaPred21),
                 method = "knn",
                 preProcess = c("center","scale"),
                 tuneGrid = data.frame(.k=1:20),
                 trControl = trainControl(method = "repeatedcv", repeats = 3, number = 10))
plot(emfit2_kNN)
emfit2_kNN$bestTune


##### Testing
metaPred1[,5] <- cbind( matrix(1,nrow=dim(house_test)[1],ncol=1) , metaPred1[,-c(5,6)] ) %*% coef(emfit1)
metaPred1[,6] <- predict( emfit2_kNN, as.data.frame(metaPred1))
metaStack<- as.data.frame(metaPred1)
colnames(metaStack)[1:4]<- method_meta
colnames(metaStack)[5:6]<- c("enOLS","enKNN")
metaStack$enRF <- predict( emfit2_RF, as.data.frame(metaPred1))

### Training
metaPred11[,5] <- cbind( matrix(1,nrow=dim(house_train)[1],ncol=1) , metaPred11[,-c(5,6)] ) %*% coef(emfit1)
metaPred11[,6] <- predict( emfit2_kNN, as.data.frame(metaPred11))
metaStack1<- as.data.frame(metaPred11)
colnames(metaStack1)[1:4]<- method_meta
colnames(metaStack1)[5:6]<- c("enOLS","enKNN")
metaStack1$enRF <- predict( emfit2_RF, as.data.frame(metaPred11))

### Here I arrange the predicted price from stacking using random forest (note this contains the methods
### "xgbTree", "rf", "gbm", "knn", stacking of those methods with OLS and a stack of all methods and OLS)
### So it is safe to call this super stack
RF_housePriceStack <- metaStack1$enRF
RF_housePriceStack <- cbind(train_housePrice,RF_housePriceStack)%>%as.data.frame() 
RF_housePriceStack <- RF_housePriceStack/1000000
RF_prediction_plot_stack <- ggplot(data=RF_housePriceStack,aes(x=train_housePrice,
                                                               y=RF_housePriceStack))+
  geom_jitter()+geom_smooth(method = loess) + scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="Predicted House Price (in Million $)") + ggtitle("Train vs Random Forest Stacked Predicted House Price")

#### Here I arrange the predicted price from stacking using OLS
OLS_housePriceStack<- metaStack1$enOLS
OLS_housePriceStack <- cbind(train_housePrice,OLS_housePriceStack)%>%as.data.frame()
OLS_housePriceStack <- OLS_housePriceStack/1000000
OLS_prediction_plot_stack <- ggplot(data=OLS_housePriceStack,aes(x=train_housePrice,
                                                                 y=OLS_housePriceStack))+
  geom_jitter()+geom_smooth(method = loess) + scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="Predicted House Price (in Million $)") + ggtitle("Train vs OLS Stacked Predicted House Price")

### Here I arrange the predicted price from stacking using kNN (note this contains the methods
### "xgbTree", "rf", "gbm", "knn", stacking of those methods with OLS and a stack of all methods OLS and RF)
### So it is safe to call this super super stack
KNN_housePriceStack <- metaStack1$enKNN
KNN_housePriceStack <- cbind(train_housePrice,KNN_housePriceStack)%>%as.data.frame() 
KNN_housePriceStack <- KNN_housePriceStack/1000000
KNN_prediction_plot_stack <- ggplot(data=KNN_housePriceStack,aes(x=train_housePrice,
                                                               y=KNN_housePriceStack))+
  geom_jitter()+geom_smooth(method = loess) + scale_x_continuous(name = "Train House Price (in Million $)") + 
  scale_y_continuous(name="Predicted House Price (in Million $)") + ggtitle("Train vs KNN Stacked Predicted House Price")




names(metaStack) <- c("priceXGB", "priceRF", "priceGBM", 
                      "priceKNN", "priceEnOLS", "priceEnKNN", "priceEnRF")

testPredictedHousePrice <- cbind(metaStack, house_test)

testPredictedHousePriceXGB <- testPredictedHousePrice %>% select(-priceRF, - priceGBM, -priceKNN,
                                                                 -priceEnOLS, -priceEnKNN, -priceEnRF)

testPredictedHousePriceXGB <- testPredictedHousePrice %>% select(-priceRF, - priceGBM, -priceKNN,
                                                                 -priceEnOLS, -priceEnKNN, -priceEnRF)


write.csv(testPredictedHousePrice,file="testPredictedHousePrice.csv")







#### Here I randomly break the data into samples.
#### Note: This is to increase the predictive power (and also to confuse the models such that they just 
### don't look at similar patterns). In my opinion this is necessary. This procedure would make us trust 
### our meta model.

shuffle_meta <- sample(dim(house_train)[1])
folds_meta <- list()

for (i in 1:10){
  if (i != 10){
    folds_meta[[i]]<- shuffle_meta[(490*(i-1)+1):(490*i)]
  } 
  else {
    folds_meta[[i]]<- shuffle_meta[(490*(i-1)+1):dim(house_train)[1]]
    }
}

# Each element in folds_meta contains a randomly sampled data. Neat stuff
# For instance lets look at the first fold
house_train[folds_meta[[1]],] %>% head()
#         housePrice bedrooms bathrooms floors waterfront view condition grade yr_built
# 9264     925000        3      3.25      2          0    0         3     9     2002
# 6211     375000        2      1.50      2          0    0         3     8     2007
# 704      264000        3      2.50      2          0    0         3     8     2014
# 7399     318000        3      2.25      1          0    0         5     7     1982
# 3857     230000        3      2.00      1          0    0         3     7     1987
# 313      635000        4      1.75      1          0    0         4     8     1969
#       yr_renovated zipcode     lat     long sqft_living15 sqft_lot15
# 9264            0   98006 47.5506 -122.187          2640      14700
# 6211            0   98122 47.6028 -122.309          1470       1768
# 704             0   98198 47.3667 -122.307          1658       2700
# 7399            0   98178 47.4972 -122.264          1950       9642
# 3857            0   98038 47.3597 -122.051          1530       7362
# 313             0   98007 47.6196 -122.139          2120      12051


meta1<- list()
meta2<- list()

meta11<- list()
meta21<- list()


# for(i in 1:length(folds_meta)){
  
  train_meta <- house_train[folds_meta[[1]],]
  housePrice_meta <- train_meta$housePrice
  test_meta <- house_test
  
  meta1[[1]] <- matrix(NA, nrow = dim(test_meta)[1], ncol = length(method_meta) + 2)
  meta2[[1]] <- matrix(NA, nrow = dim(train_meta)[1], ncol = length(method_meta))
  
  meta11[[1]] <- matrix(NA, nrow = dim(train_meta)[1], ncol = length(method_meta) + 2)
  meta21[[1]] <- matrix(NA, nrow = dim(train_meta)[1], ncol = length(method_meta))
  
  for (j in 1:length(method_meta)){
    
    if(method_meta[j] %in% c("xgbTree", "rf")){
      designMat <- model.matrix(lm(housePrice_meta~.,data=train_meta))
      designMat <- designMat[,-1]
    } else if (method_meta[j] %in% c("gbm", "knn")){
      designMat <- train_meta[,-1]
    }
    
    modelfit <- train(y = housePrice_meta,
                     x = designMat,
                     method = method_meta[j],
                     preProc = c("center","scale"),
                     trControl = trainControl(method="none"),
                     tuneGrid = parametersTuned_meta[[j]])
    
    meta1[[1]][,j]<- predict(modelfit , newdata = test_meta)
    meta2[[1]][,j]<- predict(modelfit , newdata = train_meta)
    
    meta11[[1]][,j]<- predict(modelfit , newdata = train_meta)
    meta21[[1]][,j]<- predict(modelfit , newdata = train_meta)
    
  }
  
  
  
  
# }

  train<- house_train
  test<- house_test
  pred1<- matrix(NA,nrow=dim(test)[1],ncol=length(method_meta) + 2)
  pred2<- matrix(NA,nrow=dim(train)[1],ncol=length(method_meta))
  
  pred11<- matrix(NA,nrow=dim(train)[1],ncol=length(method_meta) + 2)
  pred21<- matrix(NA,nrow=dim(train)[1],ncol=length(method_meta))
  
  for (j in 1:length(method_meta)){
    
    modelfit <- train(housePrice~.,
                     data=train,
                     method=method_meta[1],
                     preProc=c("center","scale"),
                     trControl=trainControl(method="none"),
                     tuneGrid=parametersTuned_meta[[1]])
    
    pred1[,j]<- predict(modelfit,newdata=test)
    pred2[,j]<- predict(modelfit,newdata=train)
    
    pred11[,j]<- predict(modelfit,newdata=train)
    pred21[,j]<- predict(modelfit,newdata=train)
  }
  
  emfit1<- lm(train$housePrice~pred2)
  
  emfit2<- train(y=train$housePrice,
                 x=as.data.frame(pred2),
                 tuneGrid = data.frame(mtry=1:50),
                 method = "rf", ntree = 150,
                 trControl = trainControl(method="oob"))
  
  emfit11<- lm(train$housePrice~pred21)
  emfit21<- train(y=train$housePrice,
                  x=as.data.frame(pred21),
                  tuneGrid = data.frame(mtry=1:50),
                  method = "rf", ntree = 150,
                  trControl = trainControl(method="oob"))
  
  pred1[,4]<- cbind(matrix(1,nrow=dim(test)[1],ncol=1),pred1[,-c(5)])%*%coef(emfit1)
  pred1[,5]<- predict(emfit2,as.data.frame(pred1))
  
  
  pred11[,4]<- cbind(matrix(1,nrow=dim(train)[1],ncol=1),pred11[,-c(4,5)])%*%coef(emfit11)
  pred11[,5]<- predict(emfit21,as.data.frame(pred11))







