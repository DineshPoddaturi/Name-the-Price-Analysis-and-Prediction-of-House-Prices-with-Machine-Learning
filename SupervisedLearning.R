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
                    "gbm", "earth", "Boruta", "ggcorrplot")
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
# boruta_fit <- Boruta(price~. , data = train_data , doTrace = 2)

# print(boruta_fit)
# Boruta performed 68 iterations in 15.85281 mins.
# 18 attributes confirmed important: bathrooms,
# bedrooms, condition, floors, grade and 13 more;
# No attributes deemed unimportant.
# plot(boruta_fit)

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
knnTune_housePrice$bestTune
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

nnetTune_housePrice$bestTune

nnetTune_housePrice$results


########################################################################################################
####################################### USING MULTIPLE LINEAR REGRESSION ###############################
########################################################################################################

lmTune_housePrice <- train(housePrice~., data = house_train,
                           method = "lm",
                           trControl = trainControl(method = "repeatedcv",
                                                  repeats = 2, number = 10))

lmTune_housePrice$results



########################################################################################################
####################################### USING RANDOM FOREST ############################################
########################################################################################################
designMat <- model.matrix(lm(housePrice~.,data=house_train))
designMat <- designMat[,-1]

forestTune_housePrice <- train(y = house_train[,1], x = designMat,
                             tuneGrid = data.frame(mtry=1:100),
                             method = "rf", ntree = 500,
                             trControl = trainControl(method="oob"))

########################################################################################################
####################################### USING TREE #####################################################
########################################################################################################

TreeTune_housePricing <- train(y = house_train[,1], x = house_train[,2:13],
                             method = "rpart", tuneGrid = data.frame(cp = seq(from = .0001, to = .1, length.out = 50)),
                             trControl = trainControl(method = "repeatedcv",repeats = 2, number = 10))




########################################################################################################
####################################### USING Gradient Boosting Machine ################################
########################################################################################################
gbm.grid <- expand.grid(n.trees = seq(from = 120, to = 160, length.out = 3),
                        interaction.depth = seq(1,5),
                        shrinkage = seq(from = .05, to = 0.2, length.out = 3),
                        n.minobsinnode = seq(from = 7, to = 12, length.out = 3))

cv.control_house <- trainControl(method = "repeatedcv", repeats = 2, number = 10)

GbmTune_housePricing<-train(y = house_train[,1], x = house_train[,2:13], tuneGrid = gbm.grid,
                            method = "gbm",
                            trControl = cv.control_house)

########################################################################################################
####################################### USING eXtreme Gradient BOOSTing ################################
########################################################################################################

xgbTune_housePrice<- train(y = house_train[,1], x = house_train[,2:13],
                           method = "xgbTree",
                           trControl = trainControl(method="repeatedcv", repeats = 2, number = 10))

xgbTune_housePrice$bestTune

xgbTune_housePrice$results



