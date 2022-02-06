#zhaoxin test prediction

library(readr)
library(dplyr)
library(ggplot2)
library(caret)
train <- read_csv("Data/train.csv")


str(train)
train$grade<-as.numeric(train$grade)
train$condition<-as.numeric(train$condition)

#1:the relationship between price and sqft_live, clearly the sqft_living has big influence the price
train%>%
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_living)) %>% 
  
  ggplot(aes(x=sqft_living,y=price))+
  geom_point(color = "blue")+
  
  stat_smooth(aes(x=sqft_living,y=price),method="lm", color="red")+
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("(Sqft Living)")+
  ylab("Price")

#2:the relationship between price and bedrooms is not clear 

train%>%
  filter(!is.na(price)) %>% 
  filter(!is.na(bedrooms)) %>% 
  
  ggplot(aes(x=bedrooms,y=price))+
  geom_point(color = "orange")+
  
  stat_smooth(aes(x=bedrooms,y=price),method="lm", color="red")+
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("(Bedrooms)")+
  ylab("Price")

#3:the distribution of the price, it skews to the right.  

train%>%
  ggplot(aes(x = price)) +    
  geom_histogram(alpha = 0.8) +
  scale_x_continuous(limits=c(0,2e6)) +
  
  labs(x= 'Price',y = 'Count', title = paste("Distribution of", ' Price ')) +
  theme_bw()

#4:boxplt of the price. There are many outlier of the price in the data
boxplot(train$price,train$sqft_living,train$sqft_above)

#5:price seems has stronger correlation between soft_living, grade, and sqpt_above
install.packages("corrplot")
library(corrplot)
CorrelationResults = cor(train)
corrplot(CorrelationResults)


#6 the relashionship between grade and median price. The grade 13 and 12 have huge price gap compare with other grade
train %>%
  group_by(grade) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(grade = reorder(grade,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = grade,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = grade, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'grade', 
       y = 'Median Price', 
       title = 'grade and Median Price') +
  coord_flip() + 
  theme_bw()

# 7:the relationship between yearbuilt and median price and clearly the new houses have high price, but the difference is not very big
train %>%
  group_by(yr_built) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_built = reorder(yr_built,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  ggplot(aes(x = yr_built,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = yr_built, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'year built', 
       y = 'Median Price', 
       title = 'year built and Median Price') +
  coord_flip() + 
  theme_bw()

#8the relationship between year of renovated and median price
train %>%
  group_by(yr_renovated) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_renovated = reorder(yr_renovated,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  ggplot(aes(x = yr_renovated,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = yr_renovated, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'year renovated', 
       y = 'Median Price', 
       title = 'year renovated and Median Price') +
  coord_flip() + 
  theme_bw()

formula = price ~ .

fitControl <- trainControl(method="cv",number = 5)

houseprediction = train(formula, data = train,
                        method = "lm",trControl = fitControl,metric="RMSE")
importance = varImp(houseprediction)
importance

#
#> importance
#lm variable importance

#Overall
#lat           100.000
#grade          77.918
#yr_built       64.111
#waterfront     63.585
#sqft_living    62.999
#view           40.385
#bedrooms       32.527
#zipcode        29.900
#long           26.116
#bathrooms      22.684
#condition      22.477
#date           12.614
#sqft_above     11.735
#yr_renovated    9.768
#sqft_lot15      6.562
#floors          3.363
#sqft_lot        2.661
#sqft_living15   1.869
#property        0.000

#As you can see that first 5 or 6 variables are quite important, surprisingly the lat variable ranks first

#fearue selection
retrain<-train
retrain$property<-NULL
retrain$date<-NULL
#retrain$sqft_living<-NULL
#retrain$sqft_lot<-NULL
retrain$view<-NULL
retrain$sqft_above<-NULL
retrain$sqft_basement<-NULL
retrain$indirenovated<-ifelse(retrain$yr_renovated!=0,1,0)
retrain$recent_built<-ifelse(retrain$yr_built>2000,1,0)

retest<-test
retest$property<-NULL
retest$date<-NULL
#retest$sqft_living<-NULL
#retest$sqft_lot<-NULL
retest$view<-NULL
retest$sqft_above<-NULL
retest$sqft_basement<-NULL
retest$indirenovated<-ifelse(retest$yr_renovated!=0,1,0)
retest$recent_built<-ifelse(retest$yr_built>2000,1,0)

#set zipcode as factor
retrain$zipcode<-as.factor(retrain$zipcode)
retest$zipcode<-as.factor(retest$zipcode)

# Observations: 10,000
# Variables: 18
# $ price         <dbl> 342000, 550000, 335000, 482000, 463000, 1400000, 365000, 1200000, 243000, 285000, 258000, 275000, 750000, 626000, 613000, 790000, 38...
# $ bedrooms      <int> 3, 4, 2, 4, 3, 4, 3, 5, 3, 4, 3, 3, 3, 3, 4, 3, 4, 4, 4, 2, 3, 3, 3, 3, 3, 3, 3, 4, 3, 4, 4, 3, 2, 3, 3, 2, 3, 4, 3, 3, 3, 2, 4, 3, ...
# $ bathrooms     <dbl> 2.00, 2.50, 2.00, 2.50, 1.75, 2.50, 1.00, 2.75, 1.50, 2.50, 1.75, 1.50, 1.75, 2.25, 2.50, 2.50, 1.75, 2.50, 2.50, 1.00, 3.00, 2.00, ...
# $ sqft_living   <int> 1930, 1940, 1350, 2710, 1710, 2920, 1090, 2910, 1200, 2200, 1370, 1180, 2240, 1750, 2730, 2600, 1560, 2820, 3630, 800, 1850, 1960, 1...
# $ sqft_lot      <int> 11947, 10500, 2560, 35868, 7320, 4000, 6435, 9480, 9720, 9397, 5858, 10277, 10578, 1572, 12261, 4750, 8700, 8408, 42884, 4850, 19966...
# $ floors        <dbl> 1.0, 1.0, 1.0, 2.0, 1.0, 1.5, 1.0, 1.5, 1.0, 2.0, 1.0, 1.0, 2.0, 2.5, 2.0, 1.0, 1.0, 2.0, 1.5, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, 3....
# $ waterfront    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
# $ condition     <int> 4, 4, 3, 3, 3, 5, 4, 3, 4, 3, 3, 3, 5, 3, 3, 4, 4, 3, 3, 4, 4, 5, 4, 3, 3, 3, 3, 3, 3, 4, 3, 3, 3, 3, 3, 3, 5, 3, 3, 3, 3, 3, 4, 3, ...
# $ grade         <int> 8, 7, 8, 9, 7, 8, 7, 8, 7, 8, 7, 6, 8, 9, 9, 9, 7, 9, 9, 7, 7, 8, 7, 8, 7, 8, 8, 9, 8, 7, 9, 8, 7, 8, 7, 9, 8, 8, 10, 8, 7, 8, 9, 5,...
# $ yr_built      <int> 1966, 1976, 1976, 1989, 1948, 1909, 1955, 1939, 1965, 1987, 1987, 1983, 1923, 2005, 1991, 1951, 1967, 2014, 1979, 1944, 1992, 1957, ...
# $ yr_renovated  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1954, 0, 201...
# $ zipcode       <int> 98042, 98052, 98052, 98038, 98155, 98105, 98106, 98105, 98042, 98001, 98198, 98045, 98115, 98102, 98011, 98117, 98034, 98155, 98092,...
# $ lat           <dbl> 47.3672, 47.6830, 47.6344, 47.3750, 47.7512, 47.6578, 47.5334, 47.6552, 47.4225, 47.3406, 47.3815, 47.4880, 47.6954, 47.6498, 47.741...
# $ long          <dbl> -122.151, -122.114, -122.107, -122.022, -122.281, -122.280, -122.365, -122.278, -122.153, -122.269, -122.313, -121.787, -122.292, -1...
# $ sqft_living15 <int> 2200, 2200, 1790, 2780, 2260, 2470, 1340, 2940, 1380, 2310, 1400, 1680, 1570, 2410, 2730, 2380, 2080, 1300, 2830, 1150, 1410, 1960, ...
# $ sqft_lot15    <int> 12825, 10500, 2560, 36224, 8839, 4000, 6435, 6600, 10284, 9176, 7500, 11104, 10578, 3050, 10872, 4750, 8000, 8408, 80148, 4365, 6715...
# $ indirenovated <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, ...
# $ recent_built  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, ...

#prediction
#some test algorithm based on homework
library(caret)
set.seed(400)
ctrl<-trainControl(method="repeatedcv",repeats=10,number=10)
#i) kNN prediction 

knnfit<-train(price~.,data=train[,3:21],method="knn",trControl=ctrl,preProcess=c("center","scale"),tuneLength = 10)
knnfit

# k-Nearest Neighbors 
# 
# 10000 samples
# 18 predictor
# 
# Pre-processing: centered (18), scaled (18) 
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 9000, 8998, 9001, 9001, 9001, 9001, ... 
# Resampling results across tuning parameters:
#   
#   k   RMSE      Rsquared   MAE     
# 5  177704.5  0.7860559  91844.85
# 7  176913.9  0.7905110  90980.08
# 9  175890.4  0.7960958  90762.21
# 11  176800.9  0.7967430  90774.57
# 13  178279.5  0.7959939  91019.40
# 15  179680.9  0.7946487  91341.01
# 17  180710.9  0.7936593  91535.52
# 19  182237.9  0.7913652  91984.16
# 21  183402.8  0.7898527  92346.40
# 23  184790.7  0.7878639  92803.07
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was k = 9.

plot(knnfit)
knnpredict<-predict(knnfit,test)
knnpredict
write.csv(knnpredict,file="Prediction_knn.csv")

knnfit<-train(price~.,data=retrain,method="knn",trControl=ctrl,preProcess=c("center","scale"),tuneLength = 10)
knnfit

# k-Nearest Neighbors 
# 
# 10000 samples
# 17 predictor
# 
# Pre-processing: centered (17), scaled (17) 
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 8999, 9001, 8999, 9001, 8998, 9000, ... 
# Resampling results across tuning parameters:
#   
#   k   RMSE      Rsquared   MAE     
# 5  173157.5  0.7958009  89586.80
# 7  174067.6  0.7966994  88961.41
# 9  173805.5  0.7999910  88589.58
# 11  174704.7  0.7995085  88800.87
# 13  175820.9  0.7985977  89121.98
# 15  176968.3  0.7981009  89610.38
# 17  178044.5  0.7977559  89855.07
# 19  179273.8  0.7967662  90253.45
# 21  180721.3  0.7948668  90722.38
# 23  182148.2  0.7930236  91223.11
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was k = 5.

plot(knnfit)
knnpredict<-predict(knnfit,retest)
write.csv(knnpredict,file="Prediction_knn.csv")


#ols regression
model = lm(formula = price ~  bedrooms + bathrooms + floors + waterfront + view + condition +
             + sqft_basement + yr_renovated + zipcode + sqft_living15 + sqft_living + grade,
           data = train)
model = lm(formula = price ~  bathrooms + floors + waterfront + view + condition +
             + sqft_basement + yr_renovated + zipcode + sqft_living15 + sqft_living + grade,
           data = train)
y_pred = predict(model, newdata = test)
write.csv(y_pred,file="linear.csv")

#ii) elastic net prediction
install.packages("glmnet")
library(glmnet)
cv.control <- trainControl(method = "repeatedcv", repeats = 100,number = 10)

Enet.grid <- expand.grid(alpha = seq(0,.0001,length.out=5),
                         lambda=seq(11000,15000,length.out=5))

ENetTune<-train(y=train$price,x=train[,2:18],
                method="glmnet",
                preProcess=c("center","scale"),
                tuneGrid = Enet.grid,
                trControl=cv.control)

ENetTune

ENetTune<-train(y=retrain$price,x=retrain[,2:18],
                method="glmnet",
                preProcess=c("center","scale"),
                tuneGrid = Enet.grid,
                trControl=cv.control)

ENetTune

# glmnet 
# 
# 10000 samples
# 17 predictor
# 
# Pre-processing: centered (17), scaled (17) 
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 8999, 9000, 9001, 8999, 9000, 9001, ... 
# Resampling results across tuning parameters:
#   
#   alpha    lambda  RMSE      Rsquared   MAE     
# 0.0e+00  11000   209484.8  0.6955934  126228.0
# 0.0e+00  12000   209484.8  0.6955934  126228.0
# 0.0e+00  13000   209484.8  0.6955934  126228.0
# 0.0e+00  14000   209484.8  0.6955934  126228.0
# 0.0e+00  15000   209484.8  0.6955934  126228.0
# 2.5e-05  11000   209484.9  0.6955933  126227.6
# 2.5e-05  12000   209484.9  0.6955933  126227.6
# 2.5e-05  13000   209484.9  0.6955933  126227.6
# 2.5e-05  14000   209484.9  0.6955933  126227.6
# 2.5e-05  15000   209484.9  0.6955933  126227.6
# 5.0e-05  11000   209484.4  0.6955947  126226.2
# 5.0e-05  12000   209484.4  0.6955947  126226.2
# 5.0e-05  13000   209484.4  0.6955947  126226.2
# 5.0e-05  14000   209484.4  0.6955947  126226.2
# 5.0e-05  15000   209484.4  0.6955947  126226.2
# 7.5e-05  11000   209482.7  0.6956001  126223.1
# 7.5e-05  12000   209482.7  0.6956001  126223.1
# 7.5e-05  13000   209482.7  0.6956001  126223.1
# 7.5e-05  14000   209482.7  0.6956001  126223.1
# 7.5e-05  15000   209482.7  0.6956001  126223.1
# 1.0e-04  11000   209483.6  0.6955991  126220.4
# 1.0e-04  12000   209483.6  0.6955991  126220.4
# 1.0e-04  13000   209483.6  0.6955991  126220.4
# 1.0e-04  14000   209483.6  0.6955991  126220.4
# 1.0e-04  15000   209483.6  0.6955991  126220.4
# 
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were alpha = 7.5e-05 and lambda = 15000.

plot(ENetTune)
Enet_predict<-predict(ENetTune,test)
write.csv(Enet_predict,file="enet.csv")


# #VI)OLS
# 
# ols<-lm(train$price~.,data=train)
# summary(ols)
# ols_pred<-predict(ols,train)
# ols_pred
# 
# prediction<-data.frame(knnpredict,ENetTunetest,pcrTunetest,plsTunetest,MARSTunetest,ols_pred,train[,3])
# colnames(prediction)<-c("knn","enet","pcr","pls","mars","ols","price")
# pairs(~knn+enet+pcr+pls+mars+ols+price,data=prediction)
# round(cor(prediction),2)
# 
#VII random forest

# ClassRfTune<-train(y=retrain$price,
#                    x=retrain[,2:18],
#                    method="rf",
#                    tuneGrid=data.frame(mtry=seq(1,9,1)),
#                    control=trainControl(method="oob"),
#                    ntree=500)
# plot(ClassRfTune)
# ClassRfTune

#XGBoost

#add pca variables suggested by kaggle 
PCAData <-retrain[,13:14]

pca = prcomp(PCAData, scale. = T)

HouseData_pca <- predict(pca, newdata = PCAData)

HouseData_pca = as.data.frame(HouseData_pca)

retrain2 = cbind(retrain,HouseData_pca)

PCAData <-retest[,13:14]

pca = prcomp(PCAData, scale. = T)

HouseData_pca <- predict(pca, newdata = PCAData)

HouseData_pca = as.data.frame(HouseData_pca)

retest2 = cbind(retest,HouseData_pca)

xgbGrid <- expand.grid(nrounds = 1000,
                       max_depth = 4,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .5,
                       min_child_weight = 1,
                       subsample = 1)

fitControl <- trainControl(method="repeatedcv",repeats=10,number = 10)

XGB <- train(price ~., data = retrain2,
             method = "xgbTree",trControl = fitControl,
             tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")
XGB

# eXtreme Gradient Boosting 
# 
# 10000 samples
# 17 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 9000, 9000, 9001, 8999, 9001, 9001, ... 
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 128900.9  0.8843969  69290.12
# 
# Tuning parameter 'nrounds' was held constant at a value of 2000
# Tuning parameter 'max_depth' was held constant at a value of 4
# Tuning parameter 'eta' was
# held constant at a value of 0.05
# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'colsample_bytree' was held constant at a
# value of 0.5
# Tuning parameter 'min_child_weight' was held constant at a value of 1
# Tuning parameter 'subsample' was held constant at a value of 1

XGBoost<-predict(XGB,retest2)
write.csv(XGBoost,file="XGBoost.csv")







