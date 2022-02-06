setwd("/home/xiyuansu/stat502_project/Xiyuan")

require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot) # correlation plot
require(Boruta)



train=read.csv("/home/xiyuansu/stat502_project/Data/train.csv")
test = read.csv("/home/xiyuansu/stat502_project/Data/test.csv")

dim(train) #10000    21
dim(test) #11613    21


str(train)


#random forest
# fit.rf <- randomForest(price~., data=train, importance=TRUE)
# imp.rf <- varImp(fit.rf)


# Overall
# property        6.4670191
# date            0.9065502
# bedrooms        6.0421826
# bathrooms       6.5100015
# sqft_living    29.0671704
# sqft_lot       18.2804637
# floors          8.8741531
# waterfront     23.0129720
# view           10.0188907
# condition      10.9109650
# grade          33.2313801
# sqft_above     18.2991382
# sqft_basement   7.0748873
# yr_built       27.3150073
# yr_renovated    0.4992177
# zipcode        30.5908333
# lat           118.4975169
# long           47.6380478
# sqft_living15  27.3023131
# sqft_lot15     18.6771284




#linear model
# fit.lm<-lm(price~., data=train)
# imp.lm <- varImp(fit.lm)

#boosted tree
# X <- subset(train, select=-price)
# fit.bst <- xgboost(data.matrix(X), train$price, nrounds=200)
# imp.bst <- xgb.importance(model = fit.bst, feature_names = names(X))
# 
# fit.bt <- Boruta(price~. , data=train)
# print(fit.bt)
# Boruta performed 83 iterations in 34.55251 mins.
# 19 attributes confirmed important: bathrooms, bedrooms, condition, floors, grade and 14 more;
# 1 attributes confirmed unimportant: date;
# plot(fit.bt)

# property          date      bedrooms     bathrooms   sqft_living      sqft_lot 
# Confirmed      Rejected     Confirmed     Confirmed     Confirmed     Confirmed 
# floors    waterfront          view     condition         grade    sqft_above 
# Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed 
# sqft_basement      yr_built  yr_renovated       zipcode           lat          long 
# Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed 
# sqft_living15    sqft_lot15 
# Confirmed     Confirmed 
# Levels: Tentative Confirmed Rejected

#sort by importance

# rf_varimpt <- as.matrix(rf_varimpt)
# 
# rf_varimpt <- rf_varimpt[order(rf_varimpt[,2]), ]
# 
# xtable(rf_varimpt)
# 
# 
# lm_varimpt <- as.matrix(lm_varimpt)
# lm_varimpt <- lm_varimpt[order(lm_varimpt[,2]), ]
# 
# 
# xtable(lm_varimpt)
# 
# xtable(boruta_decision)

# Look at variables not rejected by Boruta.
# keep <- names(fit.bt$finalDecision[fit.bt$finalDecision != 'Rejected'])
# length(keep)

# Deal with factor indicators -- take max rank over factor levels.
# lmranks <- rank(-imp.lm$Overall)
# lmnames <- rownames(imp.lm)
# maxrank <- sapply(keep, function(i) {
#   hits <- sapply(lmnames, function(j) grepl(i, j))
#   if (sum(hits) == 0)
#     return(NA)
#   if (sum(hits) == 1) {
#     sel <- (lmnames == i)
#     if (sum(sel) == 0)
#       return(NA)
#     return(lmranks[sel])
#   }
#   levs <- lmnames[hits]
#   idx <- lmnames %in% levs
#   res <- max(lmranks[idx])
#   return(res)
# })
# 
# ranks <- c(scale(unlist(maxrank)), 
#            scale(rank(-imp.rf$Overall)), 
#            scale(rank(-imp.bst$Gain)))
# vars <- c(names(maxrank), rownames(imp.rf), imp.bst$Feature)
# method <- rep(c('LM', 'RanForest', 'xgB'),  c(length(maxrank), nrow(imp.rf), nrow(imp.bst)))
# pd <- data.frame(normalizedRank = ranks, vars, method)

# normalizedRank          vars    method
# 1      1.41409135      property        LM
# 2     -0.60871482      bedrooms        LM
# 3     -0.10301328     bathrooms        LM
# 4      1.24552417   sqft_living        LM
# 5      1.07695699      sqft_lot        LM
# 6      0.90838981        floors        LM
# 7     -1.11441637    waterfront        LM
# 8     -0.77728200          view        LM
# 9      0.06555390     condition        LM
# 10    -1.45155073         grade        LM
# 11     0.40268827    sqft_above        LM
# 12             NA sqft_basement        LM
# 13    -1.28298355      yr_built        LM
# 14     0.57125545  yr_renovated        LM
# 15    -0.44014764       zipcode        LM
# 16    -1.62011791           lat        LM
# 17    -0.27158046          long        LM
# 18     1.24552417 sqft_living15        LM
# 19     0.73982263    sqft_lot15        LM
# 20     1.09870053      property RanForest
# 21     1.43676223          date RanForest
# 22     1.26773138      bedrooms RanForest
# 23     0.92966968     bathrooms RanForest
# 24    -0.92966968   sqft_living RanForest
# 25     0.08451543      sqft_lot RanForest
# 26     0.59160798        floors RanForest
# 27    -0.42257713    waterfront RanForest
# 28     0.42257713          view RanForest
# 29     0.25354628     condition RanForest
# 30    -1.26773138         grade RanForest
# 31    -0.08451543    sqft_above RanForest
# 32     0.76063883 sqft_basement RanForest
# 33    -0.76063883      yr_built RanForest
# 34     1.60579308  yr_renovated RanForest
# 35    -1.09870053       zipcode RanForest
# 36    -1.60579308           lat RanForest
# 37    -1.43676223          long RanForest
# 38    -0.59160798 sqft_living15 RanForest
# 39    -0.25354628    sqft_lot15 RanForest
# 40    -1.60579308   sqft_living       xgB
# 41    -1.43676223         grade       xgB
# 42    -1.26773138           lat       xgB
# 43    -1.09870053          long       xgB
# 44    -0.92966968 sqft_living15       xgB
# 45    -0.76063883    waterfront       xgB
# 46    -0.59160798      yr_built       xgB
# 47    -0.42257713          view       xgB
# 48    -0.25354628    sqft_above       xgB
# 49    -0.08451543       zipcode       xgB
# 50     0.08451543      property       xgB
# 51     0.25354628      sqft_lot       xgB
# 52     0.42257713          date       xgB
# 53     0.59160798    sqft_lot15       xgB
# 54     0.76063883     condition       xgB
# 55     0.92966968     bathrooms       xgB
# 56     1.09870053 sqft_basement       xgB
# 57     1.26773138      bedrooms       xgB
# 58     1.43676223  yr_renovated       xgB
# 59     1.60579308        floors       xgB



# ggplot(subset(pd, vars %in% keep), aes(method, vars)) +
#   geom_tile(aes(fill = normalizedRank), colour = "white") +
#   scale_fill_distiller(palette = 'Spectral') +
#   theme_bw()


#################################################################################################
#pre-process training data
#################################################################################################

#drop property, date, sqrt_living, view, zipcode, long
ks_train <- train[,c(-1, -2,-10,-13, -14)]

#add two more indicator variables: Recently built (>2000) and recently renovated(>1990)
ks_train$zipcode <- as.factor(ks_train$zipcode)

for (i in 1:dim(ks_train)[1]){
  if (ks_train$yr_built[i]>2000) {ks_train$rec_built[i]=1}
  else {ks_train$rec_built[i]=0}
}


for (i in 1:dim(ks_train)[1]){
  if (ks_train$yr_renovated[i]>1990) {ks_train$rec_renovated[i]=1}
  else {ks_train$rec_renovated[i]=0}
  
}


ks_train %>%glimpse()


##ks_test

ks_test <- test[,c(-1, -2,-3, -10,-13, -14)]

ks_test$zipcode <- as.factor(ks_test$zipcode)

for (i in 1:dim(ks_test)[1]){
  if (ks_test$yr_built[i]>2000) {ks_test$rec_built[i]=1}
  else {ks_test$rec_built[i]=0}
}

for (i in 1:dim(ks_train)[1]){
  if (ks_test$yr_renovated[i]>1990) {ks_test$rec_renovated[i]=1}
  else {ks_test$rec_renovated[i]=0}
  
}


ks_test %>%glimpse()

#export the customized training data and test data

write.csv(ks_train, "ks_train.csv", row.names=FALSE)

write.csv(ks_test, "ks_test.csv", row.names=FALSE)







