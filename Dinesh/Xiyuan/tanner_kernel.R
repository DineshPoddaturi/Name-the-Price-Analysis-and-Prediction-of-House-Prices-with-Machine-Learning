#Kaggle kernel by Tanner Carbonati (2017)
#provides a number of ideas for construction of such features

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

setwd("~/Dropbox/Spring 2018/Stat502/stat502_project/Xiyuan")

train=read.csv("~/Dropbox/Spring 2018/Stat502/stat502_project/Data/train.csv")
test = read.csv("~/Dropbox/Spring 2018/Stat502/stat502_project/Data/test.csv")

#na.cols <- which(colSums(is.na(train)) > 0) No NAs

corr.df <- cbind(train, train['price'])
correlations <- cor(corr.df)
corr.SalePrice <- as.matrix(sort(correlations[,'price'], decreasing = TRUE))
corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', 
         addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

require(GGally)
lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 20, alpha = 0.7, color = 'darkseagreen') +
    geom_smooth(method=loess, fill="red", color="red") +
    geom_smooth(method=lm, fill="blue", color="blue") +
    theme_minimal()
  return(plt)
}

plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(df.combined[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}

train['Has2ndFlr'] <- (train$floors>1)*1



#create yr_sold
train['yr_sold'] = as.numeric(substr(train$date, 1, 4))
train['NewHouse'] <- (train$yr_built == train$yr_sold) * 1

plot(x=train$NewHouse, y=train$price)







