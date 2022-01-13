library(readr)
ks_gbm_predict <- read_csv("~/Dropbox/Spring 2018/Stat502/stat502_project/Xiyuan/ks_gbm_predict.csv")

names(ks_gbm_predict) <- c('Id', 'price')
write_csv(ks_gbm_predict, "new_ks_gbm_predict.csv")

ks_knn_predict <- read_csv("~/Dropbox/Spring 2018/Stat502/stat502_project/Xiyuan/ks_knn_predict.csv")
names(ks_knn_predict) <- c('Id', 'price')
write_csv(ks_knn_predict, "new_ks_knn_predict.csv")
