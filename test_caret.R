install.packages("pmml")
install.packages("readr")
install.packages("dplyr")
install.packages("summarytools")
install.packages("RANN")
install.packages("gower")
install.packages("ModelMetrics")
install.packages("hardhat")
install.packages("tidyverse")
install.packages("skimr")
install.packages("doParallel")
install.packages("caret", dependencies = c("Depends", "Suggests"))


library(caret)
library(readr)
library(skimr)
library(summarytools)
library(tidyverse)

url = "https://www.salesanalytics.co.jp/wpv6"
df_diamond = read_csv(url) 
skim(df_diamond)
dfSummary(df_diamond) %>% view()

set.seed(100)
trainNum <- createDataPartition(df_diamond$price,p=0.8,list=FALSE)

df_train <- df_diamond[trainNum,]
y_train <- df_train[,1]
x_train <- df_train[,-1]

df_test <- df_diamond[-trainNum,]
y_test <- df_test[,1]
x_test <- df_test[,-1]

help("preProcess")
#欠測値の補完（k-近傍法）
missingdata_model <- preProcess(as.data.frame(x_train),method='knnImpute')
x_train <- predict(missingdata_model,newdata = x_train)
x_test <- predict(missingdata_model,newdata = x_test)

#変数の正規化
range_model <- preProcess(x_train, method = c("center","scale"))
x_train <- predict(range_model, newdata = x_train)
x_test <- predict(range_model, newdata = x_test)

#ダミー変数化
dummies_model <- dummyVars(~.,data = x_train, fullRank=TRUE)
x_train <- data.frame(predict(dummies_model, newdata = x_train))
x_test <- data.frame(predict(dummies_model, newdata = x_test))
#test2
skim(x_train)
skim(x_test)
