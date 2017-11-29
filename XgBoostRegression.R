install.packages("gbm")
library(gbm)
salesData<-read.csv("Train_Data.csv")
salesData[is.na(salesData$Item_Weight),]$Item_Weight<-0
for(i in 1:nrow(salesData)) {
if(salesData[i,"Item_Weight"] == 0) {
if(sum(unique(subset(salesData,Item_Identifier == salesData[i,"Item_Identifier"] & Item_Weight != 0)$Item_Weight)) != 0) {
salesData[i,"Item_Weight"] <- sum(unique(subset(salesData,Item_Identifier == salesData[i,"Item_Identifier"] & Item_Weight != 0)$Item_Weight))
}
}
}
for(i in 1:nrow(salesData)) {
if(salesData[i,"Item_Weight"] == 0) {
salesData[i,"Item_Weight"]<-mean(salesData[which(salesData$Item_Fat_Content == salesData[i,"Item_Fat_Content"] & salesData$Item_Weight != 0),]$Item_Weight)
}
}
for(i in 1:nrow(salesData)) {
if(salesData[i,"Outlet_Size"] == "") {
if(length(unique(subset(salesData,Outlet_Location_Type == salesData[i,"Outlet_Location_Type"] & Outlet_Size != "" & Outlet_Type == salesData[i,"Outlet_Type"])$Outlet_Size)) == 1) {
salesData[i,"Outlet_Size"] <- unique(subset(salesData,Outlet_Location_Type == salesData[i,"Outlet_Location_Type"] & Outlet_Size != "" & Outlet_Type == salesData[i,"Outlet_Type"])$Outlet_Size)
}
else if(length(unique(subset(salesData,Outlet_Size != "" & Outlet_Type == salesData[i,"Outlet_Type"])$Outlet_Size)) == 1) {
salesData[i,"Outlet_Size"] <- unique(subset(salesData,Outlet_Size != "" & Outlet_Type == salesData[i,"Outlet_Type"])$Outlet_Size)
}
}
}
salesData$Item_Weight<-log(1+salesData$Item_Weight)
salesData$Item_Visibility<-log(1+salesData$Item_Visibility)
salesData$Item_MRP<-log(1+salesData$Item_MRP)
salesData$Item_Outlet_Sales<-log(1+salesData$Item_Outlet_Sales)
salesData[which(salesData$Item_Fat_Content == "reg"),]$Item_Fat_Content<-"Regular"
salesData[which(salesData$Item_Fat_Content == "LF"),]$Item_Fat_Content<-"Low Fat"
salesData[which(salesData$Item_Fat_Content == "low fat"),]$Item_Fat_Content<-"Low Fat"
salesData$Outlet_Establishment_Year<-as.factor(salesData$Outlet_Establishment_Year)
gbmMod<-gbm(Item_Outlet_Sales~.,data = salesData,n.trees = 100, interaction.depth = 1, shrinkage = 0.01,n.minobsinnode = 10)
install.packages("xgboost")
library(xgboost)
View(salesData)
xgbMod<-xgboost(data = salesData[,-12], label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 4)
xgbMod<-xgboost(data = data.matrix(salesData[,-12]), label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 4)
xgbMod<-xgboost(data = data.matrix(salesData[,-12]), label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 4, nrounds = 25)
xgbMod<-xgboost(data = data.matrix(salesData[,-12]), label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 4, nrounds = 70)
xgbMod<-xgboost(data = data.matrix(salesData[,-12]), label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 4, nrounds = 30)
saleTest<-read.csv("Test_Data.csv")
saleTest[is.na(saleTest$Item_Weight),]$Item_Weight<-0
for(i in 1:nrow(saleTest)) {
if(saleTest[i,"Item_Weight"] == 0) {
if(sum(unique(subset(saleTest,Item_Identifier == saleTest[i,"Item_Identifier"] & Item_Weight != 0)$Item_Weight)) != 0) {
saleTest[i,"Item_Weight"] <- sum(unique(subset(saleTest,Item_Identifier == saleTest[i,"Item_Identifier"] & Item_Weight != 0)$Item_Weight))
}
}
}
for(i in 1:nrow(saleTest)) {
if(saleTest[i,"Item_Weight"] == 0) {
saleTest[i,"Item_Weight"]<-mean(saleTest[which(saleTest$Item_Fat_Content == saleTest[i,"Item_Fat_Content"] & saleTest$Item_Weight != 0),]$Item_Weight)
}
}
for(i in 1:nrow(saleTest)) {
if(saleTest[i,"Outlet_Size"] == "") {
if(length(unique(subset(saleTest,Outlet_Location_Type == saleTest[i,"Outlet_Location_Type"] & Outlet_Size != "" & Outlet_Type == saleTest[i,"Outlet_Type"])$Outlet_Size)) == 1) {
saleTest[i,"Outlet_Size"] <- unique(subset(saleTest,Outlet_Location_Type == saleTest[i,"Outlet_Location_Type"] & Outlet_Size != "" & Outlet_Type == saleTest[i,"Outlet_Type"])$Outlet_Size)
}
else if(length(unique(subset(saleTest,Outlet_Size != "" & Outlet_Type == saleTest[i,"Outlet_Type"])$Outlet_Size)) == 1) {
saleTest[i,"Outlet_Size"] <- unique(subset(saleTest,Outlet_Size != "" & Outlet_Type == saleTest[i,"Outlet_Type"])$Outlet_Size)
}
}
}
saleTest$Item_Weight<-log(1+saleTest$Item_Weight)
saleTest$Item_Visibility<-log(1+saleTest$Item_Visibility)
saleTest$Item_MRP<-log(1+saleTest$Item_MRP)
saleTest[which(saleTest$Item_Fat_Content == "reg"),]$Item_Fat_Content<-"Regular"
saleTest[which(saleTest$Item_Fat_Content == "LF"),]$Item_Fat_Content<-"Low Fat"
saleTest[which(saleTest$Item_Fat_Content == "low fat"),]$Item_Fat_Content<-"Low Fat"
saleTest$Outlet_Establishment_Year<-as.factor(saleTest$Outlet_Establishment_Year)
salePred<-predict(xgbMod,data.matrix(saleTest))
View(salePred)
saleTestPred<-saleTest
saleTestPred$salsPred<-predict(xgbMod,data.matrix(saleTest))
View(saleTestPred)
saleTestPred$actSales<-exp(saleTestPred$salsPred)
write.csv(saleTestPred)
write.csv(saleTestPred,"PredTest.csv")
xgbMod<-xgboost(data = data.matrix(salesData[,-12]), label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 4, nrounds = 100)
salePred<-predict(xgbMod,data.matrix(saleTest))
saleTestPred<-saleTest
saleTestPred$salsPred<-predict(xgbMod,data.matrix(saleTest))
saleTestPred$actSales<-exp(saleTestPred$salsPred)
write.csv(saleTestPred,"PredTest.csv")
params<-list(booster = "gbtree", objective = "reg:linear", eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1, subsample = 1, colsample_bytree = 1)
xgbcv<-xgb.cv(params, data = salesData, nrounds = 100, nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stopping_rounds = 10)
xgbcv<-xgb.cv(params, data = data.matrix(salesData), nrounds = 100, nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stopping_rounds = 10, label = salesData$Item_Outlet_Sales)
xgbcv<-xgb.cv(params, data = data.matrix(salesData), nrounds = 200, nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stopping_rounds = 10, label = salesData$Item_Outlet_Sales)
min(xgbcv$test.error.mean)
xgbcv$best_iteration
params<-list(booster = "gbtree", objective = "reg:linear", eta = 0.1, gamma = 0, max_depth = 6, min_child_weight = 1, subsample = 1, colsample_bytree = 1)
xgbcv<-xgb.cv(params, data = data.matrix(salesData), nrounds = 200, nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stopping_rounds = 10, label = salesData$Item_Outlet_Sales)
xgbcv<-xgb.cv(params, data = data.matrix(salesData), nrounds = 300, nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stopping_rounds = 10, label = salesData$Item_Outlet_Sales)
xgbcv<-xgb.cv(params, data = data.matrix(salesData), nrounds = 500, nfold = 5, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stopping_rounds = 10, label = salesData$Item_Outlet_Sales)
xgbMod<-xgboost(data = data.matrix(salesData[,-12]), label = salesData$Item_Outlet_Sales, eta = 0.1, max_depth = 6, nrounds = 200)
saleTestPred$salsPred<-predict(xgbMod,data.matrix(saleTest))
saleTestPred$actSales<-exp(saleTestPred$salsPred)
write.csv(saleTestPred,"PredTest2.csv")
q()
