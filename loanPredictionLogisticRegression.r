loandata<-read.csv("loanData.csv")
str(loandata)

loandata2<-loandata

chisq.test(table(loandata2$CREDIT,loandata2$CHECK_A))

loandata2$DURATION<- cut(loandata2$DURATION, seq(from = min(loandata2$DURATION),to = max(loandata2$DURATION), by = (max(loandata2$DURATION - min(loandata2$DURATION)))/5), include.lowest = TRUE)

chisq.test(table(loandata2$CREDIT,loandata2$DURATION))
chisq.test(table(loandata2$CREDIT,loandata2$C_HIST))
chisq.test(table(loandata2$CREDIT,loandata2$PURPOSE))

loandata2$AMOUNT<- cut(loandata2$AMOUNT, seq(from = min(loandata2$AMOUNT),to = max(loandata2$AMOUNT), by = (max(loandata2$AMOUNT - min(loandata2$AMOUNT)))/5), include.lowest = TRUE)

chisq.test(table(loandata2$CREDIT,loandata2$SAVE_A))
chisq.test(table(loandata2$CREDIT,loandata2$EMPLOY))

loandata2$INSTALL_R<- cut(loandata2$INSTALL_R, seq(from = min(loandata2$INSTALL_R),to = max(loandata2$INSTALL_R), by = (max(loandata2$INSTALL_R - min(loandata2$INSTALL_R)))/4), include.lowest = TRUE)

chisq.test(table(loandata2$CREDIT,loandata2$INSTALL_R))
chisq.test(table(loandata2$CREDIT,loandata2$PERSONAL))
chisq.test(table(loandata2$CREDIT,loandata2$GUARANTEE))
chisq.test(table(loandata2$CREDIT,loandata2$RESIDENCE))
chisq.test(table(loandata2$CREDIT,loandata2$PROPERTY))

loandata2$AGE<- cut(loandata2$AGE, seq(from = min(loandata2$AGE),to = max(loandata2$AGE), by = (max(loandata2$AGE - min(loandata2$AGE)))/4), include.lowest = TRUE)

chisq.test(table(loandata2$CREDIT,loandata2$AGE))
chisq.test(table(loandata2$CREDIT,loandata2$INSTALL_P))
chisq.test(table(loandata2$CREDIT,loandata2$HOUSING))
chisq.test(table(loandata2$CREDIT,loandata2$N_EXIST))
chisq.test(table(loandata2$CREDIT,loandata2$JOB))
chisq.test(table(loandata2$CREDIT,loandata2$N_PEOPLE))
chisq.test(table(loandata2$CREDIT,loandata2$TEL))
chisq.test(table(loandata2$CREDIT,loandata2$FOREIGN))

#Drop the variables
loandata3<-loandata[,!names(loandata) %in% c("INSTALL_R","RESIDENCE","N_EXIST","JOB","N_PEOPLE","TEL")]

loandata3$CREDIT<-as.factor(loandata3$CREDIT)

devIndex<-sample.int(nrow(loandata3),size = floor(0.7*nrow(loandata3)), replace = FALSE)

loanTrain<-loandata3[devIndex,]
loanTest<-loandata3[-devIndex,]

logMod<-glm(CREDIT~.,data = loanTrain, family = binomial)
summary(logMod)
1-pchisq(246.49,41)

loanTest$prob<-predict(logMod,loanTest, type="resp")

# Determine cut off probability and model evaluation
install.packages("ROCR")
library(ROCR)
predObj<-prediction(loanTest$prob,loanTest$CREDIT)

#TPR vs FPR
perfObjTpr<-performance(predObj,"tpr","fpr")
plot(perfObjTpr, print.cutoffs.at=seq(0,1,0.1))

#Accuracy measure
perfObjAcc<-performance(predObj,"acc")
plot(perfObjAcc)

#Lift chart
perfObjLift<-performance(predObj,"lift","rpp")
plot(perfObjLift)

#Area under Curve
perfObjAuc<-performance(predObj,"auc")
perfObjAuc<-as.numeric(perfObjAuc@y.values)
perfObjAuc

#Confusion Matrix
install.packages(caret)
library(caret)
install.packages("e1071", dependencies = TRUE)
library(e1071)
loanTest$PredClass<-ifelse(loanTest$prob>=0.3,1,0)
confusionMatrix(loanTest$CREDIT,loanTest$PredClass)