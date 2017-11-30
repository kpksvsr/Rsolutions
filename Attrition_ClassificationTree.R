#set working dir and read the csv file
setwd("E:/Pavan/GL/subjects/dataMining/assignment2/R_Working")
hrDataC<-read.csv("HR_Employee_Attrition_Data.csv")

#Remove unnecessary columns
hrDataC1<-hrDataC[,!names(hrDataC) %in% c("EmployeeCount","EmployeeNumber","Over18","StandardHours")]



#Convert continuous to categorical variables
hrDataC1$Age<- cut(hrDataC$Age, seq(from = min(hrDataC$Age),to = max(hrDataC$Age), by = (max(hrDataC$Age - min(hrDataC$Age)))/5), include.lowest = TRUE)

hrDataC1$DailyRate<- cut(hrDataC$DailyRate, seq(from = min(hrDataC$DailyRate),to = max(hrDataC$DailyRate), by = (max(hrDataC$DailyRate - min(hrDataC$DailyRate)))/5), include.lowest = TRUE)

hrDataC1$DistanceFromHome<- cut(hrDataC$DistanceFromHome, seq(from = min(hrDataC$DistanceFromHome),to = max(hrDataC$DistanceFromHome), by = (max(hrDataC$DistanceFromHome - min(hrDataC$DistanceFromHome)))/5), include.lowest = TRUE)

hrDataC1$HourlyRate<- cut(hrDataC$HourlyRate, seq(from = min(hrDataC$HourlyRate),to = max(hrDataC$HourlyRate), by = (max(hrDataC$HourlyRate - min(hrDataC$HourlyRate)))/5), include.lowest = TRUE)

hrDataC1$MonthlyIncome<- cut(hrDataC$MonthlyIncome, seq(from = min(hrDataC$MonthlyIncome),to = max(hrDataC$MonthlyIncome), by = (max(hrDataC$MonthlyIncome - min(hrDataC$MonthlyIncome)))/5), include.lowest = TRUE)

hrDataC1$MonthlyRate<- cut(hrDataC$MonthlyRate, seq(from = min(hrDataC$MonthlyRate),to = max(hrDataC$MonthlyRate), by = (max(hrDataC$MonthlyRate - min(hrDataC$MonthlyRate)))/5), include.lowest = TRUE)

hrDataC1$NumCompaniesWorked<- cut(hrDataC$NumCompaniesWorked, seq(from = min(hrDataC$NumCompaniesWorked),to = max(hrDataC$NumCompaniesWorked), by = (max(hrDataC$NumCompaniesWorked - min(hrDataC$NumCompaniesWorked)))/5), include.lowest = TRUE)

hrDataC1$PercentSalaryHike<- cut(hrDataC$PercentSalaryHike, seq(from = min(hrDataC$PercentSalaryHike),to = max(hrDataC$PercentSalaryHike), by = (max(hrDataC$PercentSalaryHike - min(hrDataC$PercentSalaryHike)))/5), include.lowest = TRUE)

hrDataC1$TotalWorkingYears<- cut(hrDataC$TotalWorkingYears, seq(from = min(hrDataC$TotalWorkingYears),to = max(hrDataC$TotalWorkingYears), by = (max(hrDataC$TotalWorkingYears - min(hrDataC$TotalWorkingYears)))/5), include.lowest = TRUE)

hrDataC1$TrainingTimesLastYear<- cut(hrDataC$TrainingTimesLastYear, seq(from = min(hrDataC$TrainingTimesLastYear),to = max(hrDataC$TrainingTimesLastYear), by = (max(hrDataC$TrainingTimesLastYear - min(hrDataC$TrainingTimesLastYear)))/5), include.lowest = TRUE)

hrDataC1$YearsAtCompany<- cut(hrDataC$YearsAtCompany, seq(from = min(hrDataC$YearsAtCompany),to = max(hrDataC$YearsAtCompany), by = (max(hrDataC$YearsAtCompany - min(hrDataC$YearsAtCompany)))/5), include.lowest = TRUE)

hrDataC1$YearsInCurrentRole<- cut(hrDataC$YearsInCurrentRole, seq(from = min(hrDataC$YearsInCurrentRole),to = max(hrDataC$YearsInCurrentRole), by = (max(hrDataC$YearsInCurrentRole - min(hrDataC$YearsInCurrentRole)))/5), include.lowest = TRUE)

hrDataC1$YearsSinceLastPromotion<- cut(hrDataC$YearsSinceLastPromotion, seq(from = min(hrDataC$YearsSinceLastPromotion),to = max(hrDataC$YearsSinceLastPromotion), by = (max(hrDataC$YearsSinceLastPromotion - min(hrDataC$YearsSinceLastPromotion)))/5), include.lowest = TRUE)

hrDataC1$YearsWithCurrManager<- cut(hrDataC$YearsWithCurrManager, seq(from = min(hrDataC$YearsWithCurrManager),to = max(hrDataC$YearsWithCurrManager), by = (max(hrDataC$YearsWithCurrManager - min(hrDataC$YearsWithCurrManager)))/5), include.lowest = TRUE)

#Remove spaces and special characters in fators
levels(hrDataC1$BusinessTravel)<-gsub("-","_",levels(hrDataC1$BusinessTravel))
levels(hrDataC1$Department)<-gsub(" ","",levels(hrDataC1$Department))
levels(hrDataC1$Department)<-gsub("&","And",levels(hrDataC1$Department))
levels(hrDataC1$EducationField)<-gsub(" ","",levels(hrDataC1$EducationField))
levels(hrDataC1$JobRole)<-gsub(" ","",levels(hrDataC1$JobRole))

#Mark integers as factors
hrDataC1$Education<-as.factor(hrDataC1$Education)
hrDataC1$EnvironmentSatisfaction<-as.factor(hrDataC1$EnvironmentSatisfaction)
hrDataC1$JobInvolvement<-as.factor(hrDataC1$JobInvolvement)
hrDataC1$JobLevel<-as.factor(hrDataC1$JobLevel)
hrDataC1$JobSatisfaction<-as.factor(hrDataC1$JobSatisfaction)
hrDataC1$PerformanceRating<-as.factor(hrDataC1$PerformanceRating)
hrDataC1$RelationshipSatisfaction<-as.factor(hrDataC1$RelationshipSatisfaction)
hrDataC1$StockOptionLevel<-as.factor(hrDataC1$StockOptionLevel)
hrDataC1$WorkLifeBalance<-as.factor(hrDataC1$WorkLifeBalance)

#Drop below categorical variables after chi square test
hrDataC1<-hrDataC1[,!names(hrDataC1) %in% c("Education","Gender","PerformanceRating")]

hrDataC1<-hrDataC1[,!names(hrDataC1) %in% c("HourlyRate","MonthlyRate","PercentSalaryHike")]

#Run the CART model
library(rpart)
library(rpart.plot)
devIndex<-sample.int(nrow(hrDataC1),size = floor(0.7*nrow(hrDataC1)), replace = FALSE)
hrDataDevC<-hrDataC1[devIndex,]
hrDataTestC<-hrDataC1[-devIndex,]
cart.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

View(hrDataDevC)

CartM1 <- rpart(formula = Attrition ~ ., data = hrDataDevC, method = "class", control = cart.ctrl)

prp(CartM1)
install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
install.packages("rattle")
library(rattle)
install.packages("RColorBrewer")
library(RColorBrewer)
fancyRpartPlot(CartM1)

printcp(CartM1)

Cprune<- prune(CartM1,cp=  CartM1$cptable[which.min(CartM1$cptable[,"xerror"]),"CP"])

printcp(Cprune)

fancyRpartPlot(Cprune, uniform=TRUE,  main="Pruned Tree")

## Scoring and performance measure on dev sample

## Scoring syntax
hrDataDevC$predict.class <- predict(Cprune, hrDataDevC, type="class")
hrDataDevC$predict.score <- predict(Cprune, hrDataDevC)
hrDataDevC$Acode<-ifelse(hrDataDevC$Attrition == "Yes",1,0)


## Scoring Holdout sample
hrDataTestC$predict.class <- predict(Cprune, hrDataTestC, type="class")
hrDataTestC$predict.score <- predict(Cprune, hrDataTestC)
hrDataTestC$Acode<-ifelse(hrDataTestC$Attrition == "Yes",1,0)

library(caret)

confusionMatrix(hrDataDevC$Attrition,hrDataDevC$predict.class)

confusionMatrix(hrDataTestC$Attrition,hrDataTestC$predict.class)

install.packages("ROCR")

library(ROCR)

Cpred <- prediction(hrDataDevC$predict.score[,2], hrDataDevC$Attrition)
Cperf <- performance(Cpred, "tpr", "fpr")
plot(Cperf)
KS <- max(attr(Cperf, 'y.values')[[1]]-attr(Cperf, 'x.values')[[1]])
Cauc <- performance(Cpred,"auc"); 
Cauc <- as.numeric(Cauc@y.values)

install.packages("ineq")

library(ineq)
Cgini = ineq(hrDataDevC$predict.score[,2], type="Gini")

with(hrDataDevC, table(Attrition, predict.class))




Ctpred <- ROCR::prediction(hrDataTestC$predict.score[,2], hrDataTestC$Attrition)
Ctperf <- performance(Cpred, "tpr", "fpr")
plot(Ctperf)
KS <- max(attr(Ctperf, 'y.values')[[1]]-attr(Ctperf, 'x.values')[[1]])
Ctauc <- performance(Ctpred,"auc"); 
Ctauc <- as.numeric(Ctauc@y.values)

install.packages("ineq")

library(ineq)
Ctgini = ineq(hrDataTestC$predict.score[,2], type="Gini")

with(hrDataTestC, table(Attrition, predict.class))


###############################################################
quantile(hrDataDevC$predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hrDataDevC$deciles <- decile(hrDataDevC$predict.score)

library(data.table)
library(scales)

Ctmp_Ndt = data.table(hrDataDevC)

Crank <- Ctmp_Ndt[, list(
cnt = length(Acode),
cnt_attr = sum(Acode),
cnt_non_attr = sum(Acode == 0)) ,
by=deciles][order(-deciles)]

Crank$AttrRate <- round (Crank$cnt_attr / Crank$cnt,2);
Crank$cum_attr <- cumsum(Crank$cnt_attr)
Crank$cum_non_attr <- cumsum(Crank$cnt_non_attr)
Crank$cum_rel_attr <- round(Crank$cum_attr / sum(Crank$cnt_attr),2);
Crank$cum_rel_non_attr <- round(Crank$cum_non_attr / sum(Crank$cnt_non_attr),2);
Crank$ks <- abs(Crank$cum_rel_attr - Crank$cum_rel_non_attr);
Crank$AttrRate <- percent(Crank$AttrRate)
Crank$cum_rel_attr <- percent(Crank$cum_rel_attr)
Crank$cum_rel_non_attr <- percent(Crank$cum_rel_non_attr)
View(Crank)



quantile(hrDataTestC$predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hrDataTestC$deciles <- decile(hrDataTestC$predict.score)
Ctmp_DT = data.table(hrDataTestC)

Ch_rank <- Ctmp_DT[, list(
cnt = length(Acode),
cnt_attr = sum(Acode),
cnt_non_attr = sum(Acode == 0)) ,
by=deciles][order(-deciles)]

Ch_rank$AttrRate <- round (Ch_rank$cnt_attr / Ch_rank$cnt,2);
Ch_rank$cum_attr <- cumsum(Ch_rank$cnt_attr)
Ch_rank$cum_non_attr <- cumsum(Ch_rank$cnt_non_attr)
Ch_rank$cum_rel_attr <- round(Ch_rank$cum_attr / sum(Ch_rank$cnt_attr),2);
Ch_rank$cum_rel_non_attr <- round(Ch_rank$cum_non_attr / sum(Ch_rank$cnt_non_attr),2);
Ch_rank$ks <- abs(Ch_rank$cum_rel_attr - Ch_rank$cum_rel_non_attr);
Ch_rank$AttrRate <- percent(Ch_rank$AttrRate)
Ch_rank$cum_rel_attr <- percent(Ch_rank$cum_rel_attr)
Ch_rank$cum_rel_non_attr <- percent(Ch_rank$cum_rel_non_attr)
View(Ch_rank)

