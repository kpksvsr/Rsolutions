hrData<-read.csv("HR_Employee_Attrition_Data.csv")

chisq.test(table(hrData$Attrition,hrData$BusinessTravel))
chisq.test(table(hrData$Attrition,hrData$Department))
chisq.test(table(hrData$Attrition,hrData$EducationField))
chisq.test(table(hrData$Attrition,hrData$Gender))
chisq.test(table(hrData$Attrition,hrData$JobRole))
chisq.test(table(hrData$Attrition,hrData$MaritalStatus))
chisq.test(table(hrData$Attrition,hrData$OverTime))
chisq.test(table(hrData$Attrition,hrData$Education))
chisq.test(table(hrData$Attrition,hrData$EnvironmentSatisfaction))
chisq.test(table(hrData$Attrition,hrData$JobInvolvement))
chisq.test(table(hrData$Attrition,hrData$JobLevel))
chisq.test(table(hrData$Attrition,hrData$JobSatisfaction))
chisq.test(table(hrData$Attrition,hrData$PerformanceRating))
chisq.test(table(hrData$Attrition,hrData$RelationshipSatisfaction))
chisq.test(table(hrData$Attrition,hrData$StockOptionLevel))
chisq.test(table(hrData$Attrition,hrData$WorkLifeBalance))

#Drop the variables that are not significant
hrData1<-hrData[,!names(hrData) %in% c("EmployeeCount","EmployeeNumber","Over18","StandardHours","Education","Gender","PerformanceRating")]

#Drop the significant categorical column which are text. This is interim to perform regression test and
#these will be included again.
hrData1<-hrData1[,!names(hrData1) %in% c("BusinessTravel","Department","EducationField","JobRole","MaritalStatus","OverTime")]

#Convert Attrition to 1 and 0, 1-Yes, 0-No
hrData1$Attrition<- ifelse(hrData1$Attrition == "Yes",1,0)

View(hrData1)

#Take subset of data without Attrition column and ordinal data and scale it
colScales<-subset(hrData1, select = c(!names(hrData1) %in% c("Attrition","EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")))
colIntScaled<-scale(colScales)

hrDataScaled<-as.data.frame(colIntScaled)
View(hrDataScaled)

#Bind the Attrition column
hrDataScaled<-cbind.data.frame(hrDataScaled,hrData1$Attrition)
names(hrDataScaled)
colnames(hrDataScaled)[15]
colnames(hrDataScaled)[15]<-"Attrition"
logMod<-glm(Attrition~., data = hrDataScaled, family = binomial)
summary(logMod)

hrDataScaled1<-hrDataScaled

#Based on logistic regression output, drop the variables that are not significant
hrDataScaled1<-hrDataScaled1[,!names(hrDataScaled1) %in% c("HourlyRate","MonthlyRate","PercentSalaryHike","TotalWorkingYears")]
View(hrDataScaled1)

#Clean the data to remove -, spaces and replace ampersand with And
levels(hrData$BusinessTravel)<-gsub("-","_",levels(hrData$BusinessTravel))
levels(hrData$Department)<-gsub(" ","",levels(hrData$Department))
levels(hrData$Department)<-gsub("&","And",levels(hrData$Department))
levels(hrData$EducationField)<-gsub(" ","",levels(hrData$EducationField))
levels(hrData$JobRole)<-gsub(" ","",levels(hrData$JobRole))


#Create dummy variables for categorical data
for(level in unique(hrData$BusinessTravel)){
hrData[paste("BusinessTravel", level, sep = "_")] <- ifelse(hrData$BusinessTravel == level, 1, 0)
}
for(level in unique(hrData$Department)){
hrData[paste("Department", level, sep = "_")] <- ifelse(hrData$Department == level, 1, 0)
}
for(level in unique(hrData$EducationField)){
hrData[paste("EducationField", level, sep = "_")] <- ifelse(hrData$EducationField == level, 1, 0)
}
for(level in unique(hrData$JobRole)){
hrData[paste("JobRole", level, sep = "_")] <- ifelse(hrData$JobRole == level, 1, 0)
}
for(level in unique(hrData$MaritalStatus)){
hrData[paste("MaritalStatus", level, sep = "_")] <- ifelse(hrData$MaritalStatus == level, 1, 0)
}
for(level in unique(hrData$OverTime)){
hrData[paste("OverTime", level, sep = "_")] <- ifelse(hrData$OverTime == level, 1, 0)
}


hrDataScaled2<-as.data.frame(hrDataScaled1)

#Include the scaled values of ordinal variables
hrDataScaled2$EnvironmentSatisfaction<-scale(hrData$EnvironmentSatisfaction)
hrDataScaled2$JobInvolvement<-scale(hrData$JobInvolvement)
hrDataScaled2$JobLevel<-scale(hrData$JobLevel)
hrDataScaled2$JobSatisfaction<-scale(hrData$JobSatisfaction)
hrDataScaled2$RelationshipSatisfaction<-scale(hrData$RelationshipSatisfaction)
hrDataScaled2$StockOptionLevel<-scale(hrData$StockOptionLevel)
hrDataScaled2$WorkLifeBalance<-scale(hrData$WorkLifeBalance)

#Dummy variables for BusinessTravel
hrDataScaled2<-cbind.data.frame(hrDataScaled2,hrData$BusinessTravel_Non_Travel,hrData$BusinessTravel_Travel_Frequently,hrData$BusinessTravel_Travel_Rarely)
View(hrDataScaled2)

#Dummy variables for department
hrDataScaled2<-cbind.data.frame(hrDataScaled2,hrData$Department_HumanResources,hrData$Department_ResearchAndDevelopment,hrData$Department_Sales)
View(hrDataScaled2)

#Dummy variables for EducationField
hrDataScaled2<-cbind.data.frame(hrDataScaled2,hrData$EducationField_HumanResources,hrData$EducationField_LifeSciences,hrData$EducationField_Marketing,hrData$EducationField_Medical,hrData$EducationField_Other,hrData$EducationField_TechnicalDegree)

#Dummy variables for JobRole
hrDataScaled2<-cbind.data.frame(hrDataScaled2,hrData$JobRole_HealthcareRepresentative,hrData$JobRole_HumanResources,hrData$JobRole_LaboratoryTechnician,hrData$JobRole_Manager,hrData$JobRole_ManufacturingDirector,hrData$JobRole_ResearchDirector,hrData$JobRole_ResearchScientist,hrData$JobRole_SalesExecutive,hrData$JobRole_SalesRepresentative)
View(hrDataScaled2)

#Dummy variables for MaritalStatus
hrDataScaled2<-cbind.data.frame(hrDataScaled2,hrData$MaritalStatus_Divorced,hrData$MaritalStatus_Married,hrData$MaritalStatus_Single)
hrDataScaled2

#Dummy variables for OverTime
hrDataScaled2<-cbind.data.frame(hrDataScaled2,hrData$OverTime_Yes,hrData$OverTime_No)
View(hrDataScaled2)

colnames(hrDataScaled2)
colnames(hrDataScaled2)[19:44]
colnames(hrDataScaled2)[19:44]<-c("BusinessTravel_Non_Travel","BusinessTravel_Travel_Frequently","BusinessTravel_Travel_Rarely","Department_HumanResources","Department_ResearchAndDevelopment","Department_Sales","EducationField_HumanResources","EducationField_LifeSciences","EducationField_Marketing","EducationField_Medical","EducationField_Other","EducationField_TechnicalDegree","JobRole_HealthcareRepresentative","JobRole_HumanResources","JobRole_LaboratoryTechnician","JobRole_Manager","JobRole_ManufacturingDirector","JobRole_ResearchDirector","JobRole_ResearchScientist","JobRole_SalesExecutive","JobRole_SalesRepresentative","MaritalStatus_Divorced","MaritalStatus_Married","MaritalStatus_Single","OverTime_Yes","OverTime_No")
colnames(hrDataScaled2)
View(hrDataScaled2)

devIndex<-sample.int(nrow(hrDataScaled2),size = floor(0.7*nrow(hrDataScaled2)), replace = FALSE)
hrDataDev<-hrDataScaled2[devIndex,]
hrDataTest<-hrDataScaled2[-devIndex,]
nrow(hrDataDev)
nrow(hrDataTest)

library(neuralnet)

#Build formula
neuralForm<-as.formula(paste("Attrition ~",paste(names(hrDataDev[which(!names(hrDataDev) %in% "Attrition")]), collapse = ' + ')))

#Run neuralnet program on the cleaned up data
hrNeural <- neuralnet(formula = neuralForm,
data = hrDataDev,
hidden = c(5,2),
err.fct = "sse",
act.fct= "logistic",
linear.output = FALSE,
lifesign = "full",
lifesign.step = 10,
threshold = 0.2,
stepmax = 2000
)

decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
ifelse(x<deciles[1], 1,
ifelse(x<deciles[2], 2,
ifelse(x<deciles[3], 3,
ifelse(x<deciles[4], 4,
ifelse(x<deciles[5], 5,
ifelse(x<deciles[6], 6,
ifelse(x<deciles[7], 7,
ifelse(x<deciles[8], 8,
ifelse(x<deciles[9], 9, 10
))))))))))
}

hrDataDev$prob<-hrNeural$net.result[[1]]
quantile(hrDataDev$prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hrDataDev$deciles <- decile(hrDataDev$prob)

library(data.table)
library(scales)

tmp_Ndt = data.table(hrDataDev)

rank <- tmp_Ndt[, list(
cnt = length(Attrition),
cnt_attr = sum(Attrition),
cnt_non_attr = sum(Attrition == 0)) ,
by=deciles][order(-deciles)]

rank$AttrRate <- round (rank$cnt_attr / rank$cnt,2);
rank$cum_attr <- cumsum(rank$cnt_attr)
rank$cum_non_attr <- cumsum(rank$cnt_non_attr)
rank$cum_rel_attr <- round(rank$cum_attr / sum(rank$cnt_attr),2);
rank$cum_rel_non_attr <- round(rank$cum_non_attr / sum(rank$cnt_non_attr),2);
rank$ks <- abs(rank$cum_rel_attr - rank$cum_rel_non_attr);
rank$AttrRate <- percent(rank$AttrRate)
rank$cum_rel_attr <- percent(rank$cum_rel_attr)
rank$cum_rel_non_attr <- percent(rank$cum_rel_non_attr)
View(rank)

#Predict on hrDataTest
compute.output = compute(hrNeural, hrDataTest[,-11])

hrDataTest$Predict.score = compute.output$net.result

quantile(hrDataTest$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hrDataTest$deciles <- decile(hrDataTest$Predict.score)
tmp_DT = data.table(hrDataTest)

h_rank <- tmp_DT[, list(
cnt = length(Attrition),
cnt_attr = sum(Attrition),
cnt_non_attr = sum(Attrition == 0)) ,
by=deciles][order(-deciles)]

h_rank$AttrRate <- round (h_rank$cnt_attr / h_rank$cnt,2);
h_rank$cum_attr <- cumsum(h_rank$cnt_attr)
h_rank$cum_non_attr <- cumsum(h_rank$cnt_non_attr)
h_rank$cum_rel_attr <- round(h_rank$cum_attr / sum(h_rank$cnt_attr),2);
h_rank$cum_rel_non_attr <- round(h_rank$cum_non_attr / sum(h_rank$cnt_non_attr),2);
h_rank$ks <- abs(h_rank$cum_rel_attr - h_rank$cum_rel_non_attr);
h_rank$AttrRate <- percent(h_rank$AttrRate)
h_rank$cum_rel_attr <- percent(h_rank$cum_rel_attr)
h_rank$cum_rel_non_attr <- percent(h_rank$cum_rel_non_attr)
View(h_rank)


## Assgining 0 / 1 class based on certain threshold
hrDataDev$Class = ifelse(hrDataDev$prob>0.60,1,0)
with( hrDataDev, table(Attrition, as.factor(Class)  ))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
library(caret)
confusionMatrix(hrDataDev$Attrition, hrDataDev$Class)

## Assgining 0 / 1 class based on certain threshold
hrDataTest$Class = ifelse(hrDataTest$Predict.score>0.60,1,0)
with( hrDataTest, table(Attrition, as.factor(Class)  ))

## We can use the confusionMatrix function of the caret package 
confusionMatrix(hrDataTest$Attrition, hrDataTest$Class)

#If we want to tune it by changing the parameters to neuralnet, remove the hrDataDev and hrDataTest and run the neuralnet with new parameters and again assign probabilities and deciles.

npred <- ROCR::prediction(hrDataDev$prob, hrDataDev$Attrition)
nperf <- performance(npred, "tpr", "fpr")
plot(nperf)

nKS <- max(attr(nperf, 'y.values')[[1]]-attr(nperf, 'x.values')[[1]])
nauc <- performance(npred,"auc"); 
nauc <- as.numeric(nauc@y.values)

install.packages("ineq")

library(ineq)
ngini = ineq(hrDataDev$prob, type="Gini")