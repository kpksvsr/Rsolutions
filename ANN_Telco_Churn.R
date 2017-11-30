user1<-read.csv("usage01.csv")
demo1<-read.csv("demography01.csv")
userDemo1<-merge(user1,demo1,by = "Customer")
View(userDemo1)
userDemo1<-userDemo1[,!names(userDemo1) %in% c("Customer.category","Customer")]


for(level in unique(userDemo1$Toll.free.service)) {
  userDemo1[paste("Toll.free.service",level,sep="_")]<-ifelse(userDemo1$Toll.free.service == level,1,0)
}

for(level in unique(userDemo1$Equipment.rental)) {
  userDemo1[paste("Equipment.rental",level,sep="_")]<-ifelse(userDemo1$Equipment.rental == level,1,0)
}

for(level in unique(userDemo1$Calling.card.service)) {
  userDemo1[paste("Calling.card.service",level,sep="_")]<-ifelse(userDemo1$Calling.card.service == level,1,0)
}


for(level in unique(userDemo1$Paging.service)) {
  userDemo1[paste("Paging.service",level,sep="_")]<-ifelse(userDemo1$Paging.service == level,1,0)
}

for(level in unique(userDemo1$Internet)) {
  userDemo1[paste("Internet",level,sep="_")]<-ifelse(userDemo1$Internet == level,1,0)
}

for(level in unique(userDemo1$Caller.ID)) {
  userDemo1[paste("Caller.ID",level,sep="_")]<-ifelse(userDemo1$Caller.ID == level,1,0)
}

for(level in unique(userDemo1$Call.waiting)) {
  userDemo1[paste("Call.waiting",level,sep="_")]<-ifelse(userDemo1$Call.waiting == level,1,0)
}


for(level in unique(userDemo1$Three.way.calling)) {
  userDemo1[paste("Three.way.calling",level,sep="_")]<-ifelse(userDemo1$Three.way.calling == level,1,0)
}

for(level in unique(userDemo1$Electronic.billing)) {
  userDemo1[paste("Electronic.billing",level,sep="_")]<-ifelse(userDemo1$Electronic.billing == level,1,0)
}

userDemo1$Churn.within.last.month<-ifelse(userDemo1$Churn.within.last.month == "Yes", 1, 0)


userDemo1$eduNew<-ifelse(userDemo1$Level.of.education=="Did Not Complete High School",1,ifelse(userDemo1$Level.of.education=="Completed High School",2,ifelse(userDemo1$Level.of.education=="Undergraduate",3,ifelse(userDemo1$Level.of.education=="Graduate",4,ifelse(userDemo1$Level.of.education=="Doctorate",5,0)))))

userDemo1<-userDemo1[,!names(userDemo1) %in% c("Geographic.indicator","Marital.status","Retired","Gender","Level.of.education","Toll.free.service","Equipment.rental","Calling.card.service","Wireless.service","Multiple.lines","Voice.mail","Paging.service","Internet","Caller.ID","Call.waiting","Call.forwarding","Three.way.calling","Electronic.billing")]

userDemo1$Long.distance.last.month<-scale(userDemo1$Long.distance.last.month)
userDemo1$Toll.free.last.month<-scale(userDemo1$Toll.free.last.month)
userDemo1$Equipment.last.month<-scale(userDemo1$Equipment.last.month)
userDemo1$Calling.card.last.month<-scale(userDemo1$Calling.card.last.month)
userDemo1$Wireless.last.month<-scale(userDemo1$Wireless.last.month)
userDemo1$Long.distance.over.tenure<-scale(userDemo1$Long.distance.over.tenure)
userDemo1$Toll.free.over.tenure<-scale(userDemo1$Toll.free.over.tenure)
userDemo1$Equipment.over.tenure<-scale(userDemo1$Equipment.over.tenure)
userDemo1$Calling.card.over.tenure<-scale(userDemo1$Calling.card.over.tenure)
userDemo1$Wireless.over.tenure<-scale(userDemo1$Wireless.over.tenure)
userDemo1$Months.with.service<-scale(userDemo1$Months.with.service)
userDemo1$Age.in.years<-scale(userDemo1$Age.in.years)
userDemo1$Years.at.current.address<-scale(userDemo1$Years.at.current.address)
userDemo1$Household.income.in.thousands<-scale(userDemo1$Household.income.in.thousands)
userDemo1$Years.with.current.employer<-scale(userDemo1$Years.with.current.employer)
userDemo1$Number.of.people.in.household<-scale(userDemo1$Number.of.people.in.household)

userDemo2<-userDemo1[,!names(userDemo1) %in% c("Wireless.service","Multiple.lines","Voice.mail","Call.forwarding","Geographic.indicator","Marital.status","Retired","Gender","Number.of.people.in.household")]


neuralForm<-as.formula(paste("Churn.within.last.month ~",paste(names(userDemo2[which(!names(userDemo2) %in% "Churn.within.last.month")]), collapse = ' + ')))

library(neuralnet)

userDemoNeural <- neuralnet(formula = neuralForm,
data = userDemo2,
hidden = c(5,2),
err.fct = "sse",
act.fct= "logistic",
linear.output = FALSE,
lifesign = "full",
lifesign.step = 10,
threshold = 0.1,
stepmax = 2000
)

user2<-read.csv("usage02.csv")
demo2<-read.csv("demography02.csv")
userTest1<-merge(user2,demo2,by = "Customer")
userTest1<-userTest1[,!names(userTest1) %in% c("Customer.category","Customer")]

for(level in unique(userTest1$Toll.free.service)) {
  userTest1[paste("Toll.free.service",level,sep="_")]<-ifelse(userTest1$Toll.free.service == level,1,0)
}

for(level in unique(userTest1$Equipment.rental)) {
  userTest1[paste("Equipment.rental",level,sep="_")]<-ifelse(userTest1$Equipment.rental == level,1,0)
}

for(level in unique(userTest1$Calling.card.service)) {
  userTest1[paste("Calling.card.service",level,sep="_")]<-ifelse(userTest1$Calling.card.service == level,1,0)
}

for(level in unique(userTest1$Paging.service)) {
  userTest1[paste("Paging.service",level,sep="_")]<-ifelse(userTest1$Paging.service == level,1,0)
}

for(level in unique(userTest1$Internet)) {
  userTest1[paste("Internet",level,sep="_")]<-ifelse(userTest1$Internet == level,1,0)
}

for(level in unique(userTest1$Caller.ID)) {
  userTest1[paste("Caller.ID",level,sep="_")]<-ifelse(userTest1$Caller.ID == level,1,0)
}

for(level in unique(userTest1$Call.waiting)) {
  userTest1[paste("Call.waiting",level,sep="_")]<-ifelse(userTest1$Call.waiting == level,1,0)
}

for(level in unique(userTest1$Three.way.calling)) {
  userTest1[paste("Three.way.calling",level,sep="_")]<-ifelse(userTest1$Three.way.calling == level,1,0)
}

for(level in unique(userTest1$Electronic.billing)) {
  userTest1[paste("Electronic.billing",level,sep="_")]<-ifelse(userTest1$Electronic.billing == level,1,0)
}

userTest1$Churn.within.last.month<-ifelse(userTest1$Churn.within.last.month == "Yes", 1, 0)


userTest1$eduNew<-ifelse(userTest1$Level.of.education=="Did Not Complete High School",1,ifelse(userTest1$Level.of.education=="Completed High School",2,ifelse(userTest1$Level.of.education=="Undergraduate",3,ifelse(userTest1$Level.of.education=="Graduate",4,ifelse(userTest1$Level.of.education=="Doctorate",5,0)))))

userTest1<-userTest1[,!names(userTest1) %in% c("Geographic.indicator","Marital.status","Retired","Gender","Level.of.education","Toll.free.service","Equipment.rental","Calling.card.service","Wireless.service","Multiple.lines","Voice.mail","Paging.service","Internet","Caller.ID","Call.waiting","Call.forwarding","Three.way.calling","Electronic.billing","Customer.category","Customer")]

userTest1$Long.distance.last.month<-scale(userTest1$Long.distance.last.month)
userTest1$Toll.free.last.month<-scale(userTest1$Toll.free.last.month)
userTest1$Equipment.last.month<-scale(userTest1$Equipment.last.month)
userTest1$Calling.card.last.month<-scale(userTest1$Calling.card.last.month)
userTest1$Wireless.last.month<-scale(userTest1$Wireless.last.month)
userTest1$Long.distance.over.tenure<-scale(userTest1$Long.distance.over.tenure)
userTest1$Toll.free.over.tenure<-scale(userTest1$Toll.free.over.tenure)
userTest1$Equipment.over.tenure<-scale(userTest1$Equipment.over.tenure)
userTest1$Calling.card.over.tenure<-scale(userTest1$Calling.card.over.tenure)
userTest1$Wireless.over.tenure<-scale(userTest1$Wireless.over.tenure)
userTest1$Months.with.service<-scale(userTest1$Months.with.service)
userTest1$Age.in.years<-scale(userTest1$Age.in.years)
userTest1$Years.at.current.address<-scale(userTest1$Years.at.current.address)
userTest1$Household.income.in.thousands<-scale(userTest1$Household.income.in.thousands)
userTest1$Years.with.current.employer<-scale(userTest1$Years.with.current.employer)
userTest1$Number.of.people.in.household<-scale(userTest1$Number.of.people.in.household)

userTest1<-userTest1[,!names(userTest1) %in% c("Wireless.service","Multiple.lines","Voice.mail","Call.forwarding","Geographic.indicator","Marital.status","Retired","Gender","Number.of.people.in.household")]


compute.output = compute(userDemoNeural, userTest1[,-11])

userTest1$Predict.score = compute.output$net.result

library(ROCR)

predObj<-prediction(userTest1$Predict.score,userTest1$Churn.within.last.month)

perfObjTpr<-performance(predObj,"tpr","fpr")

plot(perfObjTpr, print.cutoffs.at=seq(0,1,0.1))

userTest1$Class = ifelse(userTest1$Predict.score>0.5,1,0)

with( userTest1, table(Churn.within.last.month, as.factor(Class)))

## We can use the confusionMatrix function of the caret package 
library(caret)
confusionMatrix(userTest1$Churn.within.last.month, userTest1$Class)
