user1<-read.csv("usage01.csv")
demo1<-read.csv("demography01.csv")
userDemo1<-merge(user1,demo1,by = "Customer")
View(userDemo1)
userDemo1<-userDemo1[,!names(userDemo1) %in% c("Customer.category","Customer")]

library(shiny)
##Static bar plot with R shiny
ui<-fluidPage(
   plotOutput("churnPlot")
   )

server<-function(input, output) {
  output$churnPlot <- renderPlot({
  barplot(table(userDemo1$Churn.within.last.month), main = "Churn Data Distribution")
  })
}

shinyApp(ui = ui, server = server)

###Rective plot-With user input value for long distance within last month attribute
ui<-fluidPage(
   sliderInput(inputId = "longDistCall", label="Choose the Value above which you want to view the churn distribution", value = 2, min = 0.9, max = 99.95),
   plotOutput("churnPlot")
   )

server<-function(input, output) {
  output$churnPlot <- renderPlot({
  barplot(table(userDemo1[which(userDemo1$Long.distance.last.month > input$longDistCall),]$Churn.within.last.month))
  })
}

shinyApp(ui = ui, server = server)
##Hypothesis testing for continuous variables
## Bin the variables based on the distribution
userhyp1<-userDemo1

hist(user1$Long.distance.last.month)

userhyp1$Long.distance.last.month<-ifelse(userhyp1$Long.distance.last.month < 10, 1, ifelse(userhyp1$Long.distance.last.month<20,2,ifelse(userhyp1$Long.distance.last.month < 30, 3,ifelse(userhyp1$Long.distance.last.month < 40, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Long.distance.last.month))

hist(userhyp1$Toll.free.last.month)

userhyp1$Toll.free.last.month<-ifelse(userhyp1$Toll.free.last.month < 10, 1, ifelse(userhyp1$Toll.free.last.month<20,2,ifelse(userhyp1$Toll.free.last.month < 30, 3,ifelse(userhyp1$Toll.free.last.month < 40, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Toll.free.last.month))

hist(userhyp1$Equipment.last.month)

userhyp1$Equipment.last.month<-ifelse(userhyp1$Equipment.last.month < 10, 1, ifelse(userhyp1$Equipment.last.month<30,2,ifelse(userhyp1$Equipment.last.month < 40, 3,ifelse(userhyp1$Equipment.last.month < 50, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Equipment.last.month))

hist(userhyp1$Calling.card.last.month)

userhyp1$Calling.card.last.month<-ifelse(userhyp1$Calling.card.last.month < 10, 1, ifelse(userhyp1$Calling.card.last.month<20,2,ifelse(userhyp1$Calling.card.last.month < 30, 3,ifelse(userhyp1$Calling.card.last.month < 40, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Calling.card.last.month))

hist(userhyp1$Wireless.last.month)

userhyp1$Wireless.last.month<-ifelse(userhyp1$Wireless.last.month < 10, 1, ifelse(userhyp1$Wireless.last.month<30,2,ifelse(userhyp1$Wireless.last.month < 40, 3,ifelse(userhyp1$Wireless.last.month < 50, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Wireless.last.month))

hist(userhyp1$Long.distance.over.tenure)

userhyp1$Long.distance.over.tenure<-ifelse(userhyp1$Long.distance.over.tenure < 1000, 1, ifelse(userhyp1$Long.distance.over.tenure<2000,2,3))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Long.distance.over.tenure))

hist(userhyp1$Toll.free.over.tenure)

userhyp1$Toll.free.over.tenure<-ifelse(userhyp1$Toll.free.over.tenure < 500, 1, ifelse(userhyp1$Toll.free.over.tenure<1000,2,ifelse(userhyp1$Toll.free.over.tenure < 1500, 3,ifelse(userhyp1$Toll.free.over.tenure < 2000, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Toll.free.over.tenure))

hist(userhyp1$Equipment.over.tenure)

userhyp1$Equipment.over.tenure<-ifelse(userhyp1$Equipment.over.tenure < 500, 1, ifelse(userhyp1$Equipment.over.tenure<1000,2,ifelse(userhyp1$Equipment.over.tenure < 1500, 3,ifelse(userhyp1$Equipment.over.tenure < 2000, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Equipment.over.tenure))

hist(userhyp1$Calling.card.over.tenure)

userhyp1$Calling.card.over.tenure<-ifelse(userhyp1$Calling.card.over.tenure < 500, 1, ifelse(userhyp1$Calling.card.over.tenure<1000,2,ifelse(userhyp1$Calling.card.over.tenure < 1500, 3,ifelse(userhyp1$Calling.card.over.tenure < 2000, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Calling.card.over.tenure))

hist(userhyp1$Wireless.over.tenure)

userhyp1$Wireless.over.tenure<-ifelse(userhyp1$Wireless.over.tenure < 1000, 1, ifelse(userhyp1$Wireless.over.tenure<2000,2,ifelse(userhyp1$Wireless.over.tenure < 3000, 3, 4)))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Wireless.over.tenure))

hist(userhyp1$Months.with.service)

userhyp1$Months.with.service<-ifelse(userhyp1$Months.with.service < 10, 1, ifelse(userhyp1$Months.with.service<20,2,ifelse(userhyp1$Months.with.service < 30, 3,ifelse(userhyp1$Months.with.service < 40, 4, ifelse(userhyp1$Months.with.service < 50, 5, ifelse(userhyp1$Months.with.service < 60, 6, 7))))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Months.with.service))

hist(userhyp1$Age.in.years)

userhyp1$Months.with.service<-ifelse(userhyp1$Age.in.years < 20, 1, ifelse(userhyp1$Age.in.years<30,2,ifelse(userhyp1$Age.in.years < 40, 3,ifelse(userhyp1$Age.in.years < 50, 4, ifelse(userhyp1$Age.in.years < 60, 5, ifelse(userhyp1$Age.in.years < 70, 6, 7))))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Age.in.years))

hist(userhyp1$Years.at.current.address)

userhyp1$Years.at.current.address<-ifelse(userhyp1$Years.at.current.address < 5, 1, ifelse(userhyp1$Years.at.current.address<10,2,ifelse(userhyp1$Years.at.current.address < 15, 3,ifelse(userhyp1$Years.at.current.address < 20, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Years.at.current.address))

hist(userhyp1$Household.income.in.thousands)

userhyp1$Household.income.in.thousands<-ifelse(userhyp1$Household.income.in.thousands < 25, 1, ifelse(userhyp1$Household.income.in.thousands<50,2,ifelse(userhyp1$Household.income.in.thousands < 75, 3,ifelse(userhyp1$Household.income.in.thousands < 100, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Household.income.in.thousands))

hist(userhyp1$Years.with.current.employer)

userhyp1$Years.with.current.employer<-ifelse(userhyp1$Years.with.current.employer < 5, 1, ifelse(userhyp1$Years.with.current.employer<10,2,ifelse(userhyp1$Years.with.current.employer < 15, 3,ifelse(userhyp1$Years.with.current.employer < 20, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Years.with.current.employer))

hist(userhyp1$Number.of.people.in.household)

userhyp1$Number.of.people.in.household<-ifelse(userhyp1$Number.of.people.in.household < 2, 1, ifelse(userhyp1$Number.of.people.in.household<3,2,ifelse(userhyp1$Number.of.people.in.household < 4, 3,ifelse(userhyp1$Number.of.people.in.household < 5, 4, 5))))

chisq.test(table(userhyp1$Churn.within.last.month, userhyp1$Number.of.people.in.household))

##after hypothesis testing, drop variables
userDemo2<-userDemo1[,!names(userDemo1) %in% c("Wireless.service","Multiple.lines","Voice.mail","Call.forwarding","Geographic.indicator","Marital.status","Retired","Gender","Number.of.people.in.household")]

userDemo2$Churn.within.last.month<-ifelse(userDemo2$Churn.within.last.month == "Yes", 1, 0)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(RColorBrewer)
cart.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)
CartM1<-rpart(formula = Churn.within.last.month ~.,data = userDemo2, method = "class", control = cart.ctrl)
fancyRpartPlot(CartM1)
Cprune<- prune(CartM1,cp=  CartM1$cptable[which.min(CartM1$cptable[,"xerror"]),"CP"])
fancyRpartPlot(Cprune)

##verify on training data
userDemo2$predict.score<-predict(Cprune, userDemo2[,-20])

library(ROCR)

CpredT <- prediction(userDemo2$predict.score[,2], userDemo2$Churn.within.last.month)
CperfT <- performance(CpredT, "tpr", "fpr")
plot(CperfT,print.cutoffs.at=seq(0,1,0.1))

userDemo2$Class = ifelse(userDemo2$predict.score[,2]>0.7,1,0)
confusionMatrix(userDemo2$Churn.within.last.month, userDemo2$Class)

## KS Score
KST <- max(attr(CperfT, 'y.values')[[1]]-attr(CperfT, 'x.values')[[1]])
KST
CaucT <- performance(CpredT,"auc")
CaucT <- as.numeric(CaucT@y.values)
CaucT

library(ineq)
CginiT = ineq(userDemo2$predict.score[,2], type="Gini")
CginiT


##verify on test data
user2<-read.csv("usage02.csv")
demo2<-read.csv("demography02.csv")
userTest1<-merge(user2,demo2,by = "Customer")
userTest1<-userTest1[,!names(userTest1) %in% c("Customer.category","Customer")]


userTest1<-userTest1[,!names(userTest1) %in% c("Wireless.service","Multiple.lines","Voice.mail","Call.forwarding","Geographic.indicator","Marital.status","Retired","Gender","Number.of.people.in.household")]

userTest1$Churn.within.last.month<-ifelse(userTest1$Churn.within.last.month == "Yes", 1, 0)

userTest1$predict.score<-predict(Cprune, userTest1[,-20])

library(ROCR)

Cpred <- prediction(userTest1$predict.score[,2], userTest1$Churn.within.last.month)
Cperf <- performance(Cpred, "tpr", "fpr")
plot(Cperf,print.cutoffs.at=seq(0,1,0.1))

userTest1$Class = ifelse(userTest1$predict.score[,2]>0.7,1,0)
confusionMatrix(userTest1$Churn.within.last.month, userTest1$Class)

## KS Score
KS <- max(attr(Cperf, 'y.values')[[1]]-attr(Cperf, 'x.values')[[1]])
KS
Cauc <- performance(Cpred,"auc")
Cauc <- as.numeric(Cauc@y.values)
Cauc

library(ineq)
Cgini = ineq(userTest1$predict.score[,2], type="Gini")
Cgini

##Get the Yes or No for churn to visualize
userTest1$origChurn<-ifelse(userTest1$Churn.within.last.month == 1, "Yes", "No")

## Visualization of model output
ui<-fluidPage(
   titlePanel("Churn with Equipment Rental and Months of Service"),
   sidebarLayout(      
    
    # Define the sidebar with 2 inputs
    sidebarPanel(
      radioButtons("equipRental","Equipment Rental", c("No" = "No", "Yes" = "Yes")),
	  sliderInput(inputId = "serviceMonths", label="Choose the months of service above which the churn can be seen", value = 1, min = 1, max = 72)
   ),
   mainPanel (
   plotOutput("churnPlot")
   )
   )
   )

server<-function(input, output) {
  output$churnPlot <- renderPlot({
  barplot(table(userTest1[which(userTest1$Equipment.rental == input$equipRental & userTest1$Months.with.service > input$serviceMonths),]$origChurn))
  })
}

shinyApp(ui = ui, server = server)