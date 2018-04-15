setwd("D:/Personal/GL/hacks/stroke")
healthTrain<-read.csv("train.csv")
healthTest <- read.csv("test.csv")
anyNA(healthTrain$avg_glucose_level)
anyNA(healthTrain$bmi)
anyNA(healthTrain$smoking_status)
unique(healthTrain$smoking_status)
levels(healthTrain$smoking_status)
healthTrain[which(healthTrain$smoking_status == ""),"smoking_status"] <-NA
healthTrain[which(healthTrain$smoking_status == ""),"smoking_status"]

nrow(healthTrain[which(healthTrain$bmi>55 & healthTrain$stroke == 1),])

nrow(healthTrain[which(healthTrain$bmi<=10 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>10 & healthTrain$bmi<=20 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>20 & healthTrain$bmi<=30 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>20 & healthTrain$bmi<=25 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>25 & healthTrain$bmi<=30 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>30 & healthTrain$bmi<=40 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>40 & healthTrain$bmi<=50 & healthTrain$stroke == 1),])
nrow(healthTrain[which(healthTrain$bmi>50 & healthTrain$bmi<=60 & healthTrain$stroke == 1),])

hist(healthTrain$bmi)
count(is.na(healthTrain$bmi))

is.na(healthTrain$bmi)

mean(healthTrain[which(healthTrain$Residence_type == "Rural" & !is.na(healthTrain$bmi)),"bmi"])

mean(healthTrain[which(healthTrain$Residence_type == "Urban" & !is.na(healthTrain$bmi)),"bmi"])

mean(healthTrain[which(healthTrain$Residence_type == "Rural" & !is.na(healthTrain$bmi) & healthTrain$age <= 10),"bmi"])

nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age <= 10),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 10 & healthTrain$age <= 20),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 20 & healthTrain$age <= 30),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 30 & healthTrain$age <= 40),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 40 & healthTrain$age <= 50),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 50 & healthTrain$age <= 60),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 60 & healthTrain$age <= 70),])
nrow(healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 70),])

# fill NAs in bmi

healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age <= 10),"bmi"] = round(mean(healthTrain[which(healthTrain$age <= 10 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]),digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 10 & healthTrain$age <= 20),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 10 & healthTrain$age <= 20 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]), digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 20 & healthTrain$age <= 30),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 20 & healthTrain$age <= 30 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]), digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 30 & healthTrain$age <= 40),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 30 & healthTrain$age <= 40 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]), digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 40 & healthTrain$age <= 50),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 40 & healthTrain$age <= 50 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]), digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 50 & healthTrain$age <= 60),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 50 & healthTrain$age <= 60 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]), digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 60 & healthTrain$age <= 70),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 60 & healthTrain$age <= 70 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]), digits = 1)
healthTrain[which(is.na(healthTrain$bmi) & healthTrain$age > 70),"bmi"] = round(mean(healthTrain[which(healthTrain$age > 70 & !is.na(healthTrain$bmi) & !healthTrain$bmi>60),"bmi"]),digits = 1)
anyNA(healthTrain$bmi)
# fill NAs in smoking status

# for children of age <=12, replace NAs with never smoked
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age <= 12),"smoking_status"] = "never smoked"

# for others, replace NAs with freq smoking behavior of corresponding age bins
ux<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 12 & healthTrain$age <= 20),"smoking_status"])
ux[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 12 & healthTrain$age <= 20),"smoking_status"], ux)))]
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 12 & healthTrain$age <= 20),"smoking_status"] = ux[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 12 & healthTrain$age <= 20),"smoking_status"], ux)))]

ux1<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 20 & healthTrain$age <= 30),"smoking_status"])
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 20 & healthTrain$age <= 30),"smoking_status"] = ux1[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 20 & healthTrain$age <= 30),"smoking_status"], ux1)))]

ux2<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 30 & healthTrain$age <= 40),"smoking_status"])
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 30 & healthTrain$age <= 40),"smoking_status"] = ux2[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 30 & healthTrain$age <= 40),"smoking_status"], ux2)))]

ux3<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 40 & healthTrain$age <= 50),"smoking_status"])
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 40 & healthTrain$age <= 50),"smoking_status"] = ux3[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 40 & healthTrain$age <= 50),"smoking_status"], ux3)))]

ux4<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 50 & healthTrain$age <= 60),"smoking_status"])
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 50 & healthTrain$age <= 60),"smoking_status"] = ux4[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 50 & healthTrain$age <= 60),"smoking_status"], ux4)))]

ux5<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 60 & healthTrain$age <= 70),"smoking_status"])
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 60 & healthTrain$age <= 70),"smoking_status"] = ux5[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 60 & healthTrain$age <= 70),"smoking_status"], ux5)))]

ux6<-unique(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 70),"smoking_status"])
healthTrain[which(is.na(healthTrain$smoking_status) & healthTrain$age > 70),"smoking_status"] = ux6[which.max(tabulate(match(healthTrain[which(!is.na(healthTrain$smoking_status) & healthTrain$age > 70),"smoking_status"], ux6)))]

anyNA(healthTrain$smoking_status)

healthTrain$stroke <- as.factor(healthTrain$stroke)
healthTrain$hypertension <- as.factor(healthTrain$hypertension)
healthTrain$heart_disease <- as.factor(healthTrain$heart_disease)

names(healthTrain)
str(healthTrain)

logMod2 <- glm(stroke~gender+age+hypertension+heart_disease+avg_glucose_level+smoking_status,data=healthTrain,family = "binomial")
summary(logMod2)

#########Test Data##########
anyNA(healthTest$gender)
anyNA(healthTest$age)
anyNA(healthTest$hypertension)
anyNA(healthTest$heart_disease)
anyNA(healthTest$ever_married)
anyNA(healthTest$work_type)
anyNA(healthTest$Residence_type)
anyNA(healthTest$avg_glucose_level)
anyNA(healthTest$bmi)
anyNA(healthTest$smoking_status)
levels(healthTest$smoking_status)


# fill NAs in bmi

healthTest[which(is.na(healthTest$bmi) & healthTest$age <= 10),"bmi"] = round(mean(healthTest[which(healthTest$age <= 10 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]),digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 10 & healthTest$age <= 20),"bmi"] = round(mean(healthTest[which(healthTest$age > 10 & healthTest$age <= 20 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]), digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 20 & healthTest$age <= 30),"bmi"] = round(mean(healthTest[which(healthTest$age > 20 & healthTest$age <= 30 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]), digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 30 & healthTest$age <= 40),"bmi"] = round(mean(healthTest[which(healthTest$age > 30 & healthTest$age <= 40 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]), digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 40 & healthTest$age <= 50),"bmi"] = round(mean(healthTest[which(healthTest$age > 40 & healthTest$age <= 50 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]), digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 50 & healthTest$age <= 60),"bmi"] = round(mean(healthTest[which(healthTest$age > 50 & healthTest$age <= 60 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]), digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 60 & healthTest$age <= 70),"bmi"] = round(mean(healthTest[which(healthTest$age > 60 & healthTest$age <= 70 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]), digits = 1)
healthTest[which(is.na(healthTest$bmi) & healthTest$age > 70),"bmi"] = round(mean(healthTest[which(healthTest$age > 70 & !is.na(healthTest$bmi) & !healthTest$bmi>60),"bmi"]),digits = 1)
anyNA(healthTest$bmi)
# fill NAs in smoking status

healthTest[which(healthTest$smoking_status == ""),"smoking_status"] <-NA

# for children of age <=12, replace NAs with never smoked
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age <= 12),"smoking_status"] = "never smoked"

# for others, replace NAs with freq smoking behavior of corresponding age bins
ux<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 12 & healthTest$age <= 20),"smoking_status"])
ux[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 12 & healthTest$age <= 20),"smoking_status"], ux)))]
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 12 & healthTest$age <= 20),"smoking_status"] = ux[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 12 & healthTest$age <= 20),"smoking_status"], ux)))]

ux1<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 20 & healthTest$age <= 30),"smoking_status"])
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 20 & healthTest$age <= 30),"smoking_status"] = ux1[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 20 & healthTest$age <= 30),"smoking_status"], ux1)))]

ux2<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 30 & healthTest$age <= 40),"smoking_status"])
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 30 & healthTest$age <= 40),"smoking_status"] = ux2[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 30 & healthTest$age <= 40),"smoking_status"], ux2)))]

ux3<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 40 & healthTest$age <= 50),"smoking_status"])
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 40 & healthTest$age <= 50),"smoking_status"] = ux3[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 40 & healthTest$age <= 50),"smoking_status"], ux3)))]

ux4<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 50 & healthTest$age <= 60),"smoking_status"])
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 50 & healthTest$age <= 60),"smoking_status"] = ux4[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 50 & healthTest$age <= 60),"smoking_status"], ux4)))]

ux5<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 60 & healthTest$age <= 70),"smoking_status"])
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 60 & healthTest$age <= 70),"smoking_status"] = ux5[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 60 & healthTest$age <= 70),"smoking_status"], ux5)))]

ux6<-unique(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 70),"smoking_status"])
healthTest[which(is.na(healthTest$smoking_status) & healthTest$age > 70),"smoking_status"] = ux6[which.max(tabulate(match(healthTest[which(!is.na(healthTest$smoking_status) & healthTest$age > 70),"smoking_status"], ux6)))]

anyNA(healthTest$smoking_status)

nrow(healthTest[which(is.na(healthTest$smoking_status)),])

healthTest$hypertension <- as.factor(healthTest$hypertension)
healthTest$heart_disease <- as.factor(healthTest$heart_disease)

healthTest$logPredProb2 <- predict(logMod2,healthTest,type = "resp")
write.csv(healthTest, file = "logPredTest2.csv")
