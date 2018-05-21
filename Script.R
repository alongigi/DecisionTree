#rm(list=ls())

#lodaing the xlsx file
if(!require("readxl")) install.packages("readxl")
library(readxl)

GermanCredit <- read_excel("GermanCredit.xlsx",col_names = FALSE)

#transforming the data to matrix
x<-t(GermanCredit)

#Since the each metadata is seperated with "," then we splitted it with ",".Empty metadata is replaced with NA.
myToNumbers <- function(index) {
  tmp <- c()
  
  for (data in strsplit(x[2,index], "[,]")[[1]]) {
    if(data == "") {
      tmp <- c(tmp, c(NA))
    } else {
      tmp <- c(tmp, c(as.numeric(data)))
    }
  }
  return(tmp)
}

#Since the each metadata is seperated with "," then we splitted it with ",".Empty metadata is replaced with NA.
myToString <- function(index) {
  
  return(c(strsplit(x[2,index], "[,]")[[1]]))
  
}

#Using the functions above.
over_draft<-myToNumbers(1)
credit_usage<-myToNumbers(2)
credit_history<-myToString(3)
purpose<-myToString(4)
current_balance<-myToNumbers(5)
Average_Credit_Balance<-myToNumbers(6)
personal_status<-myToString(7)
property_magnitude<-myToString(8)
cc_age<-myToNumbers(9)
housing<-myToString(10)
job<-myToString(11)
num_dependents<-myToNumbers(12)
class<-myToString(13)

#Trnsforming the string to factor.
credit_history<-factor(credit_history)
purpose<-factor(purpose)
personal_status<-factor(personal_status)
property_magnitude<-factor(property_magnitude)
housing<-factor(housing)
job<-factor(job)
class<-factor(class)

#Creating data frame from our data.
data<-data.frame(over_draft, credit_usage, credit_history, purpose, current_balance, Average_Credit_Balance, personal_status, property_magnitude, cc_age, housing, job, num_dependents, class)

#Replacing the NA with mean of the numbers
data$over_draft[which(is.na(data$over_draft))] <- mean(over_draft, na.rm = TRUE)
data$credit_usage[which(is.na(data$credit_usage))] <- mean(credit_usage, na.rm = TRUE)
data$current_balance[which(is.na(data$current_balance))] <- mean(current_balance, na.rm = TRUE)
data$Average_Credit_Balance[which(is.na(data$Average_Credit_Balance))] <- mean(Average_Credit_Balance, na.rm = TRUE)
data$cc_age[which(is.na(data$cc_age))] <- mean(cc_age, na.rm = TRUE)
data$num_dependents[which(is.na(data$num_dependents))] <- mean(num_dependents, na.rm = TRUE)

#Replacing the NA with most frequency metadata
most_freq <- names(sort(summary(as.factor(credit_history)), decreasing=T)[1:1])
data$credit_history[which(is.na(data$credit_history))] <- most_freq
most_freq <- names(sort(summary(as.factor(purpose)), decreasing=T)[1:1])
data$purpose[which(is.na(data$purpose))] <- most_freq
most_freq <- names(sort(summary(as.factor(personal_status)), decreasing=T)[1:1])
data$personal_status[which(is.na(data$personal_status))] <- most_freq
most_freq <- names(sort(summary(as.factor(property_magnitude)), decreasing=T)[1:1])
data$property_magnitude[which(is.na(data$property_magnitude))] <- most_freq
most_freq <- names(sort(summary(as.factor(housing)), decreasing=T)[1:1])
data$housing[which(is.na(data$housing))] <- most_freq
most_freq <- names(sort(summary(as.factor(job)), decreasing=T)[1:1])
data$job[which(is.na(data$job))] <- most_freq
most_freq <- names(sort(summary(as.factor(class)), decreasing=T)[1:1])
data$class[which(is.na(data$class))] <- most_freq

tmp <- c()

#Using Equal-frequency  discritization with 5 bins. The intervals are based on the following code - 
#install.packages("classInt")
#library("classInt")
# classIntervals(data$cc_age, 4, style = 'quantile')
for (info in data$Average_Credit_Balance) {
  if(info >= 1587.4 & info <= 1998) {
    tmp <- c(tmp, c(mean(c(1587.4, 1998))))
  } else if (info >= 1195 & info <= 1587.4) {
    tmp <- c(tmp, c(mean(c(1195, 1587.4))))
  } else if (info >= 775.6 & info <= 1195) {
    tmp <- c(tmp, c(mean(c(775.6, 1195))))
  } else if (info >= 381.6 & info <= 775.6) {
    tmp <- c(tmp, c(mean(c(381.6, 775.6))))
  } else {
    tmp <- c(tmp, c(mean(c(4, 381.6))))
  }
}

data$Average_Credit_Balance <- tmp

tmp <- c()

#Using Equal-frequency  discritization with 5 bins. The intervals are based on the following code - 
#install.packages("classInt")
#library("classInt")
# classIntervals(data$cc_age, 4, style = 'quantile')
for (info in data$over_draft) {
  if(info >= 245 & info <= 300) {
    tmp <- c(tmp, c(mean(c(245, 300))))
  } else if (info >= 180 & info <= 245) {
    tmp <- c(tmp, c(mean(c(180, 245))))
  } else if (info >= 118 & info <= 180) {
    tmp <- c(tmp, c(mean(c(118, 180))))
  } else if (info >= 62 & info <= 118) {
    tmp <- c(tmp, c(mean(c(62, 118))))
  } else {
    tmp <- c(tmp, c(mean(c(0, 62))))
  }
}

data$over_draft <- tmp

tmp <- c()

#Using Equal-frequency  discritization with 5 bins. The intervals are based on the following code - 
#install.packages("classInt")
#library("classInt")
# classIntervals(data$cc_age, 4, style = 'quantile')
for (info in data$cc_age) {
  if(info >= 44.2 & info <= 75) {
    tmp <- c(tmp, c(mean(c(44.2, 75))))
  } else if (info >= 42 & info <= 44.2) {
    tmp <- c(tmp, c(mean(c(42, 44.2))))
  } else if (info >= 30 & info <= 36) {
    tmp <- c(tmp, c(mean(c(30, 36))))
  } else if (info >= 26 & info <= 30) {
    tmp <- c(tmp, c(mean(c(26, 30))))
  } else {
    tmp <- c(tmp, c(mean(c(19, 26))))
  }
}

data$cc_age <- tmp

tmp <- c()

#Using Equal-frequency  discritization with 5 bins. The intervals are based on the following code - 
#install.packages("classInt")
#library("classInt")
# classIntervals(data$cc_age, 4, style = 'quantile')
for (info in data$credit_usage) {
  if(info >= 30 & info <= 72) {
    tmp <- c(tmp, c(mean(c(30, 72))))
  } else if (info >= 24 & info <= 30) {
    tmp <- c(tmp, c(mean(c(24, 30))))
  } else if (info >= 15 & info <= 24) {
    tmp <- c(tmp, c(mean(c(15, 24))))
  } else if (info >= 12 & info <= 15) {
    tmp <- c(tmp, c(mean(c(12, 15))))
  } else {
    tmp <- c(tmp, c(mean(c(4, 12))))
  }
}

data$credit_usage <- tmp

tmp <- c()

#Using Equal-frequency  discritization with 5 bins. The intervals are based on the following code - 
#install.packages("classInt")
#library("classInt")
# classIntervals(data$cc_age, 4, style = 'quantile')
for (info in data$current_balance) {
  if(info >= 4720 & info <= 18424) {
    tmp <- c(tmp, c(mean(c(4720, 18424))))
  } else if (info >= 2852.4 & info <= 4720) {
    tmp <- c(tmp, c(mean(c(2852.4, 4720))))
  } else if (info >= 1906.8 & info <= 2852.4) {
    tmp <- c(tmp, c(mean(c(1906.8, 2852.4))))
  } else if (info >= 1262 & info <= 1906.8) {
    tmp <- c(tmp, c(mean(c(1262, 1906.8))))
  } else {
    tmp <- c(tmp, c(mean(c(250, 1262))))
  }
}

data$current_balance <- tmp

#Third part of the task. 
#Sample Indexes
indexes = sample(1:nrow(data), size=0.2*nrow(data))

#Creating testing set and training set.
testSet = data[indexes,]
trainSet = data[-indexes,]

if(!require("caret")) install.packages("caret")
library(caret)
if(!require("e1071")) install.packages("e1071")
library(e1071)
if(!require("rpart")) install.packages("rpart")
library(rpart)
if(!require("RColorBrewer")) install.packages("RColorBrewer")
library(RColorBrewer)
if(!require("GTK+")) install.packages("GTK+")
if(!require("rattle"))install.packages("rattle", dependencies = T)
if(!require("randomForest")) install.packages("randomForest", dependencies = T)
library(randomForest)

#training the test set.
ControlParameters <- trainControl(method="cv", number=5, savePredictions = TRUE, classProbs = TRUE)
parameterGrid <-expand.grid(mtry=c(2,3,4))
if(!require("rpart")) install.packages("randomForest")
modelRandom <- train(class~., data=trainSet, method="rf", trControl=ControlParameters, tuneGrid=parameterGrid)
prediction<-predict(modelRandom, testSet)
t<-table(prediction=prediction, actual=testSet$class)

#Using gini
gini1<-rpart(class~., trainSet, method="class", minsplit=20)
fancyRpartPlot(gini1)

#Using gini
gini2<-rpart(class~., trainSet, method="class", minsplit=50)
fancyRpartPlot(gini2)

#Using gini
gini3<-rpart(class~., trainSet, method="class", minsplit=100)
fancyRpartPlot(gini3)

#Using Information Gain
IG1<-rpart(class~., trainSet,parms=list(split="information"), minsplit=20)
fancyRpartPlot(IG1)

#Using Information Gain
IG2<-rpart(class~., trainSet,parms=list(split="information"), minsplit=50)
fancyRpartPlot(IG2)

#Using Information Gain
IG3<-rpart(class~., trainSet,parms=list(split="information"), minsplit=100)
fancyRpartPlot(IG3)

#predicting the results by using gini1
NbPredict1<-predict(gini1, newdata=testSet, type="class")
CM1<-confusionMatrix(NbPredict1, reference = testSet$class)

#predicting the results by using gini2
NbPredict2<-predict(gini2, newdata=testSet, type="class")
CM2<-confusionMatrix(NbPredict2, reference = testSet$class)

#predicting the results by using gini3
NbPredict3<-predict(gini3, newdata=testSet, type="class")
CM3<-confusionMatrix(NbPredict3, reference = testSet$class)

#predicting the results by using IG1
NbPredict4<-predict(IG1, newdata=testSet, type="class")
CM4<-confusionMatrix(NbPredict4, reference = testSet$class)

#predicting the results by using IG2
NbPredict5<-predict(IG2, newdata=testSet, type="class")
CM5<-confusionMatrix(NbPredict5, reference = testSet$class)

#predicting the results by using IG3
NbPredict6<-predict(IG3, newdata=testSet, type="class")
CM6<-confusionMatrix(NbPredict6, reference = testSet$class)