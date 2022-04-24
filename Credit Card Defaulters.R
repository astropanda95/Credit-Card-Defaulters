
#Calling relevant libraries
library(tidyverse)
library(caret)
library(leaps)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(arm)
library(AER)
library(reshape2) 
library(tidyr) 
library(dplyr) 
library(broom)
library(grid)
library(MASS)
library(knitr) 
library(AUC)

#Calling the csv data to a dataframe
data<-read.csv("credit card default.csv")

str(data)

#Changing the format of data as per their data type
data$EDUCATION<-as.factor(data$EDUCATION)
data$MARRIAGE<-as.factor(data$MARRIAGE)
data$PAY_0<- as.factor(data$PAY_0)
data$PAY_2<- as.factor(data$PAY_2)
data$PAY_3<- as.factor(data$PAY_3)
data$PAY_4<- as.factor(data$PAY_4)
data$PAY_5<- as.factor(data$PAY_5)
data$PAY_6<- as.factor(data$PAY_6)
data$SEX<-as.factor(data$SEX)
data$default_payment_next_month<- as.factor(data$default_payment_next_month)

#Creating a subset to test the confusion matrix later on with the models
test_data<-dplyr::select(data,default_payment_next_month)




#Random forest with all the data
library(randomForest)
set.seed(1)
rf.data=randomForest(default_payment_next_month~.,data=data,importance=TRUE)
rf.data

#Model has 81.87% accuracy but has a high error rate in false negatives



varImpPlot(rf.data,main="Plots for important predictors for 
           the response variable")





#RF with the 8 most important data
set.seed(1)
rf.data.8=randomForest(default_payment_next_month~PAY_0+BILL_AMT3+BILL_AMT4+
                       PAY_2+BILL_AMT5+BILL_AMT6+BILL_AMT2+PAY_3,data=data,
                       importance=TRUE)
rf.data.8

#Model has 81.62% accuracy but has a high error rate in false negatives

#Building a multiple linear regression model with all the predictor variables 
#in the dataset
set.seed(1)
linear_model_all<-glm(default_payment_next_month~.,family = binomial,data=data)
summary(linear_model_all)
 
#Building a multiple linear regression model with the 8 most important
#predictor variables in the dataset
set.seed(1)
linear_model_8<-glm(default_payment_next_month~PAY_0+BILL_AMT3+BILL_AMT4+
                 PAY_2+BILL_AMT5+BILL_AMT6+BILL_AMT2+PAY_3,family=binomial,
                 data=data)
summary(linear_model_8)


#Predicting using the linear_model_all model
lm_pred_all<-predict(linear_model_all,data=data)
#Predicting using the linear_model_8 model
lm_pred_8<-predict(linear_model_8,data=data)


#Creating an empty variable for model1 to enter predictions, initializing 
#the data to 0
lm_model_pred=rep(0,30000)
#Creating an empty variable for model2 to enter predictions, initializing 
#the data to 0
lm_model_pred_8=rep(0,30000)


#Change the values to 1 if model probability>0
lm_model_pred[lm_pred_all>0] <- 1
#Change prediction to 1 if model probability>0
lm_model_pred_8[lm_pred_8>0] <- 1


#Getting the confusion matrix for model1
table(lm_model_pred,data$default_payment_next_month)

#Model has 82.19% accuracy and has much lesser error rate in false negatives
#campared to random forest model

#Getting the confusion matrix for model2
table(lm_model_pred_8,data$default_payment_next_month)
#Model has 82.07% accuracy and has much lesser error rate in false negatives
#campared to random forest model




