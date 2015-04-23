# Chris Zeng
# April 21, 2015

rm(list=ls())
library(randomForest)
library(MASS)
library(caret)
library(gbm)
library(dplyr)
library(plyr)

setwd("/Users/apple1/Github_Repo/Kaggle/Forest_Cover_Type_Prediction")
TData<-read.csv("train.csv")
TestData<-read.csv("test.csv")
TestData<-rbind(TestData,TestData[1:2,])
table(TData$Cover_Type)
TestData$Cover_Type=as.numeric(c(1,2,3,4,5,6,7))
head(TestData)

NumRow<-nrow(TData) # number of records
NumCol<-ncol(TData)

#convert the categorical data to factors 
FactorColnNum<-12:56
for (i in FactorColnNum) {
     TData[,i]<-as.factor(TData[,i])
     TestData[,i]<-as.factor(TestData[,i])
     if(i!=56){
          levels(TData[,i]) <- c("0","1")
          levels(TestData[,i]) <- c("0","1")
          }
}

FullSampleIndices<-1:NumRow
# Split original into training and testing sets
TrainingVersusFullSample<-0.7 # 70% training, 30% testing

TrainSetIndices<-sample(FullSampleIndices,as.integer(NumRow*TrainingVersusFullSample),replace=F)
ValidationSetIndices<-FullSampleIndices[-TrainSetIndices]
TrainData<-TData[TrainSetIndices,]
ValidationData<-TData[ValidationSetIndices,]

# mtry: randomly select 5 attributes at a time
# tuneRF: Starting with the default value of mtry, search for the 
# optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest.
#a<-tuneRF(TData[,-1], TData$Cover_Type, mtryStart=1, ntreeTry=1001)
ForestCoverForest<- randomForest(Cover_Type ~ ., data = TData,mtry=28, ntree = 1001)
# ForestCoverForestValid<- randomForest(Cover_Type ~ ., data = TrainData, ntree = 1001)
# ValidationAct_y<-ValidationData[,NumCol]
# ValidationPred_y<-predict(ForestCoverForestValid,ValidationData,type="class") # predicted y label
# 
# confusionMatrix(unlist(ValidationPred_y),reference=ValidationAct_y)
# V_accuracy<- mean(confusionMatrix(unlist(ValidationPred_y),reference=ValidationAct_y)$byClass[,8])
# print (V_accuracy)

TestPred_y<-predict(ForestCoverForest,TestData,type="class")

TestNumRow<-nrow(TestData)
TestIndex<-TestData[,1]
Model1_Output<-as.data.frame(cbind(Id=TestIndex,Cover_Type=TestPred_y))
Model1_Output<-Model1_Output[-c(TestNumRow-1,TestNumRow),]
write.csv(Model1_Output,"Attemp11_mtry_28.csv",row.names=F)

# Gradient Boosting Method:
#gbmmodel<-train(Cover_Type~., data=TData, method="gbm", distribution="bernoulli")
#predict<-predict(gbmmodel,data=TData
