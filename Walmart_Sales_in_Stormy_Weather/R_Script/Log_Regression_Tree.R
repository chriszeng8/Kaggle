# Clear all variables
rm(list=ls())
setwd("/Users/apple1/Google Drive/Data Mining Project/Data")
weather<-read.csv("/Users/apple1/Google Drive/Data Mining Project/ProcessedData/processed_weather.csv")
training<-read.csv("joint_trainwithkey.csv")
test<-read.csv("joint_testwithkey.csv")
key<- read.csv("key.csv")
training<-training[order(training$date),]
# Log Transformation
training$units<-log(training$units+1)

# set up test data, and initialize predictions as zeros
test<-test[order(test$date),]
test_weather <- weather[(weather$date %in% test$date),]
test$units<-0

for (j in 1:45){
     storeNo <-j
     stationNo <- key[key$store_nbr==storeNo,2]     

     for (i in  1:111){
          print(paste("Current i: ", i,"j: ", j))         
          itemNo <- i
               # subset training data 
          train_sales <- subset(training,(training$store_nbr == storeNo)&(training$item_nbr == itemNo))
          # if not all training data has zero units          
          # modification here can be: igonore only if 0 data is greater than 95%
          if (sum(train_sales$units==0)/length(train_sales$units)<0.05) {   
          #if (!all(train_sales$units==0)){
               train_weather <- weather[(weather$date %in% train_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6,-21)]
               train<-cbind(units=train_sales$units,train_weather)
               train_weather <- weather[(weather$date %in% train_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6,-21)]
               train<-cbind(units=train_sales$units,train_weather)

               # a. ===== Find the test data corresponding to the store-item combination from the test file ====
               # index of the test data
               subsetTestIndex <- which((test$store_nbr==storeNo)&(test$item_nbr==itemNo))
               # extracted the test data
               Extracted_Test<-test[subsetTestIndex,]
               subsetWeatherIndex<-(test_weather$station_nbr==stationNo)&(test_weather$date %in% test[subsetTestIndex,]$date)
               test_data<-test_weather[subsetWeatherIndex,]
               
               DrawTime=500
               # row: number of test data, column: number of draws
               Predictmatrix=data.frame(matrix(nrow=nrow(test_data),ncol=DrawTime))
               for (i in 1:DrawTime) {
                    SampleIndx<-sample(1:nrow(train),nrow(train),replace=T)
                    subtrain=train[SampleIndx,]
                    tree = rpart(units ~ ., data=subtrain)
                    # Make predictions
                    tree.pred = predict(tree, newdata=test_data)
                    Predictmatrix[,i]=tree.pred
               }
               # aggregating results from multipl regression trees
               tree.pred<-apply(Predictmatrix,1,mean)
               # fill the output prediction
               test$units[subsetTestIndex]=round(exp(tree.pred)-1,0)
          }
     }
}



test$units[test$units<0]=0
write.csv(test,"test_1st_RegressionTree.csv")

Result<-read.csv("test_1st_RegressionTree.csv")
head(Result)


save.image("/Users/apple1/Google Drive/Data Mining Project/RData_images/RegressionTree.RData")

# post processing
Subtest<-read.csv("test.csv")
Data<-test
Pred<-Data[,c(3,2,4,6)]
     head(Pred)
     library("sqldf")
     FinalTest<-sqldf("SELECT t.date, t.store_nbr, t.item_nbr, p.units
FROM Subtest t, Pred p
WHERE p.date=t.date AND p.store_nbr=t.store_nbr AND p.item_nbr=t.item_nbr")
     FinalTest$id<-paste(FinalTest$store_nbr,FinalTest$item_nbr,FinalTest$date,sep="_")
     FinalTest<-FinalTest[,c(5,4)]
     
     write.csv(FinalTest,"attempt3_regression_tree.csv",row.names=F)
     ReadResults1<-read.csv("attempt3_regression_tree.csv")
     
     
     