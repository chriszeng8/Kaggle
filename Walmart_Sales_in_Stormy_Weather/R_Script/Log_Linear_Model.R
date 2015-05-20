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
sales<-matrix(rep(0),nrow=111,ncol=45)

# predict by store by item
# sink('analysis-output.txt', append=TRUE)
RMSE<-c()
ListIndex<-1
ModelList<-list()

for (j in 1:45){
     storeNo <-j
     stationNo <- key[key$store_nbr==storeNo,2]
     for (i in  1:111){
          itemNo <- i
          print(paste("hello:", i, j))
          train_sales <- subset(training,(training$store_nbr == storeNo)&(training$item_nbr == itemNo))
          if (!all(train_sales$units==0)){
               train_weather <- weather[(weather$date %in% train_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6,-21)]
               train<-cbind(units=train_sales$units,train_weather)
               sales[i,j] <- ListIndex
               min.model <-glm(units~1, data=train)
               biggest <- formula(glm(units~., data=train))
               linear_model <- step(min.model, direction='forward', scope=biggest)
               selected_model<-glm(linear_model$formula,data=train)
               
               ModelList[[length(ModelList)+1]] <- list(selected_model)
               #pred_sales<-round(exp(predict(selected_model, newdata = valid)),0)
               #RMSE <- c(RMSE,sqrt(sum((log(pred_sales+1)-log(valid$units+1))^2)/length(pred_sales)))
               #print(paste("station ",j, "item ",i))
               #flush.console()
               #print(summary(poisson_model))
               #flush.console()
               ListIndex=ListIndex+1
          }
     }
}


rm(weather)
rm(training)
rm(test)
rm(key)

#save.image("/Users/apple1/Google Drive/Data Mining Project/RData_images/LinearRegression.RData")
#load("/Users/apple1/Google Drive/Data Mining Project/RData_images/LinearRegression.RData")

key<- read.csv("key.csv")
weather<-read.csv("/Users/apple1/Google Drive/Data Mining Project/ProcessedData/processed_weather.csv")
test<-read.csv("joint_testwithkey.csv")

test<-test[order(test$date),]
test_weather <- weather[(weather$date %in% test$date),]
test$units<-0
for (m in 1:max(sales)) {
     index<-which(sales==m,arr.ind=TRUE)
     item_n<-index[1]
     store_n<-index[2]
     stationNo <- key[key$store_nbr==store_n,2]
     subsetTestIndex<-(test$item_nbr==item_n)&(test$store_nbr==store_n)
     subsetWeatherIndex<-(test_weather$station_nbr==stationNo)&(test_weather$date %in% test[subsetTestIndex,]$date)
     ModelListIndex<-sales[item_n,store_n]
     nrow(test_weather[subsetWeatherIndex,])
     nrow(test[subsetTestIndex,])
     test[subsetTestIndex,]$units<-round(exp(predict(ModelList[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,]))-1,0)
}
test$units[test$units<0]=0
write.csv(test,"test_3rdlinear.csv")
Result<-read.csv("test_3rdlinear.csv")
head(Result)


