setwd("/Users/apple1/Google Drive/Data Mining Project/Data")
weather<-read.csv("/Users/apple1/Google Drive/Data Mining Project/ProcessedData/processed_weather.csv")
training<-read.csv("joint_trainwithkey.csv")
test<-read.csv("joint_testwithkey.csv")
key<- read.csv("key.csv")

training<-training[order(training$date),]
sales<-matrix(rep(0),nrow=111,ncol=45)

# predict by store by item
# sink('analysis-output.txt', append=TRUE)
RMSE<-c()
for (j in 1:1){
  storeNo <-1
  stationNo <- key[key$store_nbr==storeNo,2]
  for (i in  1:111){
     j<-1
    i<-9
    itemNo <- i
    print(paste("hello:", i, j))

    train_sales <- subset(training,(training$store_nbr == storeNo)&(training$item_nbr == itemNo))
    train_weather <- weather[(weather$date %in% train_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6,-21)]
    index<- sample(c(1:nrow(train_sales)),size=round(0.8*nrow(train_sales),0),replace=FALSE)
    valid_sales <- train_sales[-index,]
    valid_weather<- train_weather[-index,]
    train_sales <- train_sales[index,]
    train_weather <- train_weather[index,]
    
    #test_sales <- subset(test,(test$store_nbr == storeNo)&(test$item_nbr == itemNo))
    #test_weather <- weather[(weather$date %in% test_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6,-21)]
    
    if (!all(train_sales$units==0)){
      sales[i,j] <- 1
      min.model <-glm(train_sales$units~1, data=train_weather,family=poisson())
      biggest <- formula(glm(train_sales$units~., data=train_weather,family=poisson()))
      poisson_model <- step(min.model, direction='forward', scope=biggest)
      names(poisson_model$coeff)
      
      ################
      SelectedValid<-data.frame(row.names=(1:(nrow(valid_weather))))
      for k in names(poisson_model$coeff) {
        if k %in% names(valid_weather)
        SelectedValid<-cbind(SelectedValid,eval(parse(text=paste("valid_weather$",k,sep=""))))
      }
      ################
      summary(poisson_model)
      print("step complete")
      pred_sales<-round(exp(predict(poisson_model, data = SelectedValid)),0)
      length(predict(poisson_model, data = valid_weather))
      dim(valid_weather)
      
      RMSE <- c(RMSE,sqrt(sum((log(pred_sales+1)-log(valid_sales$units+1))^2)/length(pred_sales)))
      #poisson_model<-glm(units~., data=train_station_item,family=poisson())
      print(paste("station ",j, "item ",i))
      print(summary(poisson_model))
      }
  }
}

#sink()
