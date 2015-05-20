setwd("/Users/emmama/Google Drive/Data Mining Project/Data")
# By Ling Cong Ma

#==============================================================================
# Import Data
#==============================================================================
weather<-read.csv("/Users/emmama/Google Drive/Data Mining Project/ProcessedData/processed_weather.csv")
training<-read.csv("joint_trainwithkey.csv")
test<-read.csv("joint_testwithkey.csv")
key<- read.csv("key.csv")
training<-training[order(training$date),]



#==============================================================================
# Initiation
#==============================================================================
#sales<-matrix(rep(0),nrow=111,ncol=45)  
RMSE<-matrix(rep(0),nrow=111,ncol=45)  
RMSE_PCA<-matrix(rep(0),nrow=111,ncol=45)  
RMSE_L1<-matrix(rep(0),nrow=111,ncol=45)  
RMSE_Step<-matrix(rep(0),nrow=111,ncol=45)  
lmRMSE<-matrix(rep(0),nrow=111,ncol=45)  
lmRMSE_PCA<-matrix(rep(0),nrow=111,ncol=45)  
lmRMSE_L1<-matrix(rep(0),nrow=111,ncol=45)  
lmRMSE_Step<-matrix(rep(0),nrow=111,ncol=45)  
ListIndex<-1
ModelList<-list()
ModelList_L1<-list()
lmModelList<-list()
lmModelList_L1<-list()

# Lots of items in stores have no sales in history at all
# Therefore we already have matrix "sales" only recording the #item&#store has sales
for (m in 2:max(sales)) {
  index<-which(sales==m,arr.ind=TRUE)
  storeNo <-index[2]
  stationNo <- key[key$store_nbr==storeNo,2]
  itemNo <- index[1]
  i<-itemNo
  j<-storeNo 
  print(paste("hello:", itemNo, storeNo))
  train_sales <- subset(training,(training$store_nbr == storeNo)&(training$item_nbr == itemNo))
  train_weather <- weather[(weather$date %in% train_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6)]
  train<-cbind(units=train_sales$units,train_weather)
  index_val<-sample(1:nrow(train), 0.3*round(nrow(train)),replace=F)
  validation_fold<-train[index_val,]
  train_fold<-train[-index_val,]
  y_true<-validation_fold[,1]
  y_train<-train_fold[,1]
  #==============================================================================
  # PCA
  #==============================================================================
  for (modeltype in 1:2){  
    RMSE_PCAselect<-c()
    pca = prcomp(train_fold[,c(-1)],center=T)
    validation_fold_pca<- as.matrix(validation_fold[,-1]) %*% as.matrix(pca$rotation)
    train_fold_pca<-as.matrix(train_fold[,-1])%*%as.matrix(pca$rotation)
    # Using validation fold to get the optimal column amount to be reserved 
    for (n_col in (1:20)){
      pca_select<-as.data.frame(subset(train_fold_pca,select=c(1:n_col)))       
      #Poisson Regression
      if(modeltype==1){
        selected_model<-glm(unlist(y_train)~.,data=pca_select,family=poisson())
        y_hat<-round(exp(predict(selected_model, newdata = as.data.frame(validation_fold_pca))),0)
      }
      #Linear Regression
      if(modeltype==2){
        selected_model<-lm(unlist(y_train)~.,data=pca_select)
        y_hat<-round(predict(selected_model, newdata = as.data.frame(validation_fold_pca)),0)
        y_hat<-as.matrix(y_hat)
        y_hat[y_hat<0,1]<-0
      }
      RMSE_PCAselect <- c(RMSE_PCAselect,sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat)))
    }
    if(modeltype==1){
      RMSE_PCA[i,j]<-min(RMSE_PCAselect)
    }
    if(modeltype==2){
      lmRMSE_PCA[i,j]<-min(RMSE_PCAselect)
    }
  }
  #==============================================================================
  # L1 Regulization
  #==============================================================================
  for (modeltype in 1:2){  
    #install.packages("ISLR")
    #install.packages(c('glmnet',"leaps","ISLR"))
    #library('glmnet')
    train_fold_L1<-as.matrix(train_fold[,c(-1)])
    validation_fold_L1<-validation_fold[,-1]
    y_train_L1<-as.matrix(y_train)
    #Poisson Regression
    if(modeltype==1){
      selected_model<-cv.glmnet(train_fold_L1,y_train,family="poisson",standardize=T,nfold=10)
      y_hat<-round(predict(selected_model,as.matrix(validation_fold_L1),s=0,type="response"),0)
      RMSE_L1[i,j] <- sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat))
      #save the model for test using
      ModelList_L1[[length(ModelList_L1)+1]] <- list(selected_model)
    }
    #Linear Regression
    if(modeltype==2){
      selected_model<-cv.glmnet(train_fold_L1,y_train,family="gaussian",standardize=T,nfold=10)
      y_hat<-round(predict(selected_model,as.matrix(validation_fold_L1),s=0,type="response"),0)
      y_hat<-as.matrix(y_hat)
      y_hat[y_hat<0,1]<-0
      lmRMSE_L1[i,j] <- sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat))
      #save the model for test using
      lmModelList_L1[[length(lmModelList_L1)+1]] <- list(selected_model)
    }    
  }
  #==============================================================================
  # Step Forward
  #==============================================================================    
  for (modeltype in 1:2){  
    #Poisson Regression
    if(modeltype==1){
      min.model <-glm(units~1, data=train_fold,family=poisson())
      biggest <- formula(glm(units~., data=train_fold,family=poisson()))
      poisson_model <- step(min.model, direction='forward', scope=biggest)
      selected_model<-glm(poisson_model$formula,data=train_fold,family=poisson())
      y_hat<-round(exp(predict(selected_model, newdata = validation_fold[,c(-1)])),0)
      RMSE_Step[i,j] <- sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat))
      #save the model for test using
      ModelList[[length(ModelList)+1]] <- list(selected_model)
    }
    #Linear Regression
    if(modeltype==2){
      min.model <-lm(units~1, data=train_fold)
      biggest <- formula(lm(units~., data=train_fold))
      poisson_model <- step(min.model, direction='forward', scope=biggest)
      selected_model<-lm(poisson_model,data=train_fold)
      y_hat<-round(predict(selected_model, newdata = validation_fold[,c(-1)]),0)
      y_hat<-as.matrix(y_hat)
      y_hat[y_hat<0,1]<-0
      lmRMSE_Step[i,j] <- sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat))
      #save the model for test using
      lmModelList[[length(lmModelList)+1]] <- list(selected_model)
    }
  }
  
  ListIndex=ListIndex+1
  RMSE[i,j]<-min(RMSE_Step[i,j],RMSE_L1[i,j],RMSE_PCA[i,j])
  lmRMSE[i,j]<-min(lmRMSE_Step[i,j],lmRMSE_L1[i,j],lmRMSE_PCA[i,j])
}

#==============================================================================
# Save the intermediate result
#==============================================================================
save(ModelList,file="ModelList.RData")
save(ModelList_L1,file="ModelList_L1.RData")
write.csv(sales,file="sales.csv")
write.csv(RMSE,file="RMSE.csv")
write.csv(RMSE_PCA,file="RMSE_PCA.csv")
write.csv(RMSE_L1,file="RMSE_L1.csv")
write.csv(RMSE_Step,file="RMSE_Step.csv")

save(lmModelList,file="lmModelList.RData")
save(lmModelList_L1,file="lmModelList_L1.RData")
#write.csv(sales,file="lmsales.csv")
write.csv(lmRMSE,file="lmRMSE.csv")
write.csv(lmRMSE_PCA,file="lmRMSE_PCA.csv")
write.csv(lmRMSE_L1,file="lmRMSE_L1.csv")
write.csv(lmRMSE_Step,file="lmRMSE_Step.csv")


#====================================================================
# test
#====================================================================
#test
test<-test[order(test$date),]
test_weather <- weather[(weather$date %in% test$date),]
test$units<-0
#test$units_poPCA<-0
#test$units_poL1<-0
#test$units_poStep<-0
#test$units_lmPCA<-0
#test$units_lmL1<-0
#test$units_lmStep<-0
#test$units_avg<-0
for (m in 2:max(sales)) {
  index<-which(sales==m,arr.ind=TRUE)
  item_n<-index[1]
  store_n<-index[2]
  stationNo <- key[key$store_nbr==store_n,2]
  subsetTestIndex<-(test$item_nbr==item_n)&(test$store_nbr==store_n)
  subsetWeatherIndex<-(test_weather$station_nbr==stationNo)&(test_weather$date %in% test[subsetTestIndex,]$date)
  ModelListIndex<-sales[item_n,store_n]-1
  ModelListIndex_L1<-sales[item_n,store_n]-1
  train_sales <- subset(training,(training$store_nbr == store_n)&(training$item_nbr == item_n))
  train_weather <- weather[(weather$date %in% train_sales$date)&(weather$station_nbr==stationNo),c(-1,-2,-3,-6)]
  train<-cbind(units=train_sales$units,train_weather)
  index_val<-sample(1:nrow(train), 0.3*round(nrow(train)),replace=F)
  validation_fold<-train[index_val,]
  train_fold<-train[-index_val,]
  y_true<-validation_fold[,1]
  y_train<-train_fold[,1]
  
  #Select the best model among (Poisson,Linear) *(PCA,L1,SteoForward)
  min_RMSE<-c(RMSE_PCA[item_n,store_n],RMSE_L1[item_n,store_n],RMSE_Step[item_n,store_n],lmRMSE_PCA[item_n,store_n],lmRMSE_L1[item_n,store_n],lmRMSE_Step[item_n,store_n])
  F_Model<-which.min(min_RMSE)
  
  if(nrow(test[subsetTestIndex,])>0){
  #==============================================================================
  # Poisson_PCA
  #==============================================================================
  #F_Model=1
  if(F_Model==1){
    RMSE_PCAselect<-c()
    pca = prcomp(train_fold[,c(-1)],center=T)
    validation_fold_pca<- as.matrix(validation_fold[,-1]) %*% as.matrix(pca$rotation)
    train_fold_pca<-as.matrix(train_fold[,-1])%*%as.matrix(pca$rotation)
    for (n_col in (1:15)){
      pca_select<-as.data.frame(subset(train_fold_pca,select=c(1:n_col)))       
      selected_model<-glm(unlist(y_train)~.,data=pca_select,family=poisson())
      y_hat<-round(exp(predict(selected_model, newdata = as.data.frame(validation_fold_pca))),0)
      RMSE_PCAselect <- c(RMSE_PCAselect,sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat)))
    }
    lmRMSE_PCA[i,j]<-min(RMSE_PCAselect)
    optimal_col<-which.min(RMSE_PCAselect)
    pca_select<-as.data.frame(subset(train_fold_pca,select=c(1:optimal_col)))
    selected_model<-glm(unlist(y_train)~.,data=pca_select,family=poisson())
    
    test_pca<- as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]) %*% as.matrix(pca$rotation)
    #test_pca_select<-as.data.frame(test_pca,select=c(1:optimal_col))
    test[subsetTestIndex,]$units<-round(exp(predict(selected_model, newdata = as.data.frame(test_pca))),0)
    #test[subsetTestIndex,]$units_poPCA<-round(exp(predict(selected_model, newdata = as.data.frame(test_pca))),0)
  } 
  #==============================================================================
  # Poisson_L1
  #==============================================================================
  #F_Model=2
  if(F_Model==2){
    test[subsetTestIndex,]$units<-round(predict(ModelList_L1[[ModelListIndex]][[1]],as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),s="lambda.min",type="response"),0)
    #test[subsetTestIndex,]$units_poL1<-round(predict(ModelList_L1[[ModelListIndex]][[1]],as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),s="lambda.min",type="response"),0)
    
  }
  #==============================================================================
  # Poisson_Step
  #==============================================================================
  #F_Model=3
  if(F_Model==3){
    test[subsetTestIndex,]$units<-round(exp(predict(ModelList[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)])),0)
    #test[subsetTestIndex,]$units_poStep<-round(exp(predict(ModelList[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)])),0)
  }
  #==============================================================================
  # Linear_PCA
  #==============================================================================
  #F_Model=4
  if(F_Model==4){
    RMSE_PCAselect<-c()
    pca = prcomp(train_fold[,c(-1)],center=T)
    validation_fold_pca<- as.matrix(validation_fold[,-1]) %*% as.matrix(pca$rotation)
    train_fold_pca<-as.matrix(train_fold[,-1])%*%as.matrix(pca$rotation)
    for (n_col in (1:15)){
      pca_select<-as.data.frame(subset(train_fold_pca,select=c(1:n_col)))       
      selected_model<-lm(unlist(y_train)~.,data=pca_select)
      y_hat<-round(predict(selected_model, newdata = as.data.frame(validation_fold_pca)),0)
      y_hat<-as.matrix(y_hat)
      y_hat[y_hat<0,1]<-0
      RMSE_PCAselect <- c(RMSE_PCAselect,sqrt(sum((log(y_hat+1)-log(y_true+1))^2)/length(y_hat)))
    }
    lmRMSE_PCA[i,j]<-min(RMSE_PCAselect)
    optimal_col<-which.min(RMSE_PCAselect)
    pca_select<-as.data.frame(subset(train_fold_pca,select=c(1:optimal_col)))
    selected_model<-lm(unlist(y_train)~.,data=pca_select)
    
    test_pca<- as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]) %*% as.matrix(pca$rotation)
    test[subsetTestIndex,]$units<-round(predict(selected_model, newdata = as.data.frame(test_pca)),0)
    #test[subsetTestIndex,]$units_lmPCA<-round(predict(selected_model, newdata = as.data.frame(test_pca)),0)
    
  }
  #==============================================================================
  # Linear_L1
  #==============================================================================
  #F_Model=5
  if(F_Model==5){
    test[subsetTestIndex,]$units<-round(predict(lmModelList_L1[[ModelListIndex]][[1]],as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),s="lambda.min",type="response"),0) 
    #test[subsetTestIndex,]$units_lmL1<-round(predict(lmModelList_L1[[ModelListIndex]][[1]],as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),s="lambda.min",type="response"),0) 
  }
  #==============================================================================
  # Linear_StepForward
  #==============================================================================
  #F_Model=6
  if(F_Model==6){
    test[subsetTestIndex,]$units<-round(predict(lmModelList[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),0)
    #test[subsetTestIndex,]$units_lmStep<-round(predict(lmModelList[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),0)
  }
  print(m)
}
}
test[test$units<0,6]<-0
#test[test$units_poPCA<0,7]<-0
#test[test$units_poL1<0,8]<-0
#test[test$units_poStep<0,9]<-0
#test[test$units_lmPCA<0,10]<-0
#test[test$units_lmL1<0,11]<-0
#test[test$units_lmStep<0,12]<-0
#test$units_avg<-round((test$units_poPCA+test$units_poL1+test$units_poStep+test$units_lmPCA+test$units_lmL1+test$units_lmStep)/6,0)
#test<-test[,-13]
write.csv(test,"test_bestmodel_F.csv")


test[subsetTestIndex,]$units<-round(exp(predict(ModelList[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,])),0)
test[subsetTestIndex,]$units<-round(exp(predict(ModelList_L1[[ModelListIndex_L1]][[1]],as.matrix(test_weather[subsetWeatherIndex,c(-1,-2,-3,-6)]),s="lambda.min",type="response")),0)
test[subsetTestIndex,]$units<-round(exp(predict(lmModelList_L1[[ModelListIndex]][[1]],newdata=test_weather[subsetWeatherIndex,])),0)
#text_1<-test[test$store_nbr==1,]
