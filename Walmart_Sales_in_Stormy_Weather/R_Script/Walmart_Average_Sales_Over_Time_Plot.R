library(ggplot2)

# Exploratory Data Analysis
setwd("/Users/apple1/Google Drive/Data Mining Project/Data")
rm(list=ls())
Key<-read.csv('key.csv')
Train<-read.csv('train.csv')
Test<-read.csv('test.csv')
Weather<-read.csv('weather.csv')
str(Weather)


Train_len <- dim(Train)[1]
Test_len <- dim(Test)[1]
Test_len+Train_len
Weather_len <- dim(Weather)[1]
Weather_len*111

# Convert date to date format
Train_Var_Names<-names(Train)

Train$date<-as.Date(Train$date)
Train[1:10,]
hist(Train$store_nbr,breaks=45)

qplot(Train$store_nbr,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for store nbr", 
      xlab = "Age",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,45) )

qplot(Train$item_nbr,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for item_nbr", 
      xlab = "Age",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,45) )

Train$store_nbr[1:350]
Train$item_nbr[1:350]
Train$Store_item_nbr<-Train$store_nbr*1000+Train$item_nbr
Train$Store_item_nbr[1:3000]

y<-length(table(Train$Store_item_nbr))
plot(y)

# Split the data into items ()
Train_item_split<-split(Train,f=Train$item_nbr)
# Double check that the Data across all items have the same number of dates
Date<-unique(as.data.frame(Train_item_split[[1]])$date)

Sales_split_by_items<-as.data.frame(matrix(NA,nrow=length(Date),ncol=112))
Sales_split_by_items[,1]<-Date

for (i in 1:111) {
     TempDataFrame<-as.data.frame(Train_item_split[[i]])
     class(TempDataFrame$date)
     Sales_split_by_items[,i+1]<-tapply(TempDataFrame$units,TempDataFrame$date,mean)
}

# Scale the scales of 111 items from 0 to 1
Scaled_Sales_split_by_items <- Sales_split_by_items
for (i in 1:111) {
     Scaled_Sales_split_by_items[,i+1] <- Scaled_Sales_split_by_items[,i+1]/max(Scaled_Sales_split_by_items[,i+1])
}


jpeg("Plot3.jpeg", width = 15, height = 40, units = 'in', res = 800)
par(mfrow=c(28,4))
par(mar=c(1,1,1,1))
for (i in 1:111) {
     plot(Scaled_Sales_split_by_items[,1],Scaled_Sales_split_by_items[,i+1],type='l',main=paste("item",i,sep=""))
}
dev.off()

