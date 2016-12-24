#Course : CS 513
#Vandna Yadav


rm(list=ls())
#********************loading the dataset**************************
data<-read.csv("C:/Users/vandna/Desktop/Stevens/SEM 2/513/Project/Code/dataset/2008.csv")
odataset<-data
attach(odataset)
#********************categorizing Departure Delay *****************
odataset$DepDelay_cat[DepDelay < -5] <- "EARLY"
odataset$DepDelay_cat[DepDelay >= -5 & DepDelay <= 5] <- "ONTIME"
odataset$DepDelay_cat[DepDelay > 5] <- "LATE"
#********************categorizing Arrival Delay*******************
odataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
odataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
odataset$ArrDelay_cat[ArrDelay >  10]<-"LATE"
#***********creating a data frame
filtereddataset <- data.frame(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)
detach (odataset)
attach(filtereddataset)
#we are filtering out the flights which are cancelled
filtereddataset<-filtereddataset[filtereddataset[,"Cancelled"]==0,]
#we are filtering out the flights which are Diverted
filtereddataset<-filtereddataset[filtereddataset[,"Diverted"]==0,]
#we are filtering out the flights which are latedeparture
filtereddataset<-filtereddataset[filtereddataset[,"DepDelay"]>=5,]
#*****we are taking 3 highest freq unique carrier
filtereddataset<-filtereddataset[filtereddataset$UniqueCarrier =="WN" | filtereddataset$UniqueCarrier =="OO" | filtereddataset$UniqueCarrier =="AA", ] 
#*****we are taking 3 highest freq origin
filtereddataset<-filtereddataset[filtereddataset$Origin =="ATL" | filtereddataset$Origin =="ORD" | filtereddataset$Origin =="LAX", ] 
#removing arrival time NA
filtereddataset<-filtereddataset[complete.cases(filtereddataset[,6]),]
attach(filtereddataset)
library(plyr)
data<-join(filtereddataset,count(filtereddataset,'Dest'))
attach(filtereddataset)
#*******categoraizing dest type according to the num of flights
filtereddataset$Dest_Type[data$freq > 500 & data$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[data$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[data$freq >1000] <- "High Busy"
detach(odataset)
#************normalization  & removal of na values functions ***********
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }
data<-filtereddataset
data_new<-cbind(Month=mmnorm(data$Month),
                DayOfMonth=mmnorm(data$DayofMonth),
                DayOfWeek=mmnorm(data$DayOfWeek),
                CRSDepTime=mmnorm(data$CRSDepTime),
                CRSArrTime=mmnorm(data$CRSArrTime) ,
                UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
                Origin=mmnorm(as.numeric(factor(data$Origin))),
                Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
                ArrDelay_cat=as.character(data$ArrDelay_cat)
)
#*****taking 5000 entries*******
idx1<-seq(1:5000)
data_new<-data_new[idx1,]
#****sampling data**************
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
#****training  & test dataset***********
training<-data5,]
test<-data_new[-idx,]
library(class)
####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.
for (j in 1:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-9],test[,-9],training[,9],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,9]!=newresults[,10]
    rate<-sum(wrong)/length(wrong)
     rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  print(j)
  avg=total/counter
  print(avg)
}
######################
#******applying knn***************
newpredict<-knn(training[,-9],test[,-9],training[,9],k=30)
newresults<-cbind(test,as.character(newpredict) )
head(newresults)
table(newresults[,9],newresults[,10])
#############################################################################

#####################-----------KKnn---------------------

rm(list=ls())
library(kknn)
?kknn
#normalization and na removal functions
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }
#loading dataset
filtereddataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/Code/dataset/new4.csv")
attach(filtereddataset)
#categorizing Arrival delay
filtereddataset$ArrDelay_catnew[ArrDelay < -5] <- "Early"
filtereddataset$ArrDelay_catnew[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
filtereddataset$ArrDelay_catnew[ArrDelay >  5 & ArrDelay <  20] <- "late"
filtereddataset$ArrDelay_catnew[ArrDelay >=  20] <- "VERY_late"
data<-filtereddataset
data_new_k<-cbind(
  DayOfMonth=mmnorm(data$DayofMonth),
  DayOfWeek=mmnorm(data$DayOfWeek),
  CRSDepTime=mmnorm(data$CRSDepTime),
  CRSArrTime=mmnorm(data$CRSArrTime) ,
  UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
  Origin=mmnorm(as.numeric(factor(data$Origin))),
  Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
  ArrDelay_cat=as.character(data$ArrDelay_catnew)
)
data_new_k<-as.data.frame(data_new_k)
data_new_k<-na.omit(data_new_k)
factor(data_new_k$ArrDelay_cat)
is.data.frame(data_new_k)
idx1<-seq(1:5000)
data_new_k<-data_new_k[idx1,]
idx<-sample(nrow(data_new_k),as.integer(.70*nrow(data_new_k)))
trainingk<-data_new_k[idx,]
testk<-data_new_k[-idx,]
is.data.frame(trainingk)
#applying kknn
predict_1 <- kknn(formula=ArrDelay_cat~., trainingk, testk, k=38,kernel="optimal")
head(predict_1)
fitWalc <- fitted(predict_1)
results <- cbind(testk$ArrDelay_cat, fitWalc)
wrong <- results[,1]!=results[,2]
rateWalc <- sum(wrong)/length(wrong)
rateWalc

