#Course : CS 513
#Vandna Yadav

rm(list=ls())
#******loading dataset
data<-read.csv("C:/Users/vandna/Desktop/Stevens/SEM 2/513/Project/Code/dataset/2008.csv")
odataset<-data
attach(odataset)
odataset$DepDelay_cat[DepDelay < -5] <- "EARLY"
odataset$DepDelay_cat[DepDelay >= -5 & DepDelay <= 5] <- "ONTIME"
odataset$DepDelay_cat[DepDelay > 5] <- "LATE"
odataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
odataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
odataset$ArrDelay_cat[ArrDelay >  10]<-"LATE"
filtereddataset <- data.frame(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)
detach (odataset)
attach(filtereddataset)
#departure delay
#Diverted
#we are filtering out the flights which are cancelled
filtereddataset<-filtereddataset[filtereddataset[,"Cancelled"]==0,]
#we are filtering out the flights which are Diverted
filtereddataset<-filtereddataset[filtereddataset[,"Diverted"]==0,]
#we are filtering out the flights which are latedeparture
filtereddataset<-filtereddataset[filtereddataset[,"DepDelay"]>=5,]
filtereddataset<-filtereddataset[filtereddataset$UniqueCarrier =="WN" | filtereddataset$UniqueCarrier =="OO" | filtereddataset$UniqueCarrier =="AA", ] 
filtereddataset<-filtereddataset[filtereddataset$Origin =="ATL" | filtereddataset$Origin =="ORD" | filtereddataset$Origin =="LAX", ] 
#removing arrival time NA
filtereddataset<-filtereddataset[complete.cases(filtereddataset[,6]),]
attach(filtereddataset)
library(plyr)
data<-join(filtereddataset,count(filtereddataset,'Dest'))
attach(filtereddataset)
filtereddataset$Dest_Type[data$freq > 500 & data$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[data$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[data$freq >1000] <- "High Busy"
detach(odataset)
summary(filtereddataset)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }
data<-filtereddataset
data_new<-cbind(DayOfWeek=data$DayOfWeek,
                CRSDepTime=data$CRSDepTime,
                CRSArrTime=data$CRSArrTime,
                UniqueCarrier=as.numeric(factor(data$UniqueCarrier)),
                Origin=as.numeric(factor(data$Origin)),
                Dest=as.numeric(factor(data$Dest_Type)),
                DepDelay=mmnorm(data$DepDelay),
                ArrDelay_cat=as.numeric(factor(data$ArrDelay_catnew))
)
data_new<-as.data.frame (data_new)
data_new<-na.omit(data_new)
factor(data_new$ArrDelay_cat)
is.data.frame(data_new)
idx1<-seq(1:100)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]
is.data.frame(training)
if(!require(neuralnet)) {
  install.packages("neuralnet"); require(neuralnet)}
rate<-0
wrong<-0
net.ArrDelay <- neuralnet(ArrDelay_cat~DayOfWeek+CRSDepTime+CRSArrTime+
                            UniqueCarrier+Origin+Dest+DepDelay,
                          training,hidden=5, threshold=0.01,stepmax=1e6)
#to plot the neuralnet
plot(net.ArrDelay)
net.result1 <- compute(net.ArrDelay, subset(test, select=-ArrDelay_cat))
fit <- round(net.result1$net.result, digits = 0)
results <- cbind(test$ArrDelay_cat, fit)
wrong <- results[,1]!=results[,2]
rate <- sum(wrong)/length(wrong)
rate
