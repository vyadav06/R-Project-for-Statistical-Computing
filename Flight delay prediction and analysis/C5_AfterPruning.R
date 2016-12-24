#Course : CS 513
#Vandna Yadav


rm(list=ls())
library(plyr)
#defining the function for  normalization
mmnorm<-function(x)
{
  z<-((x-min(x))/(max(x)-min(x)))
  return(z)
}

#defining the na.zero function
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
#****loading dataset
data<-read.csv("C:/Users/vandna/Desktop/Stevens/SEM 2/513/Project/Code/dataset/new4.csv")
filtereddataset<-sample(data)
filtereddataset$CarrierDelay[is.na(filtereddataset$CarrierDelay)] <- 0
filtereddataset$WeatherDelay[is.na(filtereddataset$WeatherDelay)] <- 0
filtereddataset$NASDelay[is.na(filtereddataset$NASDelay)] <- 0
filtereddataset$SecurityDelay[is.na(filtereddataset$SecurityDelay)] <- 0
filtereddataset$LateAircraftDelay[is.na(filtereddataset$LateAircraftDelay)] <- 0
attach(filtereddataset)
filtereddataset$DayofMonth_cat[DayofMonth > 15] <- "second_half"
filtereddataset$DayofMonth_cat[DayofMonth <= 15] <- "First_half"
filtereddataset$week_cat[DayOfWeek == 1 | DayOfWeek ==7] <- "weekend"
filtereddataset$week_cat[DayOfWeek < 7 & DayOfWeek > 1] <- "weekday"
filtereddataset$ArrDelay_cat[ArrDelay <= 5] <- "EARLY"
filtereddataset$ArrDelay_cat[ArrDelay > 5 & ArrDelay <= 60] <- "ONTIME"
filtereddataset$ArrDelay_cat[ArrDelay >  60] <- "LATE"
filtereddataset$Distance_cat[Distance <= 1000] <- "short_distance"
filtereddataset$Distance_cat[Distance > 1000 & Distance <=  2000] <- "Mid_distance"
filtereddataset$Distance_cat[Distance > 2000] <- "Long_distance"
filtereddataset<-join(filtereddataset,count(filtereddataset,'Dest'))
filtereddataset$Dest_Type[filtereddataset$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[filtereddataset$freq > 500 & filtereddataset$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[filtereddataset$freq >1000] <- "High Busy"
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "ONTIME"] <- 1
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "Mid Delay"] <- 2
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "High Delay"] <- 3
data <-filtereddataset
data_new<-cbind(UniqueCarrier=data$UniqueCarrier,
                DepDelay=data$DepDelay,
                weatherdly=data$WeatherDelay,
                NASdly=data$NASDelay,
                Securitydly=data$SecurityDelay,
                LateAircraftdly=data$LateAircraftDelay,
                ArrDelay_cat=as.numeric(factor(data$ArrDelay_cat))
)
idx1<-seq(1:1000)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
#generating training dataset 
training<-data_new[idx,]
#generating test dataset 
test<-data_new[-idx,]
m1 <- C5.0(training[,-7],factor(training[,7]),trials=10)
#gives summary of the tree
summary(m1)
#plotting the tree
plot(m1)
#to  check  the accuracy of model
result<-predict(m1,test,type="class")
rTable<-table(predict=result,test=test[,7])
accuracy=(sum(diag(rTable))/nrow(test))
accuracy


