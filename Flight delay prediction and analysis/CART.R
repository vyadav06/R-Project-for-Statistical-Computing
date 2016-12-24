#Course : CS 513
#Vandna Yadav
#Ankita Sawant

rm(list=ls())
#install.packages("plyr")
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
data<-read.csv("C:/Users/vandna/Desktop/Stevens/SEM 2/513/Project/Code/dataset/20April.csv")
filtereddataset<-sample(data)
filtereddataset$CarrierDelay[is.na(filtereddataset$CarrierDelay)] <- 0
filtereddataset$WeatherDelay[is.na(filtereddataset$WeatherDelay)] <- 0
filtereddataset$NASDelay[is.na(filtereddataset$NASDelay)] <- 0
filtereddataset$SecurityDelay[is.na(filtereddataset$SecurityDelay)] <- 0
filtereddataset$LateAircraftDelay[is.na(filtereddataset$LateAircraftDelay)] <- 0
attach(filtereddataset)
#******categorizing Day of Month
filtereddataset$DayofMonth_cat[DayofMonth > 15] <- "second_half"
filtereddataset$DayofMonth_cat[DayofMonth <= 15] <- "First_half"
#*******categorizing Day OF Week
filtereddataset$week_cat[DayOfWeek == 1 | DayOfWeek ==7] <- "weekend"
filtereddataset$week_cat[DayOfWeek < 7 & DayOfWeek > 1] <- "weekday"
#*******categorizing Arrival Delay
filtereddataset$ArrDelay_cat[ArrDelay <= 5] <- "EARLY"
filtereddataset$ArrDelay_cat[ArrDelay > 5 & ArrDelay <= 60] <- "ONTIME"
filtereddataset$ArrDelay_cat[ArrDelay >  60] <- "LATE"
#*****categorizing Distance
filtereddataset$Distance_cat[Distance <= 1000] <- "short_distance"
filtereddataset$Distance_cat[Distance > 1000 & Distance <=  2000] <- "Mid_distance"
filtereddataset$Distance_cat[Distance > 2000] <- "Long_distance"
filtereddataset<-join(filtereddataset,count(filtereddataset,'Dest'))
#******categorizing Dest type
filtereddataset$Dest_Type[filtereddataset$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[filtereddataset$freq > 500 & filtereddataset$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[filtereddataset$freq >1000] <- "High Busy"
#creating data frame
data_new<-data.frame(DayOfMonth=(filtereddataset$DayofMonth_cat),
                     DayOfWeek=(filtereddataset$week_cat),
                     CRSDepTime=(na.zero(filtereddataset$CRSDepTime)),
                     CRSArrTime=(na.zero(filtereddataset$CRSArrTime)),
                     UniqueCarrier=(filtereddataset$UniqueCarrier),
                     Origin=(filtereddataset$Origin),
                     Dest=(filtereddataset$Dest_Type),
                     Distance=(filtereddataset$Distance_cat),
                     ArrDelay_cat=(filtereddataset$ArrDelay_cat)
                     
)


set.seed(9850)
#creating training and test dataset  based on 70%-30% ratio
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]

#fitting the classification tree using the rpart function
library(rpart)
dtm <- rpart(ArrDelay_cat~.,data= training, method= "class")
printcp(dtm)
#gives the summary
summary(dtm)
#plotting the tree
library(rpart.plot)
#predicting for test data ad testing the accuracy
rpart.plot(dtm, type=1, extra=101)
p3 <- predict(dtm,test,type="class")
table(test[,9], predicted= p3)
