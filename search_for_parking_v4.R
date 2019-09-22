rm(list=ls())

getwd()

## Load the libraries
library(xgboost)
library(tidyverse)
library(caret)
library(mlr)
library(randomForest)
library(caTools)
library(DataExplorer)
library(Rcpp)
library(jsonlite)
library(dplyr)
library(raster)
library(ggplot2)
library(shiny)
library(ggmap)
library(hexbin)
library(Hmisc)
library(gridExtra)
library(geosphere)
library(tableplot)
library(e1071)
library(fpc)
library(rpart.plot)
library(rpart)
library(ParamHelpers)
library(sf)
library(tmap)
library(leaflet)
library(ROSE)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("train", "caret")
library(mlbench)

## Upload and view the dataset
search_for_parking <- read.csv("/Users/urvashigupta/Desktop/Final Reports/search_for_parking_2.csv")
View(search_for_parking)
str(search_for_parking)
summary(search_for_parking)

## Create JSON Parser to parse the JSON columns
ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep = " "), " ]") %>%
  fromJSON(flatten = T) %>%
  as_tibble()  
}

## Create parsed columns for Hourly Distribution variable
JSONcolumn_data_HD <- search_for_parking %>%
  select(HourlyDistribution) %>%
  map_dfc(.f = ParseJSONColumn)

## Merge the new column with the dataset
search_for_parking <- bind_cols(search_for_parking, JSONcolumn_data_HD)

## Delete the old JSON column
search_for_parking$HourlyDistribution = NULL

#Rename the columns
colnames(search_for_parking)[colnames(search_for_parking)=="00"] <- "HD_00"
colnames(search_for_parking)[colnames(search_for_parking)=="01"] <- "HD_01"
colnames(search_for_parking)[colnames(search_for_parking)=="02"] <- "HD_02"
colnames(search_for_parking)[colnames(search_for_parking)=="03"] <- "HD_03"
colnames(search_for_parking)[colnames(search_for_parking)=="04"] <- "HD_04"
colnames(search_for_parking)[colnames(search_for_parking)=="05"] <- "HD_05"
colnames(search_for_parking)[colnames(search_for_parking)=="06"] <- "HD_06"
colnames(search_for_parking)[colnames(search_for_parking)=="07"] <- "HD_07"
colnames(search_for_parking)[colnames(search_for_parking)=="08"] <- "HD_08"
colnames(search_for_parking)[colnames(search_for_parking)=="09"] <- "HD_09"
colnames(search_for_parking)[colnames(search_for_parking)=="10"] <- "HD_10"
colnames(search_for_parking)[colnames(search_for_parking)=="11"] <- "HD_11"
colnames(search_for_parking)[colnames(search_for_parking)=="12"] <- "HD_12"
colnames(search_for_parking)[colnames(search_for_parking)=="13"] <- "HD_13"
colnames(search_for_parking)[colnames(search_for_parking)=="14"] <- "HD_14"
colnames(search_for_parking)[colnames(search_for_parking)=="15"] <- "HD_15"
colnames(search_for_parking)[colnames(search_for_parking)=="16"] <- "HD_16"
colnames(search_for_parking)[colnames(search_for_parking)=="17"] <- "HD_17"
colnames(search_for_parking)[colnames(search_for_parking)=="18"] <- "HD_18"
colnames(search_for_parking)[colnames(search_for_parking)=="19"] <- "HD_19"
colnames(search_for_parking)[colnames(search_for_parking)=="20"] <- "HD_20"
colnames(search_for_parking)[colnames(search_for_parking)=="21"] <- "HD_21"
colnames(search_for_parking)[colnames(search_for_parking)=="22"] <- "HD_22"
colnames(search_for_parking)[colnames(search_for_parking)=="23"] <- "HD_23"

## Create parsed columns for Searching by Hour variable
JSONcolumn_data_SH <- search_for_parking %>%
  select(SearchingByHour) %>%
  map_dfc(.f = ParseJSONColumn)

## Merge the new dataframe with search for parking dataset
search_for_parking <- bind_cols(search_for_parking, JSONcolumn_data_SH)

## Delete the old column
search_for_parking$SearchingByHour = NULL

## Rename the columns
colnames(search_for_parking)[colnames(search_for_parking)=="00"] <- "SearchingbyHour_00"
colnames(search_for_parking)[colnames(search_for_parking)=="01"] <- "SearchingbyHour_01"
colnames(search_for_parking)[colnames(search_for_parking)=="02"] <- "SearchingbyHour_02"
colnames(search_for_parking)[colnames(search_for_parking)=="03"] <- "SearchingbyHour_03"
colnames(search_for_parking)[colnames(search_for_parking)=="04"] <- "SearchingbyHour_04"
colnames(search_for_parking)[colnames(search_for_parking)=="05"] <- "SearchingbyHour_05"
colnames(search_for_parking)[colnames(search_for_parking)=="06"] <- "SearchingbyHour_06"
colnames(search_for_parking)[colnames(search_for_parking)=="07"] <- "SearchingbyHour_07"
colnames(search_for_parking)[colnames(search_for_parking)=="08"] <- "SearchingbyHour_08"
colnames(search_for_parking)[colnames(search_for_parking)=="09"] <- "SearchingbyHour_09"
colnames(search_for_parking)[colnames(search_for_parking)=="10"] <- "SearchingbyHour_10"
colnames(search_for_parking)[colnames(search_for_parking)=="11"] <- "SearchingbyHour_11"
colnames(search_for_parking)[colnames(search_for_parking)=="12"] <- "SearchingbyHour_12"
colnames(search_for_parking)[colnames(search_for_parking)=="13"] <- "SearchingbyHour_13"
colnames(search_for_parking)[colnames(search_for_parking)=="14"] <- "SearchingbyHour_14"
colnames(search_for_parking)[colnames(search_for_parking)=="15"] <- "SearchingbyHour_15"
colnames(search_for_parking)[colnames(search_for_parking)=="16"] <- "SearchingbyHour_16"
colnames(search_for_parking)[colnames(search_for_parking)=="17"] <- "SearchingbyHour_17"
colnames(search_for_parking)[colnames(search_for_parking)=="18"] <- "SearchingbyHour_18"
colnames(search_for_parking)[colnames(search_for_parking)=="19"] <- "SearchingbyHour_19"
colnames(search_for_parking)[colnames(search_for_parking)=="20"] <- "SearchingbyHour_20"
colnames(search_for_parking)[colnames(search_for_parking)=="21"] <- "SearchingbyHour_21"
colnames(search_for_parking)[colnames(search_for_parking)=="22"] <- "SearchingbyHour_22"
colnames(search_for_parking)[colnames(search_for_parking)=="23"] <- "SearchingbyHour_23"

## Check for the missing values
plot_missing(search_for_parking)


## Impute the NAs
search_for_parking$HD_00[is.na(search_for_parking$HD_00)] = median(search_for_parking$HD_00, na.rm = TRUE)
search_for_parking$HD_01[is.na(search_for_parking$HD_01)] = median(search_for_parking$HD_01, na.rm = TRUE)
search_for_parking$HD_02[is.na(search_for_parking$HD_02)] = median(search_for_parking$HD_02, na.rm = TRUE)
search_for_parking$HD_03[is.na(search_for_parking$HD_03)] = median(search_for_parking$HD_03, na.rm = TRUE)
search_for_parking$HD_04[is.na(search_for_parking$HD_04)] = median(search_for_parking$HD_04, na.rm = TRUE)
search_for_parking$HD_05[is.na(search_for_parking$HD_05)] = median(search_for_parking$HD_05, na.rm = TRUE)
search_for_parking$HD_06[is.na(search_for_parking$HD_06)] = median(search_for_parking$HD_06, na.rm = TRUE)
search_for_parking$HD_07[is.na(search_for_parking$HD_07)] = median(search_for_parking$HD_07, na.rm = TRUE)
search_for_parking$HD_08[is.na(search_for_parking$HD_08)] = median(search_for_parking$HD_08, na.rm = TRUE)
search_for_parking$HD_09[is.na(search_for_parking$HD_09)] = median(search_for_parking$HD_09, na.rm = TRUE)
search_for_parking$HD_10[is.na(search_for_parking$HD_10)] = median(search_for_parking$HD_10, na.rm = TRUE)
search_for_parking$HD_11[is.na(search_for_parking$HD_11)] = median(search_for_parking$HD_11, na.rm = TRUE)
search_for_parking$HD_12[is.na(search_for_parking$HD_12)] = median(search_for_parking$HD_12, na.rm = TRUE)
search_for_parking$HD_13[is.na(search_for_parking$HD_13)] = median(search_for_parking$HD_13, na.rm = TRUE)
search_for_parking$HD_14[is.na(search_for_parking$HD_14)] = median(search_for_parking$HD_14, na.rm = TRUE)
search_for_parking$HD_15[is.na(search_for_parking$HD_15)] = median(search_for_parking$HD_15, na.rm = TRUE)
search_for_parking$HD_16[is.na(search_for_parking$HD_16)] = median(search_for_parking$HD_16, na.rm = TRUE)
search_for_parking$HD_17[is.na(search_for_parking$HD_17)] = median(search_for_parking$HD_17, na.rm = TRUE)
search_for_parking$HD_18[is.na(search_for_parking$HD_18)] = median(search_for_parking$HD_18, na.rm = TRUE)
search_for_parking$HD_19[is.na(search_for_parking$HD_19)] = median(search_for_parking$HD_19, na.rm = TRUE)
search_for_parking$HD_20[is.na(search_for_parking$HD_20)] = median(search_for_parking$HD_20, na.rm = TRUE)
search_for_parking$HD_21[is.na(search_for_parking$HD_21)] = median(search_for_parking$HD_21, na.rm = TRUE)
search_for_parking$HD_22[is.na(search_for_parking$HD_22)] = median(search_for_parking$HD_22, na.rm = TRUE)
search_for_parking$HD_23[is.na(search_for_parking$HD_23)] = median(search_for_parking$HD_23, na.rm = TRUE)

search_for_parking$AvgUniqueGeohashes[is.na(search_for_parking$AvgUniqueGeohashes)] <- median(search_for_parking$AvgUniqueGeohashes,na.rm=TRUE)
search_for_parking$AvgTotalGeohashes[is.na(search_for_parking$AvgTotalGeohashes)] <- median(search_for_parking$AvgTotalGeohashes,na.rm = TRUE)
search_for_parking$PercentCar[is.na(search_for_parking$PercentCar)] <- median(search_for_parking$PercentCar,na.rm=TRUE)
search_for_parking$PercentMPV[is.na(search_for_parking$PercentMPV)] <- median(search_for_parking$PercentMPV,na.rm=TRUE)
search_for_parking$PercentLDT[is.na(search_for_parking$PercentLDT)] <- median(search_for_parking$PercentLDT,na.rm=TRUE)
search_for_parking$PercentMDT[is.na(search_for_parking$PercentMDT)] <- median(search_for_parking$PercentMDT,na.rm=TRUE)
search_for_parking$PercentHDT[is.na(search_for_parking$PercentHDT)] <- median(search_for_parking$PercentHDT,na.rm=TRUE)
search_for_parking$PercentOther[is.na(search_for_parking$PercentOther)] <- median(search_for_parking$PercentOther,na.rm=TRUE)

search_for_parking$SearchingbyHour_00[is.na(search_for_parking$SearchingbyHour_00)] <- median(search_for_parking$SearchingbyHour_00,na.rm = TRUE) 
search_for_parking$SearchingbyHour_01[is.na(search_for_parking$SearchingbyHour_01)] <- median(search_for_parking$SearchingbyHour_01,na.rm = TRUE) 
search_for_parking$SearchingbyHour_02[is.na(search_for_parking$SearchingbyHour_02)] <- median(search_for_parking$SearchingbyHour_02,na.rm = TRUE) 
search_for_parking$SearchingbyHour_03[is.na(search_for_parking$SearchingbyHour_03)] <- median(search_for_parking$SearchingbyHour_03,na.rm = TRUE) 
search_for_parking$SearchingbyHour_04[is.na(search_for_parking$SearchingbyHour_04)] <- median(search_for_parking$SearchingbyHour_04,na.rm = TRUE) 
search_for_parking$SearchingbyHour_05[is.na(search_for_parking$SearchingbyHour_05)] <- median(search_for_parking$SearchingbyHour_05,na.rm = TRUE) 
search_for_parking$SearchingbyHour_06[is.na(search_for_parking$SearchingbyHour_06)] <- median(search_for_parking$SearchingbyHour_06,na.rm = TRUE) 
search_for_parking$SearchingbyHour_07[is.na(search_for_parking$SearchingbyHour_07)] <- median(search_for_parking$SearchingbyHour_07,na.rm = TRUE) 
search_for_parking$SearchingbyHour_08[is.na(search_for_parking$SearchingbyHour_08)] <- median(search_for_parking$SearchingbyHour_08,na.rm = TRUE) 
search_for_parking$SearchingbyHour_09[is.na(search_for_parking$SearchingbyHour_09)] <- median(search_for_parking$SearchingbyHour_09,na.rm = TRUE) 
search_for_parking$SearchingbyHour_10[is.na(search_for_parking$SearchingbyHour_10)] <- median(search_for_parking$SearchingbyHour_10,na.rm = TRUE) 
search_for_parking$SearchingbyHour_11[is.na(search_for_parking$SearchingbyHour_11)] <- median(search_for_parking$SearchingbyHour_11,na.rm = TRUE) 
search_for_parking$SearchingbyHour_12[is.na(search_for_parking$SearchingbyHour_12)] <- median(search_for_parking$SearchingbyHour_12,na.rm = TRUE) 
search_for_parking$SearchingbyHour_13[is.na(search_for_parking$SearchingbyHour_13)] <- median(search_for_parking$SearchingbyHour_13,na.rm = TRUE) 
search_for_parking$SearchingbyHour_14[is.na(search_for_parking$SearchingbyHour_14)] <- median(search_for_parking$SearchingbyHour_14,na.rm = TRUE) 
search_for_parking$SearchingbyHour_15[is.na(search_for_parking$SearchingbyHour_15)] <- median(search_for_parking$SearchingbyHour_15,na.rm = TRUE) 
search_for_parking$SearchingbyHour_16[is.na(search_for_parking$SearchingbyHour_16)] <- median(search_for_parking$SearchingbyHour_16,na.rm = TRUE) 
search_for_parking$SearchingbyHour_17[is.na(search_for_parking$SearchingbyHour_17)] <- median(search_for_parking$SearchingbyHour_17,na.rm = TRUE) 
search_for_parking$SearchingbyHour_18[is.na(search_for_parking$SearchingbyHour_18)] <- median(search_for_parking$SearchingbyHour_18,na.rm = TRUE) 
search_for_parking$SearchingbyHour_19[is.na(search_for_parking$SearchingbyHour_19)] <- median(search_for_parking$SearchingbyHour_19,na.rm = TRUE) 
search_for_parking$SearchingbyHour_20[is.na(search_for_parking$SearchingbyHour_20)] <- median(search_for_parking$SearchingbyHour_20,na.rm = TRUE) 
search_for_parking$SearchingbyHour_21[is.na(search_for_parking$SearchingbyHour_21)] <- median(search_for_parking$SearchingbyHour_21,na.rm = TRUE) 
search_for_parking$SearchingbyHour_22[is.na(search_for_parking$SearchingbyHour_22)] <- median(search_for_parking$SearchingbyHour_22,na.rm = TRUE) 
search_for_parking$SearchingbyHour_23[is.na(search_for_parking$SearchingbyHour_23)] <- median(search_for_parking$SearchingbyHour_23,na.rm = TRUE) 

## Remove the columns which are not required or have redundant/same values
search_for_parking$County <- NULL
search_for_parking$CirclingDistribution <- NULL
search_for_parking$City <- NULL
search_for_parking$State <- NULL
search_for_parking$Country <- NULL
search_for_parking$ISO_3166_2 <- NULL
search_for_parking$UpdateDate <- NULL
search_for_parking$Version <- NULL

## Check the data type
plot_intro(search_for_parking)

## EDA
## Plot the parking geohashes
qmplot(Longitude, Latitude, data = search_for_parking, colour = I('dark green'), size = I(3), darken = 0.3)

## Plot the numercial variables
bin1 <- hexbin(search_for_parking$Geohash, search_for_parking$AvgTimeToPark, xbins=50)
plot(bin1, xlab = "Geohash", ylab = "Avg Time To Park")

bin2 <- hexbin(search_for_parking$Geohash, search_for_parking$TotalSearching, xbins=50)
plot(bin2, xlab = "Geohash", ylab = "Total Searching")

bin3 <- hexbin(search_for_parking$Geohash, search_for_parking$PercentCar, xbins=50)
plot(bin3, xlab = "Geohash", ylab = "Percentage of Cars")

bin4 <- hexbin(search_for_parking$Geohash, search_for_parking$PercentHDT, xbins=50)
plot(bin4, xlab = "Geohash", ylab = "Percentage of HDTs")

bin5 <- hexbin(search_for_parking$Geohash, search_for_parking$PercentMPV, xbins=50)
plot(bin5, xlab = "Geohash", ylab = "Percentage of MPVs")

bin6 <- hexbin(search_for_parking$Geohash, search_for_parking$PercentLDT, xbins=50)
plot(bin6, xlab = "Geohash", ylab = "Percentage of LDTs")

bin7 <- hexbin(search_for_parking$Geohash, search_for_parking$PercentMDT, xbins=50)
plot(bin7, xlab = "Geohash", ylab = "Percentage of MDTs")

bin8 <- hexbin(search_for_parking$Geohash, search_for_parking$PercentOther, xbins=50)
plot(bin8, xlab = "Geohash", ylab = "Percentage of Other Vehicles")

## Detect and replace the outlier Values
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

search_for_parking$AvgTimeToPark=capOutlier(search_for_parking$AvgTimeToPark)
search_for_parking$AvgTimeToParkRatio=capOutlier(search_for_parking$AvgTimeToParkRatio)
search_for_parking$TotalSearching=capOutlier(search_for_parking$TotalSearching)
search_for_parking$PercentSearching=capOutlier(search_for_parking$PercentSearching)
search_for_parking$AvgUniqueGeohashes=capOutlier(search_for_parking$AvgUniqueGeohashes)
search_for_parking$AvgTotalGeohashes=capOutlier(search_for_parking$AvgTotalGeohashes)
search_for_parking$PercentCar=capOutlier(search_for_parking$PercentCar)
search_for_parking$PercentHDT=capOutlier(search_for_parking$PercentHDT)
search_for_parking$PercentLDT=capOutlier(search_for_parking$PercentLDT)
search_for_parking$PercentMDT=capOutlier(search_for_parking$PercentMDT)
search_for_parking$PercentMPV=capOutlier(search_for_parking$PercentMPV)
search_for_parking$PercentOther=capOutlier(search_for_parking$PercentOther)
search_for_parking$HD_00=capOutlier(search_for_parking$HD_00)
search_for_parking$HD_01=capOutlier(search_for_parking$HD_01)
search_for_parking$HD_02=capOutlier(search_for_parking$HD_02)
search_for_parking$HD_03=capOutlier(search_for_parking$HD_03)
search_for_parking$HD_04=capOutlier(search_for_parking$HD_04)
search_for_parking$HD_05=capOutlier(search_for_parking$HD_05)
search_for_parking$HD_06=capOutlier(search_for_parking$HD_06)
search_for_parking$HD_07=capOutlier(search_for_parking$HD_07)
search_for_parking$HD_08=capOutlier(search_for_parking$HD_08)
search_for_parking$HD_09=capOutlier(search_for_parking$HD_09)
search_for_parking$HD_10=capOutlier(search_for_parking$HD_10)
search_for_parking$HD_11=capOutlier(search_for_parking$HD_11)
search_for_parking$HD_12=capOutlier(search_for_parking$HD_12)
search_for_parking$HD_13=capOutlier(search_for_parking$HD_13)
search_for_parking$HD_14=capOutlier(search_for_parking$HD_14)
search_for_parking$HD_15=capOutlier(search_for_parking$HD_15)
search_for_parking$HD_16=capOutlier(search_for_parking$HD_16)
search_for_parking$HD_17=capOutlier(search_for_parking$HD_17)
search_for_parking$HD_18=capOutlier(search_for_parking$HD_18)
search_for_parking$HD_19=capOutlier(search_for_parking$HD_19)
search_for_parking$HD_20=capOutlier(search_for_parking$HD_20)
search_for_parking$HD_21=capOutlier(search_for_parking$HD_21)
search_for_parking$HD_22=capOutlier(search_for_parking$HD_22)
search_for_parking$HD_23=capOutlier(search_for_parking$HD_23)
search_for_parking$SearchingbyHour_00=capOutlier(search_for_parking$SearchingbyHour_00)
search_for_parking$SearchingbyHour_01=capOutlier(search_for_parking$SearchingbyHour_01)
search_for_parking$SearchingbyHour_02=capOutlier(search_for_parking$SearchingbyHour_02)
search_for_parking$SearchingbyHour_03=capOutlier(search_for_parking$SearchingbyHour_03)
search_for_parking$SearchingbyHour_04=capOutlier(search_for_parking$SearchingbyHour_04)
search_for_parking$SearchingbyHour_05=capOutlier(search_for_parking$SearchingbyHour_05)
search_for_parking$SearchingbyHour_06=capOutlier(search_for_parking$SearchingbyHour_06)
search_for_parking$SearchingbyHour_07=capOutlier(search_for_parking$SearchingbyHour_07)
search_for_parking$SearchingbyHour_08=capOutlier(search_for_parking$SearchingbyHour_08)
search_for_parking$SearchingbyHour_09=capOutlier(search_for_parking$SearchingbyHour_09)
search_for_parking$SearchingbyHour_10=capOutlier(search_for_parking$SearchingbyHour_10)
search_for_parking$SearchingbyHour_11=capOutlier(search_for_parking$SearchingbyHour_11)
search_for_parking$SearchingbyHour_12=capOutlier(search_for_parking$SearchingbyHour_12)
search_for_parking$SearchingbyHour_13=capOutlier(search_for_parking$SearchingbyHour_13)
search_for_parking$SearchingbyHour_14=capOutlier(search_for_parking$SearchingbyHour_14)
search_for_parking$SearchingbyHour_15=capOutlier(search_for_parking$SearchingbyHour_15)
search_for_parking$SearchingbyHour_16=capOutlier(search_for_parking$SearchingbyHour_16)
search_for_parking$SearchingbyHour_17=capOutlier(search_for_parking$SearchingbyHour_17)
search_for_parking$SearchingbyHour_18=capOutlier(search_for_parking$SearchingbyHour_18)
search_for_parking$SearchingbyHour_19=capOutlier(search_for_parking$SearchingbyHour_19)
search_for_parking$SearchingbyHour_20=capOutlier(search_for_parking$SearchingbyHour_20)
search_for_parking$SearchingbyHour_21=capOutlier(search_for_parking$SearchingbyHour_21)
search_for_parking$SearchingbyHour_22=capOutlier(search_for_parking$SearchingbyHour_22)
search_for_parking$SearchingbyHour_23=capOutlier(search_for_parking$SearchingbyHour_23)

## Create correlation matrix
library(ggcorrplot)
cormat <- round(cor(search_for_parking[,8:67]), 2)
ggcorrplot(cormat, method = c("square", "circle"), hc.order = TRUE, hc.method = "complete", type = "lower", outline.color = "gray")

## Perform K-Means Clustering to label the unsupervised data

## Calculate the optimal number of clusters
library(NbClust)
set.seed(1234)
nc <- NbClust(search_for_parking[,8:67], min.nc=2, max.nc=4, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

## scale the data to standardize the values
scaled.SP <- scale(search_for_parking[8:67])
head(scaled.SP, 10)
summary(search_for_parking)
kmeans.clus = kmeans(x=scaled.SP, centers = 3)
summary(kmeans.clus)

## Profile the clusters
search_for_parking$Clusters <- kmeans.clus$cluster
head(search_for_parking)
aggr = aggregate(search_for_parking[,-c(1:7,68)],list(search_for_parking$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
Freq=as.vector(table(search_for_parking$Clusters)),
aggr[,-1])
View(clus.profile)

## plotting the clusters
library(cluster)
clusplot(scaled.SP, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)


## Predict the Clusters for the data using classification algorithms

train <- search_for_parking 
## We are not creating a separate test sample because, we will be using the cross-validation method to avoid over-fitting of the models

## Ensure that dependant variable is Categorical
train$Clusters = as.factor(train$Clusters)
str(train)

## Remove the columns which are not needed for prediction
train$Geohash=train$Latitude_SW=train$Longitude_SW=train$Latitude_NE=train$Longitude_NE=train$Latitude=train$Longitude=NULL
str(train)

## Check the class imbalance
table(train$Clusters)

## Create balanced data using oversampling mechanism

train_1 <- train[train$Clusters == 1, c(1:61)]
train_2 <- train[train$Clusters == 2, c(1:61)]
train_3 <- train[train$Clusters == 3, c(1:61)]

train_12 <- rbind(train_1, train_2)
train_23 <- rbind(train_2, train_3)
train_13 <- rbind(train_1, train_3)

table(train_12$Clusters)
balanced_train_12 <- ovun.sample(Clusters~., data = train_12, p=0.5, method = "over")$data
table(balanced_train_12$Clusters)

table(train_23$Clusters)
balanced_train_23 <- ovun.sample(Clusters~., data = train_23, p=0.5, method = "over")$data
table(balanced_train_23$Clusters)

table(train_13$Clusters)
balanced_train_13 <- ovun.sample(Clusters~., data = train_13, p=0.5, method = "over")$data
table(balanced_train_13$Clusters)

b_train_1 <- balanced_train_13[balanced_train_13$Clusters == 1, c(1:61)]
b_train_2 <- balanced_train_23[balanced_train_23$Clusters == 2, c(1:61)]
b_train_3 <- balanced_train_23[balanced_train_23$Clusters == 3, c(1:61)]

bal_train_123 <- rbind(b_train_1, b_train_2, b_train_3)
train <- bal_train_123
table(train$Clusters)

# Cross-Validation
# Define train control for k fold cross validation, 10-folds to be created
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)

# Fit Naive Bayes Model
nb_model <- train(Clusters~., data=SI, trControl=train_control, method="nb")
# Summarise Results
print(nb_model)
confusionMatrix(nb_model)
# Predict
nb_predict <- predict(nb_model,SI)
# Confusion Matrix
confusionMatrix(nb_predict,SI$Clusters)

# Fit Random Forest Model
rf_model <- train(Clusters~., data=SI, trControl=train_control, method="rf")
# Summarise Results
print(rf_model)
confusionMatrix(rf_model)
# Predict
rf_predict <- predict(rf_model,SI)
# Confusion Matrix
confusionMatrix(rf_predict,SI$Clusters)

# Fit CART Model
cart_model <- train(Clusters~., data=SI, trControl=train_control, method="rpart")
# Summarise Results
print(cart_model)
confusionMatrix(cart_model)
# Predict
cart_predict <- predict(cart_model,SI)
# Confusion Matrix
confusionMatrix(cart_predict,SI$Clusters)

# Fit SVM Model
svm_model <- train(Clusters~., data=SI, trControl=train_control, method="svmRadialSigma")
# Summarise Results
print(svm_model)
confusionMatrix(svm_model)
# Predict
svm_predict <- predict(svm_model,SI)
# Confusion Matrix
confusionMatrix(svm_predict,SI$Clusters)

# Fit KNN Model
knn_model <- train(Clusters~., data=SI, trControl=train_control, method="knn")
# Summarise Results
print(knn_model)
confusionMatrix(knn_model)
# Predict
knn_predict <- predict(knn_model,SI)
# Confusion Matrix
confusionMatrix(knn_predict,SI$Clusters)

# Fit LDA Model
lda_model <- train(Clusters~., data=SI, trControl=train_control, method="stepLDA")
# Summarise Results
print(lda_model)
confusionMatrix(lda_model)
# Predict
lda_predict <- predict(lda_model,SI)
# Confusion Matrix
confusionMatrix(lda_predict,SI$Clusters)















