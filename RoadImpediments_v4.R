rm(list=ls())
getwd()

library(xgboost)
library(tidyverse)
library(caret)
library(mlr)
library(randomForest)
library(caTools)
library(DataExplorer)
library(Rcpp)
library(tidyverse)
library(jsonlite)
library(dplyr)
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
library(mlbench)
conflict_prefer("select", "dplyr")
conflict_prefer("train", "caret")


##Load the Data
getwd()
setwd("/Users/urvashigupta/Desktop/Final Reports")
road_impediments <- read.csv(file.choose())
View(road_impediments)

str(road_impediments)
summary(road_impediments)


## EDA

#Remove the columns which are not important for our analysis; these columns have basically same values
road_impediments$County <- NULL
road_impediments$City <- NULL
road_impediments$State <- NULL
road_impediments$Country <- NULL
road_impediments$ISO_3166_2 <- NULL
road_impediments$UpdateDate <- NULL
road_impediments$Version <- NULL

## Check for the missing values
plot_missing(road_impediments)

## Plot the road impediments w.r.t Geohash
qmplot(Longitude, Latitude, data = road_impediments, colour = I('dark green'), size = I(3), darken = 0.3, zoom = 14)

summary(road_impediments)

## Plot the variables

bin1 <- hexbin(road_impediments$Geohash, road_impediments$AvgAcceleration, xbins=50)
plot(bin1, xlab = "Geohash", ylab = "Avg Change in Acceleration")

bin2 <- hexbin(road_impediments$Geohash, road_impediments$PercentOfVehicles, xbins=50)
plot(bin2, xlab = "Geohash", ylab = "Percentage of Vehicles")

bin3 <- hexbin(road_impediments$Geohash, road_impediments$PercentCar, xbins=50)
plot(bin3, xlab = "Geohash", ylab = "Percentage of Cars")

bin4 <- hexbin(road_impediments$Geohash, road_impediments$PercentHDT, xbins=50)
plot(bin4, xlab = "Geohash", ylab = "Percentage of HDTs")

bin5 <- hexbin(road_impediments$Geohash, road_impediments$PercentMPV, xbins=50)
plot(bin5, xlab = "Geohash", ylab = "Percentage of MPVs")

bin6 <- hexbin(road_impediments$Geohash, road_impediments$PercentLDT, xbins=50)
plot(bin6, xlab = "Geohash", ylab = "Percentage of LDTs")

bin7 <- hexbin(road_impediments$Geohash, road_impediments$PercentMDT, xbins=50)
plot(bin7, xlab = "Geohash", ylab = "Percentage of MDTs")

bin8 <- hexbin(road_impediments$Geohash, road_impediments$PercentOther, xbins=50)
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

road_impediments$AvgAcceleration=capOutlier(road_impediments$AvgAcceleration)
road_impediments$PercentOfVehicles=capOutlier(road_impediments$PercentOfVehicles)
road_impediments$PercentCar=capOutlier(road_impediments$PercentCar)
road_impediments$PercentHDT=capOutlier(road_impediments$PercentHDT)
road_impediments$PercentLDT=capOutlier(road_impediments$PercentLDT)
road_impediments$PercentMDT=capOutlier(road_impediments$PercentMDT)
road_impediments$PercentMPV=capOutlier(road_impediments$PercentMPV)
road_impediments$PercentOther=capOutlier(road_impediments$PercentOther)

## Create a correlation plot
library(ggcorrplot)
cormat <- round(cor(road_impediments[,8:15]), 2)
ggcorrplot(cormat, method = c("square", "circle"), hc.order = TRUE, hc.method = "complete", type = "lower", outline.color = "gray")


##Clustering to label the data

# K-Means CLustering
## Calculate the optimal number of clusters
library(NbClust)
set.seed(1234)
nc <- NbClust(RI[16], min.nc=2, max.nc=4, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# Scale function standardizes the values
scaled.RI <- scale(road_impediments[,8:15])
head(scaled.RI, 10)
kmeans.clus = kmeans(x=scaled.RI, centers = 3)
summary(kmeans.clus)


## Profiling the clusters
road_impediments$Clusters <- kmeans.clus$cluster
aggr = aggregate(road_impediments[,-c(1:7,16)],list(road_impediments$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(road_impediments$Clusters)),
                            aggr[,-1])


## Plotting the clusters
library(cluster)
clusplot(scaled.RI, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)


## Predict the Clusters for the data using classification algorithms

## Subset the data
smp_size <- floor(0.3*nrow(road_impediments))
set.seed(123)
ind <- sample(seq_len(nrow(road_impediments)), size = smp_size)
subset_data <- road_impediments[ind, ]
road_impediments <- subset_data


train <- road_impediments 
## We are not creating a separate test sample because, we will be using the 10-fold cross-validation method to avoid over-fitting of the models

## Ensuring that dependant variable is Categorical
train$Clusters = as.factor(train$Clusters)
str(train)

## Remove the columns which are not needed for prediction
train$Geohash=train$Latitude_SW=train$Longitude_SW=train$Latitude_NE=train$Longitude_NE=train$Latitude=train$Longitude=NULL

### Check the class imbalance
table(train$Clusters)

##Create balanced data
#over sampling training data

train_1 <- train[train$Clusters == 1, c(1:9)]
train_2 <- train[train$Clusters == 2, c(1:9)]
train_3 <- train[train$Clusters == 3, c(1:9)]

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

b_train_1 <- balanced_train_13[balanced_train_13$Clusters == 1, c(1:9)]
b_train_2 <- balanced_train_23[balanced_train_23$Clusters == 2, c(1:9)]
b_train_3 <- balanced_train_13[balanced_train_13$Clusters == 3, c(1:9)]

bal_train_123 <- rbind(b_train_1, b_train_2, b_train_3)
train <- bal_train_123
table(train$Clusters)


# Cross-Validation
# Define train control for k fold cross validation, 10-folds to be created
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
RI <- train

# Fit Naive Bayes Model
nb_model <- train(Clusters~., data=RI, trControl=train_control, method="nb")
# Summarise Results
print(nb_model)
confusionMatrix(nb_model)
# Predict
nb_predict <- predict(nb_model,RI)
# Confusion Matrix
confusionMatrix(nb_predict,RI$Clusters)


# Fit CART Model
cart_model <- train(Clusters~., data=RI, trControl=train_control, method="rpart")
# Summarise Results
print(cart_model)
confusionMatrix(cart_model)
# Predict
cart_predict <- predict(cart_model,RI)
# Confusion Matrix
confusionMatrix(cart_predict,RI$Clusters)

# Fit KNN Model
knn_model <- train(Clusters~., data=RI, trControl=train_control, method="knn")
# Summarise Results
print(knn_model)
confusionMatrix(knn_model)
# Predict
knn_predict <- predict(knn_model,RI)
# Confusion Matrix
confusionMatrix(knn_predict,RI$Clusters)


# Fit LDA Model
lda_model <- train(Clusters~., data=RI, trControl=train_control, method="stepLDA")
# Summarise Results
print(lda_model)
confusionMatrix(lda_model)
# Predict
lda_predict <- predict(lda_model,RI)
# Confusion Matrix
confusionMatrix(lda_predict,RI$Clusters)
