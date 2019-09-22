rm(list=ls())
getwd()
setwd("/Users/namrataadke/Downloads")

## Install all the necessary libraries
library(xgboost)
library(tidyverse)
library(caret)
library(mlr)
library(randomForest)
library(caTools)
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
library(leaflet)
library(ROSE)
library(utils)
library(raster)
library(spData)
library(tmap)
library(mapview)
library(DataExplorer)
library(corrplot)

##Import the dataset
congestion = read.csv(file.choose())
View(congestion)

##Remove all the variables which do not help in the analysis
congestion$X=NULL
congestion$X.1=NULL
congestion$City.y=NULL
congestion$State.y=NULL
congestion$Country.y=NULL
congestion$County.x=NULL
congestion$State.x=NULL
congestion$City.x=NULL
congestion$Country.x=NULL
congestion$ISO_3166_2.x=NULL
congestion$Latitude_SW.x=NULL
congestion$Latitude_NE.x=NULL
congestion$Latitude_SW.y=NULL
congestion$Latitude_NE.y=NULL
congestion$County.y=NULL
congestion$UpdateDate.x=NULL
congestion$UpdateDate.y=NULL
congestion$Version.y=NULL
congestion$Version.x=NULL
congestion$Longitude_SW.x=NULL
congestion$Longitude_NE.x=NULL
congestion$Longitude_SW.y=NULL
congestion$Longitude_NE.y=NULL
congestion$ISO_3166_2.y=NULL

View(congestion)
str(congestion)
summary(congestion)

##Plot the missing values
plot_missing(congestion[,16:63])
?plot_missing
##Plot all the datapoints on the map
qmplot(Longitude.x, Latitude.x, data = congestion, colour = I('dark green'),
       size = I(1), darken = 0.3)

##Impute all the missing values
congestion$HD_00[is.na(congestion$HD_00)] = median(congestion$HD_00, na.rm = TRUE)
congestion $HD_01[is.na(congestion$HD_01)] = median(congestion$HD_01, na.rm = TRUE)
congestion$HD_02[is.na(congestion$HD_02)] = median(congestion$HD_02, na.rm = TRUE)
congestion$HD_03[is.na(congestion$HD_03)] = median(congestion$HD_03, na.rm = TRUE)
congestion$HD_04[is.na(congestion$HD_04)] = median(congestion$HD_04, na.rm = TRUE)
congestion$HD_05[is.na(congestion$HD_05)] = median(congestion$HD_05, na.rm = TRUE)
congestion$HD_06[is.na(congestion$HD_06)] = median(congestion$HD_06, na.rm = TRUE)
congestion$HD_07[is.na(congestion$HD_07)] = median(congestion$HD_07, na.rm = TRUE)
congestion$HD_08[is.na(congestion$HD_08)] = median(congestion$HD_08, na.rm = TRUE)
congestion$HD_09[is.na(congestion$HD_09)] = median(congestion$HD_09, na.rm = TRUE)
congestion$HD_10[is.na(congestion$HD_10)] = median(congestion$HD_10, na.rm = TRUE)
congestion$HD_11[is.na(congestion$HD_11)] = median(congestion$HD_11, na.rm = TRUE)
congestion$HD_12[is.na(congestion$HD_12)] = median(congestion$HD_12, na.rm = TRUE)
congestion$HD_13[is.na(congestion$HD_13)] = median(congestion$HD_13, na.rm = TRUE)
congestion$HD_14[is.na(congestion$HD_14)] = median(congestion$HD_14, na.rm = TRUE)
congestion$HD_15[is.na(congestion$HD_15)] = median(congestion$HD_15, na.rm = TRUE)
congestion$HD_16[is.na(congestion$HD_16)] = median(congestion$HD_16, na.rm = TRUE)
congestion$HD_17[is.na(congestion$HD_17)] = median(congestion$HD_17, na.rm = TRUE)
congestion$HD_18[is.na(congestion$HD_18)] = median(congestion$HD_18, na.rm = TRUE)
congestion$HD_19[is.na(congestion$HD_19)] = median(congestion$HD_19, na.rm = TRUE)
congestion$HD_20[is.na(congestion$HD_20)] = median(congestion$HD_20, na.rm = TRUE)
congestion$HD_21[is.na(congestion$HD_21)] = median(congestion$HD_21, na.rm = TRUE)
congestion$HD_22[is.na(congestion$HD_22)] = median(congestion$HD_22, na.rm = TRUE)
congestion$HD_23[is.na(congestion$HD_23)] = median(congestion$HD_23, na.rm = TRUE)

congestion$SearchingbyHour_00[is.na(congestion$SearchingbyHour_00)] <- median(congestion$SearchingbyHour_00,na.rm = TRUE) 
congestion$SearchingbyHour_01[is.na(congestion$SearchingbyHour_01)] <- median(congestion$SearchingbyHour_01,na.rm = TRUE) 
congestion$SearchingbyHour_02[is.na(congestion$SearchingbyHour_02)] <- median(congestion$SearchingbyHour_02,na.rm = TRUE) 
congestion$SearchingbyHour_03[is.na(congestion$SearchingbyHour_03)] <- median(congestion$SearchingbyHour_03,na.rm = TRUE) 
congestion$SearchingbyHour_04[is.na(congestion$SearchingbyHour_04)] <- median(congestion$SearchingbyHour_04,na.rm = TRUE) 
congestion$SearchingbyHour_05[is.na(congestion$SearchingbyHour_05)] <- median(congestion$SearchingbyHour_05,na.rm = TRUE) 
congestion$SearchingbyHour_06[is.na(congestion$SearchingbyHour_06)] <- median(congestion$SearchingbyHour_06,na.rm = TRUE) 
congestion$SearchingbyHour_07[is.na(congestion$SearchingbyHour_07)] <- median(congestion$SearchingbyHour_07,na.rm = TRUE) 
congestion$SearchingbyHour_08[is.na(congestion$SearchingbyHour_08)] <- median(congestion$SearchingbyHour_08,na.rm = TRUE) 
congestion$SearchingbyHour_09[is.na(congestion$SearchingbyHour_09)] <- median(congestion$SearchingbyHour_09,na.rm = TRUE) 
congestion$SearchingbyHour_10[is.na(congestion$SearchingbyHour_10)] <- median(congestion$SearchingbyHour_10,na.rm = TRUE) 
congestion$SearchingbyHour_11[is.na(congestion$SearchingbyHour_11)] <- median(congestion$SearchingbyHour_11,na.rm = TRUE) 
congestion$SearchingbyHour_12[is.na(congestion$SearchingbyHour_12)] <- median(congestion$SearchingbyHour_12,na.rm = TRUE) 
congestion$SearchingbyHour_13[is.na(congestion$SearchingbyHour_13)] <- median(congestion$SearchingbyHour_13,na.rm = TRUE) 
congestion$SearchingbyHour_14[is.na(congestion$SearchingbyHour_14)] <- median(congestion$SearchingbyHour_14,na.rm = TRUE) 
congestion$SearchingbyHour_15[is.na(congestion$SearchingbyHour_15)] <- median(congestion$SearchingbyHour_15,na.rm = TRUE) 
congestion$SearchingbyHour_16[is.na(congestion$SearchingbyHour_16)] <- median(congestion$SearchingbyHour_16,na.rm = TRUE) 
congestion$SearchingbyHour_17[is.na(congestion$SearchingbyHour_17)] <- median(congestion$SearchingbyHour_17,na.rm = TRUE) 
congestion$SearchingbyHour_18[is.na(congestion$SearchingbyHour_18)] <- median(congestion$SearchingbyHour_18,na.rm = TRUE) 
congestion$SearchingbyHour_19[is.na(congestion$SearchingbyHour_19)] <- median(congestion$SearchingbyHour_19,na.rm = TRUE) 
congestion$SearchingbyHour_20[is.na(congestion$SearchingbyHour_20)] <- median(congestion$SearchingbyHour_20,na.rm = TRUE) 
congestion$SearchingbyHour_21[is.na(congestion$SearchingbyHour_21)] <- median(congestion$SearchingbyHour_21,na.rm = TRUE) 
congestion$SearchingbyHour_22[is.na(congestion$SearchingbyHour_22)] <- median(congestion$SearchingbyHour_22,na.rm = TRUE) 
congestion$SearchingbyHour_23[is.na(congestion$SearchingbyHour_23)] <- median(congestion$SearchingbyHour_23,na.rm = TRUE) 

##Check the missing values after imputation
plot_missing(congestion[,16:63])

##Detect the outlier for the relevant valriables
boxplot(congestion[,c(4:63,66:74)])

##Cap the outliers using flooring and capping.
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

congestion$AvgTimeToPark=capOutlier(congestion$AvgTimeToPark)
congestion$AvgTimeToParkRatio=capOutlier(congestion$AvgTimeToParkRatio)
congestion$TotalSearching=capOutlier(congestion$TotalSearching)
congestion$PercentSearching=capOutlier(congestion$PercentSearching)
congestion$AvgUniqueGeohashes=capOutlier(congestion$AvgUniqueGeohashes)
congestion$AvgTotalGeohashes=capOutlier(congestion$AvgTotalGeohashes)
congestion$PercentCar.x=capOutlier(congestion$PercentCar.x)
congestion$PercentMPV.x=capOutlier(congestion$PercentMPV.x)
congestion$PercentLDT.x=capOutlier(congestion$PercentLDT.x)
congestion$PercentMDT.x=capOutlier(congestion$PercentMDT.x)
congestion$PercentHDT.x=capOutlier(congestion$PercentHDT.x)
congestion$PercentOther.x=capOutlier(congestion$PercentOther.x)
congestion$HD_00=capOutlier(congestion$HD_00)
congestion$HD_01=capOutlier(congestion$HD_01)
congestion$HD_02=capOutlier(congestion$HD_02)
congestion$HD_03=capOutlier(congestion$HD_03)
congestion$HD_04=capOutlier(congestion$HD_04)
congestion$HD_05=capOutlier(congestion$HD_05)
congestion$HD_06=capOutlier(congestion$HD_06)
congestion$HD_07=capOutlier(congestion$HD_07)
congestion$HD_08=capOutlier(congestion$HD_08)
congestion$HD_09=capOutlier(congestion$HD_09)
congestion$HD_10=capOutlier(congestion$HD_10)
congestion$HD_11=capOutlier(congestion$HD_11)
congestion$HD_12=capOutlier(congestion$HD_12)
congestion$HD_13=capOutlier(congestion$HD_13)
congestion$HD_14=capOutlier(congestion$HD_14)
congestion$HD_15=capOutlier(congestion$HD_15)
congestion$HD_16=capOutlier(congestion$HD_16)
congestion$HD_17=capOutlier(congestion$HD_17)
congestion$HD_18=capOutlier(congestion$HD_18)
congestion$HD_19=capOutlier(congestion$HD_19)
congestion$HD_20=capOutlier(congestion$HD_20)
congestion$HD_21=capOutlier(congestion$HD_21)
congestion$HD_22=capOutlier(congestion$HD_22)
congestion$HD_23=capOutlier(congestion$HD_23)
congestion$HD_23=capOutlier(congestion$HD_23)
congestion$SearchingbyHour_00=capOutlier(congestion$SearchingbyHour_00)
congestion$SearchingbyHour_01=capOutlier(congestion$SearchingbyHour_01)
congestion$SearchingbyHour_02=capOutlier(congestion$SearchingbyHour_02)
congestion$SearchingbyHour_03=capOutlier(congestion$SearchingbyHour_03)
congestion$SearchingbyHour_04=capOutlier(congestion$SearchingbyHour_04)
congestion$SearchingbyHour_05=capOutlier(congestion$SearchingbyHour_05)
congestion$SearchingbyHour_06=capOutlier(congestion$SearchingbyHour_06)
congestion$SearchingbyHour_07=capOutlier(congestion$SearchingbyHour_07)
congestion$SearchingbyHour_08=capOutlier(congestion$SearchingbyHour_08)
congestion$SearchingbyHour_09=capOutlier(congestion$SearchingbyHour_09)
congestion$SearchingbyHour_10=capOutlier(congestion$SearchingbyHour_10)
congestion$SearchingbyHour_11=capOutlier(congestion$SearchingbyHour_11)
congestion$SearchingbyHour_12=capOutlier(congestion$SearchingbyHour_12)
congestion$SearchingbyHour_13=capOutlier(congestion$SearchingbyHour_13)
congestion$SearchingbyHour_14=capOutlier(congestion$SearchingbyHour_14)
congestion$SearchingbyHour_15=capOutlier(congestion$SearchingbyHour_15)
congestion$SearchingbyHour_16=capOutlier(congestion$SearchingbyHour_16)
congestion$SearchingbyHour_17=capOutlier(congestion$SearchingbyHour_17)
congestion$SearchingbyHour_18=capOutlier(congestion$SearchingbyHour_18)
congestion$SearchingbyHour_19=capOutlier(congestion$SearchingbyHour_19)
congestion$SearchingbyHour_20=capOutlier(congestion$SearchingbyHour_20)
congestion$SearchingbyHour_21=capOutlier(congestion$SearchingbyHour_21)
congestion$SearchingbyHour_22=capOutlier(congestion$SearchingbyHour_22)
congestion$SearchingbyHour_23=capOutlier(congestion$SearchingbyHour_23)
congestion$AvgAcceleration=capOutlier(congestion$AvgAcceleration)
congestion$PercentOfVehicles=capOutlier(congestion$PercentOfVehicles)
congestion$PercentCar.y=capOutlier(congestion$PercentCar.y)
congestion$PercentMPV.y=capOutlier(congestion$PercentMPV.y)
congestion$PercentLDT.y=capOutlier(congestion$PercentLDT.y)
congestion$PercentMDT.y=capOutlier(congestion$PercentMDT.y)
congestion$PercentHDT.y=capOutlier(congestion$PercentHDT.y)
congestion$PercentOther.y=capOutlier(congestion$PercentOther.y)
congestion$ParkingCirclingDistribution=capOutlier(congestion$ParkingCirclingDistribution)

##Check the outliers after the flooring and capping
boxplot(congestion[,c(4:63,66:74)])

?corrplot
corcong = cor(congestion[,c(4:7,9,66:74)])
corrplot(corcong, method = "number")


#Kmeans
boxplot(congestion[,c(4:63,66:74)])
scaled.cong <- scale(congestion[,c(4:63,66:74)])
set.seed(123)
kmeans.clus = kmeans(x=scaled.cong, centers = 3)
summary(kmeans.clus)

## profiling the clusters
congestion$Clusters <- kmeans.clus$cluster
aggr = aggregate(congestion[,75],list(congestion$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(congestion$Clusters)),
                            aggr[,-1])

View(clus.profile)
library(fpc)
plotcluster(scaled.cong, congestion$Clusters)

library(cluster)
clusplot(congestion[,75], kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

write.csv(congestion, file = "congestion.csv")

view(clus.profile)
##########################################Modelling#################################################################################

##Subset the data
set.seed(123)
smp_size <- floor(0.20*nrow(congestion))
ind <- sample(seq_len(nrow(congestion)), size = smp_size)
train <- congestion[ind, ]
congestion <- train

##train and test
set.seed(123)
smp_size <- floor(0.70*nrow(congestion))
train_ind <- sample(seq_len(nrow(congestion)), size = smp_size)
train <- congestion[train_ind, ]
test <- congestion[-train_ind, ]

str(train)
View(train)
## Ensuring that dependant variable is Categorical hecne converting to factor variables

train$Clusters = as.factor(train$Clusters)
test$Clusters = as.factor(test$Clusters)
str(train)

## Remove the columns which are not needed for prediction

train$Geohash=train$Latitude.y=train$Longitude.y=train$Latitude.x=train$Longitude.x=NULL
test$Geohash=test$Latitude.y=test$Longitude.y=test$Latitude.x=test$Longitude.x=NULL

### Check the class imbalance
table(train$Clusters)

################since the data is Imbalanced we need to oversample the data####################################################################
install.packages("DMwR")
library(DMwR)

##Create balanced data
#over sampling training data
train_1 <- train[train$Clusters == 1, c(1:70)]
train_2 <- train[train$Clusters == 2, c(1:70)]
train_3 <- train[train$Clusters == 3, c(1:70)]

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


b_train_1 <- balanced_train_13[balanced_train_13$Clusters == 1, c(1:70)]
b_train_2 <- balanced_train_23[balanced_train_23$Clusters == 2, c(1:70)]
b_train_3 <- balanced_train_23[balanced_train_23$Clusters == 3, c(1:70)]

bal_train_123 <- rbind(b_train_1, b_train_2, b_train_3)
train <- bal_train_123
table(train$Clusters)

## Oversampling test data
test_1 <- test[test$Clusters == 1, c(1:70)]
test_2 <- test[test$Clusters == 2, c(1:70)]
test_3 <- test[test$Clusters == 3, c(1:70)]

test_12 <- rbind(test_1, test_2)
test_23 <- rbind(test_2, test_3)
test_13 <- rbind(test_1, test_3)

table(test_12$Clusters)
balanced_test_12 <- ovun.sample(Clusters~., data = test_12, p=0.5, method = "over")$data
table(balanced_test_12$Clusters)

table(test_23$Clusters)
balanced_test_23 <- ovun.sample(Clusters~., data = test_23, p=0.5, method = "over")$data
table(balanced_test_23$Clusters)

table(test_13$Clusters)
balanced_test_13 <- ovun.sample(Clusters~., data = test_13, p=0.5, method = "over")$data
table(balanced_test_13$Clusters)


b_test_1 <- balanced_test_13[balanced_test_13$Clusters == 1, c(1:70)]
b_test_2 <- balanced_test_23[balanced_test_23$Clusters == 2, c(1:70)]
b_test_3 <- balanced_test_13[balanced_test_13$Clusters == 3, c(1:70)]

bal_test_123 <- rbind(b_test_1, b_test_2, b_test_3)
test <- bal_test_123
table(test$Clusters)

## Variable Selection
fit_rf = randomForest(Clusters~., data=train)
importance(fit_rf)
varImp(fit_rf)


## Create task
trainTask = makeClassifTask(data = train, target = "Clusters")
testTask = makeClassifTask(data = test, target = "Clusters")

## To check the details
trainTask 

str(getTaskData(trainTask)) 

#Prediction Methods

#Method 1: LDA
lda.learner = makeLearner("classif.lda", predict.type = "response")
lda.learner
lda.model = train(lda.learner, trainTask)
lda.predict = predict(lda.model, testTask)
table(lda.predict$data$truth,lda.predict$data$response) 
confusionMatrix(lda.predict$data$truth,lda.predict$data$response)

###Method 2: CART
cart.learner = makeLearner("classif.rpart", predict.type = "response")
cart.model = train(cart.learner, trainTask)
cartModel=getLearnerModel(cart.model) ## In case you need to plot tree

library(rpart.plot)

prp(cartModel,extra=2, roundint=FALSE)## For plotting tree, you may need rpart.plot

#make predictions
cart.predict = predict(cart.model, testTask)
table(cartModel$data$truth,cartModel$data$response)
confusionMatrix(cart.predict$data$truth,cart.predict$data$response)

#Method 3: Random Forest

getParamSet("classif.randomForest")

#rf.learner = makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf.learner = makeLearner("classif.randomForest", predict.type = "response")
rf.model = train(rf.learner, trainTask)
rf.predict = predict(rf.model, testTask)

table(rf.predict$data$truth,rf.predict$data$response)
confusionMatrix(rf.predict$data$truth,rf.predict$data$response)

#Method 4: SVM

getParamSet("classif.ksvm") #do install kernlab package 


ksvm.learner = makeLearner("classif.ksvm", predict.type = "response")
ksvm.model = train(ksvm.learner, trainTask)
ksvm.predict = predict(ksvm.model, testTask)
table(ksvm.predict$data$truth,ksvm.predict$data$response)
confusionMatrix(ksvm.predict$data$truth,ksvm.predict$data$response)

#Method 5: XGB
getParamSet("classif.xgboost")

#make learner with inital parameters
xgb.learner= makeLearner("classif.xgboost", predict.type = "response")
xgb.model = train(xgb.learner, trainTask)
xgb.predict = predict(xgb.model, testTask)
table(xgb.predict$data$truth,xgb.predict$data$response)
confusionMatrix(xgb.predict$data$truth,xgb.predict$data$response)

#Method 6:GBM
library(gbm)
getParamSet("classif.gbm")
gbm.learner = makeLearner("classif.gbm", predict.type = "response", distribution = "multinomial")

gbm.model = train(gbm.learner, trainTask)

gbm.predict = predict(gbm.model, testTask)

table(gbm.predict$data$truth,gbm.predict$data$response)
confusionMatrix(gbm.predict$data$truth,gbm.predict$data$response)

#Method 7: knn
getParamSet("classif.knn")

knn.learner=makeLearner("classif.knn",predict.type = "response")
knn.model=train(knn.learner,trainTask)
knn.predict=predict(knn.model, testTask)

confusionMatrix(knn.predict$data$truth,knn.predict$data$response)
