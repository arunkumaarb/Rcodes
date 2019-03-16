
#setting the workspace
setwd("C:/Users/gsstseka/Box Sync/Greatlakes/Capstone Project/BALIC/Data")

#Loading the consolidated file
bajaj=read.csv("Consolidated rolling 6 months for Clustering.csv",header = TRUE)

#Removing the first column "Unit code" and Bajaj Cluster
bajaj1= subset(bajaj, select =-c(Agency_Code,Territory_Code,Month,Cluster_Bajaj))

#Cluster Analysis
set.seed(100)
kmeans.clus = kmeans(x=bajaj1, centers = 3, nstart = 25)
#Checking the Centroid
kmeans.clus$centers

#adding the field to source file
bajaj$kmeansclust=kmeans.clus$cluster

#confusion Matrix

table(bajaj$Cluster_Bajaj,bajaj$kmeansclust)

#Naming the Cluster based on Confusion Matrix

bajaj$kmeansclust= ifelse(bajaj$kmeansclust==1,"AMBER",(ifelse(bajaj$kmeansclust==2,"GREEN","RED")))

#Writing the output to CSV file
write.csv(bajaj, "cluster output.csv")


##########Random forest for classification#########

#Removing Agency Code,Territory and Month
bajajrf=subset(bajaj,select = -c(Agency_Code,Territory_Code,Month,Cluster_Bajaj))

#Changing cluster from characters to factor
bajajrf$kmeansclust=as.factor(bajajrf$kmeansclust)

#splitting Train and Test file
set.seed(100)
#Choosing 70/30 Train and Test
sample=sample.int(n=nrow(bajajrf),size = floor(0.70*nrow(bajajrf)))

#Random sample 70% data for train
bajajrf_train= bajajrf[sample,]

#remaining data for Train
bajajrf_test=bajajrf[-sample,]

#Randomforest Model
library(randomForest)
Model_rf=randomForest(kmeansclust~., data=bajajrf_train)

#Model Summary and confusion matrix
Model_rf

#Predicting the data in test data set
bajajrf_test$predict=predict(Model_rf,subset.data.frame(bajajrf_test))

#confusion Matrix for Test
table(bajajrf_test$kmeansclust,bajajrf_test$predict)

#############Time Series Forecast###########


library(forecast)
library(stats)
library(data.table)
library(TTR)
install.packages("MARSS")

library(MARSS)

View(ivesDataByWeek)

View()

bajajts=subset(bajaj,select = -c(Agency_Code,Territory_Code,Cluster_Bajaj,kmeansclust))
data=bajajts[,c(2,3,4,5,6,7)]
tsmodel= MARSS(data)