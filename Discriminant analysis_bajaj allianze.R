
#setting the workspace
setwd("C:/Users/gsstseka/Box Sync/Greatlakes/Capstone Project/BALIC/Data")

#Loading the consolidated file
bajaj=read.csv("Consolidated rolling 6 months for DA.csv",header = TRUE)

#Removing the first column "Unit code" and Bajaj Cluster
bajaj1= subset(bajaj, select =-c(Agency_Code,Territory_Code,Month))

str(bajaj1)

library(DiscriMiner)
X=bajaj1[,1:6]
Y=bajaj1$Bajaj.Cluster

Mahalanobis=linDA(X,Y)

Mahalanobis

write.csv(Mahalanobis$scores,"Mahalanobis.csv")

library(MASS)

X1=as.matrix(bajaj1[,1:6])
Y1=as.vector(bajaj1$Our.Cluster)
Jacknife=lda(Y1~X1,CV=TRUE)

table(Actual=bajaj1$Our.Cluster,Predicted=Jacknife$class)


