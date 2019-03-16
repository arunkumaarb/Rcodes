library(Boruta)
library(neuralnet)
library(caret)
library(data.table)

setwd("C:/Users/barun/Desktop/GreatLakes/Data mining/Assignment/")

#READING THE DATA
data = fread("HR_Employee_Attrition_Data.csv",header=T) 

#CHECK IF NULL VALUES ARE PRESENT
any(is.na(data))

#USING BORUTA PACKAGE TO FIND IMPORTANT VARIABLES
data_bor=fread("HR_Employee_Attrition_Data.csv",header=T,stringsAsFactors = TRUE) 

str(data_bor)

data_bor$Education =as.factor(data_bor$Education)
data_bor$EnvironmentSatisfaction = as.factor(data_bor$EnvironmentSatisfaction)
data_bor$JobInvolvement = as.factor(data_bor$JobInvolvement )
data_bor$JobLevel=as.factor(data_bor$JobLevel) 
data_bor$PerformanceRating=as.factor(data_bor$PerformanceRating)
data_bor$RelationshipSatisfaction=as.factor(data_bor$RelationshipSatisfaction)
data_bor$StockOptionLevel=as.factor(data_bor$StockOptionLevel)
data_bor$WorkLifeBalance=as.factor(data_bor$WorkLifeBalance)

boruta.train <- Boruta(Attrition~., data = data_bor, doTrace = 2)
plot(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

#Finally Boruta says that "EmployeeCount","EmployeeNumber","Over18","StandardHours" are insignificant varaibles

########################################################
#FINAL DATASET
#We removing EmployeeCount,standardhours,over18 and Employee Variable
########################################################

data_final = data[,-c("EmployeeCount","EmployeeNumber","StandardHours","Over18")]
View(data_final)
table(data_final$Attrition)


######################################################
#TRAIN TEST DATA
######################################################
nrow(data_final)
s=sample(1:2940, 880)
train = data_final[-s,]
test = data_final[s,]
c(nrow(train),nrow(test))

####################################################
#EDA
####################################################

names(train)
attach(train)
library(ggplot2)
ggplot(train,aes(x=factor(Gender),fill=factor(Attrition)))+geom_bar()
#Attrition rate is more in male
ggplot(train,aes(x=factor(Gender),fill=factor(Attrition)))+geom_bar()+facet_wrap(~factor(Department))
#Its more in R&D
ggplot(train,aes(x=factor(Gender),fill=factor(Attrition)))+geom_bar()+facet_wrap(~factor(MaritalStatus))

ggplot(train,aes(x=Age,y=MonthlyIncome))+geom_point()+geom_smooth(se=F)
ggplot(train,aes(x=TotalWorkingYears,y=MonthlyIncome))+geom_point()+geom_smooth(se=F)

#Stockoption
ggplot(train,aes(x=factor(StockOptionLevel),fill=factor(Attrition)))+geom_bar()


#Education
ggplot(train,aes(x=factor(Gender),fill=factor(Attrition)))+geom_bar()+facet_wrap(~factor(Education))


#Worklife
ggplot(train,aes(x=factor(Gender),fill=factor(Attrition)))+geom_bar()+facet_wrap(~factor(WorkLifeBalance))

#Rating
ggplot(train,aes(x=factor(PerformanceRating),fill=factor(Gender)))+geom_bar()
ggplot(train,aes(x=factor(PerformanceRating),fill=factor(Gender)))+geom_bar()+facet_wrap(~factor(Attrition))

#JobSatisfaction
ggplot(train,aes(x=factor(Gender),fill=factor(Attrition)))+geom_bar()+facet_wrap(~factor(JobSatisfaction))



######################################################
#USING MODEL MATRIX AND CREATING DUMMIES
######################################################
View(train.dev)

train.dev = model.matrix(~(Attrition+BusinessTravel+Department+EducationField+Gender+JobRole+MaritalStatus+OverTime),data=train)
class(train.dev)

train.dev = train.dev[,-1]
trainl = train[,-c("Attrition","BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","OverTime")]
train_final = data.frame(trainl,train.dev)
View(train_final)


#######################################################
#SCALING
#######################################################

class(train_final)
names(train_final)
train_scale = train_final[,-c(24)]
View(train_scale)
str(train_scale)
for (i in 23:44){
  train_scale[,i] = as.integer(train_scale[,i])
}

train_scale = scale(train_scale)
train_scale_final = data.frame(train_final[,24],train_scale)
View(train_scale_final)
names(train_scale_final)
setnames(train_scale_final,"train_final...24.","Attrition")
str(train_scale_final)


##################################
#CART
##################################
library(rpart)
library(rpart.plot)
library(rattle)

n <- names(train_scale_final)
f=as.formula(paste("Attrition ~", paste(n[!n %in% c("Attrition")], collapse = " + ")))


ctrl=rpart.control(minsplit=50, minbucket = 10, cp = 0, xval = 10)
rf= rpart(formula=f,data=train_scale_final,method="class",control=ctrl)

fancyRpartPlot(rf)

printcp(rf)

plotcp(rf)


#Pruning the tree



Tree = prune.rpart(rf,cp=0.007,"CP")

fancyRpartPlot(Tree)

train_scale_final$predict.class_cart = predict(Tree,train_scale_final,type='class')

train_scale_final$predict.score_cart = predict(Tree,train_scale_final)


with(train_scale_final, table(Attrition, predict.class_cart))
##################################################
#TEST DATA
##################################################

View(test)
test$Attrition = ifelse(test$Attrition=="Yes",1,0)
test.dev = model.matrix(~(Attrition+BusinessTravel+Department+EducationField+Gender+JobRole+MaritalStatus+OverTime),data=test)
class(test.dev)

test.dev = test.dev[,-1]
testl = test[,-c("Attrition","BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","OverTime")]
test_final = data.frame(testl,test.dev)
View(test_final)
str(test_final)
class(test_final)
names(test_final)
test_scale = test_final[,-c(24)]
View(test_scale)
str(test_scale)
for (i in 23:44){
  test_scale[,i] = as.integer(test_scale[,i])
}

test_scale = as.data.frame(scale(test_scale))

View(test)
nrow(test_scale)
nrow(c(test,test_scale))
##TEST DATA PREDICTION
test$predict.class_rf <- predict(rf, test_scale, type="class")
test$predict.score_rf <- predict(rf, test_scale)

#Confusion matrix
with(test, table(Attrition, predict.class_rf))


######################################################
#NEURAL NETWORK
######################################################

nn3 <- neuralnet(formula = f,
                 data = train_scale_final, 
                 hidden = c(25,5),
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.01,
                 stepmax = 4000
                 
)

plot(nn3)

# Assigning the Probabilities to Dev Sample
train_scale_final$Predict.score_nn = nn3$net.result[[1]] 
sum((train_scale_final$Attrition - train_scale_final$Predict.score_nn)^2)  /  2

# The distribution of the estimated probabilities
quantile(train_scale_final$Predict.score_nn, c(0,1,5,10,25,50,75,90,95,98,99,100)/100)
hist(train_scale_final$Predict.score_nn)

###########################################
#DECILING
###########################################

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
train_scale_final$deciles <- decile(train_scale_final$Predict.score_nn)

################################################
#RANKING
################################################
library(data.table)
library(scales)

tmp_DT = data.table(train_scale_final)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]

rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)
View(train_scale_final)

############################################
#MODEL PERFORMANCE
############################################

library(ROCR)
library(ineq)
library(caret)
library(e1071)

names(train_scale_final)
pred <- ROCR::prediction(train_scale_final$Predict.score_nn, train_scale_final$Attrition)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = ineq(train_scale_final$Predict.score_nn, type="Gini")

## Assgining 0 / 1 class based on certain threshold
train_scale_final$Predict.class_nn = ifelse(train_scale_final$Predict.score_nn>0.22,1,0)
#Confusion Matrix

confusionMatrix( as.factor(train_scale_final$Predict.class_nn),as.factor(train_scale_final$Attrition))

# Error Computation
sum((train_scale_final$Attrition - train_scale_final$Predict.score_nn)^2)/2



#test_scale_final[,1] = as.integer(test_scale_final[,1])
#setnames(test_scale_final,"test_final...24.","Attrition")



##############################
#PREDICTION OF TEST DATA
##############################

compute.output = compute(nn3, test_scale)
compute.output
test$Predict.score_nn = compute.output$net.result
View(test)

##############################
#MODEL ACCURACY OF TEST DATA
##############################

quantile(test$Predict.score_nn, c(0,1,5,10,25,50,75,90,95,99,100)/100)
test$deciles <- decile(test$Predict.score_nn)

library(data.table)
tmp_DT = data.table(test)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)
View(rank)

test$Predict.class_nn = ifelse(test$Predict.score_nn>0.22,1,0)
confusionMatrix( as.factor(test$Predict.class_nn),as.factor(test$Attrition))


#######################################
#ENSEMBLE
######################################

#TAKING AVERAGE PREDICTION
test$predict_avg<-(test$Predict.score_nn+test$predict.score_rf)/2
test$predict_avg<-as.factor(ifelse(test$predict_avg>0.5,1,0))
confusionMatrix( as.factor(test$predict_avg),as.factor(test$Attrition))
View(test)

#WEIGHTED AVERAGE
test$pred_weighted_avg<-(test$Predict.score_nn*0.8)+(test$predict.score_rf*0.2)
test$pred_weighted_avg<-as.factor(ifelse(test$pred_weighted_avg>0.5,1,0))


confusionMatrix( as.factor(test$pred_weighted_avg),as.factor(test$Attrition))

#MAJORITY VOTE
test$pred_majority<-as.factor(ifelse(test$predict.class_rf==1 & test$Predict.class_nn==1,1,0))
confusionMatrix( as.factor(test$pred_majority),as.factor(test$Attrition))









