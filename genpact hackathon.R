setwd("C:/Users/barun/Desktop/Analytics/Hackathon/Genpact/train")

library("data.table")
library("h2o")
library("ggplot2")
library("dummies")
library("dplyr")
library("Metrics")

train <- fread("train.csv", stringsAsFactors = TRUE)
centerInfo <- fread("fulfilment_center_info.csv", stringsAsFactors = TRUE)
mealInfo <- fread("meal_info.csv", stringsAsFactors = TRUE)
test <- fread("test.csv", stringsAsFactors = TRUE)

test$num_orders <- "test"
total <- rbindlist(list(train, test))

total <- left_join(total,centerInfo,by="center_id")
total <- left_join(total,mealInfo, by="meal_id")

#Add discount
total$Saving <- ifelse(total$base_price-total$checkout_price>0, total$base_price-total$checkout_price,0)
#Add ExtraMoneyShelled
total$ExtraMoneyShelled <- ifelse(total$base_price-total$checkout_price<0, total$checkout_price-total$base_price,0)

total$emailer_for_promotion <- as.factor(total$emailer_for_promotion)
total$homepage_featured <-as.factor(total$homepage_featured)
total$city_code <- as.factor(total$city_code)
total$region_code <- as.factor(total$region_code)

temp <- total %>%
  group_by(center_id, week) %>%
  summarise(MealsSold=n())
total <- left_join(total,temp,by=c("center_id", "week"))

temp <- total %>%
  group_by(center_id, week, category) %>%
  summarise(MealsCatSold=n())
total <- left_join(total,temp,by=c("center_id", "week","category"))

temp <- total %>%
  group_by(center_id, week, category, cuisine) %>%
  summarise(CatCusSold=n())
total <- left_join(total,temp,by=c("center_id", "week","category", "cuisine"))

temp <- total %>%
  group_by(city_code) %>%
  summarise(CenterInCity=length(unique(center_id)))
total <- left_join(total,temp,by=c("city_code"))

temp <- total %>%
  group_by(city_code,region_code) %>%
  summarise(CenterInCityRegion=length(unique(center_id)))
total <- left_join(total,temp,by=c("city_code","region_code"))

#Add Week cat
total$DataAge <- "Latest"
total[total$week>=1 & total$week<=24,]$DataAge <- "Ex.Old"
total[total$week>=25 & total$week<=48,]$DataAge <- "V.Old"
total[total$week>=49 & total$week<=72,]$DataAge <- "Old"
total[total$week>=73 & total$week<=96,]$DataAge <- "Mid"
total[total$week>=97 & total$week<=120,]$DataAge <- "Recent"
total$DataAge <- as.factor(total$DataAge)

#Checkout Price Cat
total$CheckoutPrice_Cat <- "V.High"
total[total$checkout_price>=0 & total$checkout_price <=200,]$CheckoutPrice_Cat <- "Low"
total[total$checkout_price>=201 & total$checkout_price <=300,]$CheckoutPrice_Cat <- "Med"
total[total$checkout_price>=301 & total$checkout_price <=450,]$CheckoutPrice_Cat <- "High"
total$CheckoutPrice_Cat <- as.factor(total$CheckoutPrice_Cat)


#split to train & test
train_new <- total[total$num_orders!="test",]
test_new <- total[total$num_orders=="test", c(-9)]

#num_orders - change it to numeric
train_new$num_orders <- as.numeric(as.character(train_new$num_orders))

#Models
localh2o <- h2o.init(nthreads = -1)

#Transfer data from R to h2o instance
train_h2o <- as.h2o(train_new)
test_h2o <- as.h2o(test_new)

split_train_h2o <- h2o.splitFrame(train_h2o,c(0.8), seed = 1234)
traindata_h2o <- h2o.assign(split_train_h2o[[1]], "train")
Valdata_h2o <- h2o.assign(split_train_h2o[[2]], "Validation")


colnames(train_h2o) # check col index numbers

#dependent variable 
dependentVar <- 9

#features to used to predict
featuresVars <- c(2,3,4,5:8,10:24)

#gbm
system.time(
  h2o_gbm1 <- h2o.gbm(y=dependentVar, 
                     x=featuresVars,
                     training_frame = train_h2o,
                     #training_frame = traindata_h2o,
                     #validation_frame = Valdata_h2o,
                     ntrees =  1000,
                     max_depth = 15,
                     learn_rate = 0.01, #0.04,
                     distribution = "gaussian",
                     seed = 1000)
)
h2o.performance(h2o_gbm1)
h2o.varimp(h2o_gbm1)
h2o.varimp_plot(h2o_gbm1,num_of_features = 10)

df_yhat_train <- as.data.frame(h2o.predict(h2o_gbm1, train_h2o) ) 
predH2OIn<- df_yhat_train$predict 
cor(predH2OIn,as.numeric(train_new$num_orders))

#make prediction on test data
pred_gbm1 <- as.data.frame(h2o.predict(h2o_gbm1, test_h2o))
#in case some prediction are <0 then make them 0
pred_gbm1$predict <- ifelse(pred_gbm1$predict<0,0,pred_gbm1$predict)
gbm_output1 <- data.frame(id=test$id, num_orders=pred_gbm1$predict)
write.csv(gbm_output1, "gbm_output1.csv", row.names = FALSE)


