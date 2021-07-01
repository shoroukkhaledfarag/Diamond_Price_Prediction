###(1) Workspace Cleaning
#=======================
rm(list=ls())
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)
cat("\014")




###(2) Setting work directory
#===========================

getwd()
setwd("../Desktop/Big Data/project")





###(3) Importing Libraries
#===========================

library(sparklyr)
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(e1071)
require(caTools)  
library(dplyr)
library(pls)
library(GGally)
library(ranger)
library(LiblineaR)


#spark_install(version = "2.1.0")



###(4) Connecting to Spark
#===========================

sc <- spark_connect(master = "local")

spark_web(sc)



###(5) Importing Dataset
#======================


diamond_tbl <- spark_read_csv(sc, name = "diamond_data", path = "dataset/diamonds_dataset.csv", header = TRUE, delimiter = ",")
diamond_tbl 
diamond_tbl<- as.data.frame(diamond_tbl)

summary(diamond_tbl)

str(diamond_tbl)

###(5) Data Cleaning 
#======================


#(5.1) Remove unwanted columns
#============================

diamond_tbl<- subset(diamond_tbl, select=-c(url,date_fetched,id))
summary(diamond_tbl)


#(5.2) Removing rows with na
#============================

na.omit(diamond_tbl)



#(5.2) Remove Noisy data
#============================

diamond_tbl<-diamond_tbl[!(diamond_tbl$price==1348720),]




#(5.3) casting char to factors
#==============================
diamond_tbl$shape <- as.factor(diamond_tbl$shape)
diamond_tbl$cut <- as.factor(diamond_tbl$cut)
diamond_tbl$color <- as.factor(diamond_tbl$color)
diamond_tbl$clarity <- as.factor(diamond_tbl$clarity)
diamond_tbl$type <- as.factor(diamond_tbl$type)
diamond_tbl$report <- as.factor(diamond_tbl$report)

summary(diamond_tbl)

str(diamond_tbl)



###(6) Data Visualization
#=========================


# 1 . what do we have in the dataset?
# 2. general trends of the dataset and relations 


ggplot(diamond_tbl,aes(x=cut,y=carat))+geom_point() 

ggplot(subset(diamond_tbl,800 < price & price < 3490 + 1.25*(3490-900)+1000),aes(x=cut,y=price, color=cut)) +
  geom_boxplot()



#(6.1) price vs shape
#=========================

p1<-ggplot(diamond_tbl,aes(x=shape, y=price))+geom_point()
p1


#(6.2) price vs clarity
#=========================
ggplot(diamond_tbl,aes(x=clarity,y=price))+geom_point()

#(6.2) type vs price
#=========================
ggplot(diamond_tbl,aes(x=type,y=price))+geom_point()

#(6.3) price vs carat vs cut
#===========================
ggplot(diamond_tbl,aes(x=cut,y=price,size = carat))+geom_point()


#(6.4) price vs carat vs cut for FL clarity
#==========================================
ggplot(subset(subset(diamond_tbl, 900 < price & price < 3490 + 1.25*(3490-900)),clarity %in% c( "FL" , "IF") , cut %in% c("Super Ideal" , "Ideal")),aes(x=carat,y=price, color = shape ))+ geom_point()

summary (subset(diamond_tbl, 900 < price & price < 3490 + 1.25*(3490-900)))

#(6.5) msh fahmaaa ????
#=========================
ggplot(diamond_tbl,aes(x=cut,y=carat, color=cut)) +
  geom_boxplot()


#(6.6) price vs carat 
#=========================
ggplot(diamond_tbl,aes(x=carat, y=price,))+geom_line()



#(6.7) price vs carat  vs cut 
#============================
ggplot(diamond_tbl,aes(x=carat,y=price,))+geom_line(aes(color = cut))
# the above plot is (meaningless) and hence shows there are other factors
# but still the general trend increase in carat = increase in price



#(6.8) price vs carat vs clarity 
#===============================
ggplot(diamond_tbl,aes(x=carat,y=cut))+geom_point(aes(color = shape))




#(6.9) price vs clarity
#=========================
ggplot(diamond_tbl, aes(x=(price))) +geom_histogram(stat="bin",binwidth= 500) 
+facet_wrap(~clarity, scales = "free")

ggplot(diamond_tbl, aes(x=(price))) +geom_histogram(stat="bin",binwidth= 500) 


#(6.10) msh fahmaa ???
#=========================
ggplot(subset(diamond_tbl,price < 3490 + 1.25*(3490-900)), aes(x=(price))) +
  geom_histogram(stat="bin",binwidth= 500) +
  facet_wrap(~clarity, scales = "free")


#(6.11) msh fahmaa ???
#=========================
ggplot(subset(diamond_tbl,price < 3490 + 1.25*(3490-900)),aes(x=(price))) +
  geom_histogram(stat="bin",binwidth= 500) +
  facet_wrap(~cut, scales = "free")

#(6.12) msh fahmaa ???
#=========================
ggplot(subset(diamond_tbl,price < 3490 + 1.25*(3490-900)), 
       aes(x = carat, y = price))+
  geom_point() + facet_wrap(~clarity, ncol = 10)




# all others are factors not numeric 

#(6.13) Denisty Plots 
#=========================

#(6.13.1) cut vs color 
#=========================
new= 
  diamonds %>%
  group_by(color, cut) %>%
  summarise(n=n()) %>%
  group_by(cut) %>%
  mutate(sum.n= sum(n)) %>%
  ungroup() %>%
  mutate(n2= n/sum.n) %>%
  select(color, cut, n2)

new %>% spread(cut,n2)


new %>%
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill=n2*100), colour = "white") +
  scale_fill_gradient(low="white",high="blue") +
  labs(fill = "Density")


#(6.13.2) cut vs clarity 
#=========================
new2= 
  diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n()) %>%
  group_by(cut) %>%
  mutate(sum.n= sum(n)) %>%
  ungroup() %>%
  mutate(n2= n/sum.n) %>%
  select(clarity, cut, n2)

new2 %>% spread(cut,n2)


new2 %>%
  ggplot(aes(clarity, cut)) +
  geom_tile(aes(fill=n2*100), colour = "white") +
  scale_fill_gradient(low="white",high="blue") +
  labs(fill = "Density")




###(7) Correlation
#====================


ggpairs(diamond_tbl)

#(7.1) price vs clarity
#=========================
cor(diamond_tbl$carat,diamond_tbl$price)


# ==================================

table1<-table(diamond_tbl$cut,diamond_tbl$carat)
chi2 = chisq.test(table1, correct=F)
sqrt(chi2$statistic / sum(table1))

#(7.2) color &  clarity
#==========================
table2<-table(diamond_tbl$color,diamond_tbl$clarity)
chi2 = chisq.test(table2, correct=F)
sqrt(chi2$statistic / sum(table2))



###(8) Model Training
#====================

#(8.1) Spliting  Data
#====================
sample_size = floor(0.8*nrow(diamond_tbl))
set.seed(123)

# randomly split data in r
picked = sample(seq_len(nrow(diamond_tbl)),size = sample_size)
trainData =diamond_tbl[picked,]
testData =diamond_tbl[-picked,]

#summary(trainData$price)
#summary(testData$price)


summary(trainData)
summary(testData)

#(8.2) Model Fitting using lr
#=============================

cut <- trainData$cut 
color <- trainData$color
clarity <- trainData$clarity
carat <- trainData$carat
shape <-  trainData$shape
type <-  trainData$type
price <-  trainData$price

cut <- factor(cut, ordered = FALSE)
color <- factor(color, ordered = FALSE)
clarity <- factor(clarity, ordered = FALSE)


fit <- lm(log(price) ~ log(carat) + cut + color + clarity + shape +type )
summ_fit <- summary(fit)
summ_fit


#(8.3) Prediction 
#====================

ypred_train <- predict(fit, trainData)
ypred_train

plot(ypred_train , log(trainData$price))



ypred <- predict(fit, testData)
ypred

plot(ypred , log(testData$price))




#(8.4) Model performance 
#========================

RMSE <-RMSE(ypred_train, log(trainData$price))
RMSE

error_rate <- RMSE/mean(log(trainData$price)) 
error_rate

accuracy <- (1- error_rate)*100
accuracy



RMSE <-RMSE(ypred, log(testData$price))
RMSE

error_rate <- RMSE/mean(log(testData$price)) 
error_rate

accuracy <- (1- error_rate)*100
accuracy


R2 <- R2(ypred, log(testData$price))
R2


#(8.4) Repeat for pcr
#=============================

fit_pcr <- pcr(log(price) ~ log(carat) + cut + color + clarity + shape +type , ncomp = 15)
summary(fit_pcr)
vimp <- varImp(fit_pcr)
vimp

plot(vimp)

fit <- fit_pcr
ypred_train <- predict(fit, trainData)
ypred_train



ypred <- predict(fit, testData)
ypred


RMSE <-RMSE(ypred_train, log(trainData$price))
RMSE

error_rate <- RMSE/mean(log(trainData$price)) 
error_rate

accuracy <- (1- error_rate)*100
accuracy




RMSE <-RMSE(ypred, log(testData$price))
RMSE

error_rate <- RMSE/mean(log(testData$price)) 
error_rate

accuracy <- (1- error_rate)*100
accuracy


#(8.5) Repeat for  plsr
#=============================



cut <- trainData$cut 
color <- trainData$color
clarity <- trainData$clarity
carat <- trainData$carat
shape <-  trainData$shape
type <-  trainData$type
price <-  trainData$price

cut <- factor(cut, ordered = FALSE)
color <- factor(color, ordered = FALSE)
clarity <- factor(clarity, ordered = FALSE)

fit_plsr <- plsr(log(price) ~ log(carat) + cut + color + clarity + shape +type, validation="CV")
summ_fit_plsr<- summary(fit_plsr)
summ_fit_plsr

fit <- fit_plsr
ypred_train <- predict(fit, trainData)
ypred_train


ypred <- predict(fit, testData)
ypred

plot(ypred , log(testData$price))


RMSE <-RMSE(ypred_train, log(trainData$price))
RMSE

error_rate <- RMSE/mean(log(trainData$price)) 
error_rate

accuracy <- (1- error_rate)*100
accuracy



RMSE <-RMSE(ypred, log(testData$price))
RMSE

error_rate <- RMSE/mean(log(testData$price)) 
error_rate

accuracy <- (1- error_rate)*100
accuracy



set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(log(price) ~ log(carat) + cut + color + clarity + shape +type, data = trainData, method = "lm",
               trControl = train.control)
# Summarize the results

summary(model)

vimp <- varImp(model)
vimp

plot(vimp)

trainData$predicted <- predict(model, newdata = trainData)
cor(log(trainData$price), trainData$predicted)

rmse(log(trainData$price), trainData$predicted)

testData$predicted <- predict(model, newdata = testData)
cor(log(testData$price) ,testData$predicted)

rmse(log(testData$price), testData$predicted)



set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(log(price) ~ log(carat) + cut + color + clarity + shape +type, data = trainData, method = "ranger",
               trControl = train.control)
# Summarize the results
randomforest_model <- model
summary(model)

vimp <- varImp(model)
vimp

plot(vimp)

trainData$predicted <- predict(model, newdata = trainData)
cor(log(trainData$price), trainData$predicted)

rmse(log(trainData$price), trainData$predicted)

testData$predicted <- predict(model, newdata = testData)
cor(log(testData$price) ,testData$predicted)

rmse(log(testData$price), testData$predicted)

######################################Spark#########################################

trainData$price <- log(trainData$price)

trainData$carat <- log(trainData$carat)

testData$price <- log(testData$price)

testData$carat <- log(testData$carat)




diamond_sc <- copy_to(sc, trainData)


vfolds <- sdf_random_split(
  diamond_sc,
  weights = purrr::set_names(rep(0.1, 10), paste0("fold", 1:10)),
  seed = 42
)

analysis_set <- do.call(rbind, vfolds[2:10])
assessment_set <- vfolds[[1]]

model <- ml_linear_regression(analysis_set,price ~ carat + cut + color + clarity + shape +type)
model


betas <- tidy(model)
betas

test_sc <- copy_to(sc, testData)
predictions <- ml_predict(model, test_sc )
predictions
predictions <- data.frame(predictions)
plot(testData$price,predictions$price)


predictions_assessment_set <- ml_predict(model, assessment_set )

validation_summary <- ml_evaluate(model, assessment_set)
RMSE_spark <- validation_summary$root_mean_squared_error
RMSE_spark

assessment_set <- data.frame(assessment_set)
error_rate <- RMSE_spark/mean(assessment_set$price) 
error_rate

accuracy <- (1- error_rate)*100
accuracy



levels(trainData$shape)
cut <- c("Very Good" , "Ideal" , "Fair")
clarity <- c("SI1", "FL", "VS2")
color <- c("E", "F", "G")
carat <- c(3.3 , 1.5 , 0.5)
shape <- c("Emerald","Pear","Round")
type <-  c("lab", "natural" , "natural")
carat <- log(carat)
mydata <- data.frame(cut,clarity,color , carat , shape , type)
predictions <- ml_predict(model, copy_to(sc, mydata))
exp(data.frame(predictions)$prediction)
spark_disconnect(sc)