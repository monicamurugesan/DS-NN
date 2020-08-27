forestfire <-read.csv(file.choose())
View(forestfire)
attach(forestfire)
forestfire$month <-as.factor(month)
forestfire$size_category <-as.integer(ifelse(size_category=='small',0,1))
forestfire$day <- as.factor(day)
forestfire$temp <-as.integer(temp)
forestfire$DMC <- as.integer(DMC)
forestfire$ISI <-as.integer(ISI)
forestfire$FFMC <-as.integer(FFMC)
forestfire$wind <-as.integer(wind)
forestfire$rain <-as.integer(rain)
forestfire$area <-as.integer(area)
forestfire$DC <-as.integer(DC)
forestfire1<-forestfire[,-c(1,2)]

library(caret)
training <-createDataPartition(forestfire1$size_category,p=0.75,list=F)
forest_fire_train <-forestfire1[training,]
forest_fire_test <-forestfire1[-training,]
View(forest_fire_test)
library(neuralnet)
library(nnet)
forest_model <-neuralnet(size_category~.,data=forest_fire_train)
summary(forest_model)
str(forest_model)
plot(forest_model)



set.seed(12323)
model_r1 <- compute(forest_model,forest_fire_test)
str(model_r1)
predicted_strength <- model_r1$net.result
table(predicted_strength,forest_fire_test$size_category)
# predicted_strength
# model_results$neurons
cor(predicted_strength,forest_fire_test$size_category)
plot(predicted_strength,forest_fire_test$size_category)


model_5<-neuralnet(size_category~.,data=forest_fire_train,hidden = 5,threshold=0.04, act.fct="logistic", linear.output=TRUE, stepmax=1e7)
model_5_res<-compute(model_5,forest_fire_test)
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forest_fire_test$size_category)
plot(pred_strn_5,forest_fire_test$size_category)


?neuralnet
model_5_tr<-neuralnet(size_category~.,data=forest_fire_train,hidden = 5,threshold=0.04, act.fct="tanh", linear.output=TRUE,stepmax=1e7)
model_5_trs<-compute(model_5_tr,forest_fire_test)
pred_strn_51<-model_5_trs$net.result
cor(pred_strn_51,forest_fire_test$size_category)
plot(pred_strn_51,forest_fire_test$size_category)
