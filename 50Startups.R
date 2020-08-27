startups <-read.csv(file.choose())
View(startups)
attach(startups)
str(startups)
startups['State'] <- as.integer(factor(startups$State,levels=c('New York','California','Florida'),labels=c(0,1,2)))
str(startups)
# start<-scale(startups[,-5])
# norm <-cbind(start,Profit)
# str(norm)
library(caret)
training <-createDataPartition(startups$Profit,p=0.75,list=F)
start_train <-startups[training,]
start_test <-startups[-training,]
library(neuralnet)
library(nnet)
startmodel <-neuralnet(Profit~.,data=start_train,hidden = 5,threshold=0.04, act.fct="tanh", linear.output=TRUE, stepmax=1e7)
str(startmodel)
plot(startmodel)


set.seed(12323)
model_results <- compute(startmodel,start_test[1:5])
str(model_results)
predicted_strength <- model_results$net.result
table(predicted_strength,start_test$Profit)
# predicted_strength
# model_results$neurons
cor(predicted_strength,start_test$strength)


startmodel1 <-neuralnet(Profit~.,data=start_train,hidden = 10,threshold=0.07, act.fct="logistic", linear.output=TRUE, stepmax=1e7)
str(startmodel1)
plot(startmodel1)


set.seed(12323)
model_results1 <- compute(startmodel1,start_test[1:5])
str(model_results1)
predicted_strength1 <- model_results1$net.result
table(predicted_strength,start_test$Profit)
# predicted_strength
# model_results$neurons
cor(predicted_strength,start_test$strength)

