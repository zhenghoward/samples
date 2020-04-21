setwd("~/Desktop/242 Data Analysis App/LC/lending club/3yr")

#install.packages("glmnet")

library(dplyr)
library(reshape2)
library(ggplot2)
library(GGally) # extend ggplot2
library(ROCR) # visualizing classifier performance in R
library(caTools) # splits
library(rpart) # CART
library(rpart.plot) # CART plotting
library(randomForest)
library(gbm) # Generalized Boosted Regression Models
library(caret) # cross validation
library(MASS) # Modern Applied Statistics with S
library(glmnet) # General Linear with Regularization

train = read.csv("training_default.csv")
test = read.csv("testing_default.csv")
#train = read.csv("training_profit.csv")
#test = read.csv("testing_profit.csv")

train = train[,-1] # remove X column
test = test[,-1] # remove X column
train$default=as.factor(train$default)
test$default=as.factor(test$default)

train = train[,-which(colnames(train)=="term")] # remove "term" - no variation
test = test[,-which(colnames(test)=="term")] # remove "term" - no variation
train = train[,-which(colnames(train)=="int_rate")] # remove interest rate / use sub_grade instead
test = test[,-which(colnames(test)=="int_rate")] # remove interest rate / use sub_grade instead
train = train[,-which(colnames(train)=="grade")] # remove grade - no variation
test = test[,-which(colnames(test)=="grade")] # remove grade - no variation
train = train[,-which(colnames(train)=="cluster")] # remove cluster
test = test[,-which(colnames(test)=="cluster")] # remove cluster
train = train[,-which(colnames(train)=="title")] # remove title - no variation
test = test[,-which(colnames(test)=="title")] # remove title - no variation
train = train[,-which(colnames(train)=="addr_state")] # remove state - introduce problem in prediction
test = test[,-which(colnames(test)=="addr_state")] # remove state - introduce problem in prediction
train = train[,-which(colnames(train)=="num_tl_120dpd_2m_log")] # remove - no variation
test = test[,-which(colnames(test)=="num_tl_120dpd_2m_log")] # remove - no variation
train = train[,-which(colnames(train)=="num_tl_30dpd_log")] # remove - no variation
test = test[,-which(colnames(test)=="num_tl_30dpd_log")] # remove - no variation

train = train[,-which(colnames(train)=="profit")] # use default rate as dependent variable
test.profit = test$profit
test = test[,-which(colnames(test)=="profit")] # use default rate as dependent variable

########################## CART ##########################
#set.seed(623)
#train.cart = train(default~.,
#                    data = train,
#                    method = "rpart",
#                    tuneGrid = data.frame(cp = seq(0, .001, by=.0001)),
#                    trControl = trainControl(method = "cv", number=10),
#                    metric = "Accuracy")

#ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point() + xlab("Complexity Parameter (cp)") + geom_line()

#train.cart$bestTune
#train.cart$results

loss.mat = cbind(c(0, 5), c(1, 0))

set.seed(623)
cartMod = rpart(default~., data=train, method="class", minbucket = 10, 
                cp = 0.003,parms=list(loss = loss.mat))
prp(cartMod, digits=3)

varImp(cartMod,scale=TRUE)

#cartPred.rocr = prediction(predict(cartMod, newdata=test, type="prob")[, 2],test$default)
#plot(performance(cartPred.rocr, "tpr", "fpr"),colorize = TRUE)
#abline(0, 1, lty = 2)
#as.numeric(performance(cartPred.rocr, "auc")@y.values)

#cartPred=predict(cartMod, newdata=test, type="class")
#table(test$default,cartPred)

########################## Logistic ##########################

logMod = glm(default~., data=train, family="binomial")
summary(logMod)
logPred = data.frame(predict(logMod, newdata=test, type="response"))

logPred.rocr = prediction(predict(logMod, newdata=test, type="response"),test$default)
plot(performance(logPred.rocr, "tpr", "fpr"),colorize = TRUE)
abline(0, 1, lty = 2)
as.numeric(performance(logPred.rocr, "auc")@y.values)

logPred$binary = if_else(row_number(desc(-logPred[,1])) <= 200, "FALSE", "TRUE") # pick top 200 loans with lowest default risk
table(test$default,logPred$binary)

########################## Logistic LASSO ##########################

x = data.matrix(train[,-which(colnames(train)=="default")])
y = if_else(train$default=="TRUE",1,0)

lambdaTune = data.frame()

for (i in 1:10)  # lambda.min shifts - so take average of 10 runs
{
  #perform grid search to find optimal value of lambda
  cv.out = cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
  #plot(cv.out)
  lambdaTune[i,"min"] = cv.out$lambda.min
  lambdaTune[i,"1se"] = cv.out$lambda.1se
  coef(cv.out,s=cv.out$lambda.1se) # display important features for every run
}

lambda_min = mean(lambdaTune$min)
lambda_1se = mean(lambdaTune$`1se`)
coef(cv.out,s=lambda_1se)

x_test = data.matrix(test[,-which(colnames(test)=="default")])
lassoProb = data.frame(predict(cv.out,newx = x_test,s=lambda_1se,type="response"))

lassoPred.rocr = prediction(lassoProb,test$default)
plot(performance(lassoPred.rocr, "tpr", "fpr"),colorize = TRUE)
abline(0, 1, lty = 2)
as.numeric(performance(lassoPred.rocr, "auc")@y.values)

lassoProb$binary = if_else(row_number(desc(-lassoProb[,1])) <= 200, "FALSE", "TRUE") # pick top 200 loans with lowest default risk
table(test$default,lassoProb$binary)

########################## LDA ##########################

ldaMod = lda(default~., data=train, family="binomial")

ldaPred = data.frame(predict(ldaMod, newdata = test))

ldaPred.rocr = prediction(ldaPred$posterior.TRUE,test$default)
plot(performance(ldaPred.rocr, "tpr", "fpr"),colorize = TRUE)
abline(0, 1, lty = 2)
as.numeric(performance(ldaPred.rocr, "auc")@y.values)

ldaPred$binary = if_else(row_number(desc(ldaPred$posterior.FALSE)) <= 200, "FALSE", "TRUE") # pick top 200 loans with lowest default risk
table(test$default,ldaPred$binary)

####################################################

test$pred_default_lda = ldaPred$binary
test$pred_default_log = logPred$binary
test$pred_default_lasso = lassoProb$binary
#test$pred_default_cart = cartPred
test$profit = test.profit
write.csv(test,"testing_default_prediction.csv")
