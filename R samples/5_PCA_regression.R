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

# train[,!(colnames(train)=="term")]
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

####################### PCA #############################

# install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
# Run factor analysis on mixed data
res.famd = FAMD(train[,-which(colnames(train)=="default")], ncp = 40, graph = FALSE)
eig.val = get_eig(res.famd)
eig.val
res.famd$var$contrib # loading values
res.famd$var$coord # x coordinates

# Regress of the loadings on the features. Trying to select out the important features
loadings = data.frame(res.famd$var$contrib)
loadings_dim1 = loadings[,1]
names(loadings_dim1) = rownames(loadings)
sort(loadings_dim1,decreasing = TRUE)

pc_train = predict.FAMD(res.famd,train) # training set using PCs
pc_train = data.frame(pc_train$coord)
pc_train$default = train$default

pc_test = predict.FAMD(res.famd,test) # testing set using PCs
pc_test = data.frame(pc_test$coord)
pc_test$default = test$default

########################## Logistic ##########################

logMod = glm(default~., data=pc_train, family="binomial")
summary(logMod)
logPred = data.frame(predict(logMod, newdata=pc_test, type="response"))

logPred.rocr = prediction(predict(logMod, newdata=pc_test, type="response"),pc_test$default)
plot(performance(logPred.rocr, "tpr", "fpr"),colorize = TRUE)
abline(0, 1, lty = 2)
as.numeric(performance(logPred.rocr, "auc")@y.values)

logPred$binary = if_else(row_number(desc(-logPred[,1])) <= 200, "FALSE", "TRUE") # pick top 200 loans with lowest default risk
table(pc_test$default,logPred$binary)

########################## Logistic LASSO ##########################

x = data.matrix(pc_train[,-which(colnames(pc_train)=="default")])
y = if_else(pc_train$default=="TRUE",1,0)

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

x_test = data.matrix(pc_test[,-which(colnames(pc_test)=="default")])
lassoProb = data.frame(predict(cv.out,newx = x_test,s=lambda_1se,type="response"))

lassoPred.rocr = prediction(lassoProb,test$default)
plot(performance(lassoPred.rocr, "tpr", "fpr"),colorize = TRUE)
abline(0, 1, lty = 2)
as.numeric(performance(lassoPred.rocr, "auc")@y.values)

lassoProb$binary = if_else(row_number(desc(-lassoProb[,1])) <= 200, "FALSE", "TRUE") # pick top 200 loans with lowest default risk
table(pc_test$default,lassoProb$binary)

########################## LDA ##########################

ldaMod = lda(default~., data=pc_train, family="binomial")

ldaPred = data.frame(predict(ldaMod, newdata = pc_test))

ldaPred.rocr = prediction(ldaPred$posterior.TRUE,pc_test$default)
plot(performance(ldaPred.rocr, "tpr", "fpr"),colorize = TRUE)
abline(0, 1, lty = 2)
as.numeric(performance(ldaPred.rocr, "auc")@y.values)

ldaPred$binary = if_else(row_number(desc(ldaPred$posterior.FALSE)) <= 200, "FALSE", "TRUE") # pick top 200 loans with lowest default risk
table(pc_test$default,ldaPred$binary)

####################################################

test$pred_default_lda = ldaPred$binary
test$pred_default_log = logPred$binary
test$pred_default_lasso = lassoProb$binary
test$profit = test.profit
write.csv(test,"testing_default_prediction_pca.csv")
