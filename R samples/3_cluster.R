setwd("~/Desktop/242 Data Analysis App/LC/lending club/3yr")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)

'%!in%' = function(x,y)!('%in%'(x,y)) # define "not in" function

master = read.csv("2012_2014_clean_3yr_c.csv")
master = master[,-1] # remove X column
master = master[,-which(colnames(master)=="issue_year")] # remove issue year because investor can only select recent loans
train = master[1:(nrow(master)*0.75),]
test = master[((nrow(master)*0.75)+1):nrow(master),]

train.default = as.factor(train$default)
train.profit = train$profit
train = train[,!sapply(train, is.factor)] # remove categorical variables for clustering
train = train[,-which(colnames(train)=="profit")] # remove dependent variable
train = train[,-which(colnames(train)=="default")] # remove dependent variable

### clustering
pp = preProcess(train, method=c("center", "scale"))
train.scaled = predict(pp, train)

# tune number of clusters
#dat = data.frame(k = 1:100)
#dat$SS = sapply(dat$k, function(k){
#  set.seed(144)
#  kmeans(train.scaled, iter.max=100, k)$tot.withinss})
#ggplot(dat, aes(x=k, y=SS)) +
#  geom_point() + geom_line() +
#  xlab("Number of Clusters (k)") +
#  ylab("Within-Cluster Sum of Squares")

# clustering for train
set.seed(144)
km = kmeans(train.scaled, iter.max=100, 10) # optimal number determined from scree plot

train$cluster=as.factor(km$cluster)
train.scaled$clust = as.factor(km$cluster)
train.scaled %>% group_by(clust) %>% summarize_all(funs(mean))
table(km$cluster)

train$default = train.default # add back dependent variable
train$profit = train.profit # add back dependent variable

# assigned clusters to test
test.default = as.factor(test$default)
test.profit = test$profit
test = test[,!sapply(test, is.factor)] # remove categorical variables for clustering
test = test[,-which(colnames(test)=="profit")] # remove dependent variable
test = test[,-which(colnames(test)=="default")] # remove dependent variable

pp = preProcess(test, method=c("center", "scale"))
test.scaled = predict(pp, test)

#install.packages("clue")
library(clue)
test$cluster = as.factor(cl_predict(km,test.scaled)) # assign clusters to test set
test$profit = test.profit
test$default = test.default

### plot default rate for each cluster
train %>% 
  group_by(cluster,default) %>% summarise(n=n()) %>% mutate(freq=n/sum(n)) %>% filter(default=="TRUE") %>%
  ggplot(., aes(x=cluster,y=freq))+stat_summary(geom="bar")+scale_y_continuous(labels = scales::percent)+
  labs(x="Cluster in C (train)",y="Default Rate")

### plot profit for each cluster
#ggplot(train, aes(x=factor(cluster), y=profit))+stat_summary(fun.y="mean", geom="bar")+
#  scale_y_continuous(labels = scales::percent)+labs(x="Cluster in C (train)",y="Average Return")

### calculate and plot ratio between return & default rate
#comp_clust = data.frame()
#comp_clust = train %>% group_by(cluster,default) %>% summarise(n=n()) %>% 
#  mutate(default_rate=n/sum(n)) %>% filter(default=="TRUE")
#comp_clust["weighted_return"] = 
#  sapply(split(train, train$cluster), function(train) weighted.mean(train$profit, w = train$loan_amnt))

#levels(comp_clust$cluster) = c(levels(train$cluster),"Average")

#comp_clust["average","weighted_return"]=weighted.mean(train$profit,train$loan_amnt)
#comp_clust["average","default_rate"]=sum(train$default=="TRUE")/length(train$default)
#comp_clust["average","cluster"]="Average"

#comp_clust["ratio"] = comp_clust$weighted_return / comp_clust$default_rate

#comp_clust = comp_clust[,-(2:3)]

#ggplot(comp_clust, aes(x=cluster,y=ratio))+stat_summary(geom="bar")+scale_y_continuous(labels = scales::percent)+
#  labs(x="Cluster in C",y="Weighted Average Return over Default Rate")

### generate output
train_output = master[1:(nrow(master)*0.75),]
test_output = master[((nrow(master)*0.75)+1):nrow(master),]
train_output$cluster = train$cluster
test_output$cluster = test$cluster

#write.csv(train_output,"train_clustering_full.csv")

# default rate - clusters 1 / 2 / 5 / 7
#write.csv(train_output[which(train_output$cluster %in% c(1,2,5,7)),],"training_default.csv")
#write.csv(test_output[which(test_output$cluster %in% c(1,2,5,7)),],"testing_default.csv")

# profit - clusters 1 / 2 / 3 / 5 / 7
#write.csv(train_output[which(train_output$cluster %in% c(1,2,3,5,7)),],"training_profit.csv")
#write.csv(test_output[which(test_output$cluster %in% c(1,2,3,5,7)),],"testing_profit.csv")

# profit/default ratio - cluster
#write.csv(train_output[which(train_output$cluster %in% c(1,2,3,5,7)),],"training_ratio.csv")
#write.csv(test_output[which(test_output$cluster %in% c(1,2,3,5,7)),],"testing_ratio.csv")