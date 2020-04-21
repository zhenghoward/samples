setwd("~/Desktop/242 Data Analysis App/LC/lending up/3yr")

library(tidyverse)
library(dplyr)
library(ggplot2)

master = read.csv("2012_2014_clean_3yr.csv")
master=master[,-1] # remove X column

### profit margin (unweighted and weighted) by grade
#ggplot(master, aes(x=factor(grade), y=profit))+stat_summary(fun.y="mean", geom="bar")+
#      scale_y_continuous(labels = scales::percent)+labs(x="Grade",y="Average Return")
master %>% group_by(grade) %>% summarise(weighted_return=weighted.mean(profit,loan_amnt)) %>%
      ggplot(., aes(x=factor(grade),y=weighted_return))+stat_summary(geom="bar")+scale_y_continuous(labels = scales::percent)+
      labs(x="Grade",y="Average Return Weighted by Loan Amount")

### default rates by grade
#master %>% 
#  group_by(grade) %>% summarise (n=n()) %>% mutate(freq=n/sum(n)) %>%
#  ggplot(., aes(x=grade,y=1-freq))+stat_summary(geom="bar")+scale_y_continuous(labels = scales::percent)+
#  labs(x="Grade",y="Default Rate")

### loan amount by grade
#ggplot(master,aes(grade))+geom_bar() + # B is most frequent grade
#  labs(x="Grade", y="Total # of Loans")
#ggplot(master, aes(x=factor(grade), y=loan_amnt))+stat_summary(fun.y="mean", geom="bar") + # loan size increases for E/F/G
#  labs(x="Grade", y="Average Loan Amount")

### profit margin by year
#ggplot(master, aes(x=factor(issue_year), y=profit))+stat_summary(fun.y="mean", geom="bar") +
#      labs(x="Year", y="Average Return") + scale_y_continuous(labels = scales::percent)

### profit margin by month
#ggplot(master, aes(x=factor(issue_month), y=profit))+stat_summary(fun.y="mean", geom="bar") +
#  labs(x="Month", y="Average Return") + scale_y_continuous(labels = scales::percent)
#ggplot(master,aes(issue_month))+geom_histogram(stat="count") + 
#  labs(x="Month", y="# of Loans")

c = master[which(master$grade=="C"),]

#write.csv(c,"2012_2014_clean_3yr_c.csv")
