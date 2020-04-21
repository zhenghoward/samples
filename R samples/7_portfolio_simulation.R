setwd("~/Desktop/242 Data Analysis App/LC/lending club/3yr")

library(tidyverse)
library(dplyr)
library(ggplot2)

clustering = read.csv("testing_default_prediction.csv")
clustering_pca = read.csv("testing_default_prediction_pca.csv")
grade = read.csv("2012_2014_clean_3yr_c.csv")
base = read.csv("2012_2014_clean_3yr.csv")

base = base[((nrow(base)*0.8)+1):nrow(base),]
grade = grade[((nrow(grade)*0.8)+1):nrow(grade),]

pred_log = clustering[which(clustering$pred_default_log=="FALSE"),]
pred_lda = clustering[which(clustering$pred_default_lda=="FALSE"),]
pred_lasso = clustering[which(clustering$pred_default_lasso=="FALSE"),]

pred_log_pca = clustering_pca[which(clustering_pca$pred_default_log=="FALSE"),]
pred_lda_pca = clustering_pca[which(clustering_pca$pred_default_lda=="FALSE"),]
pred_lasso_pca = clustering_pca[which(clustering_pca$pred_default_lasso=="FALSE"),]

manual = # manually adjust to get roughly 200 loans
  filter(clustering,
                #clustering$installment > 700 &
                #clustering$emp_length > 6 & # ?
                #clustering$dti < 15 &
                #clustering$delinq_2yrs < 3 & # ?
                #clustering$inq_last_6mths < 2 & # ?
                #(clustering$mths_since_last_delinq < 100 | clustering$mths_since_last_delinq == 600) &
                #clustering$mths_since_last_record < 200 & # ?
                clustering$open_acc < 15 & # very important!
                #clustering$pub_rec > 0 &
                #clustering$mths_since_last_major_derog < 200 &
                #clustering$mort_acc > 3 &
                #clustering$mths_since_recent_bc_dlq < 200 &
                #clustering$mths_since_recent_revol_delinq < 200 &
                #clustering$num_actv_bc_tl < 4 & #?
                #clustering$num_actv_rev_tl < 8 & #?
                #clustering$num_bc_sats < 6 & #?
                #clustering$num_bc_tl < 10 &
                #clustering$num_op_rev_tl < 10 &
                #clustering$num_rev_tl_bal_gt_0 < 8 &
                #clustering$num_sats < 12 &
                #clustering$num_tl_op_past_12m < 3 &
                #clustering$credit_length > 10 &
                #clustering$annual_inc_log > 11 &
                #clustering$revol_bal_log < 9 &
                clustering$tot_cur_bal_log > 12 &
                #clustering$total_rev_hi_lim_log < 10 &
                #clustering$avg_cur_bal_log > 8 &
                #clustering$bc_open_to_buy_log < 7.5 &
                clustering$mo_sin_old_il_acct_log > 4.5 &
                #clustering$mo_sin_old_rev_tl_op_log > 4.5 &
                #clustering$mo_sin_rcnt_rev_tl_op_log > 2 &
                #clustering$mo_sin_rcnt_tl_log > 1.25 &
                clustering$mths_since_recent_bc_log > 2.5 &
                clustering$num_il_tl_log > 1.25 &
                clustering$pct_tl_nvr_dlq_exp < 1e+43 &
                #clustering$tot_hi_cred_lim_log > 11 &
                #clustering$total_bc_limit_log < 9 &
                clustering$loan_amnt > 20000
                )

################# Loop Takes 3 Hours to Run !!! #################

sampling = data.frame()

for (i in 1:30000){
  sampling[10*i-9,"return"] = base[sample(nrow(base), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-8,"return"] = grade[sample(nrow(grade), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-7,"return"] = clustering[sample(nrow(clustering), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-6,"return"] = pred_log[sample(nrow(pred_log), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-5,"return"] = pred_lasso[sample(nrow(pred_lasso), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-4,"return"] = pred_lda[sample(nrow(pred_lda), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-3,"return"] = pred_log_pca[sample(nrow(pred_log_pca), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-2,"return"] = pred_lasso_pca[sample(nrow(pred_lasso_pca), 40), ] %>%
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-1,"return"] = pred_lda_pca[sample(nrow(pred_lda_pca), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i,"return"] = manual[sample(nrow(manual), 40), ] %>% 
    summarise(weighted_return=weighted.mean(profit,loan_amnt))
  
  sampling[10*i-9,"Class"] = "LC Average"
  sampling[10*i-8,"Class"] = "Grade C Average"
  sampling[10*i-7,"Class"] = "Clustering"
  sampling[10*i-6,"Class"] = "Clustering + Logistic"
  sampling[10*i-5,"Class"] = "Clustering + Log LASSO"
  sampling[10*i-4,"Class"] = "Clustering + LDA"
  sampling[10*i-3,"Class"] = "Cluster + PCA + Logistic"
  sampling[10*i-2,"Class"] = "Cluster + PCA + Log LASSO"
  sampling[10*i-1,"Class"] = "Cluster + PCA + LDA"
  sampling[10*i,"Class"] = "Clustering + Manual"
}

write.csv(sampling,"simulation_1.csv")

################# Loop Takes 3 Hours to Run !!! #################

sampling = read.csv("simulation_1.csv")
sampling = sampling[,-1] # remove X column

ggplot(sampling, aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling[which(sampling$Class %in% c("Grade C Average",
                                            "LC Average",
                                            "Clustering"
                                            )),], aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling[which(sampling$Class %in% c("Clustering",
                                            "Clustering + Logistic",
                                            "Clustering + LDA",
                                            "Clustering + Log LASSO",
                                            "Clustering + Manual")),], aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling[which(sampling$Class %in% c("Clustering",
                                            "Cluster + PCA + Logistic",
                                            "Cluster + PCA + LDA",
                                            "Cluster + PCA + Log LASSO",
                                            "Clustering + Manual")),], aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling[which(sampling$Class %in% c("Clustering + Logistic",
                                            "Cluster + PCA + Logistic")),], aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling[which(sampling$Class %in% c("Clustering + Log LASSO",
                                            "Cluster + PCA + Log LASSO")),], aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling[which(sampling$Class %in% c("Clustering + LDA",
                                            "Cluster + PCA + LDA")),], aes(x = return)) +
  geom_line(aes(color=Class), stat="density", size=1, alpha=0.8) +
  labs(x="Return", y="Density") + scale_x_continuous(labels = scales::percent)

ggplot(sampling, aes(x = reorder(Class, return, FUN=mean), y = return, color=Class)) + geom_boxplot() +
  labs(y="Return", x="Investment Strategy") + scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1))

t.test(sampling[which(sampling$Class=="Clustering + Logistic"),1],sampling[which(sampling$Class=="Cluster + PCA + Logistic"),1])
t.test(sampling[which(sampling$Class=="Clustering + Log LASSO"),1],sampling[which(sampling$Class=="Cluster + PCA + Log LASSO"),1])
t.test(sampling[which(sampling$Class=="Clustering + LDA"),1],sampling[which(sampling$Class=="Cluster + PCA + LDA"),1])
