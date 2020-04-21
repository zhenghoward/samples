setwd("~/Desktop/242 Data Analysis App/LC/lending club/3yr")

library(tidyverse)
library(dplyr)
library(ggplot2)

master = read.csv("train_clustering_full.csv")
master = master[,-1]

master$Target = ifelse(master$cluster %in% c(1,2,5,7), "Target Clusters", "Non-Target Clusters") # by default rate
#master$Target = ifelse(master$cluster %in% c(1,2,3,5,7), "Target Clusters", "Non-Target Clusters") # by profit

### compare Target vs. non-Target
ggplot(master, aes(profit,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Profit") + ylab("Density")
ggplot(master, aes(Target,fill=default)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Default") + ylab("Count")

######################################################################################################

ggplot(master, aes(loan_amnt,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Loan Amount") + ylab("Density")
ggplot(master, aes(int_rate,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Interest Rate") + ylab("Density")
ggplot(master, aes(installment,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Installment") + ylab("Density")
ggplot(master, aes(emp_length,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Emloyment Length") + ylab("Density")
ggplot(master, aes(dti,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Debt to Income Ratio") + ylab("Density")
ggplot(master, aes(delinq_2yrs,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Delinquencies in Past 2 Years") + ylab("Density") + scale_x_log10()
ggplot(master, aes(inq_last_6mths,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Inquiries in Past 6 Months") + ylab("Density") + scale_x_log10()
ggplot(master, aes(mths_since_last_delinq,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Last Delinquency") + ylab("Density")
ggplot(master, aes(mths_since_last_record,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Last Record") + ylab("Density")
ggplot(master, aes(open_acc,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Open Accounts") + ylab("Density")
ggplot(master, aes(pub_rec,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Public Records") + ylab("Density")
ggplot(master, aes(revol_util,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Revolving Utility") + ylab("Density")
ggplot(master, aes(collections_12_mths_ex_med,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# Collections in Past 12 Months excluding Medical Expense") + ylab("Density")
ggplot(master, aes(mths_since_last_major_derog,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Last Major Derogatory (90-day Delinquency)") + ylab("Density")
ggplot(master, aes(acc_now_delinq,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Current Delinquent Accounts") + ylab("Density")
ggplot(master, aes(acc_open_past_24mths,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Accounts Opened in Past 2 Years") + ylab("Density")
ggplot(master, aes(bc_util,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Bankcard Account Util Rate") + ylab("Density")
ggplot(master, aes(chargeoff_within_12_mths,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Charge-offs in Last 12 Months") + ylab("Density")
ggplot(master, aes(mort_acc,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Mortgage Accounts") + ylab("Density")
ggplot(master, aes(mths_since_recent_bc_dlq,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Last Bankcard Delinquency") + ylab("Density")
ggplot(master, aes(mths_since_recent_revol_delinq,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Last Revolving Delinquency") + ylab("Density")
ggplot(master, aes(num_accts_ever_120_pd,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Accounts Ever Pastdue for 120 Days or More") + ylab("Density")
ggplot(master, aes(num_actv_bc_tl,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Currently Active Bankcards") + ylab("Density")
ggplot(master, aes(num_actv_rev_tl,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Currently Acitve Revolving Accounts") + ylab("Density")
ggplot(master, aes(num_bc_sats,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Satisfactory Bankcard Accounts") + ylab("Density")
ggplot(master, aes(num_bc_tl,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Bankcard Accounts") + ylab("Density")
ggplot(master, aes(num_op_rev_tl,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Open Revolving Accounts") + ylab("Density")
ggplot(master, aes(num_rev_accts,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Open Revolving Accounts") + ylab("Density")
ggplot(master, aes(num_rev_tl_bal_gt_0,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Revolving Trades with Balance > 0") + ylab("Density")
ggplot(master, aes(num_sats,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Satisfactory Accounts") + ylab("Density")
ggplot(master, aes(num_tl_90g_dpd_24m,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Accounts 90+ Days Past Due in past 24 Months") + ylab("Density") + scale_x_log10()
ggplot(master, aes(num_tl_op_past_12m,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Accounts Opened in past 12 Months")
ggplot(master, aes(percent_bc_gt_75,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Percentage of All Bankcard Accounts > 75% of Limit.") + ylab("Density")
ggplot(master, aes(pub_rec_bankruptcies,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Bankruptcy Records") + ylab("Density") + scale_x_log10()
ggplot(master, aes(tax_liens,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Tax Liens") + ylab("Density") + scale_x_log10()
ggplot(master, aes(credit_length,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("History of Credit") + ylab("Density")
ggplot(master, aes(annual_inc_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Annual Income (log)") + ylab("Density")
ggplot(master, aes(revol_bal_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Revolving Balance (log)") + ylab("Density")
ggplot(master, aes(total_acc_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Total Currently Open Accounts (log)") + ylab("Density")
ggplot(master, aes(total_acc_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Total Currently Open Accounts (log)") + ylab("Density")
ggplot(master, aes(tot_coll_amt_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Collection Amount (log)") + ylab("Density")
ggplot(master, aes(tot_cur_bal_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Current Balance (log)") + ylab("Density")
ggplot(master, aes(total_rev_hi_lim_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Revolving High Credit (log)") + ylab("Density")
ggplot(master, aes(avg_cur_bal_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Average Current Balance (log)") + ylab("Density")
ggplot(master, aes(bc_open_to_buy_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Open-to-Buy Bankcards (log)") + ylab("Density")
ggplot(master, aes(delinq_amnt_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Delinquent Amount (log)") + ylab("Density")# + scale_x_log10()
ggplot(master, aes(mo_sin_old_il_acct_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since the Oldest Bank Installment Account Opened (log)") + ylab("Density")
ggplot(master, aes(mo_sin_old_rev_tl_op_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since the Oldest Revolving Account Opened (log)") + ylab("Density")
ggplot(master, aes(mo_sin_rcnt_rev_tl_op_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Most Recent Revolving Account Opened (log)") + ylab("Density")
ggplot(master, aes(mo_sin_rcnt_tl_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Most Recent Account Opened (log)") + ylab("Density")
ggplot(master, aes(mths_since_recent_bc_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Most Recent Bankcard Opened (log)") + ylab("Density")
ggplot(master, aes(mths_since_recent_inq_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Months since Most Recent Inquiry (log)") + ylab("Density")
ggplot(master, aes(num_il_tl_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Installment Accounts (log)") + ylab("Density")
ggplot(master, aes(num_tl_120dpd_2m_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Accounts Currently 120-days Past Due (log)") + ylab("Density")
ggplot(master, aes(num_tl_30dpd_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("# of Accounts Currently 30-days Past Due (log)") + ylab("Density")
ggplot(master, aes(pct_tl_nvr_dlq_exp,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Percent of Trades Never Delinquent (exp)") + ylab("Density")
ggplot(master, aes(tot_hi_cred_lim_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total High Credit Limit (log)") + ylab("Density")
ggplot(master, aes(total_bal_ex_mort_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Credit Balance excluding Mortgage (log)") + ylab("Density")
ggplot(master, aes(total_bc_limit_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Bankcard Limit (log)") + ylab("Density")
ggplot(master, aes(total_il_high_credit_limit_log,fill=Target)) + geom_density(alpha = 0.5) + 
  xlab("Total Installment High Credit Limit (log)") + ylab("Density")

######################################################################################################

ggplot(master, aes(Target,fill=emp_title)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=home_ownership)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=verification_status)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=verification_status)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=desc)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=purpose)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=addr_state)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
ggplot(master, aes(Target,fill=initial_list_status)) +
  geom_bar(position="dodge") + scale_y_log10() +
  xlab("Cluster") + ylab("Count")
