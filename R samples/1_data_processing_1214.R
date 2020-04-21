setwd("~/Desktop/242 Data Analysis App/LC/lending club")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)

a = read.csv("LoanStats_2012_2013.csv",skip=1)
b = read.csv("LoanStats_2014.csv",skip=1)
master = rbind(a,b)
master = master[complete.cases(master[,which(colnames(master)=="grade")]),] # remove empty rows by Grade
master = master %>% select_if(~sum(!is.na(.))>0) # remove empty columns

# clean up loan status. dont care if loan meets original credit policy
levels(master$loan_status)[levels(master$loan_status)=="Does not meet the credit policy. Status:Charged Off"] = "Charged Off"
levels(master$loan_status)[levels(master$loan_status)=="Does not meet the credit policy. Status:Fully Paid"] = "Fully Paid"

# remove currently active loans
master = filter(master,master$loan_status!="Current")
master = master[,-which(colnames(master)=="pymnt_plan")] # remove payment plan because it correlates with loan status

# separate issue month and issue year
master$issue_month = as.factor(substr(master$issue_d,start=1,stop=3))
master$issue_year = as.numeric(substr(master$issue_d,start=5,stop=8))
master = master[,-which(colnames(master)=="issue_d")] # remove issue_d column

# keep only year for earliest credit line and convert to credit length
master$earliest_cr_line = as.numeric(substr(master$earliest_cr_line,start=5,stop=8))
master$credit_length = master$issue_year-master$earliest_cr_line
master = master[,-which(colnames(master)=="earliest_cr_line")]

# convert interest rate and revolving balance util rate to numbers
master$int_rate=as.numeric(sub("%","",master$int_rate))/100
master$revol_util=as.numeric(sub("%","",master$revol_util))/100

# turn employer title into binary variable
master$emp_title = as.factor(if_else(master$emp_title=="","empty","fill"))
# turn request title into binary variable
master$title = as.factor(if_else(master$title=="","empty","fill"))
# turn description into binary variable
master$desc = as.factor(if_else(master$desc=="","empty","fill"))

# extract employment length from string
master$emp_sign = if_else(gsub("[^<+]","",master$emp_length,"")=="",99,
                          if_else(gsub("[^<+]","",master$emp_length,"")=="<",0,11))
master$emp_length = if_else(master$emp_sign!=99,master$emp_sign,
                            as.numeric(gsub("[^0-9]", "", master$emp_length), ""))
master = master[,-which(colnames(master)=="emp_sign")]

# remove zip code
master = master[,-which(colnames(master)=="zip_code")]
# remove policy code - single value
master = master[,-which(colnames(master)=="policy_code")]
# remove application type - single value
master = master[,-which(colnames(master)=="application_type")]

# remove settlement info because it is captured in loan status
master = master[,-which(colnames(master)=="settlement_status")]
master = master[,-which(colnames(master)=="settlement_amount")]
master = master[,-which(colnames(master)=="settlement_percentage")]
master = master[,-which(colnames(master)=="settlement_date")]
master = master[,-which(colnames(master)=="settlement_term")]
master = master[,-which(colnames(master)=="debt_settlement_flag")]
master = master[,-which(colnames(master)=="debt_settlement_flag_date")]

# remove hardship info / already captured in loan status
master = master[,-which(colnames(master)=="hardship_flag")]
master = master[,-which(colnames(master)=="hardship_type")]
master = master[,-which(colnames(master)=="hardship_reason")]
master = master[,-which(colnames(master)=="hardship_amount")]
master = master[,-which(colnames(master)=="hardship_dpd")]
master = master[,-which(colnames(master)=="hardship_end_date")]
master = master[,-which(colnames(master)=="hardship_length")]
master = master[,-which(colnames(master)=="hardship_status")]
master = master[,-which(colnames(master)=="hardship_start_date")]
master = master[,-which(colnames(master)=="hardship_loan_status")]
master = master[,-which(colnames(master)=="disbursement_method")]
master = master[,-which(colnames(master)=="hardship_last_payment_amount")]
master = master[,-which(colnames(master)=="hardship_payoff_balance_amount")]
master = master[,-which(colnames(master)=="orig_projected_additional_accrued_interest")]
master = master[,-which(colnames(master)=="payment_plan_start_date")]
master = master[,-which(colnames(master)=="deferral_term")]

# remove unnecessary payment data
master = master[,-which(colnames(master)=="total_pymnt_inv")]
master = master[,-which(colnames(master)=="total_rec_prncp")]
master = master[,-which(colnames(master)=="total_rec_late_fee")]
master = master[,-which(colnames(master)=="total_rec_int")]
master = master[,-which(colnames(master)=="recoveries")]
master = master[,-which(colnames(master)=="collection_recovery_fee")]
master = master[,-which(colnames(master)=="last_pymnt_d")]
master = master[,-which(colnames(master)=="last_pymnt_amnt")]
master = master[,-which(colnames(master)=="next_pymnt_d")]
master = master[,-which(colnames(master)=="out_prncp")]
master = master[,-which(colnames(master)=="out_prncp_inv")] 

master = master[,-which(colnames(master)=="funded_amnt")] # removed because correlated with loan amount
master = master[,-which(colnames(master)=="funded_amnt_inv")] # removed because correlated with loan amount
master = master[,-which(colnames(master)=="last_credit_pull_d")] # removed because it is often after the loan is fully paid
master = master[,-which(colnames(master)=="id")] # removed because column is empty

# profit margin & default
master$profit = (master$total_pymnt-master$loan_amnt)/master$loan_amnt
master$default = as.factor(if_else(master$loan_status=="Fully Paid","F","T"))
master = master[,-which(colnames(master)=="loan_status")] # correlated with default
master = master[,-which(colnames(master)=="total_pymnt")] # not leading indicator

table(sign(master$profit),master$default) # check for non-default negative return
master = filter(master,master$default!="F"|master$profit>0) # remove misclassified loans (Fully Paid with negative return)

# convert NA to meaningful value
master$mths_since_last_delinq = if_else(is.na(master$mths_since_last_delinq),600,
                                        as.numeric(master$mths_since_last_delinq))
master$mths_since_last_record = if_else(is.na(master$mths_since_last_record),600,
                                        as.numeric(master$mths_since_last_record))
master$mths_since_last_major_derog = if_else(is.na(master$mths_since_last_major_derog),600,
                                        as.numeric(master$mths_since_last_major_derog))
master$mths_since_recent_revol_delinq = if_else(is.na(master$mths_since_recent_revol_delinq),600,
                                        as.numeric(master$mths_since_recent_revol_delinq))
master$mths_since_recent_inq = if_else(is.na(master$mths_since_recent_inq),60,
                                        as.numeric(master$mths_since_recent_inq))
master$mths_since_recent_bc_dlq = if_else(is.na(master$mths_since_recent_bc_dlq),600,
                                        as.numeric(master$mths_since_recent_bc_dlq))

master = na.omit(master)

# remove non-existing factors
master$initial_list_status=factor(master$initial_list_status)
master$addr_state=factor(master$addr_state)
master$purpose=factor(master$purpose)
master$term=factor(master$term)
master$grade=factor(master$grade)
master$sub_grade=factor(master$sub_grade)
master$home_ownership=factor(master$home_ownership)
master$verification_status=factor(master$verification_status)
master$issue_month=factor(master$issue_month)

# log of $ related variables
master$annual_inc_log = log(master$annual_inc)
master = master[,-which(colnames(master)=="annual_inc")]
master$revol_bal_log = log(master$revol_bal)
master = master[,-which(colnames(master)=="revol_bal")]
master$total_acc_log = log(master$total_acc)
master = master[,-which(colnames(master)=="total_acc")]
master$tot_coll_amt_log = log(master$tot_coll_amt)
master = master[,-which(colnames(master)=="tot_coll_amt")]
master$tot_cur_bal_log = log(master$tot_cur_bal)
master = master[,-which(colnames(master)=="tot_cur_bal")]
master$total_rev_hi_lim_log = log(master$total_rev_hi_lim)
master = master[,-which(colnames(master)=="total_rev_hi_lim")]
master$avg_cur_bal_log = log(master$avg_cur_bal)
master = master[,-which(colnames(master)=="avg_cur_bal")]
master$bc_open_to_buy_log = log(master$bc_open_to_buy)
master = master[,-which(colnames(master)=="bc_open_to_buy")]
master$delinq_amnt_log = log(master$delinq_amnt)
master = master[,-which(colnames(master)=="delinq_amnt")]
master$mo_sin_old_il_acct_log = log(master$mo_sin_old_il_acct)
master = master[,-which(colnames(master)=="mo_sin_old_il_acct")]
master$mo_sin_old_rev_tl_op_log = log(master$mo_sin_old_rev_tl_op)
master = master[,-which(colnames(master)=="mo_sin_old_rev_tl_op")]
master$mo_sin_rcnt_rev_tl_op_log = log(master$mo_sin_rcnt_rev_tl_op)
master = master[,-which(colnames(master)=="mo_sin_rcnt_rev_tl_op")]
master$mo_sin_rcnt_tl_log = log(master$mo_sin_rcnt_tl)
master = master[,-which(colnames(master)=="mo_sin_rcnt_tl")]
master$mths_since_recent_bc_log = log(master$mths_since_recent_bc)
master = master[,-which(colnames(master)=="mths_since_recent_bc")]
master$mths_since_recent_inq_log = log(master$mths_since_recent_inq)
master = master[,-which(colnames(master)=="mths_since_recent_inq")]
master$num_il_tl_log = log(master$num_il_tl)
master = master[,-which(colnames(master)=="num_il_tl")]
master$num_tl_120dpd_2m_log = log(master$num_tl_120dpd_2m)
master = master[,-which(colnames(master)=="num_tl_120dpd_2m")]
master$num_tl_30dpd_log = log(master$num_tl_30dpd)
master = master[,-which(colnames(master)=="num_tl_30dpd")]
master$pct_tl_nvr_dlq_exp = exp(master$pct_tl_nvr_dlq)
master = master[,-which(colnames(master)=="pct_tl_nvr_dlq")]
master$tot_hi_cred_lim_log = log(master$tot_hi_cred_lim)
master = master[,-which(colnames(master)=="tot_hi_cred_lim")]
master$total_bal_ex_mort_log = log(master$total_bal_ex_mort)
master = master[,-which(colnames(master)=="total_bal_ex_mort")]
master$total_bc_limit_log = log(master$total_bc_limit)
master = master[,-which(colnames(master)=="total_bc_limit")]
master$total_il_high_credit_limit_log = log(master$total_il_high_credit_limit)
master = master[,-which(colnames(master)=="total_il_high_credit_limit")]

# remove negative infinity
master = do.call(data.frame,lapply(master, function(x) replace(x,is.infinite(x),-1)))

three_yr = master[which(master$term==" 36 months"),]
five_yr = master[which(master$term==" 60 months"),]
five_yr = five_yr[which(five_yr$issue_year!=2015),]

#write.csv(three_yr,"2012_2014_clean_3yr.csv")
#write.csv(five_yr,"2012_2014_clean_5yr.csv")
