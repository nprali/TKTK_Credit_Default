account_cleaning_function<-function(account_cleaning)
{
  #Payment_History Number of dues calculation
  account_cleaning$Payment_History1_Transformed<-account_cleaning$paymenthistory1
  aa<-str_replace_all(account_cleaning$Payment_History1_Transformed,"STD","030")
  a<-str_replace_all(aa,"SMA","060")
  b<-str_replace_all(a,"SUB","090")
  c<-str_replace_all(b,"DBT","120")
  d<-str_replace_all(c,"LSS","150")
  e<-str_replace_all(d,"XXX","000")
  f<-str_replace_all(e,'"""',"'")
  account_cleaning$Payment_History1_Transformed<-f
  account_cleaning$Payment_History2_Transformed<-account_cleaning$paymenthistory2
  aa<-str_replace_all(account_cleaning$Payment_History2_Transformed,"STD","030")
  a<-str_replace_all(aa,"SMA","060")
  b<-str_replace_all(a,"SUB","090")
  c<-str_replace_all(b,"DBT","120")
  d<-str_replace_all(c,"LSS","150")
  e<-str_replace_all(d,"XXX","000")
  f<-str_replace_all(e,'"""',"'")
  
  account_cleaning$Payment_History2_Transformed<-f
  account_cleaning$Payment_History1_Transformed<-str_replace_all(account_cleaning$Payment_History1_Transformed,"'","")
  history1_length<-length(account_cleaning$Payment_History1_Transformed)
  ind<-which(account_cleaning$Payment_History1_Transformed=="" | is.na(account_cleaning$Payment_History1_Transformed==""))
  account_cleaning$Payment_History1_Transformed[ind]<-"000"
  account_cleaning$DPD<-0
  account_cleaning$Avg_DPD_Count<-0
  account_cleaning$Minimum_Month_DPD<-0
  for(i in 1:history1_length){
    v<-account_cleaning$Payment_History1_Transformed[i]
    vv<-sapply(seq(from=1, to=nchar(v), by=3), function(j) substr(v, j, j+2))
    length_vv<-length(vv)
    account_cleaning$Avg_DPD_Count[i]<-length_vv
    ss<-sum(as.numeric(vv))
    account_cleaning$DPD[i]<-ss
    rev_str<-rev(vv)
    ind<-which(as.numeric(rev_str)>29)
    if(length(ind>0)){
      minimum_month<-ind[1]}else{minimum_month=0}
    account_cleaning$Minimum_Month_DPD[i]<-minimum_month
  }
  
  account_cleaning$Payment_History2_Transformed<-str_replace_all(account_cleaning$Payment_History2_Transformed,"'","")
  history2_length<-length(account_cleaning$Payment_History2_Transformed)
  ind<-which(account_cleaning$Payment_History2_Transformed=="" | is.na(account_cleaning$Payment_History2_Transformed==""))
  account_cleaning$Payment_History2_Transformed[ind]<-"000"
  account_cleaning$DPD1<-0
  account_cleaning$Avg_DPD1_Count<-0
  account_cleaning$Minimum_Month_DPD1<-0
  for(i in 1:history2_length){
    v<-account_cleaning$Payment_History2_Transformed[i]
    vv<-sapply(seq(from=1, to=nchar(v), by=3), function(j) substr(v, j, j+2))
    length_vv<-length(vv)
    account_cleaning$Avg_DPD1_Count[i]<-length_vv
    ss<-sum(as.numeric(vv))
    account_cleaning$DPD1[i]<-ss
    rev_str<-rev(vv)
    ind<-which(as.numeric(rev_str)>29)
    if(length(ind>0)){
      account_cleaning$Minimum_Month_DPD[i]<-ind[1]}else{}
    #account_cleaning$Minimum_Month_DPD[ind]<-minimum_month
  }
  
  
  
  Total_DPD<-cbind(account_cleaning$DPD,account_cleaning$DPD1)
  account_cleaning$Final_DPD<-apply(Total_DPD,1, sum)
  
  account_cleaning$Average_DPD<-account_cleaning$DPD/account_cleaning$Avg_DPD_Count
  account_cleaning$Average_DPD1<-account_cleaning$DPD1/account_cleaning$Avg_DPD1_Count
  
  Total_DPD<-cbind(account_cleaning$Average_DPD,account_cleaning$Average_DPD1)
  account_cleaning$Final_Average_DPD_sum<-apply(Total_DPD,1, sum)
  
  account_cleaning$Payment_history_subcount<-ifelse(account_cleaning$Final_Average_DPD_sum<30,1,0)
  
  account_cleaning<-data.table(account_cleaning)
  
  #Payment_history_avg_dpd_0_29_bucket
  Payment_history<-account_cleaning[,mean(Payment_history_subcount),by="customer_no"]
  names(Payment_history)<-c("customer_no","Payment_history_avg_dpd_0_29_bucket")
  account_cleaning<-merge(account_cleaning,Payment_history,by.x="customer_no",by.y="customer_no")
  
  #sum of count of account_cleanings which are 0-29 DPD per user
  Payment_history<-account_cleaning[,sum(Payment_history_subcount),by="customer_no"]
  names(Payment_history)<-c("customer_no","Payment_history_sum_dpd_0_29_bucket")
  account_cleaning<-merge(account_cleaning,Payment_history,by.x="customer_no",by.y="customer_no")
  
  group_account_cleaning<-account_cleaning[,mean(Final_DPD),by="customer_no"]
  names(group_account_cleaning)<-c("customer_no","Average_NDPD")
  account_cleaning<-merge(account_cleaning,group_account_cleaning,by.x="customer_no",by.y="customer_no")
  
  #Lubridate
  account_cleaning$dt_opened<-dmy(account_cleaning$dt_opened)
  account_cleaning$upload_dt<-dmy(account_cleaning$upload_dt)
  account_cleaning$opened_dt<-dmy(account_cleaning$opened_dt)
  account_cleaning$last_paymt_dt<-dmy(account_cleaning$last_paymt_dt)
  account_cleaning$closed_dt<-dmy(account_cleaning$closed_dt)
  account_cleaning$reporting_dt<-dmy(account_cleaning$reporting_dt)
  account_cleaning$paymt_str_dt<-dmy(account_cleaning$paymt_str_dt)
  account_cleaning$paymt_end_dt<-dmy(account_cleaning$paymt_end_dt)
  
  
  #replace NA in numeric field with 0
  ind<-which(is.na(account_cleaning$high_credit_amt))
  account_cleaning$high_credit_amt[ind]<-0
  ind<-which(is.na(account_cleaning$amt_past_due))
  account_cleaning$amt_past_due[ind]<-0
  ind<-which(is.na(account_cleaning$creditlimit))
  account_cleaning$creditlimit[ind]<-0
  ind<-which(is.na(account_cleaning$cashlimit))
  account_cleaning$cashlimit[ind]<-0
  ind<-which(is.na(account_cleaning$paymentfrequency))
  account_cleaning$paymentfrequency[ind]<-0
  ind<-which(is.na(account_cleaning$rateofinterest))
  account_cleaning$rateofinterest<-as.numeric(account_cleaning$rateofinterest)
  account_cleaning$rateofinterest[ind]<-0
  ind<-which(is.na(account_cleaning$paymentfrequency))
  account_cleaning$paymentfrequency[ind]<-0
  ind<-which(is.na(account_cleaning$actualpaymentamount))
  account_cleaning$actualpaymentamount[ind]<-0
  
  #total_diff_lastpaymt_opened_dt
  account_cleaning$Total_all<-account_cleaning$last_paymt_dt-account_cleaning$opened_dt
  ind<-which(is.na(account_cleaning$Total_all))
  account_cleaning$Total_all[ind]<-0
  account_cleaning$total_diff_in_months_lastpaymt_opened_dt<-account_cleaning$Total_all/30
  
  account_cleaning$total_diff_lastpaymt_opened_dt<-account_cleaning$Total_all
  
  #mean_diff_lastpaymt_opened_dt
  mean_diff_lastpaymt_opened<-account_cleaning[,mean(Total_all),by="customer_no"]
  names(mean_diff_lastpaymt_opened)<-c("customer_no","mean_diff_lastpaymt_opened_dt")
  account_cleaning<-merge(account_cleaning,mean_diff_lastpaymt_opened,by.x="customer_no",by.y="customer_no")
  account_cleaning$mean_diff_lastpaymt_opened_dt<-as.numeric(account_cleaning$mean_diff_lastpaymt_opened_dt)
  
  #utilisation_trend
 
  #Total Current balance amount
  sum_cur_bal_amt<-account_cleaning[,sum(cur_balance_amt),by="customer_no"]
  names(sum_cur_bal_amt)<-c("customer_no","total_cur_bal_amt")
  account_cleaning<-merge(account_cleaning,sum_cur_bal_amt,by.x="customer_no",by.y="customer_no")
  
  
  #Total Credit limit
  sum_creditlimit<-account_cleaning[,sum(creditlimit),by="customer_no"]
  names(sum_creditlimit)<-c("customer_no","total_credit_limit")
  account_cleaning<-merge(account_cleaning,sum_creditlimit,by.x="customer_no",by.y="customer_no")
  
  
  #Mean current balance amount
  mean_cur_bal<-account_cleaning[,mean(cur_balance_amt),by="customer_no"]
  names(mean_cur_bal)<-c("customer_no","mean_cur_bal_amt")
  account_cleaning<-merge(account_cleaning,mean_cur_bal,by.x="customer_no",by.y="customer_no")
  
  #Mean credit limit
  mean_credit_lim<-account_cleaning[,mean(creditlimit),by="customer_no"]
  names(mean_credit_lim)<-c("customer_no","mean_credit_limit")
  account_cleaning<-merge(account_cleaning,mean_credit_lim,by.x="customer_no",by.y="customer_no")
  
  #Mean cash limit
  mean_cash_lim<-account_cleaning[,mean(cashlimit),by="customer_no"]
  names(mean_cash_lim)<-c("customer_no","mean_cash_limit")
  account_cleaning<-merge(account_cleaning,mean_cash_lim,by.x="customer_no",by.y="customer_no")
  
  
  numerator<-account_cleaning$total_cur_bal_amt/account_cleaning$total_credit_limit
  denominator<-account_cleaning$mean_cur_bal_amt/(account_cleaning$mean_cash_limit+account_cleaning$mean_credit_limit)
  account_cleaning$utilisation_trend<-numerator/denominator
  
  account_cleaning$Ratio_currbalance_creditlimit<-numerator
  
  
  dup<-duplicated(account_cleaning$customer_no)
  account_cleaning<-account_cleaning[!dup,]
  account_cleaning_transformed<-account_cleaning[,c("customer_no","owner_indic","Minimum_Month_DPD","Final_DPD","Average_NDPD","Final_Average_DPD_sum","Payment_history_subcount","Payment_history_avg_dpd_0_29_bucket","Payment_history_sum_dpd_0_29_bucket","total_diff_in_months_lastpaymt_opened_dt","Total_all","total_diff_lastpaymt_opened_dt","mean_diff_lastpaymt_opened_dt","total_cur_bal_amt","total_credit_limit","mean_cur_bal_amt","mean_cash_limit","mean_credit_limit","utilisation_trend","Ratio_currbalance_creditlimit")]
  
}