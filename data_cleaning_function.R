data_cleaning_function<-function(data_cleaning,account_transformed_features,enquiry_transformed_features)
{
  #account_transformed_features<-account_train_transformed
  #enquiry_transformed_features<-enquiry_train_transformed
  #data
  Data_cleaning_transformed<-data_cleaning[,c("dt_opened","customer_no","entry_time","feature_1","feature_3","feature_7","feature_12"
                            ,"feature_26","feature_27","feature_28","feature_29","feature_35","feature_36","feature_37"
                            ,"feature_40","feature_41","feature_43","feature_44"
                            ,"feature_52"
                            ,"feature_65","feature_66","feature_71"
                            ,"Bad_label")]
  
  
  #Combine account and enquiry columns with data
  data_account<-merge(Data_cleaning_transformed,account_transformed_features,by.x="customer_no",by.y="customer_no",all.x=T)
  data_account_enquiry<-merge(data_account,enquiry_transformed_features,by.x="customer_no",by.y="customer_no",all.x=T)
  data_account_enquiry<-data.table(data_account_enquiry)
  
  data_account_enquiry$max_freq_enquiry<-as.factor(data_account_enquiry$max_freq_enquiry)
  
  ind<-which(is.na(data_account_enquiry$feature_3))
  data_account_enquiry$feature_3[ind]<-0
  #Need normalisation
  ind<-which(is.na(data_account_enquiry$feature_7))
  data_account_enquiry$feature_7[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_26))
  data_account_enquiry$feature_26[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_29))
  data_account_enquiry$feature_29[ind]<-0
  
  
  ind<-which(is.na(data_account_enquiry$feature_35))
  data_account_enquiry$feature_35[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_40))
  data_account_enquiry$feature_40[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_41))
  data_account_enquiry$feature_41[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_44))
  data_account_enquiry$feature_44[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_52))
  data_account_enquiry$feature_52[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_65))
  data_account_enquiry$feature_65[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_66))
  data_account_enquiry$feature_66[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$feature_71))
  data_account_enquiry$feature_71[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$utilisation_trend) |data_account_enquiry$utilisation_trend=="Inf" | data_account_enquiry$utilisation_trend=="-Inf")
  data_account_enquiry$utilisation_trend[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Ratio_currbalance_creditlimit) |data_account_enquiry$Ratio_currbalance_creditlimit=="Inf" | data_account_enquiry$Ratio_currbalance_creditlimit=="-Inf")
  data_account_enquiry$Ratio_currbalance_creditlimit[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Total_enquiry))
  data_account_enquiry$Total_enquiry[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Difference_between_Opened_enquiry))
  data_account_enquiry$Difference_between_Opened_enquiry[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Enquiries_less_than_365))
  data_account_enquiry$Enquiries_less_than_365[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Enquiries_less_than_90))
  data_account_enquiry$Enquiries_less_than_90[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$count_enquiry_recency_365))
  data_account_enquiry$count_enquiry_recency_365[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$mean_diff_open_enquiry_dt))
  data_account_enquiry$mean_diff_open_enquiry_dt[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$max_freq_enquiry))
  data_account_enquiry$max_freq_enquiry[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Enquiries_less_than_90))
  data_account_enquiry$Enquiries_less_than_90[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$count_enquiry_recency_90))
  data_account_enquiry$count_enquiry_recency_90[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$count_enquiry_recency_90))
  data_account_enquiry$count_enquiry_recency_90[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$Number_Of_Secured_Loan))
  data_account_enquiry$Number_Of_Secured_Loan[ind]<-0
  
  ind<-which(is.na(data_account_enquiry$perc_unsecured_others))
  data_account_enquiry$perc_unsecured_others[ind]<-0
  
  #change datatype from date to numeric
  a<-as.numeric(data_account_enquiry$total_diff_in_months_lastpaymt_opened_dt)
  data_account_enquiry$total_diff_in_months_lastpaymt_opened_dt<-a
  b<-as.numeric(data_account_enquiry$Total_all)
  data_account_enquiry$Total_all<-b
  c<-as.numeric(data_account_enquiry$total_diff_lastpaymt_opened_dt)
  data_account_enquiry$total_diff_lastpaymt_opened_dt<-c
  d<-as.numeric(data_account_enquiry$Difference_between_Opened_enquiry)
  data_account_enquiry$Difference_between_Opened_enquiry<-d
  e<-as.numeric(data_account_enquiry$mean_diff_open_enquiry_dt)
  data_account_enquiry$mean_diff_open_enquiry_dt<-e
  
  
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  dfNorm <- as.data.frame(lapply(data_account_enquiry[,c(6,5,11,12,18,19,20,21)], normalize))
  data_account_enquiry$feature_7<-dfNorm$feature_7
  data_account_enquiry$feature_3<-dfNorm$feature_3
  data_account_enquiry$feature_29<-dfNorm$feature_29
  data_account_enquiry$feature_35<-dfNorm$feature_35
  data_account_enquiry$feature_44<-dfNorm$feature_44
  data_account_enquiry$feature_52<-dfNorm$feature_52
  data_account_enquiry$feature_65<-dfNorm$feature_65
  data_account_enquiry$feature_66<-dfNorm$feature_66
  data_transformed_features<-data_account_enquiry
  
}