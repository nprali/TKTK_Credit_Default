library(xgboost)
library(mgcv)
library(dtplyr)
library(dplyr)
library(data.table)
library(stringr)
library(stats)
library(stringi)
library(countrycode)
library(lubridate)
library(gains)


#Read Training Dataset from csv file
account<-read.csv('test_data/raw_account_70_new.csv',header=TRUE, na.strings=c(""," ","NA"))
enquiry<-read.csv('test_data/raw_enquiry_70_new.csv',header=TRUE, na.strings=c(""," ","NA"))
data<-read.csv('test_data/raw_data_70_new.csv',header=TRUE, na.strings=c(""," ","NA"))   

account_train_transformed<-account_cleaning_function(account)

#Unique values from transformed matrix

enquiry_train_transformed<-enquiry_cleaning_function(enquiry)

data_train_transformed<-data_cleaning_function(data,account_train_transformed,enquiry_train_transformed)

Description_train<-describe(data_train_transformed[,c(4:53)])
capture.output(Description_train, file = "Description_train.txt")


#Training model
data_train_prob<-xgboost_model_TKTK(data_train_transformed)

#Read Testing Dataset from csv file
account_test<-read.csv('test_data/raw_account_30_new.csv',header=TRUE, na.strings=c(""," ","NA"))
enquiry_test<-read.csv('test_data/raw_enquiry_30_new.csv',header=TRUE, na.strings=c(""," ","NA"))
data_test<-read.csv('test_data/raw_data_30_new.csv',header=TRUE, na.strings=c(""," ","NA"))   

account_test_transformed<-account_cleaning_function(account_test)
#Unique values from transformed matrix

enquiry_test_transformed<-enquiry_cleaning_function(enquiry_test)

data_test_transformed<-data_cleaning_function(data_test,account_test_transformed,enquiry_test_transformed)


#Predict test
model1<-readRDS("model_Pred-1.rds")
model2<-readRDS("model_Pred-2.rds")
model3<-readRDS("model_Pred-3.rds")

#Model 1 results
train_target1<-rbind(data_train_transformed[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")],data_test_transformed[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")])  
 
p<-matrix(predict(model1, data.matrix(sapply(train_target1,as.numeric)),missing="NA"))
length_fulldata<-dim(train_target1)[1]
length_train<-dim(data_train_transformed)[1]
p<-p[(length_train+1):length_fulldata]
testbind<-cbind(data_test_transformed,"Pred1"=p)

#Model 2 results
train_target2<-rbind(data_train_transformed[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")],data_test_transformed[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")])  

p<-matrix(predict(model2, data.matrix(sapply(train_target2,as.numeric)),missing="NA"))
length_fulldata<-dim(train_target2)[1]
length_train<-dim(data_train_transformed)[1]
p<-p[(length_train+1):length_fulldata]
testbind<-cbind(testbind,"Pred2"=p)

#Model 3 results
train_target3<-rbind(data_train_transformed[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")],data_test_transformed[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")])  

p<-matrix(predict(model3, data.matrix(sapply(train_target3,as.numeric)),missing="NA"))
length_fulldata<-dim(train_target3)[1]
length_train<-dim(data_train_transformed)[1]
p<-p[(length_train+1):length_fulldata]
testbind<-cbind(testbind,"Pred3"=p)

#Final results
sum_Pred<-cbind(testbind$Pred1,testbind$Pred2,testbind$Pred3)
Avg_Pred<-apply(sum_Pred,1, mean)  
testbind$Prediction<-Avg_Pred
write.csv(testbind,"testing_predictions.csv")

#Calculate gain for rank ordering 
gains.cross.test <- gains(actual=testbind$Bad_label,
                          predicted=testbind$Prediction, 
                          groups=10)

print(gains.cross.test)
capture.output(gains.cross.test, file="gain_test.txt")
testbind<-data.table(testbind)

#gain table
label_gain_test<-cbind(gains.cross.test$depth,gains.cross.test$cume.obs,gains.cross.test$cume.pct.of.total)
label_gain_test<-data.frame(label_gain_test)
label_gain_test$X4<-label_gain_test$X3[1]

for(i in 2:10)
{
  label_gain_test$X4[i]<-label_gain_test$X3[i+1]-label_gain_test$X3[i]
}

label_gain_test$X4[10]<-label_gain_test$X3[10]-label_gain_test$X3[9]
names(label_gain_test)<-c("Decile","Cum_sum","Cum_sum_label","Response")
label_gain_test$Response<- round(label_gain_test$Response, digits=2)

#Plot rank ordering
ggplot(label_gain_test, aes(x=Decile, y=Response)) +
  geom_line() +
  geom_text(aes(label=Response))  + ggtitle("Rank Ordering")

ggsave("label_gain_test.png")

#Gini Value
gini_val_test<-Gini(testbind$Prediction,testbind$Bad_label)

