#Model Building
library(inneq)
library(MLmetrics)
library(gains)
library(ROCR)
xgboost_model_TKTK<-function(data_account_enquiry)
{
  data_account_enquiry<-data_train_transformed
#Xgboost
#tuning parameters
  nround_v=200
  objective_v='binary:logistic'
  eval_metric_v='auc'
  gamma_v=0.007
  eta_v=0.01
  set.seed_v=777
  max_delta_step_v=7
  max_depth_v=10
  min_child_weight_v=10
  colsample_bytree_v=0.5
  subsample_v=0.5
  scale_pos_weight=0.4
  gamma_v<-c(0.001,0.005,0.007)
  eta_v<-c(0.01,0.01,0.01)
  max_depth_v<-c(9,11,10)
  tbind<-data_train_transformed
  
  #Iterate 3 times with different parameter tuning
for(i in 1:3){
dtrain_credit<-xgb.DMatrix(data.matrix(sapply(data_account_enquiry[,-c("customer_no","dt_opened","entry_time","Bad_label","owner_indic","Payment_history_sum_dpd_0_29_bucket","feature_40","total_credit_limit","total_cur_bal_amt","total_diff_in_months_lastpaymt_opened_dt","feature_12","Final_DPD","Average_NDPD","feature_43","feature_26","Total_all","Payment_history_subcount","Average_NDPD","Enquiries_less_than_365","Enquiries_less_than_90","Secured","Number_Of_Secured_Loan")],as.numeric)),label=data_account_enquiry$Bad_label,missing=NA)
cv.res<-xgb.cv(data=dtrain_credit,nfold=5,nround=nround_v,objective=objective_v,eval_metric=eval_metric_v,maximize = TRUE,
               gamma=gamma_v[i],eta=eta_v[i],set.seed=set.seed_v,scale_pos_weight=0.4,max_delta_step=max_delta_step_v,max_depth=max_depth_v[i],min_child_weight=min_child_weight_v,colsample_bytree=colsample_bytree_v,subsample=subsample_v)

max.auc.idx = which.max(cv.res$evaluation_log[,test_auc_mean])
max.auc.idx 
#Predict with best result obtained from Cross validation
bst.cv_Dec2017<-xgboost(data=dtrain_credit,objective=objective_v,nrounds=max.auc.idx,eval_metric=eval_metric_v,eta=eta_v[i],gamma=gamma_v[i],set.seed=set.seed_v,scale_pos_weight=0.4,nthread=1,colsample_bytree=colsample_bytree_v,subsample=subsample_v,max_depth=max_depth_v[i],min_child_weight=min_child_weight_v,max_delta_step=max_delta_step_v,maximize = TRUE)

saveRDS(bst.cv_Dec2017,paste0(paste('model_Pred',i,sep='-'),'.rds'))
impo<-xgb.importance(colnames(dtrain_credit),model=bst.cv_Dec2017)
print(impo)

p<-predict(bst.cv_Dec2017,dtrain_credit)
pp<-paste0("Pred",i)
tbind<-cbind(tbind,pp=p)
gains.cross <- gains(actual=tbind$Bad_label,
                     predicted=tbind$pp, 
                     groups=10)
print(gains.cross)
capture.output(gains.cross, file = paste0(paste('gains.cross',i,sep='-'),'.txt'))
names(tbind)[names(tbind) == 'pp'] <- paste0("Pred",i)
tbind<-data.table(tbind)

#gain table

label_gain<-cbind(gains.cross$depth,gains.cross$cume.obs,gains.cross$cume.pct.of.total)
label_gain<-data.frame(label_gain)
label_gain$X4<-label_gain$X3[1]

for(i in 2:10)
{
  label_gain$X4[i]<-label_gain$X3[i+1]-label_gain$X3[i]
  
}
label_gain$X4[10]<-label_gain$X3[10]-label_gain$X3[9]


names(label_gain)<-c("Decile","Cum_sum","Cum_sum_label","Response")
label_gain$Response<- round(label_gain$Response, digits=2)


ggplot(label_gain, aes(x=Decile, y=Response)) +
  geom_line() +
  geom_text(aes(label=Response))  + ggtitle("Rank Ordering")

ggsave("label_gain.png")

}

  sum_Pred<-cbind(tbind$Pred1,tbind$Pred2,tbind$Pred3)
  Avg_Pred<-apply(sum_Pred,1, mean)  
  tbind$Prediction<-Avg_Pred
#write.csv(tbind,"training_predictions.csv")

label_gain<-cbind(gains.cross$depth,gains.cross$cume.obs,gains.cross$cume.pct.of.total)
label_gain<-data.frame(label_gain)
label_gain$X4<-label_gain$X3[1]

for(i in 2:10)
{
  label_gain$X4[i]<-label_gain$X3[i+1]-label_gain$X3[i]
  
}
label_gain$X4[10]<-label_gain$X3[10]-label_gain$X3[9]


names(label_gain)<-c("Decile","Cum_sum","Cum_sum_label","Response")
label_gain$Response<- round(label_gain$Response, digits=2)


ggplot(label_gain, aes(x=Decile, y=Response)) +
  geom_line() +
  geom_text(aes(label=Response))  + ggtitle("Rank Ordering")

gini_val<-Gini(tbind$Prediction,tbind$Bad_label)

}




