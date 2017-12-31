enquiry_cleaning_function<-function(enquiry_cleaning)
{
#enquiry_cleaning
enquiry_cleaning<-data.table(enquiry_cleaning)
enquiry_cleaning$Count<-1
#Total_enquiry_cleaning_done
Total_enquiry_cleaning_done<-enquiry_cleaning[,sum(Count),by="customer_no"]
names(Total_enquiry_cleaning_done)<-c("customer_no","Total_enquiry")
enquiry_cleaning<-merge(enquiry_cleaning,Total_enquiry_cleaning_done,by.x="customer_no",by.y="customer_no")

#lubridate
enquiry_cleaning$dt_opened<-dmy(enquiry_cleaning$dt_opened)
enquiry_cleaning$enquiry_dt<-dmy(enquiry_cleaning$enquiry_dt)
enquiry_cleaning$upload_dt<-dmy(enquiry_cleaning$upload_dt)
#Difference dt opened and enquiry_cleaning date
enquiry_cleaning$Difference_between_Opened_enquiry<-enquiry_cleaning$dt_opened-enquiry_cleaning$enquiry_dt
enquiry_cleaning$Enquiries_less_than_365<-ifelse(enquiry_cleaning$Difference_between_Opened_enquiry<366,1,0)
#count_enquiry_cleaning_recency_365
count_enquiry_cleaning_recency<-enquiry_cleaning[,sum(Enquiries_less_than_365,na.rm = T),by="customer_no"]
names(count_enquiry_cleaning_recency)<-c("customer_no","count_enquiry_recency_365")
enquiry_cleaning<-merge(enquiry_cleaning,count_enquiry_cleaning_recency,by.x="customer_no",by.y="customer_no")

enquiry_cleaning$Enquiries_less_than_90<-ifelse(enquiry_cleaning$Difference_between_Opened_enquiry<91,1,0)

#count_enquiry_cleaning_recency_90
count_enquiry_cleaning_recency90<-enquiry_cleaning[,sum(Enquiries_less_than_90,na.rm=T),by="customer_no"]
names(count_enquiry_cleaning_recency90)<-c("customer_no","count_enquiry_recency_90")
enquiry_cleaning<-merge(enquiry_cleaning,count_enquiry_cleaning_recency90,by.x="customer_no",by.y="customer_no")

#mean_diff_open_enquiry_cleaning_dt
mean_diff_open_enquiry_cleaning<-enquiry_cleaning[,mean(Difference_between_Opened_enquiry,na.rm = T),by="customer_no"]
names(mean_diff_open_enquiry_cleaning)<-c("customer_no","mean_diff_open_enquiry_dt")
enquiry_cleaning<-merge(enquiry_cleaning,mean_diff_open_enquiry_cleaning,by.x="customer_no",by.y="customer_no")
enquiry_cleaning$mean_diff_open_enquiry_dt<-as.numeric(enquiry_cleaning$mean_diff_open_enquiry_dt)

#max_freq_enquiry_cleaning
max_frequent_enquiry_cleaning<-enquiry_cleaning[,names(table(enq_purpose))[which.max(table(enq_purpose))],by="customer_no"]
names(max_frequent_enquiry_cleaning)<-c("customer_no","max_freq_enquiry")
enquiry_cleaning<-merge(enquiry_cleaning,max_frequent_enquiry_cleaning,by.x="customer_no",by.y="customer_no")

#Secured account 
v<-c(5,6,8,9,10,12,16,18,19,20,35,40,41,43,00)
v[v %in% enquiry_cleaning$enq_purpose]
ind<-which(!(enquiry_cleaning$enq_purpose==5 | enquiry_cleaning$enq_purpose==6 |enquiry_cleaning$enq_purpose==8 |enquiry_cleaning$enq_purpose==9|enquiry_cleaning$enq_purpose==10 |enquiry_cleaning$enq_purpose==12 |enquiry_cleaning$enq_purpose==16 | enquiry_cleaning$enq_purpose==18 |enquiry_cleaning$enq_purpose==19 | enquiry_cleaning$enq_purpose==20 | enquiry_cleaning$enq_purpose==35 |enquiry_cleaning$enq_purpose==40 |enquiry_cleaning$enq_purpose==41 |enquiry_cleaning$enq_purpose==43 | enquiry_cleaning$enq_purpose==00))
enquiry_cleaning$Secured<-0
enquiry_cleaning$Secured[ind]<-1

#Secured_Loan_Type
Secured_Loan_Type<-enquiry_cleaning[,sum(Secured),by="customer_no"]
names(Secured_Loan_Type)<-c("customer_no","Number_Of_Secured_Loan")
enquiry_cleaning<-merge(enquiry_cleaning,Secured_Loan_Type,by.x="customer_no",by.y="customer_no")

#perc_unsecured_others
enquiry_cleaning$perc_unsecured_others<-enquiry_cleaning$Number_Of_Secured_Loan/enquiry_cleaning$Total_enquiry

dup<-duplicated(enquiry_cleaning$customer_no)
enquiry_cleaning<-enquiry_cleaning[!dup,]
enquiry_cleaning_transformed<-enquiry_cleaning[,c("customer_no","Total_enquiry","Difference_between_Opened_enquiry","Enquiries_less_than_365","count_enquiry_recency_365","mean_diff_open_enquiry_dt","max_freq_enquiry","Enquiries_less_than_90","count_enquiry_recency_90","Secured","Number_Of_Secured_Loan","perc_unsecured_others")]

}