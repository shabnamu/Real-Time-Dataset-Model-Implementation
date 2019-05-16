#Import dataset
Credit_Risk1<-read.csv("CreditRisk.csv", na.strings = "")
View(Credit_Risk1)

#Data cleaning
Credit_Risk1<-Credit_Risk1[,-1]
head(Credit_Risk1)

summary(Credit_Risk1)
dim(Credit_Risk1)

var1<-mean(Credit_Risk1$LoanAmount,na.rm = TRUE)
var1

is.na(Credit_Risk1$LoanAmount)
Credit_Risk1$LoanAmount[is.na(Credit_Risk1$LoanAmount)]<-var1
summary(Credit_Risk1)

var2<-mean(Credit_Risk1$Loan_Amount_Term,na.rm = TRUE)
var2

is.na(Credit_Risk1$Loan_Amount_Term)
Credit_Risk1$Loan_Amount_Term[is.na(Credit_Risk1$Loan_Amount_Term)]<-var2
summary(Credit_Risk1)

var3<-mean(Credit_Risk1$Credit_History,na.rm = TRUE)
var3

is.na(Credit_Risk1$Credit_History)
Credit_Risk1$Credit_History[is.na(Credit_Risk1$Credit_History)]<-var3
summary(Credit_Risk1)

Credit_Risk1$Gender[is.na(Credit_Risk1$Gender)]<-'Male'
summary(Credit_Risk1)

Credit_Risk1$Married[is.na(Credit_Risk1$Married)]<-'No'
summary(Credit_Risk1)

Credit_Risk1$Dependents[is.na(Credit_Risk1$Dependents)]<-0
summary(Credit_Risk1)

Credit_Risk1$Self_Employed[is.na(Credit_Risk1$Self_Employed)]<-'No'
summary(Credit_Risk1)

library(dplyr)

Credit_Risk1<-mutate(Credit_Risk1,Gender1=ifelse(Gender=="Female",1,0))
Credit_Risk1<-mutate(Credit_Risk1,Married1=ifelse(Married=="No",1,0))
Credit_Risk1<-mutate(Credit_Risk1,Education1=ifelse(Education=="Graduate",1,0))
Credit_Risk1<-mutate(Credit_Risk1,Self_Employed1=ifelse(Self_Employed=="Yes",1,0))
Credit_Risk1<-mutate(Credit_Risk1,Newarea=ifelse(Property_Area=="Urban",2,ifelse(Property_Area=="Rural",0,1)))
Credit_Risk1<-mutate(Credit_Risk1,LoanStatus1=ifelse(Loan_Status=="Y",1,0))

dim(Credit_Risk1)
summary(Credit_Risk1)

Credit_Risk1$Gender1<-factor(Credit_Risk1$Gender1)
Credit_Risk1$Married1<-factor(Credit_Risk1$Married1)
Credit_Risk1$Education1<-factor(Credit_Risk1$Education1)
Credit_Risk1$Self_Employed1<-factor(Credit_Risk1$Self_Employed1)
Credit_Risk1$Newarea<-factor(Credit_Risk1$Newarea)
Credit_Risk1$LoanStatus1<-factor(Credit_Risk1$LoanStatus1)

summary(Credit_Risk1)

dim(Credit_Risk1)

Credit_Risk2<-select(Credit_Risk1,3,6,7,8,9,10,13,14,15,16,17,18)
dim(Credit_Risk2)
summary(Credit_Risk2)

Credit_Risk3<-select(Credit_Risk2,1,2,3,4,5,6,8,9,10,11,12)
summary(Credit_Risk3)


Credit_Risk3<-na.omit(Credit_Risk3)
dim(Credit_Risk3)
summary(Credit_Risk3)

CRS<-sample(2,nrow(Credit_Risk3),replace = TRUE,prob = c(.8,.2))
CRS_train<-Credit_Risk3[CRS==1,]
CRS_test<-Credit_Risk3[CRS==2,]

## Buid model on complete data - 
Model_CRS<-glm(LoanStatus1~.,family = binomial,data = Credit_Risk3)
summary(Model_CRS)


## Build Model - A - using all the variables
Model_CRS<-glm(LoanStatus1~.,family = binomial,data = CRS_train)
summary(Model_CRS)

# Build Model - B - using credit history
Model_CRS<-glm(LoanStatus1~Credit_History,family = binomial,data = CRS_train)
summary(Model_CRS)

# Build Model - C - using all except Credit History
Model_CRS<-glm(LoanStatus1~Dependents+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Married1+Education1+Self_Employed1+LoanStatus1,family = binomial,data = CRS_train)
summary(Model_CRS)

pred_val<-predict(Model_CRS,CRS_train,type = "response")
pred_val

pred_actual_df1<-data.frame(pred_val,Credit_Risk3$LoanStatus1)
pred_actual_df1

pred_actual_df1<-mutate(pred_actual_df1,pred_val=ifelse(pred_val>0.5,1,0))
pred_actual_df1

colnames(pred_actual_df1)<-c("predict","actual")
pred_actual_df1

tab<-table(pred_actual_df1$predict,pred_actual_df1$actual)
tab

accuracy=( 141+ 702)/( 141+128+10+702)
accuracy

#TPR
tpr<-141/(141+702);tpr

#fpr
fpr<-10/(10+128);fpr

#precision
precision<-141/(10+141);precision

#F1 score
f1score<-(2*(0.9337748)*0.1672598)/((0.9337748)+0.1672598);f1score


## In this model, we want to give loan so class=1 will be major class = 702 ppl will be given loan 

##model has class imbalance - solution to this problem is oversampling or undersampling here we will do oversampling by adding duplicate records

CRS<-sample(2,nrow(Credit_Risk3),replace = TRUE,prob = c(.8,.2))
CRS_train<-Credit_Risk3[CRS==1,]
CRS_test<-Credit_Risk3[CRS==2,]

#filtering all the data with loan status=0 in aa
aa<-filter(CRS_train,LoanStatus1==0)

#adding all these rows to train data
CRS_train<-rbind(CRS_train,aa,aa,aa,aa)

Model_CRS<-glm(LoanStatus1~Credit_History,family = binomial,data = CRS_train)
summary(Model_CRS)

pred_val<-predict(Model_CRS,CRS_test,type = "response")
pred_val

pred_actual_df1<-data.frame(pred_val,CRS_test$LoanStatus1)
pred_actual_df1

pred_actual_df1<-mutate(pred_actual_df1,pred_val=ifelse(pred_val>0.5,1,0))
pred_actual_df1

colnames(pred_actual_df1)<-c("predict","actual")
pred_actual_df1

tab<-table(pred_actual_df1$predict,pred_actual_df1$actual)
tab






