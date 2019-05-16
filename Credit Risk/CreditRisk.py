# -*- coding: utf-8 -*-
"""
Created on Sat May 11 10:51:32 2019

@author: HP
"""

import pandas as pd
import numpy as np

# Logistics regression - to check if person is eligible to get loan or not

cr = pd.read_csv("CreditRisk.csv")

cr.head()
cr.shape
cr.describe()
cr.describe(include = "all")

cr.columns

cr.isna().any()

cr1 = cr.iloc[:, 1:13]

cr1.isna().any()

cr1.Gender.describe()
cr1.Dependents.describe()
cr1.ApplicantIncome.describe()


cr1.Gender.replace({"Male":1, "Female":0}, inplace = True)
cr1.Married.replace({"No":0, "Yes":1}, inplace = True)
cr1.Education.replace({"Graduate":1, "Not Graduate":0}, inplace = True)

cr1.Self_Employed.replace({"No":1, "Yes":0}, inplace = True)
cr1.Property_Area.replace({"Semiurban":1, "Urban":2, "Rural":3}, inplace = True)
cr1.Loan_Status.replace({"Y":1, "N":0}, inplace = True)

cr1.Married = cr1.Married.astype('category')
cr1.Gender = cr1.Gender.astype('category')
cr1.Education = cr1.Education.astype('category')
cr1.Self_Employed = cr1.Self_Employed.astype('category')
cr1.Property_Area = cr1.Property_Area.astype('category')
cr1.Loan_Status = cr1.Loan_Status.astype('category')

cr1.describe(include="all")

#drop NA values
cr1 = cr1.dropna()
cr1.shape

"""
cr1.Gender = cr1.Gender.fillna("Male")
cr1.Married = cr1.Married.fillna("No")
cr1.Dependents = cr1.Dependents.fillna(0)
cr1.Self_Employed = cr1.Self_Employed.fillna("No")
cr1.LoanAmount = cr1.LoanAmount.fillna(0)
cr1.Loan_Amount_Term = cr1.Loan_Amount_Term.fillna(0)
"""

# Sampling
X = cr1.iloc[:,0:11]
y = cr1.iloc[:,11]
X.shape

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.2, random_state = 0)

X_train.shape
y_train.shape
X_test.shape
y_test.shape


#Model building
from sklearn.linear_model import LogisticRegression
log_model = LogisticRegression()
#fit used to build model
log_model.fit(X_train,y_train)

#Predictions
y_pred = log_model.predict(X_test)
y_pred_prob = log_model.predict_proba(X)

y_pred_prob = pd.DataFrame(y_pred_prob)

y_pred_prob.rename(columns = {y_pred_prob.columns[0] : "ClassZero", y_pred_prob.columns[1] : "ClassOne"}, inplace = True)

y_pred_prob["Loan_ID"] = cr.Loan_ID

Final_Data = y_pred_prob.sort_values(by = "ClassOne", ascending=False)
Final_Data


y_pred1 = log_model.predict(X)

#confusion matirx
pd.crosstab(y_test,y_pred)
pd.crosstab(y,y_pred1)

from sklearn.metrics import confusion_matrix
con_matrix = confusion_matrix(y_pred,y_test)
con_matrix

con_matrix_all = confusion_matrix(y_pred1,y)
con_matrix_all

#Accuracy
acc = sum(con_matrix_all.diagonal())/con_matrix_all.sum()
acc

#Give top 10 customers - customer with probability > 0.9
#Now to run campaign we need to target customers which are most eligible so predicting on only test data which is only 20% then we will miss on other eligible
#customers and hence we need to make predictions on full data



