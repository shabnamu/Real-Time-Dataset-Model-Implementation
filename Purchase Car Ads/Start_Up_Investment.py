# -*- coding: utf-8 -*-
"""
Created on Fri Apr 19 20:29:32 2019

@author: HP
"""

#50_start up dataset = Business challenge - 50 companies
#extract of profit, how much did the company spend on r&D,admin(salary),marketing - 3 major operational, state the company is in
#dataset is anonymous as company name is not mentioned
#the dataset will help the veg capitalist fund in which type of company it should invest in main criteria profit
#Profit - DV
#Fund company want to analyse where the company performs better and gets profit - based on statewise,based on less or more amount they spend in marketing,admin,r&d
#and based on these factors the fund company will set up guidelines - such as we will provide fund if the company operate in new york, or if a 
#company spends more on r&d than in marketing

#Multiple LR
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
#Step1 - Prepare model
dataset = pd.read_csv("50_Startups.csv")
X = dataset.iloc[:,:-1].values
y = dataset.iloc[:,4].values

#Handle categorical text values
from sklearn.preprocessing import LabelEncoder,OneHotEncoder
labelencoder_X = LabelEncoder()
X[:,3] = labelencoder_X.fit_transform(X[:,3])
onehotencoder = OneHotEncoder(categorical_features = [3])
X = onehotencoder.fit_transform(X).toarray()

#Avoid the dummy variable trap
X = X[:,1:]  #do this to eliminate dummy var at index 0

#Split the dataset to training and test
from sklearn.model_selection import train_test_split
X_train,X_test,y_train,y_test = train_test_split(X,y,test_size = 0.2,random_state = 0)

#Fitting multiple LR model to training set
from sklearn.linear_model import LinearRegression
regressor = LinearRegression()
regressor.fit(X_train,y_train)

#Predict result on test set
y_pred = regressor.predict(X_test)

#Building the optimal model using backward elimination
#here we will check statistical significance of variables and will build model based on only those variables whose statisticak sugnificance is high 
#and has great impact on the DV
import statsmodels.formula.api as sm
#the stats model we are using does not take into account constant b0 from LR formula (y = b0+b1x1+b2x2..) 
#so to avoid this problem we will add x0 column to X
#X = np.append(arr=X,values = np.ones(50,1),astype(int),axis=1)   this will create matrix of 1 but will not add it to tge beginning to do this do below
X = np.append(arr=np.ones((50,1)).astype(int),values=X,axis=1)
#optimal matrix - below will have IV having high impact on profit(DV)
X_opt = X[:,[0,1,2,3,4,5]]
#P of IV >0.05 - remove var else keep var
#Create full model with all possible predictors
#OLS - ordinal least squares
regressor_OLS = sm.OLS(endog = y, exog = X_opt).fit()
#need to specif endog= DV and exog will be IV

regressor_OLS.summary()
#the lower the P value the more significant the independent variable is going to be
#x3 - r&d
#                coef    std err          t      P>|t|      [0.025      0.975]
#const       5.013e+04   6884.820      7.281      0.000    3.62e+04     6.4e+04
#x1           198.7888   3371.007      0.059      0.953   -6595.030    6992.607
#x2           -41.8870   3256.039     -0.013      0.990   -6604.003    6520.229
#x3             0.8060      0.046     17.369      0.000       0.712       0.900
#x4            -0.0270      0.052     -0.517      0.608      -0.132       0.078
#x5             0.0270      0.017      1.574      0.123      -0.008       0.062


#here const is x0,x1 & x2 -dummy var added for states,x3-r&d, x4-admin, x5-marketing
#R-squared:                       0.951
#Adj. R-squared:                  0.945


#build model with the optimal vars
X_opt = X[:,[0,1,3,4,5]]  # model without one states variables
regressor_OLS = sm.OLS(endog=y, exog = X_opt).fit()
regressor_OLS.summary()
#R-squared:                       0.951
#Adj. R-squared:                  0.946

#build model with the optimal vars
X_opt = X[:,[0,3,4,5]] #model removing all states
regressor_OLS = sm.OLS(endog=y, exog = X_opt).fit()
regressor_OLS.summary()
#R-squared:                       0.951
#Adj. R-squared:                  0.948

#build model with the optimal vars admin as its significance level is >0.05 and hence has no impact on the profit
X_opt = X[:,[0,3,5]] #model removing all states
regressor_OLS = sm.OLS(endog=y, exog = X_opt).fit()
regressor_OLS.summary()
#R-squared:                       0.950
#Adj. R-squared:                  0.948

#build model with the optimal vars r&d
X_opt = X[:,[0,3]] #model removing all states
regressor_OLS = sm.OLS(endog=y, exog = X_opt).fit()
regressor_OLS.summary()
#here we can see that the P value is 0.00 which makes r&d a very imp factor for profit, and hence it is the most significant variable of all 

#R-squared:                       0.950
#Adj. R-squared:                  0.948





#we cannot plot graph as we have 5 IV




