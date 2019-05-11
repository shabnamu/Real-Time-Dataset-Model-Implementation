#Import the data
flight<-read.csv("Flight1987.csv")
View(flight)

library(dplyr)
summary(flight)
flight = na.omit(flight)
dim(flight1)

#factorize categorical variables
flight$Month = factor(flight$Month)
flight$DayofMonth = factor(flight$DayofMonth)
flight$DayOfWeek = factor(flight$DayOfWeek)


#Analyse Numeric Data
flight_numeric = dplyr::select(flight, 5,6,7,8,11,12,13,14,17)
summary(flight_numeric)
corr <- round(cor(flight_numeric), 2)
corr

#Correlation matrix - Heat map
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, lab = TRUE)

#We can see that observe that - 
#1. Distance,CRSElapsedTime, ActualElapsedTime - Highly Positively correlated
#2. DepTime, CRSDepTime,ArrTime,CRSArrTime - Highly positively correlated
#3. ArrDelay, DepDelay - Highly positively correlated

#Analyze Categorical Data
flight_categorical = dplyr::select(flight,2,3,4,9,15,16,14 )
summary(flight_categorical)
flight_categorical$DepDelay = ifelse(flight_categorical$DepDelay>15,1,0)
flight_categorical$DepDelay = factor(flight_categorical$DepDelay)

#Analyze frequence of flight delayed on days of months
plot(flight_categorical$DayofMonth,flight_categorical$DepDelay, col = c('blue','red'), xlab = 'Day of Month', ylab = 'Delay of Flights', main = 'Departure Delay vs Day of Month' )

#Analyze the frequence of the flights delayed on days of week
plot(flight_categorical$DayOfWeek,flight_categorical$DepDelay, col = c('blue','red'), xlab = 'Day of Week ', ylab = 'Delay of Flights', main = 'Departure Delay vs Day of Week' )

#Analyze the delay in flights in each months 
plot(flight_categorical$Month,flight_categorical$DepDelay, col = c('blue','red'), xlab = 'Months', ylab = 'Delay of Flights', main = 'Departure Delay vs Month' )

#Analyze the delay in flights based on Origin
plot(flight_categorical$Origin,flight_categorical$DepDelay, col = c('blue','red'), xlab = 'Origin', ylab = 'Delay of Flights', main = 'Departure Delay vs Origin' )

#Count of flights delayed
count<-table(flight1$y);count
barplot(count,xlab = "Departure Delay",ylab="Count of flights delayed",main = "Time vs Delay Flight",
        col=c("blue","red"),legend=c('Delay by less than 20minutes','Delay by more than 20minutes'))


#Building the Model to fit the dataset
library(dplyr)
library(randomForest)

flight1 = dplyr::select(flight,3,4,5,6,8,11,12,14,17)
summary(flight1)

flight1 = mutate(flight1, y =ifelse(DepDelay>15,1,0))
flight1$y = factor(flight1$y)

flight2 = dplyr::select(flight1,1,2,3,4,5,6,7,9,10)
summary(flight2)

flight_sample = sample(2,nrow(flight2),replace = TRUE, prob= c(.8,.2))
train = flight2[flight_sample ==1,]
test = flight2[flight_sample==2,]


model1 = randomForest(y~.,data = flight2,ntree=10)
summary(model1)
dim(flight2)
pred_1 = predict(model1,test)
cm_1 = table(pred_1,test$y)
cm_1
# pred_1      0      1
# 0         179614  768
# 1             7   26624

model2 = randomForest(y~DayofMonth+DayOfWeek+DepTime+ActualElapsedTime+CRSElapsedTime,data = flight2,ntree=10)
summary(model2)
pred_2 = predict(model2,test)
cm_2 = table(pred_2,test$y)
cm_2
# pred_2      0      1
# 0       179278  11892
# 1         343   15500

model3 = randomForest(y~DayofMonth+DayOfWeek+DepTime+ActualElapsedTime,data = flight2,ntree=10)
summary(model3)
pred_3 = predict(model3,test)
cm_3 = table(pred_3,test$y)
cm_3
# pred_3      0      1
# 0       179038  20054
# 1         583   7338


model4 = randomForest(y~DayofMonth+DayOfWeek+DepTime+ActualElapsedTime+CRSDepTime,data = flight2,ntree=10)
summary(model4)
pred_4 = predict(model4,test)
cm_4 = table(pred_4,test$y)
cm_4
# pred_4      0      1
# 0         179608   1240
# 1             13   26152


model5 = randomForest(y~DayofMonth+DayOfWeek+CRSDepTime+Distance,data = flight2,ntree=10)
summary(model5)
pred_5 = predict(model5,test)
cm_5 = table(pred_5,test$y)
cm_5
# pred_5      0      1
# 0 179078  22230
# 1    543   5162

model6 = randomForest(y~DayofMonth+DayOfWeek+CRSArrTime+ActualElapsedTime+Distance, data = flight2, ntree = 10)
summary(model6)
pred_6 = predict(model6,test)
cm6 = table(pred_6,test$y)
cm6
# pred_6  0     1
# 0     179274  12260
# 1       347   15132

