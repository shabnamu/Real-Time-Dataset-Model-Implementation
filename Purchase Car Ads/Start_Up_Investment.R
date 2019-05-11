dataset = read.csv("50_Startups.csv")
View(dataset)

dataset$State = as.numeric(dataset$State)
dataset$State = factor(dataset$State)

summary(dataset)

sample1 = sample(2,nrow(dataset),replace = TRUE,prob = c(.8,.2))
train = dataset[sample1==1,]
test = dataset[sample1==2,]

model1 = lm(Profit~.,data = train)
summary(model1)

pred = predict(model1,test)
pred
pred_df = data.frame(pred,test$Profit); pred_df

model2 = lm(Profit~R.D.Spend,data = train)
summary(model2)

#Building the model using backward elimination
model3 = lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = train)
summary(model3)

model4 = lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = train)
summary(model4)

model5 = lm(Profit~R.D.Spend+Marketing.Spend,data = train)
summary(model5)
#Cannot completely remove Marketing dicely