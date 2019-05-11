network_original = read.csv("Network_Intrusion_Train.csv")
network_category = read.csv("Network_Intrusion_Train.csv")
network = read.csv("Network_Intrusion_Train.csv")
View(network)

table(network_original$class)
table(network_category$class)
table(network$class)

summary(network)
library(dplyr)

network_numeric = select(network,1,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)
names(network_numeric)

network$protocol_type = factor(as.numeric(network$protocol_type))
network$service = as.numeric(network$service)
network$service = factor(network$service)
network$flag = factor(as.numeric(network$flag))

network$class = ifelse(network$class=="normal",0,1)
network$class = factor(network$class)

network_category$class = ifelse(network_category$class=="normal",0,1)
network_category$class = factor(network_category$class)

network_numeric$class = ifelse(network_numeric$class=="normal",0,1)
network_numeric$class = factor(network_numeric$class)


summary(network_numeric)

#Numeric Data
#cor(network,network$class)
corr <- round(cor(network_numeric), 2)
corr

#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, lab = TRUE)

#Categorical Data
#1. Protocol type
plot(network_category$protocol_type, network$class, col = c("blue", "red"), xlab ="Protocol Type", ylab = "Class", main = "Protocol Type vs Class")
#icmp - 1655, tcp-20526 udp - 3011

#2.Service
#table(network_category$service)
#network_category$service = factor(as.numeric(network_category$service))
#scatter.smooth(network_category$service, network_category$class)
plot(network_category$service, network_category$class, col = c("blue", "red"), xlab ="Service Type", ylab = "Class", main = "Service Type vs Class")

#3. Flag
table(network_category$flag)
plot(network_category$flag, network_category$class, col = c("blue", "red"), xlab ="Flag Type", ylab = "Class", main = "Flag Type vs Class")



#sampling
sample1 = sample(2,nrow(network), replace = TRUE, prob = c(.8,.2))
train = network[sample1==1,]
test = network[sample1==2,]

#Logistics Regression
model_glm_1 = glm(class~., family = "binomial", data = train)
summary(model_glm)

model_glm_2 = glm(class~protocol_type+service+flag+src_bytes+dst_bytes+num_failed_logins+logged_in+num_compromised+root_shell+
                 count+srv_count+serror_rate+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_srv_serror_rate+dst_host_rerror_rate+dst_host_srv_rerror_rate,family = "binomial",  data = train)

summary(model_glm_2)

model_glm_3 = glm(class~src_bytes+dst_bytes+logged_in+count+
                    srv_count+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_srv_serror_rate+dst_host_rerror_rate,family = "binomial",  data = train)
summary(model_glm_3)

model_glm_4 = glm(class~src_bytes+logged_in+count+
                    srv_count+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_srv_serror_rate,family = "binomial",  data = train)
summary(model_glm_4)

model_glm_5 = glm(class~logged_in+count+
                    srv_count+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_srv_serror_rate,family = "binomial",  data = train)
summary(model_glm_5)

y_pred_glm = predict(model_glm_5, test, type = "response")
y_pred_glm

library(dplyr)
pred_glm_df = data.frame(y_pred_glm, test$class)
colnames(pred_glm_df) = c("predict","actual")
pred_glm_df = mutate(pred_glm_df, predict = ifelse(pred_glm_df$predict >0.5,1,0))
pred_glm_df

tab = table(pred_glm_df$predict, pred_glm_df$actual); tab
accuracy = (2624+2228)/(196+148+2624+2228) ; accuracy

#install.packages("pROC")
#library(pROC)
auc(pred_glm_df$predict, pred_glm_df$actual)


#logged_in+count+
#srv_count+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_srv_serror_rate

plot(network$srv_count, network$class)



library(dplyr)
library(randomForest)
#network_df1 = select(network,2,4,5,3,6,11,12,13,14,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,39,40,41,42)
#summary(network_df1)


# summary(network)
# network_anova = aov(class~., data = network)
# network_anova

library(Boruta)
network_boruta<-Boruta(class~.,data=network)

#plot(network_boruta)

attStats(network_boruta)

getSelectedAttributes(network_boruta,withTentative = F)

library(party)
model2 = ctree(class~duration+protocol_type+service+flag+src_bytes+dst_bytes+wrong_fragment+hot+num_failed_logins+logged_in+num_compromised+root_shell+su_attempted+num_root+num_file_creations+
             num_access_files+is_guest_login+count+srv_count+serror_rate+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_serror_rate+dst_host_srv_serror_rate+dst_host_rerror_rate+dst_host_srv_rerror_rate,  data = train)


y_pred = predict(model2, test)
cm = table(y_pred, test$class)
cm
accuracy = (2585+2264)/(27+17+2585+2264) ; accuracy


