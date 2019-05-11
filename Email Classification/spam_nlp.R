#Spam filter
library(stringr)
library(tm)

sms_spam_df<-read.csv("spam.csv")

dim(sms_spam_df)
head(sms_spam_df)
View(sms_spam_df)


#excel has only 2 cols and all rows so perform below
sms_spam_df=sms_spam_df[,c(1,2)]
dim(sms_spam_df)

colnames(sms_spam_df)<-c("type","text")

#creating documents of 6778 rows only for text col
#each row is a message = each message is document
sms_corpus<-Corpus(VectorSource(sms_spam_df$text))
sms_corpus

# <<SimpleCorpus>>
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 6778

clean_corpus<-tm_map(sms_corpus,tolower) #change to lower case
clean_corpus<-tm_map(clean_corpus,removeNumbers) #remove numbers
clean_corpus<-tm_map(clean_corpus,removeWords,stopwords()) #remove stopwords

clean_corpus<-tm_map(clean_corpus,stripWhitespace) #remove spacecs
clean_corpus<-tm_map(clean_corpus,removePunctuation) #remove punctuations

clean_corpus
class(clean_corpus)

sms_dtm<-DocumentTermMatrix(clean_corpus)
dim(sms_dtm)
class(sms_dtm)
# [1] 6778 7885      6778- no of rows , 7885 - total no of unique words

#Sampling on original data with 2 cols and 6778 rows
sms_raw_train<-sms_spam_df[1:5000,] #5000th row will be in both train test
sms_raw_test<-sms_spam_df[5000:6778,]

#Sample doc term matrix
sms_dtm_train<-sms_dtm[1:5000,]
sms_dtm_test<-sms_dtm[5000:6778,] #optional

#Sample corpus 
sms_corpus_train<-clean_corpus[1:5000]
View(sms_corpus_train)
inspect(sms_corpus_train)
sms_corpus_test<-clean_corpus[5000:6778]


five_times_words<-findFreqTerms(sms_dtm_train,5) #select only those words which are appearing 5 or more times

View(five_times_words)

#build new doc term matrix of words occuring >=5 times
#need to do this to remove words which have less occurence less than 5
#select msgs with words which occur 5 times

sms_train<-DocumentTermMatrix(sms_corpus_train,control = list(dictionary=five_times_words))
dim(sms_train)
sms_test<-DocumentTermMatrix(sms_corpus_test,control = list(dictionary=five_times_words))
View(sms_train)


#function to check if word is occuring in five_times_words
convert_count<-function(x){
  y<-ifelse(x>0,1,0)
  y<-factor(y,levels = c(0,1),labels = c("No","Yes"))
}

#apply function on each row or column where 1 indicates - row and 2 indicates column
sms_train<-apply(sms_train,2,convert_count)
sms_test<-apply(sms_test,2,convert_count)

View(sms_train)


library(wordcloud)
wordcloud(sms_corpus_train,min.freq=40, random.order = FALSE) # biggest words are nearer the centre

View(sms_train)

spam_mail = subset(sms_spam_df, type == "spam")
wordcloud(spam_mail$text,min.freq=40, random.order = FALSE) # biggest words are nearer the centre

ham_mail = subset(sms_spam_df, type = "ham")
wordcloud(ham_mail$text, min.freq = 70, random.order = FALSE)

#make predictions
library(e1071)

#now y is in different data frame and x in different data frame
sms_classifier<-naiveBayes(sms_train,factor(sms_raw_train$type))
sms_test_pred<-predict(sms_classifier,newdata = sms_test)
sms_test_pred
tab<-table(sms_test_pred,sms_raw_test$type)
tab

df<-data.frame(sms_test_pred,sms_raw_test$type)
df

