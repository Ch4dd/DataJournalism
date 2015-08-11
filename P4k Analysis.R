library(quanteda)
library(foreign)
library(dplyr)
library(e1071)
library(SparseM)
library(MASS)
library(ggplot2)
library(glmnet)
###p4k author reviews 


data<-read.csv("C:/Users/kevin/Dropbox/fun/results.csv")
text<-as.character(data$text)
mydfm<-dfm(text, ignoredFeatures=stopwords())
scores.rd<-round(data$score)
scores<-data$score



###let's try glm

##for CV
train <- mydfm[1:2000]
test <-mydfm[2001:2440]
score_train <- scores.rd[1:2000]
score_test <- scores.rd[2001:2440]

##optimize for lambda
cv<-cv.glmnet(mydfm, scores )
lambda<-cv$lambda

##optimize for alpha
alphas<-vector()
for (o in 1:10){
predictions<-vector()
model<-NULL
for (i in 1:10){
  j<-1 + (i-1)*1000
  k<-i*(1000)
  test <- mydfm[j:k]
  train <-mydfm[-j:-k]
  score_test <- scores[j:k]
  score_train <- scores[-j:-k]
  model<-glmnet(train, score_train, lambda=lambda , alpha =(o-1)/1000, family="gaussian")
  pred<-predict(model, test)
  predictions[j:k]<-pred
}

alphas[o]<-cor(predictions, scores)
}

alpha<-5/1000


#run it
predictions<-vector()
model<-NULL
for (i in 1:10){
  j<-1 + (i-1)*1000
  k<-i*(1000)
  test <- mydfm[j:k]
  train <-mydfm[-j:-k]
  score_test <- scores[j:k]
  score_train <- scores[-j:-k]
  model<-glmnet(train, score_train, lambda=lambda , alpha =.01, family="gaussian")
  pred<-predict(model, test)
  predictions[j:k]<-pred
}


#verify
plot(predictions, scores)


##generate error scores
error<-predictions-scores
auth<-data$author
ppl<-as.data.frame(table(auth))
##only include ppl who've written 5 articles
real.ppl<-filter(ppl, Freq>4)


results<-data.frame(aggregate(error ~ auth, df=money, FUN="mean"))


##filter by author
results<-results[which(results$auth %in% real.ppl$auth) ,]

sorted <- results[order(results$error),] 

###time for da graphic design


##ok so to actually plot it we need to restrict to the most prolific



realest.ppl<-filter(ppl, Freq>24)


realz<-results[which(results$auth %in% realest.ppl$auth) ,]

sortedz <- realz[order(realz$error),] 

setwd("C:/Users/Kevin/Dropbox/Fun/")

pdf("FUN.pdf", 5, 10)
ggplot(sortedz, aes(y=reorder(auth, error), x=error )) + geom_point() +geom_vline(xintercept=0) + xlab("Gentless Index") +
  ylab("Authors") +theme_bw() + ggtitle("The Meanest Pitchforker")
dev.off()




