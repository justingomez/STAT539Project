train<-read.csv("train.csv",header=TRUE)[,-c(1:2)]

View(train)

if(!require(psych)){install.packages("psych")}
  library(psych)

pairs.panels(train[,10:ncol(train)])

#correlations
library(corrplot)


#initial data analysis
apply(train[,c(2:5,10:ncol(train))],2, summary)

#raw scores


#differentials
plot(train$astdiff, train$winloss)
cut.diff <- cut(train$astdiff,breaks = 10)

sum()






