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
cut.diff <- cut(train$astdiff,breaks = c(-17,-5,0,5,28))

par(mfrow = c(3,2))
#assist differential
a <- sum(train$winloss[which(train$astdiff > 0)])/length(which(train$astdiff > 0))
b <- sum(train$winloss[which(train$astdiff <= 0)])/length(which(train$astdiff <= 0))
plot(seq(1,2), c(b,a),xaxt = "n", ylim = c(.4,.8), ylab = "Proportion of Wins", xlab = "Differential", type = "l", col ="orange", lwd = 2)
axis(1, at = 1:2, labels = c("Negative", "Positive"))
points(1:2,c(b,a), pch = 20)
title("Assist Differential")

#tdiff
a <- sum(train$winloss[which(train$tdiff > 0)])/length(which(train$tdiff > 0))
b <- sum(train$winloss[which(train$tdiff <= 0)])/length(which(train$tdiff <= 0))
plot(seq(1,2), c(b,a),xaxt = "n", ylim = c(.4,.8), ylab = "Proportion of Wins", xlab = "Differential", type = "l", col ="green", lwd = 2)
axis(1, at = 1:2, labels = c("Negative", "Positive"))
points(1:2,c(b,a), pch = 20)
title("Turnover Differential")


#sdiff
a <- sum(train$winloss[which(train$sdiff > 0)])/length(which(train$sdiff > 0))
b <- sum(train$winloss[which(train$sdiff <= 0)])/length(which(train$sdiff <= 0))
plot(seq(1,2), c(b,a),xaxt = "n", ylim = c(.4,.8), ylab = "Proportion of Wins", xlab = "Differential", type = "l", col ="dodgerblue", lwd = 2)
axis(1, at = 1:2, labels = c("Negative", "Positive"))
points(1:2,c(b,a), pch = 20)
title("Steal Differential")

#pfdiff
a <- sum(train$winloss[which(train$pfdiff > 0)])/length(which(train$pfdiff > 0))
b <- sum(train$winloss[which(train$pfdiff <= 0)])/length(which(train$pfdiff <= 0))
plot(seq(1,2), c(b,a),xaxt = "n",ylim = c(.4,.8),ylab = "Proportion of Wins", xlab = "Differential", type = "l", col ="blue3", lwd = 2)
axis(1, at = 1:2, labels = c("Negative", "Positive"))
points(1:2,c(b,a), pch = 20)
title("Personal Foul Differential")

#blkdiff
a <- sum(train$winloss[which(train$blkdiff > 0)])/length(which(train$blkdiff > 0))
b <- sum(train$winloss[which(train$blkdiff <= 0)])/length(which(train$blkdiff <= 0))
plot(seq(1,2), c(b,a),xaxt = "n", ylim = c(.4,.8), ylab = "Proportion of Wins", xlab = "Differential", type = "l", col ="yellowgreen", lwd = 2)
axis(1, at = 1:2, labels = c("Negative", "Positive"))
points(1:2,c(b,a), pch = 20)
title("Block Differential")


#plot(winloss ~ astdiff, data = train, xlab = "Differential in Assists", ylab = "Count of ", main = "")




###some cha cha cha changes that Paul is going to make







