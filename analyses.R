train<-read.csv("train.csv",header=TRUE)[,-c(1:2)]
train<-train[-c(which(is.na(train$lftp))),]
train<-train[-c(which(is.na(train$wftp))),]
View(train)

if(!require(psych)){install.packages("psych")}
  library(psych)

pairs.panels(train[,c(10:15)])
pairs.panels(train[,c(16:21)])

#correlations
library(corrplot)
c<-cor(train[,c(8,10:21)])
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))
corrplot.mixed(c,upper="square",title="Correlation Between Predictors",mar=c(2,0,1,0),
               cl.ratio=.2,cl.align.text="l",cl.offset=.3,cl.length=11,cl.cex=.9,
               outline=TRUE,addgrid.col="black",number.cex=1,col=col4(10),tl.cex=.9)

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




###lets fit some models


#additive model
model1 <- glm(winloss ~ factor(ot) + wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff, family = binomial(link = "logit"), data = train)
summary(model1)

mean(resid(model1, type = "pearson")^2) #this looks as good as I've ever seen in my statistical career


#interactive model (interacted with the differentials)
model2 <- glm(winloss ~ wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff + ot*trdiff + ot*sdiff + ot*blkdiff + ot*pfdiff + ot*astdiff, family = binomial(link = "logit"), data = train)
summary(model2)
 
mean(resid(model2, type = "pearson")^2)

#neither model appears to have a problem with overdispersion

#interactive model with only sdiff:ot
model3<-glm(winloss~.+sdiff:ot,data=train[,-c(1:7)],family=binomial)
summary(model3)
#ot:sdiff is not significant now....
mean(resid(model3, type = "pearson")^2)

#model with only percentages
model4<-glm(winloss~.,data=train[,c(8,10:15)],family=binomial)
summary(model4)
#all the stars....
mean(resid(model4, type = "pearson")^2)

#model with only diffs
model5<-glm(winloss~.,data=train[,c(8,16:21)],family=binomial)
summary(model5)
#yup...
mean(resid(model5, type = "pearson")^2)



#Don't try to fit the full interaction model

#what if we tried a probit link?
probit.1 <- glm(winloss ~ factor(ot) + wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff, family = binomial(link = "probit"), data = train)
summary(probit.1)
#results are quite similar


probit.2 <- glm(winloss ~ wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff + ot*trdiff + ot*sdiff + ot*blkdiff + ot*pfdiff + ot*astdiff, family = binomial(link = "probit"), data = train)
summary(probit.2)
#


#we could try a quasibinomial regression although I think it's unneccessary
quasi.1 <- glm(winloss ~ factor(ot) + wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff, family = quasibinomial(link = "logit"), data = train)
summary(quasi.1)


quasi.2 <- glm(winloss ~ wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff + ot*trdiff + ot*sdiff + ot*blkdiff + ot*pfdiff + ot*astdiff, family = quasibinomial(link = "logit"), data = train)
summary(quasi.2)



#lets try fitting a random effect, but I can't get this to work right. Screw it
library(MASS); library(lme4)
model1 <- glmer(winloss ~ factor(ot) + wfgp + lfgp + w3p + l3p + wftp + lftp + trdiff + astdiff + tdiff + sdiff + blkdiff + pfdiff + (1|Season), family = binomial(link = "logit"), data = train)
summary(model1)





##Training Testing Split

MSE <- function(pred, obs) {mean((pred - obs)^2)}

test <- read.csv("test.csv", header = TRUE)
test<- test[-c(3239,5069),-c(1,2)]

p<-predict(model1,newdata=test,type="response")
tab<-table(p,test$winloss)


plot(predict(model2))









