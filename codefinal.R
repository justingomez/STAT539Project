#packages
library(xtable)
library(ResourceSelection)

#bring in data, work into final form
train<-read.csv("train.csv",header=TRUE)[,-c(1,2,20)]
miss<-rep(12,ncol(train))
for(i in 1:ncol(train)) {
  miss[i]<-length(which(is.na(train[,i])))
}
train<-train[-c(which(is.na(train[,11]))),]

test<-read.csv("test.csv",header=TRUE)[,-c(1,2,20)]
miss<-rep(12,ncol(test))
for(i in 1:ncol(test)) {
  miss[i]<-length(which(is.na(test[,i])))
}
test<-test[-c(which(is.na(test[,11]))),]
#missing data has been removed

#data sets for nice labels
train1<-train[,c(9:11)] #for percentages
colnames(train1)<-c("Field Goals","Three Pointers","Free Throws")
train2<-train[,c(12:17)] #for raw diffs
colnames(train2)<-c("Turnovers","Assists","Steals","Blocks","Personal Fouls","T Rebounds")

#boxplots for explanatory vars
boxplot(train1,col=2:4,cex.axis=1.5,ylab="Difference in Percent",cex.lab=1.5) #named percentbox
boxplot(train2,at=seq(1,10.5,1.75),col=2:7,xaxt="n",cex.axis=1.5,ylab="Difference in Count",cex.lab=1.5) #named diffbox
xtick<-seq(1,10.5,1.75)
axis(side=1,at=xtick,labels=FALSE)
text(x=xtick,par("usr")[3],labels=colnames(train2),srt=13,pos=1,xpd=TRUE,cex=1.5,offset=1)


#plots for winning/losing at home
boxplot(fgp~winloss,data=train,xaxt="n",cex.axis=1.5)
xtick<-seq(1,2,1)
axis(side=1,at=xtick,labels=FALSE)
text(x=xtick,par("usr")[3],labels=c("Regulation","Overtime"),srt=0,pos=1,xpd=TRUE,cex=1.5,offset=1)

rtrain<-train[,c(7:17)]
par(mfrow=c(3,3),mai = c(0.4, 0.3, 0.2, 0.1))
titles<-c(colnames(train1),colnames(train2))
for(i in 3:ncol(rtrain)) {
boxplot(rtrain[,i]~winloss,data=train,xaxt="n",cex.axis=1.5,main=titles[i-2],col=c("lightblue","lightcoral"))
xtick<-seq(1,2,1)
axis(side=1,at=xtick,labels=FALSE)
text(x=xtick,par("usr")[3],labels=c("Loss","Win"),srt=0,pos=1,xpd=TRUE,cex=1.5,offset=1)
} #saved as toomanyboxes

#numerical summaries for explanatory vars
summary(train1)
summary(train2)

fg<-c(min(train1[,1]),median(train1[,1]),mean(train1[,1]),max(train1[,1]))
tp<-c(min(train1[,2]),median(train1[,2]),mean(train1[,2]),max(train1[,2]))
ft<-c(min(train1[,3]),median(train1[,3]),mean(train1[,3]),max(train1[,3]))
tab1<-data.frame(fg=round(fg,3),tp=round(tp,3),ft=round(ft,3))
colnames(tab1)<-colnames(train1)
rownames(tab1)<-c("Minimum","Median","Mean","Maximum")
xtable(tab1) #table for percent summary stats

to<-c(min(train2[,1]),median(train2[,1]),mean(train2[,1]),max(train2[,1]))
ast<-c(min(train2[,2]),median(train2[,2]),mean(train2[,2]),max(train2[,2]))
stl<-c(min(train2[,3]),median(train2[,3]),mean(train2[,3]),max(train2[,3]))
blk<-c(min(train2[,4]),median(train2[,4]),mean(train2[,4]),max(train2[,4]))
pf<-c(min(train2[,5]),median(train2[,5]),mean(train2[,5]),max(train2[,5]))
tr<-c(min(train2[,6]),median(train2[,6]),mean(train2[,6]),max(train2[,6]))
tab2<-data.frame(to=round(to,3),ast=round(ast,3),stl=round(stl,3),blk=round(blk,3),
                 pf=round(pf,3),tr=round(tr,3))
colnames(tab2)<-colnames(train2)
rownames(tab2)<-c("Minimum","Median","Mean","Maximum")
xtable(tab2) #table for diff summary stats

#summaries for x vars with response
tab3<-addmargins(table(as.factor(train$winloss),as.factor(train$ot)))
colnames(tab3)<-c("Regulation","Overtime","Total")
rownames(tab3)<-c("Loss","Won","Total")
xtable(tab3) #table for win/loss by overtime

#model time
train$ot<-as.factor(train$ot)
test$ot<-as.factor(test$ot)
mod1<-glm(winloss~ot*fgp+ot*tpp+ot*ftp+ot*todiff+ot*astdiff+ot*stldiff+
               ot*blkdiff+ot*pfdiff+ot*trdiff,data=train,family=binomial)
mod2<-update(mod1,.~.-ot:tpp-ot:ftp-ot:astdiff-ot:stldiff-ot:blkdiff-ot:pfdiff,
             data=train)
anova(mod2,mod1,test="LRT") #looks like we want them model with 3 interactions
#what about additive effects model?
mod3<-update(mod2,.~.-ot:fgp-ot:todiff-ot:trdiff,data=train)
anova(mod3,mod2,test="LRT") #only main effects would be a bad idea I guess

tab4<-AIC(mod2,mod1,mod3) #model 2 wins!
tab4<-data.frame(tab4,diff=c(tab4[1,2]-tab4[1,2],tab4[2,2]-tab4[1,2],tab4[3,2]-tab4[1,2]))
rownames(tab4)<-c("Reduced Interaction Model","Full Interaction Model","Additive Model")
colnames(tab4)<-c("df","AIC","Change in AIC")
xtable(tab4)

hoslem.test(mod2$y,fitted(mod2),g=10)

ci<-confint(mod2)
tab5<-data.frame(low=round(exp(ci[,1]),3),est=round(exp(summary(mod2)$coefficients[,1]),3),up=round(exp(ci[,2]),3))
colnames(tab5)<-c("Lower Bound","Estimate","Upper Bound")
rownames(tab5)<-c("Intercept","Overtime","Field Goal %","Three Point %","Free Throw %","Turnovers","Assists","Steals","Blocks","Personal Fouls","Total Rebounds","Overtime:Field Goal %","Overtime:Turnovers","Overtime:Total Rebounds")
xtable(tab5) #table for estimated odds ratios and 95% CI

misclass<-function(mod){
  p<-predict(mod,newdata=test,type="response")
  for(i in 1:length(p)){
    ifelse(p[i]>.5,p[i]<-1,p[i]<-0)
  }
  tab<-table(p,test$winloss)
  mis<-1-sum(diag(tab)/sum(tab))
}

mis1<-misclass(mod1)
mis2<-misclass(mod2)
mis3<-misclass(mod3)
