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
cut.diff <- cut(train$astdiff,breaks = 10)

sum()






