library(splitstackshape)

ncaa<-read.csv("ncaa.final.csv",header=TRUE)
ncaa$season<-as.factor(ncaa$season)
#14 seasons, grab 3207 from each season

#split train/test 70/30
set.seed(41415)
train<-stratified(ncaa,"season",3207)
train.rows<-sort(train$X) 

test<-ncaa[-train.rows,]

write.csv(train,"train.csv")
write.csv(test,"test.csv")
