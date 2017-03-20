library(splitstackshape)

ncaa<-read.csv("ncaa.final.csv",header=TRUE)
ncaa$Season<-as.factor(ncaa$Season)
#14 seasons, grab 3562 from each season

#split train/test 70/30
set.seed(41415)
train<-stratified(ncaa,"Season",3562)
train.rows<-sort(train$X) 

test<-ncaa[-train.rows,]

write.csv(train,"train.csv")
write.csv(test,"test.csv")
