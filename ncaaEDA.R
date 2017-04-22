ncaa<-read.csv("RegularSeasonDetailedResults.csv",header=TRUE)
ncaa<-ncaa[ncaa$Wloc!="N",]
write.csv(ncaa,"inter.csv")
#write a CSV of data without neutral sites
ncaa<-read.csv("inter.csv",header=TRUE)[,-1]
ncaa$WinLoss <- ifelse(ncaa$Wloc == "H",1,0)


#
teams<-read.csv("Teams.csv",header=TRUE)
ncaa$Season<-as.factor(ncaa$Season)
length(levels(as.factor(ncaa$Wteam))) #355 teams
length(levels(as.factor(ncaa$Lteam))) #355 teams
#364 team codes... some must hsve dropped out
length(teams$Team_Id %in% ncaa$Wteam)
ncaa$Team_Id <- ncaa$Wteam
for(i in 1:nrow(ncaa)) {
  if(ncaa$Numot[i]>0) {
    ncaa$Numot[i]<-1
  } else(ncaa$Numot[i]<-0)
}
ncaa$ot<-as.factor(ncaa$Numot)


length(which(teams$Team_Id %in% ncaa$Wteam))
length(which(teams$Team_Id %in% ncaa$Lteam))

ncaa <- merge(ncaa,teams, by = "Team_Id")[,-1]


#confirmed. The data are. They just are.

#some slight cleaning
#create differentials
ncaa.final<-data.frame(team=ncaa[,37],ncaa[,c(3:7)],winloss=ncaa[,35],ot=ncaa[,36],
                       fgp=ncaa[,9]/ncaa[,10]-ncaa[,22]/ncaa[,23],
                       tpp=ncaa[,11]/ncaa[,12]-ncaa[,24]/ncaa[,25],
                       ftp=ncaa[,13]/ncaa[,14]-ncaa[,26]/ncaa[,27],todiff=ncaa[,18]-ncaa[,31],
                       astdiff=ncaa[,17]-ncaa[,30],stldiff=ncaa[,19]-ncaa[,32],
                       blkdiff=ncaa[,20]-ncaa[,33],pfdiff=ncaa[,21]-ncaa[,34],
                       trdiff=(ncaa[,15]+ncaa[,16])-(ncaa[,28]+ncaa[,29]),season=ncaa[,1])

write.csv(ncaa.final, "ncaa.final.csv")

#should be good to go
