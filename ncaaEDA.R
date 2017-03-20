setwd("~/STAT 539/Project")
ncaa<-read.csv("RegularSeasonDetailedResults.csv",header=TRUE)
teams<-read.csv("Teams.csv",header=TRUE)
ncaa$Season<-as.factor(ncaa$Season)
length(levels(as.factor(ncaa$Wteam))) #355 teams
length(levels(as.factor(ncaa$Lteam))) #355 teams
#364 team codes... some must hsve dropped out
length(teams$Team_Id %in% ncaa$Wteam)
ncaa$Team_Id <- ncaa$Wteam

length(which(teams$Team_Id %in% ncaa$Wteam))
length(which(teams$Team_Id %in% ncaa$Lteam))

ncaa <- merge(ncaa,teams, by = "Team_Id")[,-1]


#confirmed. The data are. They just are.

#some slight cleaning
#create differentials
ncaa.final<-data.frame(team=ncaa[,36],ncaa[,-c(2,9:35)],wfgp=ncaa[,9]/ncaa[,10],
                       lfgp=ncaa[,22]/ncaa[,23],w3p=ncaa[,11]/ncaa[,12],
                       l3p=ncaa[,24]/ncaa[,25],wftp=ncaa[,13]/ncaa[,14],
                       lftp=ncaa[,26]/ncaa[,27],
                       trdiff=(ncaa[,15]+ncaa[,16])-(ncaa[,28]+ncaa[,29]),
                       astdiff=ncaa[,17]-ncaa[,30],tdiff=ncaa[,18]-ncaa[,31],
                       sdiff=ncaa[,19]-ncaa[,32],blkdiff=ncaa[,20]-ncaa[,33],
                       pfdiff=ncaa[,21]-ncaa[,34])

write.csv(ncaa.final, "ncaa.final.csv")
