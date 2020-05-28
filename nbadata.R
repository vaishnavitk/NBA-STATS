library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)

#datacleaning

nba_stats<-nba
nba_stats[c('DATE','TEAM')]<-str_split_fixed(nba$MATCHUP, " - ", 2)
nba_stats$OPPOSITION_TEAM<-str_sub(nba_stats$TEAM,start=-3,end=-1)
nba_stats$TEAM<-str_sub(nba_stats$TEAM,start=1,end=3)  
nba_stats<-merge(teams,nba_stats,by.x="TEAM",by.y="TEAM")
nba_stats<-merge(teams,nba_stats,by.x="TEAM",by.y="OPPOSITION_TEAM")
nba_stats<-nba_stats[,-c(1,3,6,10)]
colnames(nba_stats)[2]<-"TEAM"
colnames(nba_stats)[1]<-"OPPOSITION_TEAM"
nba_stats$LOCATION=ifelse(nba_stats$LOCATION=="A","Out-of-State","Home-State")
nba_stats$W=ifelse(nba_stats$W=="W","Won","Lost")
nba_stats$player_name<-toupper(nba_stats$player_name)

#Extracting data and creating data frames

playerinfo<-nba_stats %>% select(player_name,TEAM,OPPOSITION_TEAM,GAME_ID,PTS)
teamstats<-unique(nba_stats%>%select(TEAM,OPPOSITION_TEAM,LOCATION,W,FINAL_MARGIN,GAME_ID))
gametrend<-nba_stats%>%select(GAME_ID,GAME_CLOCK,PTS,OPPOSITION_TEAM,TEAM,PERIOD)
shotrange<-nba_stats%>%select(GAME_ID,SHOT_CLOCK,SHOT_DIST,PERIOD,TEAM,OPPOSITION_TEAM)

#Changing the clock into minutes

gametrend$GAME_CLOCK<-sapply(strsplit(gametrend$GAME_CLOCK,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

#DataWrangling for CLOSEST_DEFENDER column
playerattributes<-nba_stats%>%select(player_name,CLOSEST_DEFENDER,SHOT_RESULT)
playerattributes[c('first','second')]<-str_split_fixed(playerattributes$CLOSEST_DEFENDER, ", ", 2)
playerattributes$Defender<-paste(playerattributes$second, playerattributes$first)
playerattributes<-playerattributes%>%select(-c("first","second","CLOSEST_DEFENDER"))
playerattributes$SHOT_RESULT<-ifelse(playerattributes$SHOT_RESULT=="made",1,0)
playerattributes$Defender=removePunctuation(playerattributes$Defender)

#Grouping the data to calculate the points scored and allowed
points_scored<-playerattributes%>%group_by(player_name)%>%summarise(pts_scored =sum(SHOT_RESULT, na.rm = TRUE))
points_allowed<-playerattributes%>%group_by(Defender)%>%summarise(pts_allowed= sum(SHOT_RESULT, na.rm = TRUE))
names(points_allowed)[1]<- "player_name"
points_allowed$player_name<-toupper(points_allowed$player_name)
playerattributes<- merge(points_scored,points_allowed, by.x="player_name",by.y="player_name")
rm(points_allowed)
rm(points_scored)

#Calculating the contribution of each player towards the team score
playerefficiency<-nba_stats%>%select(GAME_ID,TEAM,W,player_name,PTS)
teamstat<-summarize(group_by(playerefficiency,GAME_ID,TEAM,W),Game_Points=sum(PTS,na.rm=TRUE))
player_style<-summarize(group_by(playerefficiency,GAME_ID,TEAM,player_name),Player_Points=sum(PTS,na.rm=TRUE))
playerefficiency<-merge(teamstat,player_style,by.x=c("TEAM","GAME_ID"),by.y=c("TEAM","GAME_ID"))
playerefficiency$usage_rate<-(playerefficiency$Player_Points/playerefficiency$Game_Points)*100
playerefficiency<-playerefficiency%>%select(TEAM,W,player_name,usage_rate)
playerefficiency<-playerefficiency%>%group_by(TEAM,W,player_name)%>%summarise(usage_rate=mean(usage_rate))
rm(teamstat)
rm(player_style)

#Calculating the frequency of 2 point and 3 point shots
shooting_made<-nba_stats%>%group_by(player_name)%>%count(player_name,PTS)%>%filter(PTS!=0)
total_points<-shooting_made%>%group_by(player_name)%>%summarise(all_points=sum(n))
shooting_made<-merge(shooting_made,total_points,by.x="player_name",by.y="player_name")
shooting_made$all_points<-(shooting_made$n/shooting_made$all_points)*100
shooting_made<-shooting_made[-c(3)]

#To adjust the labels in pie chart 
shooting_made <- shooting_made %>% group_by(player_name)%>%
  arrange(desc(PTS)) %>%
  mutate(lab.ypos = cumsum(all_points) - 0.5*(all_points))

rm(total_points)


