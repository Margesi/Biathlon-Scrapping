library(RSelenium)
library(magrittr)
library(rvest)
library(tidyverse)
library(plyr)

rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")

remDr <- rD[["client"]]

results<- function(season,gender,wcnr) {
  
remDr$navigate(paste0("https://www.realbiathlon.com/races.html?raceId=BT",season,"SWRLCP0",wcnr,"S",gender,"SP&localtime=false&level=1"))
Sys.sleep(8)

source1<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()

res <-source1[[1]][-1,] 
}

##year must be the ending year for the season example season 1920 is year 2020
mean(all_df$skiing[all_df$top3==1])
shooting<- function(season,gender,wcnr,year) {
remDr$navigate(paste0("https://www.realbiathlon.com/races.html?raceId=BT",season,"SWRLCP0",wcnr,"S",gender,"SP&year=",year,"&level=1&discipline=Total&category=shooting&relative=total&overview=false&min=120&chart=false&stat=&rank=false&movingavg=none&localtime=false&compare=total"))
  Sys.sleep(8)
  source2<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
res1<-source2[[1]][-1,] 

}


##results
results1<-list()
for (i in 1:9) {
  if(!(i %in% c(7,8))) {
results1[[i]]<-results("2021","W",as.character(i))
  }
}

# assign function within loop

for(i in 1:length(results1)) {
  if(!(i %in% c(7,8))) {
    results1[[i]]<-results1[[i]] %>% separate(`Total Time` ,c("min","sec"), sep=":")
    results1[[i]][[8]]<-as.numeric(results1[[i]][[8]])
    results1[[i]][[9]]<-as.numeric(results1[[i]][[9]])
    results1[[i]]$time_sec<-results1[[i]][[8]]*60+results1[[i]][[9]]
  }
}
#shootings
# shootingsstat<-list()
# 
# for (i in 1:5) {
#   shootingsstat[[i]]<-shooting("1920","W",as.character(i),"2020")
# }

shootingsstat21<-list()

for (i in 1:9) {
  if(i != 7 & i!=8) {
  shootingsstat21[[i]]<-shooting("2021","W",as.character(i),"2021")

  }
}

# for(i in 1:length(shootingsstat)) {  
#  
#   names(shootingsstat[[i]])<-make.unique(names(shootingsstat[[i]]))
#   names(shootingsstat[[i]])<-make.unique(names(shootingsstat[[i]]))
#   shootingsstat[[i]]<-separate(shootingsstat[[i]],`Shooting 1`, c("t1", "t2","t3","t4","t5","t6"), "s")
#   shootingsstat[[i]]<-separate(shootingsstat[[i]],`Shooting 2`, c("t11", "t22","t33","t44","t55","t66"), "s")
#   shootingsstat[[i]][[c(13:21)]]<-as.numeric(shootingsstat[[i]][[c(13:21)]])
#   shootingsstat[[i]]$fullname<-paste0(shootingsstat[[i]]$`Given Name`," ",shootingsstat[[i]]$`Family Name`)
#   assign(paste0("SHW1920_", i), shootingsstat[[i]])
#   
# }
for(i in 1:length(shootingsstat21)) {  
  if(i != 7 & i!=8) {
  names(shootingsstat21[[i]])<-make.unique(names(shootingsstat21[[i]]))
  names(shootingsstat21[[i]])<-make.unique(names(shootingsstat21[[i]]))
  shootingsstat21[[i]]<-separate(shootingsstat21[[i]],`Shooting 1`, c("t1", "t2","t3","t4","t5","t6"), "s")
  shootingsstat21[[i]]<-separate(shootingsstat21[[i]],`Shooting 2`, c("sh1", "sh2","sh3","sh4","sh6","hitmiss"), "s")
  shootingsstat21[[i]]$fullname<-paste0(shootingsstat21[[i]]$`Given Name`," ",shootingsstat21[[i]]$`Family Name`)
  for (j in 7:21) {
    shootingsstat21[[i]][[j]]<-as.numeric(str_trim(shootingsstat21[[i]][[j]]))
  }
  assign(paste0("SHW19211_", i), shootingsstat21[[i]])
  
  }
}


# as.numeric(str_trim(shootingsstat21[[1]][[9]]))
# shootingsstat21[[9]]

# prove2<-trimws(c(shootingsstat21[[1]][[9]],which = "both"))
# as.numeric(trimws(prove2[1],which = "l"))
# as.numeric(prove2)
# prove2[1]
# trim <- function (x) gsub(" ^\\s+|\\s+$", "", x)
# trim.leading <- function (x)  sub("^\\s+", "", x)
# gsub(" ", "", c(shootingsstat21[[1]][[9]]))




remDr$navigate("https://www.biathlonworld.com/competitions/world-cup/standings/season/2021/level/1/score/sp/gender/m/")
  namesm<-remDr$getPageSource()[[1]] %>% 
    read_html() %>% html_nodes(".dcm-athlete--name-given") %>% html_text()
  ranksm<-c(1:length(namesw))
 countrym<-remDr$getPageSource()[[1]] %>% 
   read_html() %>% html_nodes(".standings-item__country-name") %>% html_text()
rankingtm<-data.frame(rank=ranksm,athlete=namesm,country=countrym)

remDr$navigate("https://www.biathlonworld.com/competitions/world-cup/standings/season/2021/level/1/score/sp/gender/w/")
namesw<-remDr$getPageSource()[[1]] %>% 
  read_html() %>% html_nodes(".dcm-athlete--name-given") %>% html_text()
ranksw<-c(1:length(namesw))
countryw<-remDr$getPageSource()[[1]] %>% 
  read_html() %>% html_nodes(".standings-item__country-name") %>% html_text()
rankingtw<-data.frame(rank=ranksw,athlete=namesw,country=countryw)

top10<-namesw[1:10]
top10f<-substring(top10, regexpr(" ", top10) + 1)
top10full<- c("Tiril Eckhoff", "Marte Olsbu Roeiseland","Hanna Oeberg", "Dorothea Wierer",
              "Anais Chevalier-Bouchet", "Dzinara Alimbekava", "Franziska Preuss", "Lisa Theresa Hauser",
              "Denise Herrmann", "Ingrid Landmark Tandrevold")
top10last<-(c("Eckhoff","Roeiseland","Oeberg","Wierer","Chevalier-Bouchet","Alimbekava","Preuss","Hauser","Herrmann","Tandrevold"))
total=0
top3<-namesw[1:3]
top3f<-top10f[1:3]
top3last<-top10last[1:3]
top3<-c("Tiril Eckhoff","Marte Olsbu Roeiseland", "Hanna Oeberg")
for (i in 1:length(shootingsstat21)) {
  if(!(i%in%c(7,8))) {
  total=mean(shootingsstat21[[i]]$Time.1[!(shootingsstat21[[i]]$`Given Name` %in%top10f&shootingsstat21[[i]]$`Family Name`%in%top10last)],na.rm = TRUE)+mean(shootingsstat21[[i]]$Time[!(shootingsstat21[[i]]$`Given Name`%in%top10f&shootingsstat21[[i]]$`Family Name`%in%top10last)], na.rm = TRUE)+total
  }
}
avgshootime<-total/14
avgshootime


total1=0
for (i in 1:length(shootingsstat21)) {
  if(!(i%in%c(7,8))) {
  total1= mean(shootingsstat21[[i]]$Time.1[shootingsstat21[[i]]$`Given Name` %in%top10f&shootingsstat21[[i]]$`Family Name`%in%top10last],na.rm = TRUE)+mean(shootingsstat21[[i]]$Time[(shootingsstat21[[i]]$`Given Name`%in%top10f&shootingsstat21[[i]]$`Family Name`%in%top10last)], na.rm = TRUE)+total1
  }
}

avgshootimetop<-total1/14
avgshootimetop

newdf<-data.frame()
for (i in 1:length(shootingsstat21)) {
  if(!(i%in%c(7,8))) {
    newdf<- rbind(newdf, shootingsstat21[[i]] %>% filter(`Given Name` %in%top10f&`Family Name`%in%top10last))
  
  }
}


newdf2<-data.frame()
for (i in 1:length(shootingsstat21)) {
  if(!(i%in%c(7,8))) {
    newdf2<- rbind(newdf2, shootingsstat21[[i]] %>% filter(!(`Given Name` %in%top10f&`Family Name`%in%top10last)))
    
  }
}


(fig_1<-newdf %>% ggplot(aes(x=Time))+geom_density()+geom_vline(aes(xintercept=mean(Time)),
                                                       color="blue", linetype="dashed", size=1)+labs(title = "Total time distribution of top 10 woman biathlon athletes,laying"))
(fig_11<-newdf %>% ggplot(aes(x=Time.1))+geom_density()+geom_vline(aes(xintercept=mean(Time.1)),
                                                                color="blue", linetype="dashed", size=1)+labs(title = "Total time distribution of top 10 woman biathlon athletes,standing"))

(fig_2<-newdf2 %>% ggplot(aes(x=Time))+geom_density()+geom_vline(aes(xintercept=mean(Time, na.rm=TRUE)),
                                                         color="blue", linetype="dashed", size=1)+labs(title = "Total time distribution the rest woman Biathlon athletes,laying"))

(fig_22<-newdf2 %>% ggplot(aes(x=Time.1))+geom_density()+geom_vline(aes(xintercept=mean(Time.1, na.rm=TRUE)),
                                                                 color="blue", linetype="dashed", size=1)+labs(title = "Total time distribution the rest woman Biathlon athletes,standing"))



newdf %>%  ggplot(aes(x=`Given Name`,  y=mean(Time,na.rm=TRUE)))+
  geom_bar(stat="identity", color="black")


# for (i in 1:length(results1)) {
#   if(!(i %in% c(7,8))) {
#     results1[[i]] <- results1[[i]] %>% inner_join(shootingsstat21[[i]])
#   }
  
# }



# for (i in 1:length(shootingsstat21)) {
#   if(!(i %in% c(7,8))) {
#     shootingsstat21[[i]]<- shootingsstat21[[i]] %>% rename(Rank=shrank)
#   }
# }
all_list<-list()
all_df<-data.frame()
all_topdf<-data.frame()
all_restdf<-data.frame()

for (i in 1:length(results1)) {
  if(!(i %in% c(7,8))) {
    shootingsstat21[[i]]<-shootingsstat21[[i]] %>% mutate(shooting_total=Time.1+Time)
    
    shootingsstat21[[i]]$shoot_rank[order(shootingsstat21[[i]]$shooting_total)]<-1:nrow(shootingsstat21[[i]])

    
    all_list[[i]]<-results1[[i]] %>% left_join(shootingsstat21[[i]])
all_list[[i]]$skiing<-all_list[[i]]$time_sec-all_list[[i]]$Time-all_list[[i]]$Time.1
all_list[[i]]$skiing_rank[order(all_list[[i]]$skiing)]<-1:nrow(all_list[[i]])

all_list[[i]]$competition<-factor(i)
all_list[[i]]$top<-factor(ifelse(all_list[[i]]$`Given Name` %in%top10f&all_list[[i]]$`Family Name`%in%top10last,1,0))
all_list[[i]]$top3<-factor(ifelse(all_list[[i]]$fullname%in%top3,1,0))

all_df<-rbind(all_df,all_list[[i]])
all_topdf<-rbind(all_topdf, all_list[[i]] %>% filter(`Given Name` %in%top10f&`Family Name`%in%top10last))
all_restdf<-rbind(all_topdf, all_list[[i]] %>% filter(!(`Given Name` %in%top10f&`Family Name`%in%top10last)))

  }
}






all_df %>% ggplot(aes(x=skiing,col=top)) +geom_density(alpha=0.25)+labs(title="Skiing Performance of top 10 ranked women athletes vs the rest") 



all_df %>% filter(`Given Name` %in%top3f&`Family Name`%in%top3last) %>%  group_by(Time.1,Time,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = competition, y =Time.1+Time ,group=fullname,colour=fullname)) +
  geom_line()+ geom_point()+ geom_text(aes(label=Rank),hjust=0, vjust=0)+labs(title = "Changing in shooting performance and their competition ranking throughout the season for top 3 Athletes", x="World Cup", y="Total shooting time")
all_df %>% filter(`Given Name` %in%top3f&`Family Name`%in%top3last) %>%  group_by(shoot_rank,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = competition, y =shoot_rank ,group=fullname,colour=fullname)) +
  geom_line()+ geom_point()+ geom_text(aes(label=Rank),hjust=0, vjust=0)+labs(title = "Changing in shooting performance and their competition ranking throughout the season for top 3 Athletes",subtitle = "numbers near vertices show the overall rank on that race", x="World Cup", y="Shooting Rank")


all_df %>% filter(`Given Name` %in%top3f&`Family Name`%in%top3last) %>%  group_by(Time.1,Time,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = competition, y =Time.1+Time ,group=fullname,colour=fullname)) +
  geom_line()+ geom_point()+ geom_text(aes(label=Rank),hjust=0, vjust=0)+labs(title = "Changing in shooting performance and their competition ranking throughout the season for top 3 Athletes", x="World Cup", y="Total shooting time")


all_df %>% filter(`Given Name` %in%top10f&`Family Name`%in%top10last) %>%  group_by(shoot_rank,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = as.numeric(Rank), y =shoot_rank )) + geom_point()+ stat_smooth(method="lm") + 
  labs(title = "Ranking and ranking on shooting performance for top 10 athletes", x="Rank")
all_df %>% filter(`Given Name` %in%top10f&`Family Name`%in%top10last) %>%  group_by(skiing_rank,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = as.numeric(Rank), y =skiing_rank )) + geom_point()+ stat_smooth(method="lm") + 
  labs(title = "Ranking and ranking on skiing performance for top 10 athletes", x="Rank")

all_df %>% filter(`Given Name` %in%top3f&`Family Name`%in%top3last) %>%  group_by(skiing,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = competition, y =skiing ,group=fullname,colour=fullname)) +
  geom_line()+ geom_point()+ geom_text(aes(label=Rank),hjust=0, vjust=0)+labs(title = "Change skiing performance and their competition ranking throughout the season for top 3 Athletes", x="World Cup", y="Skiing time")

all_df %>% filter(`Given Name` %in%top3f&`Family Name`%in%top3last) %>%  group_by(skiing_rank,competition, Rank, fullname) %>% tally() %>% ggplot( aes(x = competition, y =skiing_rank ,group=fullname,colour=fullname)) +
  geom_line()+ geom_point()+ geom_text(aes(label=Rank),hjust=0, vjust=0)+geom_text(label="Final Rank",x=2.5,y=31.2,show.legend = FALSE)+labs(title = "Change skiing performance and their competition ranking throughout the season for top 3 Athletes",subtitle = "numbers near vertices show the overall rank on that race", x="World Cup", y="Ranked on Skiing only")


all_df %>% ggplot(aes(x=skiing,col=top)) +geom_density(alpha=0.25)+labs(title="Skiing Performance of top 10 ranked women athletes vs the rest") 

all_df %>% ggplot(aes(x=Time.1,col=top)) +geom_density(alpha=0.25)+labs(title="Standing Shooting Time of top 10 ranked women athletes vs the rest") 

all_df %>% ggplot(aes(x=Time,col=top)) +geom_density(alpha=0.25)+labs(title="Laying Shooting Time of top 10 ranked women athletes vs the rest") 

all_df %>% ggplot(aes(x=Time.1,col=top3)) +geom_density(alpha=0.25)+labs(title="Standing Shooting Time of top 3 ranked women athletes vs the rest") 

all_df %>% ggplot(aes(x=Time,col=top3)) +geom_density(alpha=0.25)+labs(title="Laying Shooting Time of top 3 ranked women athletes vs the rest") 

all_df %>% ggplot(aes(x=skiing,col=top3)) +geom_density(alpha=0.25)+labs(title="Skiing Performance of top 3 ranked women athletes vs the rest") 

all_df %>% ggplot(aes(x=Time.1+Time,col=top3)) +geom_density(alpha=0.25)+labs(title="Total Shooting Time of top 3 ranked women athletes vs the rest",x="Total shooting Time") 



#top3




all_topdf %>%  ggplot(aes(x=skiing))+geom_density()+geom_vline(aes(xintercept=mean(skiing, na.rm=TRUE)),
                                                               color="blue", linetype="dashed", size=1)+labs(title = "skiing time distribution top woman Biathlon athletes,standing",x="skiing,seconds")

all_restdf %>%  ggplot(aes(x=skiing))+geom_density()+geom_vline(aes(xintercept=mean(skiing, na.rm=TRUE)),
                                                               color="blue", linetype="dashed", size=1)+labs(title = "skiing time distribution rest woman Biathlon athletes,standing",x="skiing,seconds")
class(newdf2$Time)


class(newdf$Time)


for (i in 1:length(shootingsstat)) {
  top10db= mean(shootingsstat[[i]]$Time.1[shootingsstat[[i]]$`Given Name`%in%top10f],na.rm = TRUE)+mean(shootingsstat[[i]]$Time[shootingsstat[[i]]$`Given Name`%in%top10f], na.rm = TRUE)+total1
}





top10 %>% separate(c(name,given))


shootingsstat[[1]]$fullname  
  
t2<-as.numeric(unlist((shootingsstat[[i]][c(13,21)])))

mean(shootingsstat[[1]]$Time.1,na.rm = TRUE)
shootingsstat[[4]][[21]]
shootingsstat[[1]][[c(13,21)]]

remDr$close()
rm(list = ls())
library(hrbrthemes)


###create a table
mean(all_df$skiing[all_df$top3==1])
mean(all_df$skiing,na.rm=TRUE)
mean(all_df$shooting_total,na.rm=TRUE)
mean(all_df$shooting_total[all_df$top3==1])


top3
top5<-top10full[1:5]
top5
top5last<-top10last[1:5]
top5first<-top10f[1:5]


table1<-data.frame(Athlete=c(top5[1:5],"rest"),season_rank=c(1:5,""),
                   Skiing_average=c(mean(all_df$skiing[all_df$`Family Name`==top5last[1]&all_df$`Given Name`==top5first[1]]),
                                                           mean(all_df$skiing[all_df$`Family Name`==top5last[2]&all_df$`Given Name`==top5first[2]]),
                                                           mean(all_df$skiing[all_df$`Family Name`==top5last[3]&all_df$`Given Name`==top5first[3]]),
                                                           mean(all_df$skiing[all_df$`Family Name`==top5last[4]&all_df$`Given Name`==top5first[4]]),
                                                           mean(all_df$skiing[all_df$`Family Name`==top5last[5]&all_df$`Given Name`==top5first[5]]),
                                                           mean(all_df$skiing[!(all_df$`Family Name`%in%top5last[1:5]&all_df$`Given Name`%in%top5first[1:5])],na.rm = TRUE)),
                   Shooting_average=c(mean(all_df$shooting_total[all_df$`Family Name`==top5last[1]&all_df$`Given Name`==top5first[1]]),  
                                   mean(all_df$shooting_total[all_df$`Family Name`==top5last[2]&all_df$`Given Name`==top5first[2]]),
                               mean(all_df$shooting_total[all_df$`Family Name`==top5last[3]&all_df$`Given Name`==top5first[3]]),
                               mean(all_df$shooting_total[all_df$`Family Name`==top5last[4]&all_df$`Given Name`==top5first[4]]),
                               mean(all_df$shooting_total[all_df$`Family Name`==top5last[5]&all_df$`Given Name`==top5first[5]]),
                               mean(all_df$shooting_total[!(all_df$`Family Name`%in%top5last[1:5]&all_df$`Given Name`%in%top5first[1:4])],na.rm = TRUE)))

diff_rest<-vector()
diff_rest1<-vector()
for (i in 1:5) {
  diff_rest[i]<-table1$Skiing_average[6]-table1$Skiing_average[i]
  diff_rest1[i]<-table1$Shooting_average[6]-table1$Shooting_average[i]
  
}
diff_rest<-format(round(diff_rest, 2), nsmall = 2)
diff_rest1<-format(round(diff_rest1, 2), nsmall = 2)

table2<-data.frame(Athlete=c(top5[1:5],"rest"),season_rank=c(1:5,""),
                   Skiing_average=c(mean(all_df$skiing[all_df$`Family Name`==top5last[1]&all_df$`Given Name`==top5first[1]]),
                                    mean(all_df$skiing[all_df$`Family Name`==top5last[2]&all_df$`Given Name`==top5first[2]]),
                                    mean(all_df$skiing[all_df$`Family Name`==top5last[3]&all_df$`Given Name`==top5first[3]]),
                                    mean(all_df$skiing[all_df$`Family Name`==top5last[4]&all_df$`Given Name`==top5first[4]]),
                                    mean(all_df$skiing[all_df$`Family Name`==top5last[5]&all_df$`Given Name`==top5first[5]]),
                                    mean(all_df$skiing[!(all_df$`Family Name`%in%top5last[1:5]&all_df$`Given Name`%in%top5first[1:5])],na.rm = TRUE)),
                   avg_diff_rest=c(diff_rest,""),
                   Shooting_average=c(mean(all_df$shooting_total[all_df$`Family Name`==top5last[1]&all_df$`Given Name`==top5first[1]]),  
                                      mean(all_df$shooting_total[all_df$`Family Name`==top5last[2]&all_df$`Given Name`==top5first[2]]),
                                      mean(all_df$shooting_total[all_df$`Family Name`==top5last[3]&all_df$`Given Name`==top5first[3]]),
                                      mean(all_df$shooting_total[all_df$`Family Name`==top5last[4]&all_df$`Given Name`==top5first[4]]),
                                      mean(all_df$shooting_total[all_df$`Family Name`==top5last[5]&all_df$`Given Name`==top5first[5]]),
                                      mean(all_df$shooting_total[!(all_df$`Family Name`%in%top5last[1:5]&all_df$`Given Name`%in%top5first[1:4])],na.rm = TRUE)),
                   av_diff_rest=c(diff_rest1,""))
formattable(table2)
 xtable(table1,type="html")                                                     
 library(kableExtra)
 table1%>%
   kbl() %>%
   kable_material_dark()
 