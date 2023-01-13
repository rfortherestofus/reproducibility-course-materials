library(tidyverse)
library(haven)
library(table1)
library(finalfit)
library(knitr)
library(xfun)
globalData2019<-read_spss("Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav")
globalData2019samp <- sample_n(globalData2019,10000)
globalData2019clean<-globalData2019samp%>%filter(country%in%c(3,5,15,19,21,22,27,34))%>%select(country,POLICY_CLIMATE,POLICY_THEWALL,POLICY_NUCLEAR,POLICY_IMMIG,POLICY_TARIFF,POLICY_NORTHKOREA)%>%zap_labels()%>%
mutate(country=recode_factor(country,'3'='Brazil','5'='Canada','15'='Japan','19'='Mexico','21'='Nigeria','22'='Phillippines','27'='South Korea','34'='United States'))%>%
mutate(POLICY_CLIMATE=recode_factor(POLICY_CLIMATE,'1'='Approve','2'='Disapprove','8'=NA_character_,'9'=NA_character_))%>%mutate(POLICY_THEWALL=recode_factor(POLICY_THEWALL,'1'='Approve','2'='Disapprove','8'=NA_character_,'9'=NA_character_))%>%mutate(POLICY_NUCLEAR=recode_factor(POLICY_NUCLEAR,'1'='Approve','2'='Disapprove','8'=NA_character_,'9'=NA_character_))%>%
mutate(POLICY_IMMIG=recode_factor(POLICY_IMMIG,'1'='Approve','2'='Disapprove','8'=NA_character_,'9'=NA_character_))%>%mutate(POLICY_TARIFF=recode_factor(POLICY_TARIFF,'1'='Approve','2'='Disapprove','8'=NA_character_,'9'=NA_character_))%>%
mutate(POLICY_NORTHKOREA=recode_factor(POLICY_NORTHKOREA,'1'='Approve','2'='Disapprove','8'=NA_character_,'9'=NA_character_))
summary(globalData2019clean)
label(globalData2019clean$country)<-"Country"
label(globalData2019clean$POLICY_CLIMATE)<-"U.S. withdrawal from international climate change agreements" 
label(globalData2019clean$POLICY_THEWALL)<-"Building a wall on the border between the U.S. and Mexico"
label(globalData2019clean$POLICY_NUCLEAR)<-"U.S. withdrawal from the Iran nuclear weapons agreement" 
label(globalData2019clean$POLICY_IMMIG)<-"Allowing fewer immigrants into the U.S."
label(globalData2019clean$POLICY_TARIFF)<-"U.S. increasing tariffs or fees on imported goods from other countries" 
label(globalData2019clean$POLICY_NORTHKOREA)<-"U.S. negotiations with North Korean leader Kim Jong-Un about the country’s nuclear weapons program"
tableDesc<-globalData2019clean%>%summary_factorlist(explanatory=c('POLICY_CLIMATE','POLICY_THEWALL','POLICY_NUCLEAR','POLICY_IMMIG','POLICY_TARIFF','POLICY_NORTHKOREA'))
kable(tableDesc,row.names=FALSE,col.names=c("Characteristic","Category","n (%)"),align=c("l","l","r"))
globalData2019clean%>%drop_na(POLICY_CLIMATE)%>%group_by(country,POLICY_CLIMATE)%>%count()%>%group_by(country)%>%mutate(percCo=100*(n/sum(n)))%>%ggplot(aes(x=country,y=percCo,fill=POLICY_CLIMATE))+geom_col(position="dodge")+coord_flip()+labs(caption="Data source: Pew Research Center Global Attitudes Survey (2019)",fill="U.S. withdrawal\nclimate\nagreement",y="Percent in Country")+theme_bw(base_size=10,base_family="serif")
globalData2019clean%>%drop_na(POLICY_THEWALL)%>%group_by(country,POLICY_THEWALL)%>%count()%>%group_by(country)%>%mutate(percCo=100*(n/sum(n)))%>%ggplot(aes(x=country,y=percCo,fill=POLICY_THEWALL))+geom_col(position="dodge")+coord_flip()+labs(caption="Data source: Pew Research Center Global Attitudes Survey (2019)",fill="Build wall\nbetween U.S.\nand Mexico",y="Percent in Country")+theme_bw(base_size=10,base_family="serif")
globalData2019clean%>%drop_na(POLICY_NUCLEAR)%>%group_by(country,POLICY_NUCLEAR)%>%count()%>%group_by(country)%>%mutate(percCo=100*(n/sum(n)))%>%ggplot(aes(x=country,y=percCo,fill=POLICY_NUCLEAR))+geom_col(position="dodge")+coord_flip()+labs(caption="Data source: Pew Research Center Global Attitudes Survey (2019)",fill="U.S. withdrawal\nIran\nagreement",y="Percent in Country")+theme_bw(base_size=10,base_family="serif")
globalData2019clean%>%drop_na(POLICY_IMMIG)%>%group_by(country,POLICY_IMMIG)%>%count()%>%group_by(country)%>%mutate(percCo=100*(n/sum(n)))%>%ggplot(aes(x=country,y=percCo,fill=POLICY_IMMIG))+geom_col(position="dodge")+coord_flip()+labs(caption="Data source: Pew Research Center Global Attitudes Survey (2019)",fill="Fewer immigrants\nallowed in U.S.",y="Percent in Country")+theme_bw(base_size=10,base_family="serif")
globalData2019clean%>%drop_na(POLICY_TARIFF)%>%group_by(country,POLICY_TARIFF)%>%count()%>%group_by(country)%>%mutate(percCo=100*(n/sum(n)))%>%ggplot(aes(x=country,y=percCo,fill=POLICY_TARIFF))+geom_col(position="dodge")+coord_flip()+labs(caption="Data source: Pew Research Center Global Attitudes Survey (2019)",fill="U.S. increase\nimport tarrifs",y="Percent in Country")+theme_bw(base_size=10,base_family="serif")
globalData2019clean%>%drop_na(POLICY_NORTHKOREA)%>%group_by(country,POLICY_NORTHKOREA)%>%count()%>%group_by(country)%>%mutate(percCo=100*(n/sum(n)))%>%ggplot(aes(x=country,y=percCo,fill=POLICY_NORTHKOREA))+geom_col(position="dodge")+coord_flip()+labs(caption="Data source: Pew Research Center Global Attitudes Survey (2019)",fill="U.S. nuclear\nnegotiations with\nNorth Korea",y="Percent in Country")+theme_bw(base_size=10,base_family="serif")
